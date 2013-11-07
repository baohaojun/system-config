//
// BuildIndex.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Text.RegularExpressions;
using System.Threading;
using System.Reflection;

using System.Xml;
using System.Xml.Serialization;

using Lucene.Net.Documents;
using Lucene.Net.Index;
using LNS = Lucene.Net.Search;

using Beagrep;
using Beagrep.Util;

using Stopwatch = Beagrep.Util.Stopwatch;
using FSQ = Beagrep.Daemon.FileSystemQueryable.FileSystemQueryable;

// Assembly information
[assembly: AssemblyTitle ("beagrep-build-index")]
[assembly: AssemblyDescription ("Build an index.")]

namespace Beagrep.Daemon
{
        class BuildIndex
        {
                static bool arg_recursive, arg_delete, arg_debug;
                static bool arg_cache_text, arg_disable_filtering;
                static bool arg_disable_restart, arg_disable_directories;
                static string arg_output, arg_tag, arg_source;

                /////////////////////////////////////////////////////////


                /////////////////////////////////////////////////////////

                // 1. Different handling of --disable-directories
                const int MINOR_VERSION = 1;

                // Files and directories that are allowed to be in the target
                // directory before we blow it away.  If we encounter any file
                // or dir not in this list, we'll bail out.
                static string [] allowed_files = {
                        "FileAttributesStore.db",
                        "fingerprint",
                        "version",
                        "filterver.dat",
                        "StaticIndex.xml"
                };

                static string [] allowed_dirs = {
                        "config",
                        "Locks",
                        "PrimaryIndex",
                        "SecondaryIndex",
                        "TextCache"
                };

                /////////////////////////////////////////////////////////

                static FileAttributesStore_Sqlite backing_fa_store;
                static FileAttributesStore fa_store;

                static LuceneIndexingDriver driver;

                static bool indexing = true, restart = false;

                static Regex allowed_regex = null;
                static Regex denied_regex = null;
                static Regex denied_dir_regex = null;

                static Queue pending_files = new Queue ();
                static Queue pending_directories = new Queue ();
                static IndexerRequest pending_request;

                const int BATCH_SIZE = Lucene.Net.Index.IndexWriter.DEFAULT_MAX_BUFFERED_DOCS;

                /////////////////////////////////////////////////////////

                static void Main (string [] args)
                {
                        try {
                                DoMain (args);
                        } catch (Exception ex) {
                                Logger.Log.Error (ex, "Unhandled exception thrown.  Exiting immediately.");
                                Environment.Exit (1);
                        }
                }

                static void DoMain (string [] args)
                {
                        SystemInformation.SetProcessName ("beagrep-build-index");

                        if (args.Length < 2)
                                PrintUsage ();

                        ArrayList allowed_patterns = new ArrayList ();
                        ArrayList denied_patterns = new ArrayList ();
                        ArrayList denied_dir_patterns = new ArrayList ();

                        int i = 0;
                        while (i < args.Length) {

                                string arg = args [i];
                                ++i;
                                string next_arg = i < args.Length ? args [i] : null;

                                switch (arg) {
                                case "-h":
                                case "--help":
                                        PrintUsage ();
                                        break;

                                case "--tag":
                                        if (next_arg != null)
                                                arg_tag = next_arg;
                                        ++i;
                                        break;

                                case "-r":
                                case "--recursive":
                                        arg_recursive = true;
                                        break;

                                case "--enable-deletion":
                                        arg_delete = true;
                                        break;

                                case "--disable-directories":
                                        arg_disable_directories = true;
                                        break;

                                case "--enable-text-cache":
                                        arg_cache_text = true;
                                        break;

                                case "--target":
                                        if (next_arg != null)
                                                arg_output = Path.IsPathRooted (next_arg) ? next_arg : Path.GetFullPath (next_arg);
                                        ++i;
                                        break;

                                case "--disable-filtering":
                                        arg_disable_filtering = true;
                                        break;

                                case "--allow-pattern":
                                        if (next_arg == null)
                                                break;

                                        if (next_arg.IndexOf (',') != -1) {
                                                foreach (string pattern in next_arg.Split (','))
                                                        allowed_patterns.Add (pattern);

                                        } else {
                                                allowed_patterns.Add (next_arg);
                                        }

                                        ++i;
                                        break;

                                case "--deny-directory-pattern":
                                        if (next_arg == null)
                                                break;

                                        if (next_arg.IndexOf (',') != -1) {
                                                foreach (string pattern in next_arg.Split (','))
                                                        denied_dir_patterns.Add (pattern);
                                        } else {
                                                denied_dir_patterns.Add (next_arg);
                                        }

                                        ++i;
                                        break;

                                case "--deny-pattern":
                                        if (next_arg == null)
                                                break;

                                        if (next_arg.IndexOf (',') != -1) {
                                                foreach (string pattern in next_arg.Split (','))
                                                        denied_patterns.Add (pattern);

                                        } else {
                                                denied_patterns.Add (next_arg);
                                        }

                                        ++i;
                                        break;

                                case "--disable-restart":
                                        arg_disable_restart = true;
                                        break;

                                case "--source":
                                        if (next_arg == null)
                                                break;

                                        arg_source = next_arg;
                                        ++i;
                                        break;

                                default:
                                        if (arg.StartsWith ("-") || arg.StartsWith ("--"))
                                                PrintUsage ();

                                        string path = Path.IsPathRooted (arg) ? arg : Path.GetFullPath (arg);
                                        if (path != "/" && path.EndsWith ("/"))
                                                path = path.TrimEnd ('/');

                                        if (Directory.Exists (path))
                                                pending_directories.Enqueue (new DirectoryInfo (path));
                                        else if (File.Exists (path))
                                                pending_files.Enqueue (new FileInfo (path));
                                        break;
                                }
                        }

                        /////////////////////////////////////////////////////////

                        if (arg_output == null) {
                                Logger.Log.Error ("--target must be specified");
                                Environment.Exit (1);
                        }

                        // Set the storage dir, this should be used to store log messages
                        // and filterver.dat
                        PathFinder.StorageDir = arg_output;

                        foreach (FileSystemInfo info in pending_directories) {
                                if (Path.GetFullPath (arg_output) == info.FullName) {
                                        Logger.Log.Error ("Target directory cannot be one of the source paths.");
                                        Environment.Exit (1);
                                }
                        }

                        foreach (FileSystemInfo info in pending_files) {
                                if (Path.GetFullPath (arg_output) == info.FullName) {
                                        Logger.Log.Error ("Target directory cannot be one of the source paths.");
                                        Environment.Exit (1);
                                }
                        }

                        if (!Directory.Exists (Path.GetDirectoryName (arg_output))) {
                                Logger.Log.Error ("Index directory not available for construction: {0}", arg_output);
                                Environment.Exit (1);
                        }

                        // Be *EXTRA PARANOID* about the contents of the target
                        // directory, because creating an indexing driver will
                        // nuke it.
                        if (Directory.Exists (arg_output)) {

                                foreach (FileInfo info in DirectoryWalker.GetFileInfos (arg_output)) {
                                        if (Array.IndexOf (allowed_files, info.Name) == -1) {
                                                Logger.Log.Error ("{0} doesn't look safe to delete: non-Beagrep file {1} was found", arg_output, info.FullName);
                                                Environment.Exit (1);
                                        }
                                }

                                foreach (DirectoryInfo info in DirectoryWalker.GetDirectoryInfos (arg_output)) {
                                        if (Array.IndexOf (allowed_dirs, info.Name) == -1) {
                                                Logger.Log.Error ("{0} doesn't look safe to delete: non-Beagrep directory {1} was found", arg_output, info.FullName);
                                                Environment.Exit (1);
                                        }
                                }
                        }

                        string config_file_path = Path.Combine (arg_output, "StaticIndex.xml");
                        string prev_source = null;
                        if (File.Exists (config_file_path)) {
                                Config static_index_config = Conf.LoadFrom (config_file_path);
                                if (static_index_config == null) {
                                        Log.Error ("Invalid configuation file {0}", config_file_path);
                                        Environment.Exit (1);
                                }

                                prev_source = static_index_config.GetOption ("Source", null);
                                if (arg_source != null && prev_source != arg_source) {
                                        Log.Error ("Source already set to {0} for existing static index. Cannot set source to {1}.", prev_source, arg_source);
                                        Environment.Exit (1);
                                }

                                // If arg_source is not given, and prev_source is present, use prev_source
                                // as the arg_source. This is useful for re-running build-index without
                                // giving --arg_source for already existing static index
                                arg_source = prev_source;
                        }


                        if (arg_source == null) {
                                DirectoryInfo dir = new DirectoryInfo (StringFu.SanitizePath (arg_output));
                                arg_source = dir.Name;
                        }

                        string global_files_config = Path.Combine (PathFinder.ConfigDataDir, "config-files");
                        global_files_config = Path.Combine (global_files_config, Conf.Names.FilesQueryableConfig + ".xml");
                        if (! File.Exists (global_files_config)) {
                                Log.Error ("Global configuration file not found {0}", global_files_config);
                                Environment.Exit (0);
                        }

                        // Setup regexes for allowed/denied patterns
                        if (allowed_patterns.Count > 0) {
                                allowed_regex = StringFu.GetPatternRegex (allowed_patterns);
                        } else {
                                // Read the exclude values from config
                                // For system-wide indexes, only the global config value will be used
                                Config config = Conf.Get (Conf.Names.FilesQueryableConfig);
                                List<string[]> values = config.GetListOptionValues (Conf.Names.ExcludePattern);
                                if (values != null)
                                        foreach (string[] exclude in values)
                                                denied_patterns.Add (exclude [0]);

                                if (denied_patterns.Count > 0)
                                        denied_regex = StringFu.GetPatternRegex (denied_patterns);
                        }

                        if (denied_dir_patterns.Count > 0) {
                                denied_dir_regex = StringFu.GetPatternRegex (denied_dir_patterns);
                                Log.Always("Will ignore directories matching regular expression: {0}", denied_dir_regex);
                        }

                        Log.Always ("Starting beagrep-build-index (pid {0}) at {1}", Process.GetCurrentProcess ().Id, DateTime.Now);

                        // Set system priorities so we don't slow down the system
                        SystemPriorities.ReduceIoPriority ();
                        SystemPriorities.SetSchedulerPolicyBatch ();
                        SystemPriorities.Renice (19);

                        driver = new LuceneIndexingDriver (arg_output, MINOR_VERSION, false);
                        driver.TextCache = (arg_cache_text) ? new TextCache (arg_output) : null;
                        if (driver.TextCache != null)
                                driver.TextCache.WorldReadable = true;

                        backing_fa_store = new FileAttributesStore_Sqlite (driver.TopDirectory, driver.Fingerprint);
                        fa_store = new FileAttributesStore (backing_fa_store);

                        // Set up signal handlers
#if MONO_1_9
                        Shutdown.SetupSignalHandlers (delegate (int signal)
                                                        {
                                                                if (signal == (int) Mono.Unix.Native.Signum.SIGINT ||
                                                                    signal == (int) Mono.Unix.Native.Signum.SIGTERM)
                                                                        Shutdown.BeginShutdown ();
                                                        });
#else
                        SetupSignalHandlers ();
#endif

                        Thread monitor_thread = null;

                        Stopwatch watch = new Stopwatch ();
                        watch.Start ();

                        if (!arg_disable_restart) {
                                // Start the thread that monitors memory usage.
                                monitor_thread = ExceptionHandlingThread.Start (new ThreadStart (MemoryMonitorWorker));
                        }

                        // Start indexworker to do the crawling and indexing
                        IndexWorker ();

                        // Join any threads so that we know that we're the only thread still running
                        if (monitor_thread != null)
                                monitor_thread.Join ();

                        watch.Stop ();
                        Logger.Log.Debug ("Elapsed time {0}.", watch);

                        // Write this after indexing is done. This is because, if creating a new index,
                        // LuceneIndexingDriver.Create() is called which purges the entire directory.

                        if (prev_source == null) {
                                Config static_index_config = Conf.LoadNew ("StaticIndex.xml");

                                // Write StaticIndex.xml containing:
                                // The name of the source
                                static_index_config.SetOption ("Source", arg_source);
                                static_index_config ["Source"].Description = "Source of the static index";


                                Conf.SaveTo (static_index_config, config_file_path);
                        }

                        if (restart) {
                                Logger.Log.Debug ("Restarting beagrep-build-index");
                                Process p = new Process ();
                                p.StartInfo.UseShellExecute = false;
                                // FIXME: Maybe this isn't the right way to do things?  It should be ok,
                                // the PATH is inherited from the shell script which runs mono itself.
                                p.StartInfo.FileName = "mono";
                                p.StartInfo.Arguments = String.Join (" ", Environment.GetCommandLineArgs ());
                                p.Start ();
                        }

                        Log.Always ("Exiting beagrep-build-index (pid {0}) at {1}", Process.GetCurrentProcess ().Id, DateTime.Now);
                }

                /////////////////////////////////////////////////////////////////

                static void IndexWorker ()
                {
                        Logger.Log.Debug ("Starting IndexWorker");

                        try {
                                DoIndexing ();
                        } catch (Exception e) {
                                Logger.Log.Debug ("Encountered exception while indexing: {0}", e);
                        }

                        Logger.Log.Debug ("IndexWorker Done");
                        indexing = false;
                }

                static void DoIndexing ()
                {
                        int count_dirs = 0;
                        int count_files = 0;

                        Indexable indexable;
                        pending_request = new IndexerRequest ();
                        Queue modified_directories = new Queue ();

                        while (pending_directories.Count > 0) {
                                DirectoryInfo dir = (DirectoryInfo) pending_directories.Dequeue ();

                                AddToRequest (DirectoryToIndexable (dir, modified_directories));

                                try {
                                        if (arg_recursive)
                                                foreach (DirectoryInfo subdir in DirectoryWalker.GetDirectoryInfos (dir))
                                                        if (!Ignore (subdir)
                                                            && !FileSystem.IsSpecialFile (subdir.FullName))
                                                                pending_directories.Enqueue (subdir);

                                        foreach (FileInfo file in DirectoryWalker.GetFileInfos (dir))
                                                if (!Ignore (file)
                                                    && !FileSystem.IsSpecialFile (file.FullName)) {
                                                        AddToRequest (FileToIndexable (file));
                                                        count_files ++;
                                                }

                                } catch (DirectoryNotFoundException) {}

                                if (Shutdown.ShutdownRequested)
                                        break;

                                count_dirs++;
                        }

                        Logger.Log.Debug ("Scanned {0} files and directories in {1} directories", count_dirs + count_files, count_dirs);

                        if (Shutdown.ShutdownRequested) {
                                backing_fa_store.Flush ();
                                return;
                        }

                        // Time to remove deleted directories from the index and attributes store
                        while (modified_directories.Count > 0) {
                                DirectoryInfo subdir = (DirectoryInfo) modified_directories.Dequeue ();
                                Logger.Log.Debug ("Checking {0} for deleted files and directories", subdir.FullName);

                                // Get a list of all documents from lucene index with ParentDirUriPropKey set as that of subdir
                                ICollection all_dirent = GetAllItemsInDirectory (subdir);
                                foreach (Dirent info in all_dirent) {
                                        // check if the item exists
                                        if ((! info.IsDirectory && File.Exists (info.FullName)) ||
                                            (info.IsDirectory && Directory.Exists (info.FullName)))
                                                continue;

                                        if (info.IsDirectory)
                                                // Recursively remove deleted subdirectories
                                                modified_directories.Enqueue (new DirectoryInfo (info.FullName));

                                        // remove
                                        Uri uri = PathToUri (info.FullName);
                                        indexable = new Indexable (IndexableType.Remove, uri);
                                        AddToRequest (indexable);
                                }
                        }

                        bool reschedule = false;
                        // Call Flush until our request is empty.  We have to do this in a loop
                        // because Flush happens in a batch size and some indexables might generate more indexables
                        while (reschedule || pending_request.Count > 0) {
                                if (Shutdown.ShutdownRequested)
                                        break;

                                reschedule = FlushIndexer (driver);
                        }

                        backing_fa_store.Flush ();

                        if (Shutdown.ShutdownRequested)
                                return;

                        Logger.Log.Debug ("Optimizing index");
                        driver.OptimizeNow ();
                }

                /////////////////////////////////////////////////////////////////

                static void AddToRequest (Indexable indexable)
                {
                        if (indexable == null)
                                return;

                        // Disable filtering and only index file attributes
                        if (arg_disable_filtering)
                                indexable.Filtering = IndexableFiltering.Never;

                        // Tag the item for easy identification (for say, removal)
                        if (arg_tag != null)
                                indexable.AddProperty (Property.NewUnsearched("Tag", arg_tag));

                        indexable.Source = arg_source;

                        pending_request.Add (indexable);
                        bool reschedule = false;

                        do {
                                if (Shutdown.ShutdownRequested)
                                        break;

                                if (! reschedule && pending_request.Count < BATCH_SIZE)
                                        break;

                                if (reschedule)
                                        Logger.Log.Debug ("Continuing indexing indexer generated indexables");
                                else
                                        Logger.Log.Debug ("Flushing driver, {0} items in queue", pending_request.Count);

                                reschedule = FlushIndexer (driver);

                                // Super Lame Hack: gtk-sharp up to 2.10 requires a main loop
                                // to dispose of any managed wrappers around GObjects.  Since
                                // we don't have one, we'll process all the pending items in
                                // a loop here.  This is particularly an issue with maildirs,
                                // because we need the loop to clean up after GMime.  Without
                                // it, GMime's streams are never completely unref'd, the
                                // file descriptors aren't closed, and we run out and crash.
                                while (GLib.MainContext.Pending ())
                                        GLib.MainContext.Iteration ();

                        } while (reschedule);
                }

                // This is mostly a copy of LuceneQueryable.Flush + FSQ.PostAddHooks/PostRemoveHook
                static bool FlushIndexer (IIndexer indexer)
                {
                        IndexerRequest flushed_request;
                        if (pending_request.IsEmpty)
                                return false;

                        flushed_request = pending_request;
                        pending_request = new IndexerRequest ();

                        IndexerReceipt [] receipts;
                        receipts = indexer.Flush (flushed_request);

                        // Flush will return null if it encounters a shutdown during flushing
                        if (receipts == null)
                                return false;

                        fa_store.BeginTransaction ();
                        bool indexer_indexable_receipt = false;

                        foreach (IndexerReceipt raw_r in receipts) {

                                if (raw_r is IndexerAddedReceipt) {
                                        // Update the file attributes
                                        IndexerAddedReceipt r = (IndexerAddedReceipt) raw_r;

                                        Indexable indexable = flushed_request.RetrieveRequestIndexable (r);

                                        if (indexable == null) {
                                                Logger.Log.Debug ("Should not happen! Previously requested indexable with id #{0} has eloped!", r.Id);
                                                continue;
                                        }

                                        // We don't need to write out any file attributes for
                                        // children.
                                        if (indexable.ParentUri != null)
                                                continue;

                                        string path = indexable.Uri.LocalPath;

                                        FileAttributes attr;
                                        attr = fa_store.ReadOrCreate (path);

                                        attr.LastWriteTime = indexable.Timestamp;
                                        attr.FilterName = r.FilterName;
                                        attr.FilterVersion = r.FilterVersion;

                                        fa_store.Write (attr);

                                } else if (raw_r is IndexerRemovedReceipt) {
                                        // Update the file attributes
                                        IndexerRemovedReceipt r = (IndexerRemovedReceipt) raw_r;

                                        Indexable indexable = flushed_request.RetrieveRequestIndexable (r);
                                        if (indexable == null) { // Should never happen
                                                Log.Warn ("Unable to match indexable-remove #{0} to any request!", r.Id);
                                                continue;
                                        }

                                        string path = indexable.Uri.LocalPath;
                                        Logger.Log.Debug ("Removing: '{0}'", path);
                                        fa_store.Drop (path);

                                } else if (raw_r is IndexerIndexablesReceipt) {
                                        indexer_indexable_receipt = true;
                                }
                        }

                        pending_request.DeferredIndexables = flushed_request.DeferredIndexables;

                        // Reschedule if some indexable generated more indexables
                        if (indexer_indexable_receipt) {
                                pending_request.ContinueIndexing = true;
                                return true;
                        }

                        fa_store.CommitTransaction ();
                        return false;
                }

                static Indexable FileToIndexable (FileInfo file)
                {
                        if (!file.Exists)
                                return null;

                        if (fa_store.IsUpToDateAndFiltered (PathInIndex (file.FullName),
                                                            FileSystem.GetLastWriteTimeUtc (file.FullName)))
                                return null;

                        // Create the indexable and add the standard properties we
                        // use in the FileSystemQueryable.
                        Uri uri = PathToUri (file.FullName);
                        Indexable indexable = new Indexable (uri);
                        indexable.Timestamp = file.LastWriteTimeUtc;
                        indexable.FlushBufferCache = true;
                        indexable.AddProperty (Property.NewUnsearched ("fixme:filesize", file.Length));
                        FSQ.AddStandardPropertiesToIndexable (indexable, file.Name, Guid.Empty, false);

                        // Store directory name in the index
                        string dirname = file.DirectoryName;
                        indexable.AddProperty (Property.NewUnsearched (Property.ParentDirUriPropKey, PathToUri (dirname)));

                        return indexable;
                }

                static Indexable DirectoryToIndexable (DirectoryInfo dir, Queue modified_directories)
                {
                        if (!dir.Exists)
                                return null;

                        // Check if the directory information is stored in attributes store
                        // And if the mtime of the directory is same as that in the attributes store
                        FileAttributes attr = fa_store.Read (PathInIndex (dir.FullName));

                        // If the directory exists in the fa store, then it is already indexed.
                        if (attr != null) {
                                // If we don't care about deleted content then we are fine.
                                // If the attributes are up-to-date, then we are fine too.
                                if (! arg_delete || FileAttributesStore.IsUpToDate (attr, FileSystem.GetLastWriteTimeUtc (dir.FullName)))
                                        return null;

                                // But the last write time needs to be uptodate to support enable-deletion,
                                // so we actually index the directories, even if --disable-directories
                                // is set.
                                modified_directories.Enqueue (dir);
                        }

                        // Create the indexable and add the standard properties we
                        // use in the FileSystemQueryable.
                        Uri uri = PathToUri (dir.FullName);
                        Indexable indexable = new Indexable (uri);
                        indexable.MimeType = "inode/directory";
                        indexable.NoContent = true;
                        indexable.Timestamp = dir.LastWriteTimeUtc;

                        // Store the directory information in the index anyway, but if --disable-directories
                        // was passed, then do not store the names and other standard properties
                        // used during searching
                        if (! arg_disable_directories)
                                FSQ.AddStandardPropertiesToIndexable (indexable, dir.Name, Guid.Empty, false);

                        // Add directory name property
                        string dirname = dir.Parent.FullName;
                        indexable.AddProperty (Property.NewUnsearched (Property.ParentDirUriPropKey, PathToUri (dirname)));

                        indexable.AddProperty (Property.NewBool (Property.IsDirectoryPropKey, true));

                        return indexable;
                }

                static string PathInIndex (string fullpath)
                {
                        return fullpath;
                }

                static Uri PathToUri (string fullpath)
                {
                        return UriFu.PathToFileUri (fullpath);
                }

                ///////////////////////////////////////////////////

                class Dirent {
                        private bool is_directory;
                        private string path;

                        public Dirent (string path, bool is_dir)
                        {
                                this.path = path;
                                this.is_directory = is_dir;
                        }

                        public bool IsDirectory {
                                get { return is_directory; }
                        }

                        public string Path {
                                get { return path; }
                        }

                        public string FullName {
                                get { return path.Substring (7); }
                        }
                }

                private class BitArrayHitCollector : LNS.HitCollector {

                        private BetterBitArray matches;

                        public BitArrayHitCollector (BetterBitArray matches)
                        {
                                this.matches = matches;
                        }

                        public override void Collect (int id, float score)
                        {
                                matches [id] = true;
                        }
                }

                // Returns a list of all files and directories in dir
                static ICollection GetAllItemsInDirectory (DirectoryInfo dir)
                {
                        // form the query
                        string parent_uri_str = PathToUri (dir.FullName).ToString ();

                        // Instead of taking the painfull way of using BeagrepAnalyzer, lets just add the prefix manually
                        // LuceneCommon thinks exposing secret property type encoding is bad, I think so too... except for now
                        string key = "prop:k:" + Property.ParentDirUriPropKey;
                        //Logger.Log.Debug ("Querying for {0}={1}", parent_uri_str, key);
                        LNS.Query query = new LNS.TermQuery (new Term (key, parent_uri_str));

                        // do the search
                        LNS.IndexSearcher searcher;
                        searcher = LuceneCommon.GetSearcher (driver.PrimaryStore);

                        BetterBitArray matches;
                        matches = new BetterBitArray (searcher.MaxDoc ());

                        BitArrayHitCollector collector;
                        collector = new BitArrayHitCollector (matches);

                        searcher.Search (query, null, collector);

                        // Finally we pull all of the matching documents,
                        // convert them to Dirent, and store them in a list.

                        ArrayList match_list = new ArrayList ();
                        int i = 0;
                        while (i < matches.Count) {

                                i = matches.GetNextTrueIndex (i);
                                if (i >= matches.Count)
                                        break;

                                Document doc;
                                doc = searcher.Doc (i);

                                Dirent info;
                                info = DocumentToDirent (doc);

                                match_list.Add (info);

                                ++i;
                        }

                        LuceneCommon.ReleaseSearcher (searcher);
                        //Logger.Log.Debug ("Found {0} items in {1}", match_list.Count, dir.FullName);

                        return match_list;
                }

                static private Dirent DocumentToDirent (Document doc)
                {
                        string path;
                        bool is_dir = false;

                        path = doc.Get ("Uri");

                        string prop_key = "prop:k:" + Property.IsDirectoryPropKey;
                        foreach (Field f in doc.Fields ()) {
                                if (f.Name () != prop_key)
                                        continue;

                                // Format of fields: from LuceneCommon.cs:AddPropertyToDocument
                                is_dir = (f.StringValue ().Substring (3) == "true");
                                break;
                        }

                        //Logger.Log.Debug ("Found: " + path + " (" + is_dir + ")");
                        return new Dirent (path, is_dir);
                }

                /////////////////////////////////////////////////////////////////

                static void MemoryMonitorWorker ()
                {
                        int vmrss_original = SystemInformation.VmRss;

                        const double threshold = 60.0;
                        int last_vmrss = 0;

                        while (! Shutdown.ShutdownRequested && indexing) {

                                // Check resident memory usage
                                int vmrss = SystemInformation.VmRss;
                                double size = vmrss / (double) vmrss_original;
                                if (vmrss != last_vmrss)
                                        Logger.Log.Debug ("Size: VmRSS={0:0.0} MB, size={1:0.00}, {2:0.0}%",
                                                          vmrss/1024.0, size, 100.0 * (size - 1) / (threshold - 1));
                                last_vmrss = vmrss;
                                if (size > threshold) {
                                        Logger.Log.Debug ("Process too big, shutting down!");
                                        restart = true;
                                        Shutdown.ShutdownRequested = true;
                                        return;
                                } else {
                                        Thread.Sleep (3000);
                                }
                        }
                }

                /////////////////////////////////////////////////////////////////

                // From BeagrepDaemon.cs

                static void SetupSignalHandlers ()
                {
                        // Force OurSignalHandler to be JITed
                        OurSignalHandler (-1);

                        // Set up our signal handler
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGINT, OurSignalHandler);
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGTERM, OurSignalHandler);
                        if (Environment.GetEnvironmentVariable("BEAGREP_THERE_BE_NO_QUITTIN") == null)
                                Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGQUIT, OurSignalHandler);
                }

                static void OurSignalHandler (int signal)
                {
                        // This allows us to call OurSignalHandler w/o doing anything.
                        // We want to call it once to ensure that it is pre-JITed.
                        if (signal < 0)
                                return;

                        Logger.Log.Debug ("Shutdown Requested");
                        Shutdown.ShutdownRequested = true;
                }

                /////////////////////////////////////////////////////////////////

                static void PrintUsage ()
                {
                        VersionFu.PrintHeader ();

                        string usage =
                                "Usage: beagrep-build-index [OPTIONS] --target <index_path> <path> [path]\n\n" +

                                "** WARNING **\n" +
                                "beagrep-build-index will *delete all existing data* within the target\n" +
                                "directory.  Ensure that the target path is set correctly before running.\n\n" +

                                "Options:\n" +
                                "  --source [name]\t\tThe index's source name.  Defaults to the target directory name\n" +
                                // FIXME: remap doesnt seem to be implemented !
                                // Implementing remap might some fixes to --enable-deletion, see IndexWorker
                                //"  --remap [path1:path2]\t\tRemap data paths to fit target. \n" +
                                "  --tag [tag]\t\t\tTag index data for identification.\n" +
                                "  --recursive\t\t\tCrawl source path recursivly.\n" +
                                "  --enable-deletion\t\tRemove deleted files and directories from index.\n" +
                                "                   \t\tIndex should be created and always updated with this option.\n" +
                                "  --enable-text-cache\t\tBuild text-cache of documents used for snippets.\n" +
                                "  --disable-directories\t\tDon't add directories to the index.\n" +
                                "  --deny-directory-pattern\t\tKeep any directory and files under it matching this pattern from being indexed.\n" +
                                "  --disable-filtering\t\tDisable all filtering of files. Only index attributes.\n" +
                                "  --allow-pattern [pattern]\tOnly allow files that match the pattern to be indexed.\n" +
                                "  --deny-pattern [pattern]\tKeep any files that match the pattern from being indexed.\n" +
                                "  --disable-restart\t\tDon't restart when memory usage gets above a certain threshold.\n" +
                                "  --disable-on-battery\t\tDisable indexer while on battery power.\n";

                        Console.WriteLine (usage);
                        Environment.Exit (0);
                }

                /////////////////////////////////////////////////////////

                static bool Ignore (DirectoryInfo directory)
                {
                        if (denied_dir_regex.IsMatch (directory.FullName)) {
                                return true;
                        } else if (File.Exists(Path.Combine(directory.FullName, ".beagrep-ignore"))) {
                                return true;
                        } else {
                                return false;
                        }
                }

                static bool Ignore (FileInfo file)
                {
                        // if (file.Name.StartsWith ("."))
                        //      return true;

                        if (FileSystem.IsSpecialFile (file.FullName))
                                return true;

                        if (allowed_regex != null)
                                return ! allowed_regex.IsMatch (file.Name);

                        if (denied_regex == null)
                                return false;

                        return denied_regex.IsMatch (file.Name);
                }
        }
}
