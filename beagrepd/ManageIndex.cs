//
// ManageIndex.cs
//
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
using System.IO;
using System.Net;

using Beagrep;
using Beagrep.Util;
using Beagrep.Daemon;

using Lucene.Net.Index;
using Lucene.Net.Search;
using Lucene.Net.Documents;

namespace Beagrep.Daemon
{
        class ManageIndex
        {
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


                static void Main (string [] args)
                {
                        if (args.Length < 2)
                                PrintUsage ();

                        string index_dir = (Path.IsPathRooted (args [0])) ? args [0] : Path.GetFullPath (args [0]);

                        if (!Directory.Exists (index_dir)) {
                                Console.WriteLine ("Could not find index: {0}", index_dir);
                                Environment.Exit (1);
                        }

                        // Be *EXTRA PARANOID* about the contents of the target
                        // directory, because creating an indexing driver will
                        // nuke it.
                        if (args [1] != "info" && Directory.Exists (index_dir)) {

                                foreach (FileInfo info in DirectoryWalker.GetFileInfos (index_dir)) {
                                        if (Array.IndexOf (allowed_files, info.Name) == -1) {
                                                Logger.Log.Error ("{0} doesn't look safe to delete: non-Beagrep file {1} was found", index_dir, info.FullName);
                                                Environment.Exit (1);
                                        }
                                }

                                foreach (DirectoryInfo info in DirectoryWalker.GetDirectoryInfos (index_dir)) {
                                        if (Array.IndexOf (allowed_dirs, info.Name) == -1) {
                                                Logger.Log.Error ("{0} doesn't look safe to delete: non-Beagrep directory {1} was found", index_dir, info.FullName);
                                                Environment.Exit (1);
                                        }
                                }
                        }

                        switch (args [1]) {
#if false
                        case "list":
                                ExecuteList ();
                                break;
                        case "remove":
                                ExecuteRemove (args [2]);
                                break;
#endif
                        case "info":
                                ExecuteInfo (index_dir);
                                break;

                        case "merge":
                                ExecuteMerge (index_dir, args [2]);
                                break;

                        case "optimize":
                                ExecuteOptimize (index_dir);
                                break;
                        default:
                                Console.WriteLine ("Unknown command: {0}", args [1]);
                                PrintUsage ();
                                break;
                        }
                }

                /////////////////////////////////////////////////////////

                static void PrintUsage ()
                {
                        string usage =
                                "beagrep-manage-index: Low-level Lucene index management\n" +
                                "Web page: http://www.gnome.org/projects/beagrep\n" +
                                "Copyright (C) 2004-2005 Novell, Inc.\n\n";

                        usage +=
                                "Usage: beagrep-manage-index <index_path> <command> [OPTIONS]\n\n" +
                                "Commands:\n" +
#if false
                                "  list\t\t\t\tList all entries in the index.\n" +
                                "  remove <uri|tag>\t\tRemove entries corresponding to the criterias specified.\n" +
#endif
                                "  merge <index to merge>\tMerge another Lucene index into the target.\n" +
                                "  info\t\t\t\tPrint basic index information.\n" +
                                "  optimize\t\t\tOptimize index.\n";


                        Console.WriteLine (usage);
                        Environment.Exit (0);
                }

                /////////////////////////////////////////////////////////

#if false
                static void ExecuteList ()

{                       LuceneDriver driver = new LuceneDriver (index_dir, true);

                        IndexReader reader = IndexReader.Open (driver.Store);

                        for (int i = 0; i < reader.NumDocs (); i++) {
                                if (reader.IsDeleted (i))
                                        continue;
                                Console.WriteLine (reader.Document (i));
                        }

                        reader.Close ();
                }

                /////////////////////////////////////////////////////////

                static void ExecuteRemove (string arg)
                {
                        LuceneDriver driver = new LuceneDriver (index_dir);

                        if (arg.IndexOf ("://") != -1) {
                                Uri uri = new Uri (arg);
                                ICollection hits = driver.DoQueryByUri (uri);

                                if (hits == null || hits.Count == 0) {
                                        Console.WriteLine ("Uri not found in the index: {0}", uri);
                                        Environment.Exit (1);
                                }

                                driver.Remove (uri);
                                driver.Flush ();

                                Console.WriteLine ("Successfully removed Uri: {0}", uri);
                        } else {
                                IndexSearcher searcher = new IndexSearcher (driver.Store);
                                BooleanQuery query = new BooleanQuery ();

                                Term term = new Term ("prop:k:Tag", arg); // Argh
                                TermQuery term_query = new TermQuery (term);
                                query.Add (term_query, false, false);

                                Hits hits = searcher.Search (query);
                                int n_hits = hits.Length ();

                                string uri;

                                for (int i = 0; i < n_hits; ++i) {
                                        Document doc = hits.Doc (i);

                                        uri = doc.Get ("Uri");

                                        if (uri == null)
                                                continue;

                                        driver.Remove (UriFu.UriStringToUri (uri));
                                }

                                driver.Flush ();

                                Console.WriteLine ("Successfully removed {0} items with tag: {1}", n_hits, arg);
                        }
                }
#endif
                /////////////////////////////////////////////////////////

                // Merge an external Beagrep index to the current index. Merging will
                // join the primary- and secondary lucene indexes and if available, the
                // file attributes store.

                static void ExecuteMerge (string index_dir, string index_to_merge)
                {
                        LuceneIndexingDriver driver = new LuceneIndexingDriver (index_dir, false);

                        if (!Path.IsPathRooted (index_to_merge))
                                index_to_merge = Path.GetFullPath (index_to_merge);

                        if (!Directory.Exists (index_to_merge)) {
                                Console.WriteLine ("Could not find index to merge: {0}", index_to_merge);
                                Environment.Exit (1);
                        }

                        LuceneQueryingDriver driver_to_merge = new LuceneQueryingDriver (index_to_merge, -1, false);

                        Stopwatch watch = new Stopwatch ();
                        watch.Start ();

                        // Merge the lucene index

                        try {
                                driver.Merge (driver_to_merge);
                        } catch (Exception e) {
                                Console.WriteLine ("Index merging (lucene) failed: {0}", e);
                                Environment.Exit (1);
                        }

                        // Merge file attributes stores

                        FileAttributesStore_Sqlite store;

                        try {
                                store = new FileAttributesStore_Sqlite (driver.TopDirectory, driver.Fingerprint);
                                store.Merge (new FileAttributesStore_Sqlite (driver_to_merge.TopDirectory, driver_to_merge.Fingerprint));
                        } catch (Exception e) {
                                Console.WriteLine ("Index merging (attributes store) failed: {0}", e);
                                Environment.Exit (1);
                        }

                        watch.Stop ();

                        Console.WriteLine ("Successfully merged index {0} into {1} in {2}", index_to_merge, driver.TopDirectory, watch);
                }

                /////////////////////////////////////////////////////////

                // Get the total number of entries from the index.

                static void ExecuteInfo (string index_dir)
                {
                        LuceneQueryingDriver driver = new LuceneQueryingDriver (index_dir, true);

                        Console.WriteLine ("Total number of entries in index: {0}", driver.GetItemCount());
                }

                /////////////////////////////////////////////////////////

                // Execute a lucene optimize-task on the index.

                static void ExecuteOptimize (string index_dir)
                {
                        LuceneIndexingDriver driver = new LuceneIndexingDriver (index_dir, false);

                        Stopwatch watch = new Stopwatch ();
                        watch.Start ();

                        driver.OptimizeNow ();

                        watch.Stop ();

                        Console.WriteLine ("Optimized index {0} in {1}", driver.TopDirectory, watch);
                }
        }
}
