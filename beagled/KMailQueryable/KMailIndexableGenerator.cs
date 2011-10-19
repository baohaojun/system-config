
//
// KMailIndexableGenerator.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2005 Debajyoti Bera
//
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

using System;
using System.Collections;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using System.Xml;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Daemon.KMailQueryable {

	/**
	 * Indexable generator for maildir mails
	 */
	public class KMaildirIndexableGenerator : IIndexableGenerator {
		// store the indexer
		private KMailIndexer indexer;
		// message file currently indexing
		private FileInfo CrawlFile;
		// directory currently parsing
		private DirectoryInfo current_dir;
		// list of files in current directory
		private IEnumerable files_to_parse;
		// list of directories to scan
		private ArrayList dirs_to_scan;
		private IEnumerator dir_enumerator = null;
		private IEnumerator file_enumerator = null;

		// counts for reporting progress percent
		private int num_dirs = 0;
		private int num_dir_crawled = -1;
		private int num_file_in_dir = 0;
		private int num_file_in_dir_crawled = 0;

		private string account_name {
			get { return indexer.AccountName; }
		}

		public KMaildirIndexableGenerator (KMailIndexer indexer, ArrayList mail_directories)
		{
			this.indexer = indexer;
			this.indexer.Progress = 0;
			this.indexer.Queryable.Indexing = true;

			dirs_to_scan = new ArrayList ();

			foreach (string directory in mail_directories) {
				AddDirectory (directory);
			}
			dir_enumerator = dirs_to_scan.GetEnumerator ();

			this.num_dirs = dirs_to_scan.Count;
		}

		public void PostFlushHook ()
		{
			if (num_file_in_dir == 0)
				return;
			indexer.Progress = (num_dir_crawled + ((double) num_file_in_dir_crawled / num_file_in_dir)) / num_dirs;
			//Log.Debug ("Progress {4} = ({0} + {1}/{2})/{3}", num_dir_crawled, num_file_in_dir_crawled, num_file_in_dir, num_dirs, current_dir.FullName);
		}

		private void AddDirectory (string _dir) {
			DirectoryInfo dir;
			
			// scan mails in directory cur and new, not tmp
			if (Directory.Exists (Path.Combine (_dir, "cur"))) {
				dir = new DirectoryInfo (Path.Combine (_dir, "cur"));
				dirs_to_scan.Add (dir);
			}

			if (Directory.Exists (Path.Combine (_dir, "new"))) {
				dir = new DirectoryInfo (Path.Combine (_dir, "new"));
				dirs_to_scan.Add (dir);
			}
		}

		public string StatusName {
			get {
				if (current_dir == null)
					return indexer.MailRoot;
				return current_dir.FullName;
			}
		}

		public Indexable GetNextIndexable ()
		{
			FileInfo file = (FileInfo) file_enumerator.Current;
			return indexer.MaildirMessageToIndexable (file.FullName, true);
		}

		public bool IsUpToDate (string path)
		{
			return indexer.Queryable.FileAttributesStore.IsUpToDate (path);
		}

		public bool HasNextIndexable ()
		{
			do {
				while (file_enumerator == null || !file_enumerator.MoveNext ()) {
					if (!dir_enumerator.MoveNext ()) {
						dir_enumerator = null;
						indexer.Queryable.Indexing = false;
						return false;
					}

					if (Shutdown.ShutdownRequested)
						return false;

					current_dir = (DirectoryInfo) dir_enumerator.Current;
					num_dir_crawled ++;
					num_file_in_dir = DirectoryWalker.GetNumItems (current_dir.FullName);
					num_file_in_dir_crawled = 0;
					indexer.Progress = (double) num_dir_crawled / num_dirs;
					Log.Info ("Scanning {0} maildir mails in {1}", num_file_in_dir, current_dir.FullName);

					files_to_parse = DirectoryWalker.GetFileInfos (current_dir);
					file_enumerator = files_to_parse.GetEnumerator ();
				}
				num_file_in_dir_crawled ++;
				CrawlFile = (FileInfo) file_enumerator.Current;
			} while (IsUpToDate (CrawlFile.FullName));
		    
			return true;
		}

	}

	/**
	 * Indexable generator for mbox mail files
	 * based on Evo code
	 */
	public class KMailMboxIndexableGenerator : IIndexableGenerator {
		// path of the mbox file
		private string mbox_file;
		// fd, stream, parser needed for gmime parsing
		private int mbox_fd = -1;
		private GMime.StreamFs mbox_stream;
		private GMime.Parser mbox_parser;
		// store the indexer
		private KMailIndexer indexer;
		// number of mails scanned
		private int indexed_count;
		// is this initial scan - in which case the mbox might have been modified since last scan
		private bool initial_scan;
		
		private string account_name {
			get { return indexer.AccountName; }
		}

		private string folder_name {
			get { return indexer.GetFolderMbox (mbox_file); }
		}

		public KMailMboxIndexableGenerator (KMailIndexer indexer, string mbox_file, bool initial_scan)
		{
			this.indexer = indexer;
			this.mbox_file = mbox_file;
			this.initial_scan = initial_scan;
		}

		public void PostFlushHook ()
		{
			Checkpoint ();
		}

		/**
		 * store how long indexing is done on the disk
		 * in case indexing stops midway we dont have to restart from the beginning
		 *   if the mbox file hasnt been modified
		 */
		public void Checkpoint ()
		{
			if (mbox_parser != null) {
				MboxLastOffset = mbox_parser.Tell ();
				indexer.Queryable.FileAttributesStore.AttachLastWriteTime (mbox_file, DateTime.UtcNow);
			}
		}

		public string StatusName {
			get { return mbox_file; }
		}

		private long MboxLastOffset {
			get {
				string offset_str = indexer.Queryable.ReadDataLine ("offset-" + mbox_file.Replace ('/', '-'));
				long offset = Convert.ToInt64 (offset_str);
				return offset;
			}

			set {
				indexer.Queryable.WriteDataLine ("offset-" + mbox_file.Replace ('/', '-'), value.ToString ());
			}
		}

		public bool IsUpToDate (string path)
		{
			//Logger.Log.Info (path + " is uptodate:" + indexer.Queryable.FileAttributesStore.IsUpToDate (path));
			return indexer.Queryable.FileAttributesStore.IsUpToDate (path);
		}

		/**
		 * Advance to the next mail in the mbox file.
		 */
		public bool HasNextIndexable ()
		{	
			if (mbox_fd < 0) {
				Logger.Log.Debug ("Opening mbox {0}", mbox_file);

				try {
					KMailQueryable.InitializeGMime ();
				} catch (Exception e) {
					Logger.Log.Warn (e, "Caught exception trying to initalize gmime:");
					return false;
				}

				
				try {
					mbox_fd = Mono.Unix.Native.Syscall.open (mbox_file, Mono.Unix.Native.OpenFlags.O_RDONLY);
				} catch (System.IO.FileNotFoundException e) {
					Logger.Log.Warn ("mbox " + mbox_file + " deleted while indexing.");
					return false;
				}
				mbox_stream = new GMime.StreamFs (mbox_fd);
				if (initial_scan && !IsUpToDate (mbox_file))
					// this is the initial scan and
					// file has changed since last scan =>
					// set mboxlastoffset to 0 and seek to 0
					mbox_stream.Seek ((int)(MboxLastOffset = 0));
				else
					mbox_stream.Seek ((int) MboxLastOffset);
				mbox_parser = new GMime.Parser (mbox_stream);
				mbox_parser.ScanFrom = true;
			}

			if (mbox_parser.Eos ()) {
				// save the state ASAP
				Checkpoint ();

				mbox_stream.Close ();
				mbox_fd = -1;
				mbox_stream.Dispose ();
				mbox_stream = null;
				mbox_parser.Dispose ();
				mbox_parser = null;
				
				Logger.Log.Debug ("{0}: Finished indexing {1} messages", folder_name, indexed_count);
				return false;
			} else
				return true;
		}

		public Indexable GetNextIndexable ()
		{
			GMime.Message message = null;
			try {
				message = mbox_parser.ConstructMessage ();
			} catch (System.IO.FileNotFoundException e) {
				Logger.Log.Warn ("mbox " + mbox_file + " deleted while parsing.");
				return null;
			}

			try {
				// Again comment from Evo :P
				// Work around what I think is a bug in GMime: If you
				// have a zero-byte file or seek to the end of a
				// file, parser.Eos () will return true until it
				// actually tries to read something off the wire.
				// Since parser.ConstructMessage() always returns a
				// message (which may also be a bug), we'll often get
				// one empty message which we need to deal with here.
				//
				// Check if its empty by seeing if the Headers
				// property is null or empty.
				if (message == null || message.Headers == null || message.Headers == "") {
					return null;
				}
			
				// mbox KIO slave uses the From line as URI - how weird!
				// are those lines supposed to be unique ???
				string id = mbox_parser.From;
				System.Uri uri = EmailUri (id);
			
				Indexable indexable = indexer.MessageToIndexable (mbox_file, uri, message, indexer.GetFolderMbox (mbox_file));
			
				if (indexable == null)
					return null;

				++indexed_count;

				return indexable;
			} finally {
				if (message != null)
					message.Dispose ();
			}
		}

		// TODO: confirm that this works with the mbox kio-slave from new kdepim
		public Uri EmailUri (string id)
		{
			FileInfo fi = new FileInfo (mbox_file);
			return new Uri (String.Format ("mbox:///{0}/{1}", fi.FullName, id));
		}
	}
}
