
//
// KMailIndexer.cs
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
using System.Collections.Generic;
using System.IO;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Daemon.KMailQueryable {
	
	/**
	 * Main indexer class
	 * The bulk of the indexing work is done here
	 */
	public class KMailIndexer {
		// location of mail folder
		private string mail_root;
		public string MailRoot {
			get { return mail_root; }
		}
		// account name for this folder
		private string account_name;
		public string AccountName {
			get { return account_name; }
		}
		// mail folders not to scan
		private ArrayList excludes;
		// list of maildir directories which store mails in cur/, new/, tmp/ subdirs
		private ArrayList mail_directories;
		// list of directories which contain mbox files and other mail folders
		private ArrayList folder_directories;
		// list of mbox files
		private ArrayList mbox_files;
		// also store the queryable
		private KMailQueryable queryable;
		public KMailQueryable Queryable {
		    get { return queryable; }
		}

		////////////// Progress tracking //////////////////

		private double progress = 0;

		internal double Progress {
			set { progress = value; }
			get { return progress; }
		}

		///////////////////////////////////////////////////

		private string lastGoodDirPath = ""; // cache last successful directory

		public KMailIndexer (KMailQueryable queryable, string account, string root)
		{
			this.queryable = queryable;
			account_name = account;
			mail_root = root;
			mail_directories = new ArrayList ();
			folder_directories = new ArrayList ();
			mbox_files = new ArrayList ();

			excludes = new ArrayList ();
			List<string[]> values = Conf.Daemon.GetListOptionValues (Conf.Names.ExcludeMailfolder);
			if (values == null)
				return;

			foreach (string[] item in values)
				excludes.Add (item [0].ToLower ());
		}

		/**
		 * inotify callback
		 */
		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;
			string fullPath = Path.Combine (path, subitem);

			// we need to watch for all kinds of events - this is tricky

			// Case: new file is created
			// - if it is one of the folder_directories, index it
			// - if is in one of the mail_directories, index it if it is an mbox file
			if ((type & Inotify.EventType.Create) != 0 && (type & Inotify.EventType.IsDirectory) == 0) {
				if (IsMailDir (path)) {
					Indexable indexable = MaildirMessageToIndexable (fullPath, false);
					AddIndexableTask (indexable, fullPath);
				} else {
					// add mbox file to mbox_files
					string mbox = GetMboxFile (path, subitem);
					if (mbox != null) {
						mbox_files.Add (mbox);
						IndexMbox (mbox, true);
					}
				}
				return;
			}

			// Case: file is deleted
			// - if it is a mail file, we might like it to be deleted
			if ((type & Inotify.EventType.MovedFrom) != 0 ||
			    ((type & Inotify.EventType.Delete) != 0 &&
			    (type & Inotify.EventType.IsDirectory) == 0)) {
				if (IsMailDir (path))
					RemoveMail (fullPath);
				else if (mbox_files.Contains (fullPath)) {
					RemoveMbox (fullPath);
					mbox_files.Remove (fullPath);
				}
				return;
			}

			// Case: file is moved
			// - files are moved from tmp/new to cur
			// - need to delete from the source
			if ((type & Inotify.EventType.MovedTo) != 0 && (type & Inotify.EventType.IsDirectory) == 0) {
				if (IsMailDir (path)) {
					Indexable indexable = MaildirMessageToIndexable (fullPath, false);
					AddIndexableTask (indexable, fullPath);
				}
				if (IsMailDir (srcpath))
					RemoveMail (srcpath);
				if (mbox_files.Contains (fullPath)) {
					// check if this because of compaction, in which case need to delete previous mbox
					if (srcpath != null && srcpath.EndsWith ("." + subitem + ".compacted"))
						RemoveMbox (fullPath);
					// FIXME need to ensure IndexMbox is scheduled *after* RemoveMbox finishes
					// RemoveMbox creates a job with immediate priority while
					// IndexMbox creates a job with the default priority of a generator
					// Is there a better way to ensure the order ?
					IndexMbox (fullPath, true);
				}
				return;
			}

			// Case: file is modified i.e. there was no create event but closewrite event
			// - possibly some mbox was changed
			// FIXME kmail doesnt physically delete the deleted mails from mbox files unless compacted
			// - which means one has to read the .index files to find deleted messages...
			// - need to find the format of the .index/.index.ids etc files and parse them
			if ((type & Inotify.EventType.Modify) != 0 && (type & Inotify.EventType.IsDirectory) == 0) {
				if (mbox_files.Contains (fullPath))
					IndexMbox (fullPath, false);
				return;
			}

			// Case: a directory is created:
			// well watch it anyway but also make sure its a maildir directory
			// if it a maildir directory, then add it to maildir_dirs
			if ((type & Inotify.EventType.Create) != 0 && (type & Inotify.EventType.IsDirectory) != 0) {
			    	if (!IgnoreFolder (fullPath)) {
					Watch (fullPath);
					UpdateDirectories(fullPath);
				}
				return;
			}

			// Case: if a directory is deleted:
			// remove watch
			if ((type & Inotify.EventType.Delete) != 0 && (type & Inotify.EventType.IsDirectory) != 0) {
				watch.Unsubscribe ();
				mail_directories.Remove (fullPath);
				folder_directories.Remove (fullPath);
				return;
			}

			// Case: directory is moved
			// FIXME: implement renaming of mail folders
			
		}

		/**
		 * Add watch to the parameter directory and its subdirs, recursively
		 */
		public void Watch (string path)
		{
			DirectoryInfo root = new DirectoryInfo (path);
			if (! root.Exists)
				return;

			Queue queue = new Queue ();
			queue.Enqueue (root);

			while (queue.Count > 0) {
				DirectoryInfo dir = queue.Dequeue () as DirectoryInfo;

				if (! dir.Exists)
					continue;

				//log.Debug ("Adding inotify watch to " + dir.FullName);
				Inotify.Subscribe (dir.FullName, OnInotifyEvent,
							Inotify.EventType.Create
							| Inotify.EventType.Delete
							| Inotify.EventType.MovedFrom
							| Inotify.EventType.MovedTo);

				foreach (DirectoryInfo subdir in DirectoryWalker.GetDirectoryInfos (dir))
					queue.Enqueue (subdir);
			}
		}
		
		/**
		 * Recursively traverse the files and dirctories under mail_root
		 * to find files that need to be indexed, directories that
		 * need to be watched for changes
		 */
		public void Crawl ()
		{
			if (!Directory.Exists (mail_root))
				return;

			mail_directories.Clear ();
			folder_directories.Clear ();
			mbox_files.Clear();

			Queue pending = new Queue ();
			pending.Enqueue (mail_root);
			folder_directories.Add (mail_root);
			// add inotify watch to root folder
			if (Inotify.Enabled)
				Inotify.Subscribe (mail_root, OnInotifyEvent,
					Inotify.EventType.Create
					| Inotify.EventType.Delete
					| Inotify.EventType.MovedFrom
					| Inotify.EventType.MovedTo
					| Inotify.EventType.Modify);

			while (pending.Count > 0) {

				string dir = (string) pending.Dequeue ();
				Logger.Log.Debug ("Searching for mbox and maildirs in " + dir);

				foreach (FileInfo fi in DirectoryWalker.GetFileInfos (dir)) {
					if (!fi.Name.EndsWith (".index"))
						continue;
					string indexFile = fi.Name;
					string mailFolderName = 
						indexFile.Substring (1, indexFile.LastIndexOf (".index")-1);
					string mailFolder = Path.Combine (dir, mailFolderName);
					if (IgnoreFolder (mailFolder))
						continue;
					if (Directory.Exists (mailFolder)) {
						mail_directories.Add (mailFolder);
						if (Inotify.Enabled)
							Watch (mailFolder);
					} else if (File.Exists (mailFolder)) {
						mbox_files.Add (mailFolder);
					}
					// if there is a directory with name .<mailFolderName>.directory
					// then it contains sub-folders
					string subFolder = 
						Path.Combine (dir, "." + mailFolderName + ".directory");
					if (Directory.Exists (subFolder)) {
						pending.Enqueue (subFolder);
						folder_directories.Add (subFolder);
						if (Inotify.Enabled)
							Inotify.Subscribe (subFolder, OnInotifyEvent,
								Inotify.EventType.Create
								| Inotify.EventType.Delete
								| Inotify.EventType.MovedFrom
								| Inotify.EventType.MovedTo
								| Inotify.EventType.Modify);
					}
				}
			}	

			// copy the contents as mail_directories, mbox_files might change due to async events
			ArrayList _mail_directories = new ArrayList (mail_directories);
			ArrayList _mbox_files = new ArrayList (mbox_files);
			
			if (queryable.ThisScheduler.ContainsByTag (mail_root)) {
				Logger.Log.Debug ("Not adding task for already running task: {0}", mail_root);
				return;
			} else {
				KMaildirIndexableGenerator generator = new KMaildirIndexableGenerator (this, _mail_directories);
				AddIIndexableTask (generator, mail_root);
			}

			foreach (string mbox_file in _mbox_files) {
				IndexMbox (mbox_file, true);
			}
		}

		private void AddIndexableTask (Indexable indexable, string tag)
		{
			if (indexable == null)
				return;

			Scheduler.Task task = queryable.NewAddTask (indexable);
			task.Priority = Scheduler.Priority.Immediate;
			task.Tag = tag;
			queryable.ThisScheduler.Add (task);
		}	

		private void AddIIndexableTask (IIndexableGenerator generator, string tag)
		{
			if (generator == null)
				return;

			Scheduler.Task task = queryable.NewAddTask (generator);
			task.Tag = tag;
			queryable.ThisScheduler.Add (task);
		}	

		/**
		 * Start a task for indexing an mbox file
		 */
		public void IndexMbox (string mbox_file, bool initial_scan)
		{
			if (queryable.ThisScheduler.ContainsByTag (mbox_file)) {
				Logger.Log.Debug ("Not adding task for already running task: {0}", mbox_file);
				return;
			}

			//Logger.Log.Debug ("Creating task to index mbox {0}", mbox_file);
			KMailMboxIndexableGenerator generator = new KMailMboxIndexableGenerator (this, mbox_file, initial_scan);
			AddIIndexableTask (generator, mbox_file);
		}

		/**
		 * Remove maildir mail file
		 */
		private void RemoveMail (string file)
		{
			Logger.Log.Debug ("Removing mail:" + file);
			Uri uri = UriFu.PathToFileUri (file);
			Scheduler.Task task = queryable.NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			queryable.ThisScheduler.Add (task);
		}

		/** 
		 * Create an indexable from a maildir message
		 */
		public Indexable MaildirMessageToIndexable (string filename, bool crawl)
		{
			//Logger.Log.Debug ("+ indexing maildir mail:" + filename);
			String folder = GetFolderMaildir(filename);
			Uri file_uri = UriFu.PathToFileUri (filename);

			Indexable indexable = new Indexable (file_uri);
			indexable.HitType = "MailMessage";
			indexable.MimeType = "message/rfc822";
			indexable.CacheContent = true;
			indexable.FlushBufferCache = crawl;

			indexable.AddProperty (Property.NewUnsearched ("fixme:client", "kmail"));
			indexable.AddProperty (Property.NewUnsearched ("fixme:account", account_name));
                        indexable.AddProperty (Property.NewUnsearched ("fixme:folder", folder));
			indexable.ContentUri = file_uri;

			return indexable;
		}
	
		/**
		 * Create an indexable from an mbox message
		 * Most of the code here is from Evo backend
		 */
		public Indexable MessageToIndexable (string file_name, System.Uri uri, GMime.Message message, string folder_name)
		{
			//Logger.Log.Debug ("Indexing " + uri + " in folder " + folder_name);
			Indexable indexable = new Indexable (uri);
			// set parent uri to the filename so that when an mbox file
			// is deleted, all the messages in that file can be deleted
			indexable.ParentUri = UriFu.PathToFileUri (file_name);

			indexable.Timestamp = message.Date.ToUniversalTime ();
			indexable.HitType = "MailMessage";
			indexable.MimeType = "message/rfc822";
			indexable.CacheContent = true;

			indexable.AddProperty (Property.NewUnsearched ("fixme:client", "kmail"));
			indexable.AddProperty (Property.NewUnsearched ("fixme:account", account_name));
                        indexable.AddProperty (Property.NewUnsearched ("fixme:folder", folder_name));

			GMime.InternetAddressList addrs;

			addrs = message.GetRecipients (GMime.Message.RecipientType.To);
			foreach (GMime.InternetAddress ia in addrs) {
				if (folder_name == Queryable.SentMailFolderName && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewKeyword ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = message.GetRecipients (GMime.Message.RecipientType.Cc);
			foreach (GMime.InternetAddress ia in addrs) {
				if (folder_name == Queryable.SentMailFolderName && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewKeyword ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = GMime.InternetAddressList.ParseString (GMime.Utils.HeaderDecodePhrase (message.Sender));
			foreach (GMime.InternetAddress ia in addrs) {
				if (folder_name != Queryable.SentMailFolderName && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewKeyword ("fixme:gotFrom", ia.Addr));
			}
			addrs.Dispose ();

			if (folder_name == Queryable.SentMailFolderName)
				indexable.AddProperty (Property.NewFlag ("fixme:isSent"));
			else {
				string kmail_msg_sent = message.GetHeader ("X-KMail-Link-Type");
				if (kmail_msg_sent == "reply")
					indexable.AddProperty (Property.NewFlag ("fixme:isSent"));
			}
				
// no need to store date again, use the issent flag to determine if the date is sentdate or not			
#if false
			if (folder_name == Queryable.SentMailFolderName)
				indexable.AddProperty (Property.NewDate ("fixme:sentdate", message.Date.ToUniversalTime ()));
			else
				indexable.AddProperty (Property.NewDate ("fixme:received", message.Date.ToUniversalTime ()));
#endif

			indexable.SetBinaryStream (message.Stream);

			return indexable;
		}
		
		/**
		 * deleting mbox means deleting all the mails which were in this mbox
		 * we use the idea of parent-uri
		 * while creating indexables, we set the parent uri to be the uri of the mbox file
		 * so to delete all mails in the mbox we just delete all documents whose parent uri
		 *     is the uri of the mbox file
		 */
		public void RemoveMbox (string file)
		{
			Logger.Log.Debug ("Removing mbox:" + file);
			Uri uri = UriFu.PathToFileUri (file);
			Scheduler.Task task = queryable.NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			queryable.ThisScheduler.Add (task);
		}

		///////////////////////////////////////////////////////////

		// Helpers

		/**
		 * a maildir is of format:
		 * some_dir_in_currently_watched_directories/{cur,new,tmp}
		 * again we ignore tmp - no point trying to watch it - it will be moved anyway
		 * should we check with the kmail directory structure ?
		 * presence of files like directory.index, directory.index.ids ?
		 */
		public bool IsMailDir (string dirPath)
		{
			if (dirPath == null || ! (dirPath.EndsWith("cur") || dirPath.EndsWith("new")))
				return false;

			string possibleMaildir = (Directory.GetParent (dirPath)).FullName;
			if (lastGoodDirPath == possibleMaildir)
				return true;
			Logger.Log.Debug ("checking if " + possibleMaildir + " is a maildir ?");
			if (mail_directories.Contains (possibleMaildir)) {
				lastGoodDirPath = possibleMaildir;
				return true;
			} else
				return false;
		}

		/**
		 * how to decide if this filename denotes an mbox file ?
		 * if its of the form .aaa.index, then aaa is the inbox file
		 * if its of the form aaa (no .index) then there should be a .aaa.index
		 */
		public string GetMboxFile (string dir, string filename)
		{
			int pos = filename.LastIndexOf (".index"); 
			if (pos > 0) {
				string possible_mbox_name = filename.Substring (1, pos - 2); //Remove (pos, 6).Remove (0,1);
				possible_mbox_name = Path.Combine (dir, possible_mbox_name);
				if (File.Exists (possible_mbox_name))
					return possible_mbox_name;
			} else {
				string possible_index_name = "." + filename + ".index";
				possible_index_name = Path.Combine (dir, possible_index_name);
				if (File.Exists (possible_index_name))
					return Path.Combine (dir, filename);
			}
			
			return null; // not found
		}
		
		/**
		 * Called when a new directory is created
		 * Decide what to do with this new directory
		 */
		public void UpdateDirectories (string dirPath)
		{
			string parentDir = (Directory.GetParent (dirPath)).FullName;
			DirectoryInfo dirinfo = new DirectoryInfo (dirPath);
			string dirName = dirinfo.Name;
			
			if (dirName == "cur" || dirName == "new" || dirName == "tmp") {
				// check and add the parentdir to mail_directories
				if (!mail_directories.Contains (parentDir))
					mail_directories.Add (parentDir);
				return;
			}
			
			// format .name.directory - in which case add it to folder_dir
			// format name  - in which case add it to mail_dir
			if (dirName.EndsWith (".directory"))
				folder_directories.Add (dirPath);
			else
				mail_directories.Add (dirPath);
		}

		/**
		 * FIXME:if we can parse kmailrc file, then we might be
		 * able to deduce the mail folder name
		 * currently get it from the file name (mbox) or parent.parent directory name
		 */
		
		public string GetFolderMbox (string mbox_file)
		{
			FileInfo fi = new FileInfo (mbox_file);
			return fi.Name;
		}
		
		public string GetFolderMaildir (string mailFile)
		{
			return (Directory.GetParent ((Directory.GetParent (mailFile)).FullName).Name);
		}

		private bool IgnoreFolder (string path)
		{
			foreach (string exclude in excludes) {
				if (path.ToLower().EndsWith (exclude))
					return true;
			}
			return false;
		}
	}
}
