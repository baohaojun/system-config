//
// PidginIndexableGenerator.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.IO;
using System.Text;
using System.Threading;
using System.Collections;
using System.Collections.Generic;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.PidginQueryable {

	public class PidginIndexableGenerator : IIndexableGenerator {

		private PidginQueryable queryable = null;
		private string pidgin_dir = null;
		private string logs_dir = null;
		private const uint polling_interval_in_seconds = 60;

		public PidginIndexableGenerator (PidginQueryable queryable, string pidgin_dir)
		{
			this.queryable = queryable;
			this.pidgin_dir = pidgin_dir;
			this.logs_dir = Path.Combine (pidgin_dir, "logs");

			StartWorker ();
		}

		private void StartWorker() 
		{	
			Log.Info ("Starting Pidgin IM generator ({0})", pidgin_dir);

			if (Inotify.Enabled) {
				Log.Info ("Setting up inotify watches on Pidgin log directories");
				Crawl (false);
			}

			Scheduler.Task task = queryable.NewAddTask (this);
			task.Tag = String.Format ("Crawling Pidgin logs in {0}", pidgin_dir);

			if (!Inotify.Enabled) {
				Scheduler.TaskGroup group;
				group = Scheduler.NewTaskGroup ("Repeating Pidgin log crawler", null, AddCrawlTask);
				task.AddTaskGroup (group);
			}
				
			queryable.ThisScheduler.Add (task);

			Log.Info ("Pidgin IM generator for {0} done", pidgin_dir); 
		}
		
		/////////////////////////////////////////////////

		private void AddCrawlTask ()
		{
			Scheduler.Task task = Scheduler.TaskFromHook (new Scheduler.TaskHook (CrawlHook));
			task.Tag = String.Format ("Crawling {0} to find new logfiles", pidgin_dir);
			task.Source = this;
			queryable.ThisScheduler.Add (task);
		}

		private void CrawlHook (Scheduler.Task task)
		{
			Crawl (true);
			task.Reschedule = true;
			task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
		}

		private void Crawl (bool index)
		{
			//queryable.IsIndexing = true;

			if (Inotify.Enabled)
				Inotify.Subscribe (logs_dir, OnInotifyNewProtocol, Inotify.EventType.Create);

			// Walk through protocol subdirs
			foreach (string proto_dir in DirectoryWalker.GetDirectories (logs_dir))
				CrawlProtocolDirectory (proto_dir, index);
		}

		private void CrawlProtocolDirectory (string proto_dir, bool index)
		{
			if (Inotify.Enabled)
				Inotify.Subscribe (proto_dir, OnInotifyNewAccount, Inotify.EventType.Create);

			// Walk through accounts
			foreach (string account_dir in DirectoryWalker.GetDirectories (proto_dir))
				CrawlAccountDirectory (account_dir, index);
		}

		private void CrawlAccountDirectory (string account_dir, bool index)
		{
			if (Inotify.Enabled)
				Inotify.Subscribe (account_dir, OnInotifyNewRemote, Inotify.EventType.Create);

			// Walk through remote user conversations
			foreach (string remote_dir in DirectoryWalker.GetDirectories (account_dir)) {
				if (remote_dir.IndexOf (".system") < 0)
					CrawlRemoteDirectory (remote_dir, index);
			}
		}

		private void CrawlRemoteDirectory (string remote_dir, bool index)
		{
			if (Inotify.Enabled)
				Inotify.Subscribe (remote_dir, OnInotifyNewConversation, Inotify.EventType.CloseWrite | Inotify.EventType.Modify);

			if (index) {
				foreach (FileInfo file in DirectoryWalker.GetFileInfos (remote_dir))
					if (FileIsInteresting (file.Name))
						IndexLog (file.FullName, Scheduler.Priority.Delayed);

				//queryable.IsIndexing = false;
			}
		}

		/////////////////////////////////////////////////

		private IEnumerator log_files = null;

		public void PostFlushHook ()
		{
		}

		public bool HasNextIndexable ()
		{
			if (log_files == null)
				log_files = DirectoryWalker.GetFileInfosRecursive (logs_dir).GetEnumerator ();

			if (log_files.MoveNext ()) {
				return true;
			} else {
				//queryable.IsIndexing = false;
				return false;
			}
		}

		public Indexable GetNextIndexable ()
		{
			FileInfo file = (FileInfo) log_files.Current;

			if (!file.Exists)
				return null;

			if (file.Directory.Name == ".system")
				return null;

			if (queryable.IsUpToDate (file.FullName))
				return null;

			Indexable indexable = PidginLogToIndexable (file.FullName);
			
			return indexable;
		}

		/////////////////////////////////////////////////

		private bool FileIsInteresting (string filename)
		{
			if (filename.Length < 21)
				return false;

			string ext = Path.GetExtension (filename);
			if (ext != ".txt" && ext != ".html")
				return false;

			// Pre-gaim 2.0.0 logs are in the format "2005-07-22.161521.txt".  Afterward a
			// timezone field as added, ie. "2005-07-22.161521-0500EST.txt".
			//
			// This is a lot uglier than a regexp, but they are so damn expensive.

			return Char.IsDigit (filename [0]) && Char.IsDigit (filename [1])
				&& Char.IsDigit (filename [2]) && Char.IsDigit (filename [3])
				&& filename [4] == '-'
				&& Char.IsDigit (filename [5]) && Char.IsDigit (filename [6])
				&& filename [7] == '-'
				&& Char.IsDigit (filename [8]) && Char.IsDigit (filename [9])
				&& filename [10] == '.'
				&& Char.IsDigit (filename [11]) && Char.IsDigit (filename [12])
				&& Char.IsDigit (filename [13]) && Char.IsDigit (filename [14])
				&& Char.IsDigit (filename [15]) && Char.IsDigit (filename [16])
				&& (filename [17] == '+' || filename [17] == '-' || filename [17] == '.');
		}

		/////////////////////////////////////////////////

		private void OnInotifyNewProtocol (Inotify.Watch watch,
						string path, string subitem, string srcpath,
						Inotify.EventType type)
		{
			if (subitem.Length == 0 || (type & Inotify.EventType.IsDirectory) == 0)
				return;

			CrawlProtocolDirectory (Path.Combine (path, subitem), true);
		}

		private void OnInotifyNewAccount (Inotify.Watch watch,
						string path, string subitem, string srcpath,
						Inotify.EventType type)
		{
			if (subitem.Length == 0 || (type & Inotify.EventType.IsDirectory) == 0)
				return;

			CrawlAccountDirectory (Path.Combine (path, subitem), true);
		}

		private void OnInotifyNewRemote (Inotify.Watch watch,
						string path, string subitem, string srcpath,
						Inotify.EventType type)
		{
			if (subitem.Length == 0 || (type & Inotify.EventType.IsDirectory) == 0)
				return;

			CrawlRemoteDirectory (Path.Combine (path, subitem), true);
		}

		private void OnInotifyNewConversation (Inotify.Watch watch,
						string path, string subitem, string srcpath,
						Inotify.EventType type)
		{
			if (subitem.Length == 0 || (type & Inotify.EventType.IsDirectory) != 0)
				return;

			if (FileIsInteresting (subitem))
				IndexLog (Path.Combine (path, subitem), Scheduler.Priority.Immediate);			
		}

		/////////////////////////////////////////////////
		
		private Indexable PidginLogToIndexable (string filename)
		{
			FileInfo info = new FileInfo (filename);
			Uri uri = UriFu.PathToFileUri (filename);

			Indexable indexable = new Indexable (uri);
			indexable.ContentUri = uri;
			indexable.Timestamp = info.LastWriteTimeUtc;
			indexable.MimeType = "beagle/x-pidgin-log";
			indexable.HitType = "IMLog";
			indexable.CacheContent = false;

			ImBuddy buddy = queryable.ImBuddyListReader.Search (info.Directory.Name);

			if (buddy != null && !String.IsNullOrEmpty (buddy.Alias))
				indexable.AddProperty (Property.NewKeyword ("fixme:alias", buddy.Alias));

			return indexable;
		}

		private void IndexLog (string filename, Scheduler.Priority priority)
		{
			if (! File.Exists (filename))
				return;

			if (queryable.IsUpToDate (filename))
				return;

			Indexable indexable = PidginLogToIndexable (filename);

			Scheduler.Task task = queryable.NewAddTask (indexable);
			task.Priority = priority;
			task.SubPriority = 0;

			queryable.ThisScheduler.Add (task);
		}		

		/////////////////////////////////////////////////

		public string StatusName {
			get { return "PidginQueryable"; }
		}
	}
}

