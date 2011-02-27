//
// EmpathyQueryable.cs
//
// Copyright (C) 2007 Kevin Kubasik <kevin@kubasik.net>
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
using System.Xml;
using System.Text;
using System.Threading;
using System.Collections;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.EmpathyQueryable {

	[QueryableFlavor (Name="Empathy", Domain=QueryDomain.Local, RequireInotify=true)]
	public class EmpathyQueryable : LuceneFileQueryable {

		private string log_dir;

		private int polling_interval_in_seconds = 60;

		private EmpathyCrawler crawler;

		public EmpathyQueryable () : base ("EmpathyIndex")
		{
			log_dir = Path.Combine (PathFinder.HomeDir, ".gnome2/Empathy/logs");
		}

		/////////////////////////////////////////////////
					
		private void StartWorker() 
		{		
			if (! Directory.Exists (log_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			Logger.Log.Info ("Starting Empathy log backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

			if (Inotify.Enabled)
				Watch (log_dir);

			crawler = new EmpathyCrawler (log_dir);
			Crawl ();

			if (!Inotify.Enabled) {
				Scheduler.Task task = Scheduler.TaskFromHook (new Scheduler.TaskHook (CrawlHook));
				task.Tag = "Crawling ~/.gnome2/Empathy to find new logfiles";
				task.Source = this;
				ThisScheduler.Add (task);
			}

			stopwatch.Stop ();

			Logger.Log.Info ("Empathy log backend worker thread done in {0}", stopwatch); 
		}
		
		public override void Start () 
		{
			base.Start ();
			
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		/////////////////////////////////////////////////

		private void Crawl ()
		{
			crawler.Crawl ();
			foreach (FileInfo file in crawler.Logs)
				IndexLog (file.FullName, Scheduler.Priority.Delayed);

			if (Inotify.Enabled)
				crawler = null; // Free crawler since we do not need it any more
		}

		private void CrawlHook (Scheduler.Task task)
		{
			Crawl ();
			task.Reschedule = true;
			task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
		}

		/////////////////////////////////////////////////

		// Sets up an Inotify watch on all subdirectories
		private void Watch (string path)
		{
			DirectoryInfo root = new DirectoryInfo (path);
			
			if (! root.Exists)
				return;	
			
			Queue queue = new Queue ();
			queue.Enqueue (root);

			while (queue.Count > 0) {
				DirectoryInfo dir = queue.Dequeue () as DirectoryInfo;
				
				// Setup watches on the present directory.
				Inotify.Subscribe (dir.FullName, OnInotifyEvent,
						   Inotify.EventType.Create | Inotify.EventType.CloseWrite);
				
				// Add all subdirectories to the queue so their files can be indexed.
				foreach (DirectoryInfo subdir in dir.GetDirectories ())
					queue.Enqueue (subdir);
			}
		}
		
		/////////////////////////////////////////////////

		private bool CheckForExistence ()
		{
			if (!Directory.Exists (log_dir))
				return true;

			this.Start ();

			return false;
		}

		/////////////////////////////////////////////////

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;

			string full_path = Path.Combine (path, subitem);

			if ((type & Inotify.EventType.Create) != 0 && (type & Inotify.EventType.IsDirectory) != 0) {
				Watch (full_path);
				return;
			}

			if ((type & Inotify.EventType.Modify) != 0) {
				IndexLog (full_path, Scheduler.Priority.Immediate);
				return;
			}
		}

		/////////////////////////////////////////////////
		
		private static Indexable ImLogToIndexable (string filename)
		{
			Uri uri = UriFu.PathToFileUri (filename);
			Indexable indexable = new Indexable (uri);
			indexable.ContentUri = uri;
			indexable.Timestamp = File.GetLastWriteTimeUtc (filename);
			indexable.MimeType = "beagle/x-empathy-log";
			indexable.HitType = "IMLog";
			indexable.CacheContent = true;

			return indexable;
		}

		private void IndexLog (string filename, Scheduler.Priority priority)
		{
			if (! File.Exists (filename))
				return;

			if (IsUpToDate (filename))
				return;

			Indexable indexable = ImLogToIndexable (filename);
			Scheduler.Task task = NewAddTask (indexable);
			task.Priority = priority;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		protected override double RelevancyMultiplier (Hit hit)
		{
			return HalfLifeMultiplierFromProperty (hit, 0.25, "fixme:endtime", "fixme:starttime");
		}
	}
}
