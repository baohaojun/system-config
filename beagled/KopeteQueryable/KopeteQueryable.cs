//
// KopeteQueryable.cs
//
// Copyright (C) 2005 Novell, Inc.
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

namespace Beagle.Daemon.KopeteQueryable {

	[QueryableFlavor (Name="Kopete", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KopeteQueryable : LuceneFileQueryable {

		private string config_dir, log_dir;

		private int polling_interval_in_seconds = 60;

		private KopeteCrawler crawler;

		private KopeteBuddyListReader list = new KopeteBuddyListReader ();

		public KopeteQueryable () : base ("KopeteIndex")
		{
			config_dir = Path.Combine (KdeUtils.KDEUserDir, "share/apps/kopete");
			log_dir = Path.Combine (config_dir, "logs");
		}

		/////////////////////////////////////////////////
					
		private void StartWorker() 
		{		
			if (! Directory.Exists (log_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			Logger.Log.Info ("Starting Kopete log backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

			if (Inotify.Enabled)
				Watch (log_dir);

			crawler = new KopeteCrawler (log_dir);
			Crawl ();

			if (!Inotify.Enabled) {
				Scheduler.Task task = Scheduler.TaskFromHook (new Scheduler.TaskHook (CrawlHook));
				task.Tag = "Crawling ~/.kopete/logs to find new logfiles";
				task.Source = this;
				ThisScheduler.Add (task);
			}

			stopwatch.Stop ();

			Logger.Log.Info ("Kopete log backend worker thread done in {0}", stopwatch); 
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
		}

		private void CrawlHook (Scheduler.Task task)
		{
			Crawl ();
			task.Reschedule = true;
			task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
		}

		/////////////////////////////////////////////////

		// Sets up an Inotify watch on all subdirectories withing ~/.kopete/logs
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
			indexable.MimeType = "beagle/x-kopete-log";
			indexable.HitType = "IMLog";
			indexable.CacheContent = false;

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

		protected override bool HitFilter (Hit hit) 
		{
			ImBuddy buddy = list.Search (hit ["fixme:speakingto"]);
			
			if (buddy != null) {
				if (!String.IsNullOrEmpty (buddy.Alias))
					hit ["fixme:speakingto_alias"] = buddy.Alias;

				// FIXME: Icons?
			}
			
			return true;
		}

		public override ISnippetReader GetSnippet (string [] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			TextReader reader = TextCache.UserCache.GetReader (hit.Uri);

			if (reader == null)
				return null;

			KopeteSnippetReader snippet_reader = new KopeteSnippetReader (reader);

			return SnippetFu.GetSnippet (query_terms, snippet_reader, full_text, ctx_length, snp_length);
		}

		private class KopeteSnippetReader : TextReader {
			
			private StringBuilder sb;
			private XmlTextReader reader;
			
			public KopeteSnippetReader (TextReader reader)
			{
				this.sb = new StringBuilder ();
				this.reader = new XmlTextReader (reader);
			}
			
			public override string ReadLine ()
			{				
				while (reader.Read ()) {
					if (reader.NodeType != XmlNodeType.Element && reader.Name != "msg")
						continue;
				
					sb.Length = 0;

					if (!String.IsNullOrEmpty (reader ["nick"]))
						sb.Append (reader["nick"]);
					else
						sb.Append (reader ["from"]);

					sb.Append (": ");

					// Advance to the text node for the actual message
					reader.Read ();
				
					sb.Append (reader.Value);

					return sb.ToString ();
				}

				return null;
			}

			public override void Close ()
			{
				reader.Close ();
			}
		}
	}
}
