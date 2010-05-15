//
// EvolutionMailQueryable.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
//
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
using System.Threading;

using Beagle.Util;

namespace Beagle.Daemon.EvolutionMailQueryable {

	[QueryableFlavor (Name="EvolutionMail", Domain=QueryDomain.Local, RequireInotify=false)]
	public class EvolutionMailQueryable : LuceneQueryable {

		public int polling_interval_in_seconds = 60;

		private string local_path, imap_path, imap4_path;

		private MailCrawler crawler;

		private ArrayList running_generators = new ArrayList ();

		// Index versions
		// 1: Original version, stored all recipient addresses as a
		//    RFC822 string
		// 2: Stores recipients in separate properties,
		//    filters/indexes all attachments
		// 3: Make email addresses non-keyword, add sanitized version
		//    for searching for parts of an email address.
		// 4: Make the flags property mutable, and create a property
		//    change Indexable when they change for IMAP generators.
		// 5: No need for a separate sanitized version of email addresses.
		private const int INDEX_VERSION = 5;

		public EvolutionMailQueryable () : base ("EvolutionMailIndex", INDEX_VERSION)
		{
			this.local_path = Path.Combine (PathFinder.HomeDir, ".evolution/mail/local");
			this.imap_path = Path.Combine (PathFinder.HomeDir, ".evolution/mail/imap");
			this.imap4_path = Path.Combine (PathFinder.HomeDir, ".evolution/mail/imap4");
		}

		private void CrawlHook (Scheduler.Task task)
		{
			crawler.Crawl ();
			task.Reschedule = true;
			task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
		}

		//////////////////////////////////////////////////////////////////////////////////////////////

		private void StartWorker ()
		{
			Logger.Log.Info ("Starting Evolution mail backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

			// Check that we have data to index
			if ((! Directory.Exists (this.local_path)) && (! Directory.Exists (this.imap_path))) {
				// No mails present, repoll every minute
				Logger.Log.Warn ("Evolution mail store not found, watching for it.");
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForMailData));
				return;
			}

			Logger.Log.Debug ("Starting mail crawl");
			crawler = new MailCrawler (this.local_path, this.imap_path, this.imap4_path);
			crawler.MboxAddedEvent += IndexMbox;
			crawler.SummaryAddedEvent += IndexSummary;
			crawler.Crawl ();
			Logger.Log.Debug ("Mail crawl finished");

			// If we don't have inotify, we have to poll the file system.  Ugh.
			if (! Inotify.Enabled) {
				Scheduler.Task task = Scheduler.TaskFromHook (new Scheduler.TaskHook (CrawlHook));
				task.Tag = "Crawling ~/.evolution to find summary changes";
				task.Source = this;
				ThisScheduler.Add (task);
			}

			stopwatch.Stop ();
			Logger.Log.Info ("Evolution mail driver worker thread done in {0}",
					 stopwatch);
		}

		public override void Start () 
		{
			base.Start ();
			
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private bool CheckForMailData ()
		{
			if ((! Directory.Exists (this.local_path)) && (! Directory.Exists (this.imap_path)))
				return true; // continue polling
			
			// Otherwise stop polling and start indexing
			StartWorker();
			return false;
		}

		public string Name {
			get { return "EvolutionMail"; }
		}

		public void IndexSummary (FileInfo summaryInfo, bool inotify_event)
		{
			// If there's already a task running for this folder,
			// don't interrupt it.
			if (ThisScheduler.ContainsByTag (summaryInfo.FullName)) {
				Logger.Log.Debug ("Not adding task for already running task: {0}", summaryInfo.FullName);
				return;
			}

			Logger.Log.Debug ("Will index summary {0}", summaryInfo.FullName);
			EvolutionMailIndexableGeneratorImap generator = new EvolutionMailIndexableGeneratorImap (this, summaryInfo);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = summaryInfo.FullName;
			ThisScheduler.Add (task);

			AddGenerator (generator, inotify_event);
		}

		public void IndexMbox (FileInfo mboxInfo, bool inotify_event)
		{
			// If there's already a task running for this mbox,
			// don't interrupt it.
			if (ThisScheduler.ContainsByTag (mboxInfo.FullName)) {
				Logger.Log.Debug ("Not adding task for already running task: {0}", mboxInfo.FullName);
				return;
			}

			Logger.Log.Debug ("Will index mbox {0}", mboxInfo.FullName);
			EvolutionMailIndexableGeneratorMbox generator = new EvolutionMailIndexableGeneratorMbox (this, mboxInfo);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = mboxInfo.FullName;
			ThisScheduler.Add (task);

			AddGenerator (generator, inotify_event);
		}

		internal void AddGenerator (EvolutionMailIndexableGenerator generator, bool inotify_event)
		{
			lock (running_generators)
				running_generators.Add (generator);

			if (! inotify_event)
				IsIndexing = true;
		}

		internal void RemoveGenerator (EvolutionMailIndexableGenerator generator)
		{
			lock (running_generators) {
				running_generators.Remove (generator);

				if (running_generators.Count == 0)
					IsIndexing = false;
			}
		}

		protected override int ProgressPercent {
			get {
				lock (running_generators) {
					if (running_generators.Count == 0)
						return -1;

					// An embarrassingly unscientific attempt at getting progress
					// information from the mail backend as a whole.  Unfortunately
					// the IMAP and mbox backends don't have a common unit of
					// measurement (IMAP has number of messages, mbox number of
					// bytes), so we can't get anything really accurate.
					double total_percent = 0;

					foreach (EvolutionMailIndexableGenerator generator in running_generators)
						total_percent += generator.ProgressPercent;

					return (int) (total_percent / running_generators.Count);
				}
			}
		}

		public static Uri EmailUri (string accountName, string folderName, string uid)
		{
			// Set the "don't escape" flag on the Uri constructor
			// so that the hostname part of the URI isn't
			// automatically lowercased.  Evolution is case-
			// sensitive with the whole URI.
			return new Uri (String.Format ("email://{0}/{1};uid={2}",
						       accountName, folderName, uid), true);
		}
	}
}
