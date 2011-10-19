//
// KMailQueryable.cs
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
using System.Threading;

using Beagle.Util;

namespace Beagle.Daemon.KMailQueryable {

	[QueryableFlavor (Name="KMail", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KMailQueryable : LuceneFileQueryable {

		// for non-inotify case, poll after this number of seconds
		public const int polling_interval_in_seconds = 300;
		// mail folder paths
		private string local_path, dimap_path;
		// indexers - one for each mailfolder path
		private KMailIndexer local_indexer, dimap_indexer;
		// global variable
		public static bool gmime_initialized = false;
		public static void InitializeGMime ()
		{
			if (!gmime_initialized) {
				GMime.Global.Init ();
				gmime_initialized = true;
			}
		}

		// name of the sentmail folder - should be parsed from kmailrc
		private string sentmail_foldername;
		public string SentMailFolderName {
			get { return sentmail_foldername; }
		}
		
		public KMailQueryable () : base ("KMailIndex")
		{
			// the local mail path is different for different distributions
			local_path = GuessLocalFolderPath (true);
			if (local_path == null) {
				Log.Debug ("KMail folders not found. Will keep trying ");
			} else
				Log.Debug ("Guessing location of KMail folders: found at " + local_path);
			// I hope there is no ambiguity over imap path :P
			dimap_path = Path.Combine (PathFinder.HomeDir, ".kde");
			dimap_path = Path.Combine (dimap_path, "share");
			dimap_path = Path.Combine (dimap_path, "apps");
			dimap_path = Path.Combine (dimap_path, "kmail");
			dimap_path = Path.Combine (dimap_path, "dimap");

			local_indexer = null;
			dimap_indexer = null;
			sentmail_foldername = "sent-mail";
		}

		//////////////////////////////////////////////////////////////////////////////////////////////

		/**
		 * initial method called by the daemon
		 */
		public override void Start () 
		{
			base.Start ();
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		/**
		 * for non-inotify case, this method is invoked repeatedly
		 */
		private void CrawlHook (Scheduler.Task task)
		{
			if (local_indexer != null)
				local_indexer.Crawl ();
			if (dimap_indexer != null)
				dimap_indexer.Crawl ();
			task.Reschedule = true;
			task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
		}

		/**
		 * called by Start(), starts actual work
		 * create indexers
		 * ask indexers to crawl the mails
		 * for non-inotify case, ask to poll
		 */
		private void StartWorker ()
		{
			Log.Debug ("Starting KMail backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

			// check if there is at all anything to crawl
                        if ( local_path == null && (!Directory.Exists (dimap_path))) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				Log.Debug ("KMail directories (local mail) " + dimap_path + " not found, will repoll.");
                                return;
			}

			Log.Debug ("Starting mail crawl");
			if (local_path != null) {
				local_indexer = new KMailIndexer (this, "local", local_path);
				local_indexer.Crawl ();
			}
			// FIXME: parse kmailrc to get dimap account name
			if (Directory.Exists (dimap_path)) {
				dimap_indexer = new KMailIndexer (this, "dimap", dimap_path);
				dimap_indexer.Crawl ();
			}
			Log.Debug ("Mail crawl done");

			if (! Inotify.Enabled) {
				Scheduler.Task task = Scheduler.TaskFromHook (new Scheduler.TaskHook (CrawlHook));
				task.Tag = "Crawling Maildir directories";
				task.Source = this;
				task.TriggerTime = DateTime.Now.AddSeconds (polling_interval_in_seconds);
				ThisScheduler.Add (task);
			}

			stopwatch.Stop ();
			Log.Debug ("KMail driver worker thread done in {0}", stopwatch);
		}

		/** 
		 * use this method to determine if we have anything to crawl and index
		 */
		private bool CheckForExistence ()
                {
			local_path = GuessLocalFolderPath (false);
                        if (local_path == null && (!Directory.Exists (dimap_path)))
                                return true;

			StartWorker();
                        return false;
                }

		/////////////////////////////////////////////////////////////////////////////

		// FIXME: How to determine if an mbox hit is valid without scanning the whole file

		public string Name {
			get { return "KMail"; }
		}

		protected override int ProgressPercent {
			get {
				if (! IsIndexing)
					return -1;

				double local_progress = 0.5;
				if (local_indexer != null)
					local_progress = local_indexer.Progress/2;
				
				double dimap_progress = 0.5;
				if (dimap_indexer != null)
					dimap_progress = dimap_indexer.Progress/2;

				return (int) (local_progress * 100 + dimap_progress * 100);
			}
		}

		private int indexer_indexing = 0;
		internal bool Indexing {
			set {
				// FIXME: This only works because both indexers call
				// Indexing=true exactly once and Indexing=false exactly once.
				indexer_indexing += (value ? 1 : -1);

				if (indexer_indexing == 0)
					IsIndexing = false;
				else
					IsIndexing = true;
			}
		}

		/** 
		 * path of local maildir - mine is in ~/.Mail
		 * This is distribution specific. Mandrake puts kmail mails in
		 * ~/.Mail whereas default kmail folder location is ~/Mail
		 * I guess each distribution can fix this path as they know what is
		 * the path.
		 * It is possible to have the path specified in kmailrc. It might not
		 * be present, in which case try to play a guessing game.
		 * Till then, using a guesser to find out which of ~/.Mail and ~/Mail
		 * is valid.
		 * Guesses the kmail local folder path
		 * first try ~/.kde/share/apps/kmail/mail
		 * then try ~/.kde4/share/apps/kmail/mail (for KDE4 distro but KDE3 PIM)
		 * then try ~/.Mail, then try ~/Mail
		 * Also do not try to "guess" for folder location specified
		 * in config or the .kde/share/... location
		 */
		private string GuessLocalFolderPath (bool verbose)
		{
			string locationrc = GetLocalFolderPathFromKmailrc ();
			//Log.Debug ("Reading kmail local-mail location from kmailrc: " + 
			//		    (locationrc == null ? "Unavailable" : locationrc));
			string location1 = Path.Combine (PathFinder.HomeDir, "Mail");
			string location2 = Path.Combine (PathFinder.HomeDir, ".Mail");

			string location3 = Path.Combine (PathFinder.HomeDir, ".kde");
			location3 = Path.Combine (location3, "share");
			location3 = Path.Combine (location3, "apps");
			location3 = Path.Combine (location3, "kmail");
			location3 = Path.Combine (location3, "mail");

			string location4 = Path.Combine (PathFinder.HomeDir, ".kde4");
			location4 = Path.Combine (location3, "share");
			location4 = Path.Combine (location3, "apps");
			location4 = Path.Combine (location3, "kmail");
			location4 = Path.Combine (location3, "mail");

			if (locationrc != null)
				// If location is present in config file,
				// do not check for other locations.
				return locationrc;
			else if (Directory.Exists (location3))
				return location3;
			else if (Directory.Exists (location4))
				return location4;
			else if (GuessLocalFolder (location1, verbose))
				return location1;
			else if (GuessLocalFolder (location2, verbose))
				return location2;
			else 
				return null;
		}

		/**
		 * To check if the path represents a kmail directory:
		 * for all directories and files named "ddd" and not starting with a '.',
		 * there should be matching index file named .ddd.index
		 * Ignore zero length files; zero-length mbox files might not have index files.
		 */
		private bool GuessLocalFolder (string path, bool verbose)
		{
			if (! Directory.Exists (path))
				return false;

			if (verbose)
				Log.Debug ("Checking if {0} is kmail local mail directory ?", path);

			bool no_content = true;

			foreach (string entry in DirectoryWalker.GetItemNames (path, null)) {
				if (entry.StartsWith ("."))
					continue;

				// Ignore zero size mbox files
				string fullpath = Path.Combine (path, entry);
				if (File.Exists (fullpath)) {
					FileInfo fi = new FileInfo (fullpath);
					if (fi.Length == 0)
						continue;
				}

				// index-file name is of pattern .name.index
				string indexfile = Path.Combine (path, "." + entry + ".index");
				if (! File.Exists (indexfile)) {
					if (verbose)
						Log.Warn ( String.Format (
							"KMail backend: No index file for {0}." +
							"Ignoring {1}, probably not a kmail directory.",
							fullpath, path));
					return false;
				} else
					no_content = false;
			}

			return (! no_content);
		}

		/**
		 * tries to extract folder name from ~/.kde/share/config/kmailrc
		 */
		private static string GetLocalFolderPathFromKmailrc ()
		{
			string path = ReadKDEConfig (Path.Combine (PathFinder.HomeDir, ".kde"));
			if (path != null)
				return path;

			path = ReadKDEConfig (Path.Combine (PathFinder.HomeDir, ".kde4"));
			return path;
		}

		private static string ReadKDEConfig (string kde_dir)
		{
			string kmailrc = kde_dir;
			kmailrc = Path.Combine (kmailrc, "share");
			kmailrc = Path.Combine (kmailrc, "config");
			kmailrc = Path.Combine (kmailrc, "kmailrc");

			if (! File.Exists (kmailrc))
				return null;

			using (StreamReader reader = new StreamReader (kmailrc)) {
				string section = "";
				string line;

				while ((line = reader.ReadLine ()) != null) {
					if (line.StartsWith ("[") && line.EndsWith ("]")) {
						section = line;
					}
					if (section == "[General]") {
						if (line.StartsWith ("folders=") && line.Length > 8) {
							return StringFu.ExpandEnvVariables (line.Substring(8));
						}
					}
				}
			}

			return null;
		}

	}

}
