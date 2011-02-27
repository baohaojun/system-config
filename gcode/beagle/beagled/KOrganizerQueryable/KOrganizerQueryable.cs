//
// KOrganizerQueryable.cs
//
// Copyright (C) 2007 Stephan Binner <binner@kde.org>
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2004 Novell, Inc.
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
using System.Collections;
using System.Threading;
using System.Globalization;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.KOrganizerQueryable {

	[QueryableFlavor (Name="KOrganizer", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KOrganizerQueryable : LuceneFileQueryable {

		private static Logger log = Logger.Get ("KOrganizerQueryable");

		private string korganizer_dir;
		private string korganizer_file;
		private Hashtable last_modified_table;

		public KOrganizerQueryable () : base ("KOrganizerIndex")
		{
			korganizer_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			korganizer_dir = Path.Combine (korganizer_dir, "share");
			korganizer_dir = Path.Combine (korganizer_dir, "apps");
			korganizer_dir = Path.Combine (korganizer_dir, "korganizer");

			korganizer_file = Path.Combine (korganizer_dir, "std.ics");

			last_modified_table = new Hashtable ();
		}

		/////////////////////////////////////////////////

		public override void Start ()
		{
			base.Start ();

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (!Directory.Exists (korganizer_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			if (Inotify.Enabled) {
				Inotify.EventType mask =  Inotify.EventType.CloseWrite
							| Inotify.EventType.MovedTo;
				Inotify.Subscribe (korganizer_dir, OnInotifyEvent, mask);
			} else {
				FileSystemWatcher fsw = new FileSystemWatcher ();
			       	fsw.Path = korganizer_dir;
				fsw.Filter = korganizer_file;

				fsw.Changed += new FileSystemEventHandler (OnChangedEvent);
				fsw.Created += new FileSystemEventHandler (OnChangedEvent);
				fsw.Renamed += new RenamedEventHandler (OnChangedEvent);

				fsw.EnableRaisingEvents = true;
			}

			if (File.Exists (korganizer_file)) {
				if (! FileAttributesStore.IsUpToDate (korganizer_file))
					Index ();
				else
					ScanEntriesInitial ();
			}
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (korganizer_dir))
                                return true;

                        this.Start ();

                        return false;
                }

		// We need to scan the entries initially to fill up last_modified_table
		// Otherwise, we might miss deletions that occur before any addition
		private void ScanEntriesInitial ()
		{
			EntriesIndexableGenerator generator = new EntriesIndexableGenerator (this, korganizer_file, last_modified_table, true);

			// just a dummy scan
			while (generator.HasNextIndexable ())
				generator.GetNextIndexable ();
		}

		/////////////////////////////////////////////////

		// Modified event using Inotify
		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (Path.Combine (path, subitem) != korganizer_file)
				return;

			Index ();
		}

		// Modified/Created event using FSW
		private void OnChangedEvent (object o, FileSystemEventArgs args)
		{
			Index ();
		}

		/////////////////////////////////////////////////

		private void Index ()
		{
			if (ThisScheduler.ContainsByTag ("KOrganizer")) {
				Logger.Log.Debug ("Not adding task for already running KOrganizer task");
				return;
			}

			// Then add the entries from the KOrganizer file
			EntriesIndexableGenerator generator = new EntriesIndexableGenerator (this, korganizer_file, last_modified_table, false);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = "KOrganizer";
			// Make sure add task gets scheduled after delete task
			task.Priority = Scheduler.Priority.Delayed;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		internal void RemoveDeletedEntries (Hashtable deleted_entries)
		{
			ArrayList to_delete = new ArrayList ();
			lock (last_modified_table) {
				foreach (string uid in last_modified_table.Keys) {
					if (! deleted_entries.Contains (uid) ||
					    (bool)deleted_entries [uid] == true) {
						to_delete.Add (uid);
					}
				}
			}

			foreach (string uid in to_delete) {
				RemoveEntry (uid);
				last_modified_table.Remove (uid);
			}
		}

		private void RemoveEntry (string uid)
		{
			Uri uri = new Uri (String.Format ("korganizer:///{0}", uid));
			Logger.Log.Debug ("Removing entry {0}", uri);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}

	/**
	 * Indexable generator for KOrganizer Feeds
	 */
	internal class EntriesIndexableGenerator : IIndexableGenerator {
		private string korganizer_file;
		private StreamReader reader;
		private KOrganizerQueryable queryable;
		private bool is_valid_file = true;
		private Hashtable last_modified_table;
		private Hashtable deleted_entries;
		private bool initial_scan;

		public EntriesIndexableGenerator (KOrganizerQueryable queryable,
						string korganizer_file,
						Hashtable last_modified_table,
						bool initial_scan)
		{
			this.queryable = queryable;
			this.korganizer_file = korganizer_file;
			this.initial_scan = initial_scan;

			CheckEntryHeader ();
			if (is_valid_file)
				string_builder = new StringBuilder ();

			this.last_modified_table = last_modified_table;
			lock (last_modified_table) {
				this.deleted_entries = new Hashtable (last_modified_table.Count);
				foreach (string uid in last_modified_table.Keys)
					this.deleted_entries [uid] = true;
			}
		}

		public void PostFlushHook ()
		{
			//queryable.FileAttributesStore.AttachLastWriteTime (korganizer_file, DateTime.UtcNow);
		}

		public string StatusName {
			get { return korganizer_file; }
		}

		private bool IsUpToDate (string path)
		{
			return queryable.FileAttributesStore.IsUpToDate (path);
		}

		private void CheckEntryHeader () {
			if ( (! initial_scan) && IsUpToDate (korganizer_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Logger.Log.Debug ("Checking if {0} is a valid KOrganizer file.", korganizer_file);
				/** KOrganizer file std.ics should start with something like
BEGIN:VCALENDAR
PRODID:-//K Desktop Environment//NONSGML
VERSION:2.0
				*/
				// FIXME: Encoding of std.ics
				reader = new StreamReader (korganizer_file);
				if (reader.ReadLine () != "BEGIN:VCALENDAR" ||
				    !reader.ReadLine ().StartsWith("PRODID:-//K Desktop Environment//NONSGML") ||
				    reader.ReadLine () != "VERSION:2.0") {
					is_valid_file = false;
					reader.Close ();
					return;
				}
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception parsing KOrganizer file:");
				is_valid_file = false;
				reader.Close ();
			}
		}

		private StringBuilder string_builder;
		public bool HasNextIndexable ()
		{
			if (!is_valid_file || reader == null)
				return false;

			string line;
			bool end = false;
			while ((line = reader.ReadLine ()) != null) {
				if (line == "BEGIN:VTODO" || line == "BEGIN:VEVENT")
					break;
				else if (line == "END:VCALENDAR") {
					end = true;
					break;
				}
			}
			if (line == null || end) {
				reader.Close ();
				if (! initial_scan)
					queryable.RemoveDeletedEntries (deleted_entries);
				return false;
			}
			return true;
		}

		string[] fmts = {"yyyyMMddTHHmmsszzz"};

		public Indexable GetNextIndexable ()
		{
			string line;
			string_builder.Length = 0;

			DateTime dt = DateTime.MinValue;
			string uid = null;

			// Keep reading till "END:EVENT" or "END:VTODO"
			while ((line = reader.ReadLine ()) != null) {
//UID:libkcal-1467827482.768
//LAST-MODIFIED:20061015T085606Z
				if (line == "END:VEVENT" || line == "END:VTODO")
					break;
				else if (line.StartsWith ("UID:"))
					uid = line.Substring (4);
				else if (line.StartsWith ("LAST-MODIFIED:")) {
					string dt_string = line.Substring (14);
					dt_string = dt_string.Replace ("Z", "+00:00");
					dt = DateTime.ParseExact (
						dt_string,
						fmts,
						DateTimeFormatInfo.InvariantInfo,
						DateTimeStyles.AdjustToUniversal);
				} else {
					string_builder.Append (line);
					string_builder.Append ('\n');
				}
			}

			if (line == null) {
				reader.Close ();
				return null;
			}

			// Bad entry
			if (string_builder.Length == 0 ||
			    uid == null ||
			    dt == DateTime.MinValue)
				return null;

			// Mark entry with uid as seen ('undeleted')
			deleted_entries [uid] = false;

			lock (last_modified_table) {
				if (last_modified_table.Contains (uid)) {
					DateTime old_dt = (DateTime) last_modified_table [uid];
					// FIXME: Returning null for more than 179 times will cause trouble
					if (dt == old_dt)
						return null;
					else {
						//Log.Debug ("Updating last_mod_date [{0}] = {1}", uid, dt);
						last_modified_table [uid] = dt;
					}
				} else {
					//Log.Debug ("Adding last_mod_date [{0}] = {1}", uid, dt);
					last_modified_table [uid] = dt;
				}
			}

			if (initial_scan)
				return null;

			Uri uri = new Uri (String.Format ("korganizer:///{0}", uid));
			Indexable indexable = new Indexable (uri);
			indexable.ParentUri = UriFu.PathToFileUri (korganizer_file);

			if (line == "END:VEVENT") {
				indexable.MimeType = ICalParser.KOrganizerEventMimeType;
				indexable.HitType = "Calendar";
			} else {
				indexable.MimeType = ICalParser.KOrganizerTodoMimeType;
				indexable.HitType = "Task";
			}

			indexable.Timestamp = dt;
			// Add uid as a keyword field for convenience
			indexable.AddProperty (Property.NewUnsearched ("fixme:uid", uid));

			// FIXME: Comment this Debug statement after the backend stabilizes
			//Log.Debug ("Creating {0} from:[{1}]", uri, string_builder.ToString ());
			StringReader string_reader = new StringReader (string_builder.ToString());
			indexable.SetTextReader (string_reader);

			return indexable;
		}

	}

}
