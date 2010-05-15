//
// KNotesQueryable.cs
//
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

namespace Beagle.Daemon.KNotesQueryable {

	[QueryableFlavor (Name="KNotes", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KNotesQueryable : LuceneFileQueryable {

		private static Logger log = Logger.Get ("KNotesQueryable");

		private string knotes_dir;
		private string knotes_file;
		private Hashtable last_modified_table;

		public KNotesQueryable () : base ("KNotesIndex")
		{
			knotes_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			knotes_dir = Path.Combine (knotes_dir, "share");
			knotes_dir = Path.Combine (knotes_dir, "apps");
			knotes_dir = Path.Combine (knotes_dir, "knotes");

			knotes_file = Path.Combine (knotes_dir, "notes.ics");

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
			if (!Directory.Exists (knotes_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			if (Inotify.Enabled) {
				Inotify.EventType mask =  Inotify.EventType.CloseWrite
							| Inotify.EventType.MovedTo;
				Inotify.Subscribe (knotes_dir, OnInotifyEvent, mask);
			} else {
				FileSystemWatcher fsw = new FileSystemWatcher ();
			       	fsw.Path = knotes_dir;
				fsw.Filter = knotes_file;

				fsw.Changed += new FileSystemEventHandler (OnChangedEvent);
				fsw.Created += new FileSystemEventHandler (OnChangedEvent);
				fsw.Renamed += new RenamedEventHandler (OnChangedEvent);
				
				fsw.EnableRaisingEvents = true;
			}

			if (File.Exists (knotes_file)) {
				if (! FileAttributesStore.IsUpToDate (knotes_file))
					Index ();
				else
					ScanNotesInitial ();
			}
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (knotes_dir))
                                return true;

                        this.Start ();

                        return false;
                }

		// We need to scan the notes initially to fill up last_modified_table
		// Otherwise, we might miss deletions that occur before any addition
		private void ScanNotesInitial ()
		{
			NotesIndexableGenerator generator = new NotesIndexableGenerator (this, knotes_file, last_modified_table, true);

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
			if (Path.Combine (path, subitem) != knotes_file)
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
			if (ThisScheduler.ContainsByTag ("KNotes")) {
				Logger.Log.Debug ("Not adding task for already running KNotes task");
				return;
			}

			// Then add the notes from the notes file
			NotesIndexableGenerator generator = new NotesIndexableGenerator (this, knotes_file, last_modified_table, false);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = "KNotes";
			// Make sure add task gets scheduled after delete task
			task.Priority = Scheduler.Priority.Delayed;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		internal void RemoveDeletedNotes (Hashtable deleted_notes)
		{
			ArrayList to_delete = new ArrayList ();
			lock (last_modified_table) {
				foreach (string uid in last_modified_table.Keys) {
					if (! deleted_notes.Contains (uid) ||
					    (bool)deleted_notes [uid] == true) {
						to_delete.Add (uid);
					}
				}
			}

			foreach (string uid in to_delete) {
				RemoveNote (uid);
				last_modified_table.Remove (uid);
			}
		}

		private void RemoveNote (string uid)
		{
			Uri uri = new Uri (String.Format ("knotes:///{0}", uid));
			Logger.Log.Debug ("Removing note {0}", uri);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}

	/**
	 * Indexable generator for KNotes Feeds
	 */
	internal class NotesIndexableGenerator : IIndexableGenerator {
		private string knotes_file;
		private StreamReader reader;
		private KNotesQueryable queryable;
		private bool is_valid_file = true;
		private Hashtable last_modified_table;
		private Hashtable deleted_notes;
		private bool initial_scan;
		
		public NotesIndexableGenerator (KNotesQueryable queryable,
						string knotes_file,
						Hashtable last_modified_table,
						bool initial_scan)
		{
			this.queryable = queryable;
			this.knotes_file = knotes_file;
			this.initial_scan = initial_scan;

			CheckNoteHeader ();
			if (is_valid_file)
				string_builder = new StringBuilder ();

			this.last_modified_table = last_modified_table;
			lock (last_modified_table) {
				this.deleted_notes = new Hashtable (last_modified_table.Count);
				foreach (string uid in last_modified_table.Keys)
					this.deleted_notes [uid] = true;
			}
		}

		public void PostFlushHook ()
		{
			//queryable.FileAttributesStore.AttachLastWriteTime (knotes_file, DateTime.UtcNow);
		}

		public string StatusName {
			get { return knotes_file; }
		}

		private bool IsUpToDate (string path)
		{
			return queryable.FileAttributesStore.IsUpToDate (path);
		}

		private void CheckNoteHeader () {
			if ( (! initial_scan) && IsUpToDate (knotes_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Logger.Log.Debug ("Checking if {0} is a valid KNotes file.", knotes_file);
				/** KNotes file notes.ics should start with
BEGIN:VCALENDAR
PRODID:-//K Desktop Environment//NONSGML libkcal 3.5//EN
VERSION:2.0
				*/
				// FIXME: Encoding of notes.ics
				reader = new StreamReader (knotes_file);
				if (reader.ReadLine () != "BEGIN:VCALENDAR" ||
				    reader.ReadLine () != "PRODID:-//K Desktop Environment//NONSGML libkcal 3.5//EN" ||
				    reader.ReadLine () != "VERSION:2.0") {
					is_valid_file = false;
					reader.Close ();
					return;
				}
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception parsing knotes file:");
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
				if (line == "BEGIN:VJOURNAL")
					break;
				else if (line == "END:VCALENDAR") {
					end = true;
					break;
				}
			}
			if (line == null || end) {
				reader.Close ();
				if (! initial_scan)
					queryable.RemoveDeletedNotes (deleted_notes);
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

			// Keep reading till "END:VJOURNAL"
			while ((line = reader.ReadLine ()) != null) {
//UID:libkcal-1467827482.768
//LAST-MODIFIED:20061015T085606Z
				if (line == "END:VJOURNAL")
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

			// Bad note
			if (string_builder.Length == 0 ||
			    uid == null ||
			    dt == DateTime.MinValue)
				return null;

			// Mark note with uid as seen ('undeleted')
			deleted_notes [uid] = false;

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
			
			// Open knotes notes as
			//dcop knotes KNotesIface text <UID>
			// where the uri is given as knotes://uid
			Uri uri = new Uri (String.Format ("knotes:///{0}", uid));
			Indexable indexable = new Indexable (uri);
			indexable.ParentUri = UriFu.PathToFileUri (knotes_file);
			indexable.MimeType = ICalParser.KnotesMimeType;
			indexable.HitType = "Note";
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
