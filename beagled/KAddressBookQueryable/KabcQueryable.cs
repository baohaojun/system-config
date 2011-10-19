//
// KabcQueryable.cs
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

// Currently this backend is awfully similar to the KNotes backend! Tons of duplication of code.
// However, kabc will ultimately be different when it tries to grab data from non-disk stores.

namespace Beagle.Daemon.KabcQueryable {

	[QueryableFlavor (Name="KAddressBook", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KabcQueryable : LuceneFileQueryable {

		private string kabc_dir;
		private string kabc_file;
		private Hashtable last_modified_table;

		// 1: Update KAddressbook filter.
		const int MINOR_VERSION = 1;

		public KabcQueryable () : base ("KAddressBookIndex", MINOR_VERSION)
		{
			kabc_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			kabc_dir = Path.Combine (kabc_dir, "share");
			kabc_dir = Path.Combine (kabc_dir, "apps");
			kabc_dir = Path.Combine (kabc_dir, "kabc");

			kabc_file = Path.Combine (kabc_dir, "std.vcf");

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
			if (!Directory.Exists (kabc_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			if (Inotify.Enabled) {
				Inotify.EventType mask = Inotify.EventType.Create | Inotify.EventType.MovedTo;
				Inotify.Subscribe (kabc_dir, OnInotifyEvent, mask);
			} else {
				FileSystemWatcher fsw = new FileSystemWatcher ();
			       	fsw.Path = kabc_dir;
				fsw.Filter = kabc_file;

				fsw.Created += new FileSystemEventHandler (OnChangedEvent);
				fsw.Renamed += new RenamedEventHandler (OnChangedEvent);
				
				fsw.EnableRaisingEvents = true;
			}

			if (File.Exists (kabc_file)) {
				if (! FileAttributesStore.IsUpToDate (kabc_file))
					Index ();
				else
					ScanAddrInitial ();
			}
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (kabc_dir))
                                return true;

                        this.Start ();

                        return false;
                }

		// We need to scan the addresses nitially to fill up last_modified_table
		// Otherwise, we might miss deletions that occur before any addition
		private void ScanAddrInitial ()
		{
			AddressIndexableGenerator generator = new AddressIndexableGenerator (this, kabc_file, last_modified_table, true);

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
			if (Path.Combine (path, subitem) != kabc_file)
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
			if (ThisScheduler.ContainsByTag ("KAddressBook")) {
				Logger.Log.Debug ("Not adding task for already running Kabc task");
				return;
			}

			AddressIndexableGenerator generator = new AddressIndexableGenerator (this, kabc_file, last_modified_table, false);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = "KAddressBook";
			task.Priority = Scheduler.Priority.Delayed;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		internal void RemoveDeletedContacts (Hashtable deleted_contacts)
		{
			ArrayList to_delete = new ArrayList ();
			lock (last_modified_table) {
				foreach (string uid in last_modified_table.Keys) {
					if (! deleted_contacts.Contains (uid) ||
					    (bool)deleted_contacts [uid] == true) {
						to_delete.Add (uid);
					}
				}
			}

			foreach (string uid in to_delete) {
				RemoveContact (uid);
				last_modified_table.Remove (uid);
			}
		}

		private void RemoveContact (string uid)
		{
			Uri uri = new Uri (String.Format ("kabc:///{0}", uid));
			Logger.Log.Debug ("Removing contact {0}", uri);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}

	/**
	 * Indexable generator for Kabc Feeds
	 */
	internal class AddressIndexableGenerator : IIndexableGenerator {
		private string kabc_file;
		private StreamReader reader;
		private KabcQueryable queryable;
		private bool is_valid_file = true;
		private Hashtable last_modified_table;
		private Hashtable deleted_contacts;
		private bool initial_scan;
		private DateTime file_last_write_time;
		private string current_uid = String.Empty;
		private DateTime current_dt;
		
		public AddressIndexableGenerator (KabcQueryable queryable,
						string kabc_file,
						Hashtable last_modified_table,
						bool initial_scan)
		{
			this.queryable = queryable;
			this.kabc_file = kabc_file;
			this.initial_scan = initial_scan;

			CheckContactHeader ();
			if (is_valid_file)
				string_builder = new StringBuilder ();

			this.last_modified_table = last_modified_table;
			lock (last_modified_table) {
				this.deleted_contacts = new Hashtable (last_modified_table.Count);
				foreach (string uid in last_modified_table.Keys)
					this.deleted_contacts [uid] = true;
			}

			// cache the last write time
			file_last_write_time = FileSystem.GetLastWriteTimeUtc (kabc_file);
		}

		public void PostFlushHook ()
		{
			//queryable.FileAttributesStore.AttachLastWriteTime (kabc_file, DateTime.UtcNow);
		}

		public string StatusName {
			get { return current_uid; }
		}

		private bool IsUpToDate (string path)
		{
			return queryable.FileAttributesStore.IsUpToDate (path);
		}

		private void CheckContactHeader () {
			if ( (! initial_scan) && IsUpToDate (kabc_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Logger.Log.Debug ("Checking if {0} is a valid Kabc file.", kabc_file);
				reader = new StreamReader (kabc_file);
			} catch (Exception ex) {
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
			while ((line = reader.ReadLine ()) != null) {
				if (line == "BEGIN:VCARD")
					if (ReadContact ())
						return true;
			}

			reader.Close ();
			if (! initial_scan)
				queryable.RemoveDeletedContacts (deleted_contacts);

			return false;
		}

		string[] fmts = {"yyyy-MM-ddTHH:mm:sszzz"};

		private bool ReadContact ()
		{
			string line;
			string_builder.Length = 0;

			current_uid = null;
			// date is set to REV:date if specified, o/w file write time
			current_dt = file_last_write_time;

			// Keep reading till "END:VCARD"
			while ((line = reader.ReadLine ()) != null) {
//UID:4QBCOYKj4c
//REV:2006-09-30T17:41:52Z
				if (line == "END:VCARD")
					break;
				else if (line.StartsWith ("UID:"))
					current_uid = line.Substring (4);
				else if (line.StartsWith ("REV:")) {
					string dt_string = line.Substring (4);
					dt_string = dt_string.Replace ("Z", "+00:00");
					current_dt = DateTime.ParseExact (
						dt_string,
						fmts,
						DateTimeFormatInfo.InvariantInfo,
						DateTimeStyles.AdjustToUniversal);
				} else {
					string_builder.Append (line);
					string_builder.Append ('\n');
				}
			}

			if (line == null)
				return false;

			// Bad contact
			if (string_builder.Length == 0 || current_uid == null)
				return false;

			// Mark address with current_uid as seen ('undeleted')
			deleted_contacts [current_uid] = false;

			lock (last_modified_table) {
				if (last_modified_table.Contains (current_uid)) {
					DateTime old_dt = (DateTime) last_modified_table [current_uid];
					// Address up-to-date
					if (current_dt == old_dt)
						return false;
					else {
						//Log.Debug ("Updating last_mod_date [{0}] = {1}", current_uid, current_dt);
						last_modified_table [current_uid] = current_dt;
					}
				} else {
					//Log.Debug ("Adding last_mod_date [{0}] = {1}", current_uid, current_dt);
					last_modified_table [current_uid] = current_dt;
				}
			}

			return true;

		}

		public Indexable GetNextIndexable ()
		{
			if (initial_scan)
				return null;
			
			Uri uri = new Uri (String.Format ("kabc:///{0}", current_uid));
			Indexable indexable = new Indexable (uri);
			indexable.ParentUri = UriFu.PathToFileUri (kabc_file);
			indexable.MimeType = ICalParser.KabcMimeType;
			indexable.HitType = "Contact";
			indexable.Timestamp = current_dt;
			indexable.AddProperty (Property.NewKeyword ("fixme:client", "KAddressBook"));
			// Add uid as a keyword field for convenience
			indexable.AddProperty (Property.NewUnsearched ("fixme:uid", current_uid));

			// FIXME: Comment this Debug statement after the backend stabilizes
			//Log.Debug ("Creating {0} from:[{1}]", uri, string_builder.ToString ());
			StringReader string_reader = new StringReader (string_builder.ToString());
			indexable.SetTextReader (string_reader);

			return indexable;
		}

	}

}
