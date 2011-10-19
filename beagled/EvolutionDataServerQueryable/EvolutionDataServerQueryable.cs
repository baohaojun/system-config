//
// EvolutionDataServerQueryable.cs
//
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
using System.Collections;
using System.Globalization;
using System.Text;
using System.Threading;
using System.IO;

using Beagle.Daemon;
using Beagle.Util;

using Evolution;

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	[QueryableFlavor (Name="EvolutionDataServer", Domain=QueryDomain.Local, RequireInotify=false)]
	public class EvolutionDataServerQueryable : LuceneQueryable {

		// Index versions
		// 1: Original version
		// 2: Updated URI scheme for Evolution 2.4/EDS 1.4
		// 3: Add a "item_type" for calendar items, to differentiate between events, tasks, and memos
		// 4: Modify memos to be comaptible with Tomboy notes and make tasks a seperate HitType

		private const int INDEX_VERSION = 4;

		private string photo_dir;
		private SchedulingIndexableGenerator generator;

		// This is our text cache that gets flushed in our post
		// add hook and is currently only used for memos
		// snippets for compability with Tomboy notes :-(
		private Hashtable indexable_text_cache;

		public EvolutionDataServerQueryable () : base ("EvolutionDataServerIndex", INDEX_VERSION)
		{
			indexable_text_cache = UriFu.NewHashtable ();

			photo_dir = Path.Combine (Driver.TopDirectory, "Photos");
			System.IO.Directory.CreateDirectory (photo_dir);

			generator = new SchedulingIndexableGenerator (this, "Evolution Data Server");
		}

		public override void Start ()
		{
			base.Start ();

			// Defer the actual startup until the main loop starts.
			// EDS requires StartWorker to run in mainloop,
			// hence it is not started in a separate thread.
			GLib.Idle.Add (new GLib.IdleHandler (delegate () { StartWorker (); return false; }));
		}

		private void StartWorker ()
		{
			Logger.Log.Info ("Scanning EDS sources (Addressbook, Calendars, Memos, ...)");

			Stopwatch timer = new Stopwatch ();
			timer.Start ();

			IsIndexing = true;

			try {
				ConnectToEDS ();
			} catch (Exception e) {
				Logger.Log.Error (e, "Unable to start EvolutionDataServer backend");
			} finally {
				IsIndexing = false;
			}

			timer.Stop ();

			Logger.Log.Info ("Scanned EDS sources in {0}", timer);
		}

		private void ConnectToEDS ()
		{
			try {
				// FIXME: This is a total hack.  We call into a
				// method inside libevolutionglue so that we can
				// possibly catch a DllNotFoundException if it
				// fails to load.
				CalUtil.datetime_to_icaltimetype (new DateTime ()); // This is a no-op

				// This is the first code which tries to open the
				// evolution-data-server APIs.  Try to catch
				// DllNotFoundException and bail out if things go
				// badly.
				new SourcesHandler ("/apps/evolution/addressbook/sources", typeof (BookContainer), this, Driver.Fingerprint);
				new SourcesHandler ("/apps/evolution/calendar/sources", typeof (CalContainer), this, Driver.Fingerprint, CalSourceType.Event);
				new SourcesHandler ("/apps/evolution/tasks/sources", typeof (CalContainer), this, Driver.Fingerprint, CalSourceType.Todo);
				new SourcesHandler ("/apps/evolution/memos/sources", typeof (CalContainer), this, Driver.Fingerprint, CalSourceType.Journal);
			} catch (DllNotFoundException ex) {
				Logger.Log.Error (ex, "Unable to start EvolutionDataServer backend: Unable to find or open libraries:");			
				return;
			} catch (EntryPointNotFoundException ex) {
				Logger.Log.Error (ex, "Unable to start EvolutionDataServer backend: Unable to find or open libraries:");			
				return;
			}		       
		}

		public void ScheduleIndexable (Indexable indexable, Scheduler.Priority priority)
		{
			generator.Add (indexable, priority);
		}

		public void RemovePropertyIndexable (Property prop)
		{
			Scheduler.Task task = NewRemoveByPropertyTask (prop);
			task.Priority = Scheduler.Priority.Immediate;
			ThisScheduler.Add (task);
		}

		protected override Uri PostAddHook (Indexable indexable, IndexerAddedReceipt receipt)
		{
			base.PostAddHook (indexable, receipt);
			
			if (indexable_text_cache.ContainsKey (indexable.Uri)) {
				string text = (string)indexable_text_cache [indexable.Uri];
				TextCache.UserCache.WriteFromString (indexable.Uri, text);			       

				indexable_text_cache.Remove (indexable.Uri);
			}

			return indexable.Uri;
		}

		public string PhotoDir {
			get { return photo_dir; }
		}

		public Hashtable IndexableTextCache {
			get { return indexable_text_cache; }
		}
	}
}
