//
// CalContainer.cs
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
using System.Collections;
using System.Globalization;
using System.IO;
using System.Runtime.InteropServices;

using Beagle.Util;

using Evolution;

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	public class CalContainer : Container {

		private Cal cal;
		private CalView cal_view;
		private CalSourceType cal_source_type;

		private Scheduler.Priority priority = Scheduler.Priority.Delayed;

		public CalContainer (Evolution.Source source, EvolutionDataServerQueryable queryable, string fingerprint, CalSourceType cal_source_type) 
			: base (source, queryable, fingerprint)
		{
			this.cal_source_type = cal_source_type;
		}

		public override bool OpenClient ()
		{
			if (!this.source.IsLocal ()) {
				Logger.Log.Debug ("Skipping remote calendar {0}", this.source.Uri);
				return false;
			}

			try {
				this.cal = new Cal (this.source, cal_source_type);
				this.cal.Open (true);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Unable to open calendar {0}:", this.source.Uri);
				return false;
			}

			return true;
		}

		public override void OpenView ()
		{
			this.cal_view = this.cal.GetCalView ("#t");

			this.cal_view.ObjectsAdded += OnObjectsAdded;
			this.cal_view.ObjectsModified += OnObjectsModified;
			this.cal_view.ObjectsRemoved += OnObjectsRemoved;
			this.cal_view.ViewDone += OnViewDone;

			this.cal_view.Start ();
		}

		public override void IndexAll ()
		{
			CalComponent[] event_list = this.cal.GetItems ("#t");

			Logger.Log.Debug ("Calendar has {0} items", event_list.Length);

			foreach (CalComponent cc in event_list)
				AddCalComponent (cc);
		}

		public override void IndexChanges ()
		{
			CalComponent[] added, changed;
			string[] removed;

			Logger.Log.Debug ("Getting calendar changes for {0}", this.source.Uri);

			try { 
				this.cal.GetChanges ("beagle-" + this.fingerprint, out added, out changed, out removed);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Unable to get changes for {0}:", this.source.Uri);
				return;
			}

			Logger.Log.Debug ("Calendar {0}: {1} added, {2} changed, {3} removed",
					  this.cal.Uri, added.Length, changed.Length, removed.Length);

			foreach (CalComponent cc in added)
				AddCalComponent (cc);

			foreach (CalComponent cc in changed)
				AddCalComponent (cc);

			foreach (string id in removed) {
				RemoveCalComponent (id);
			}
		}

		public override void Remove ()
		{
			Logger.Log.Debug ("Removing calendar source {0}", this.source.Uid);

			Property prop = Property.NewUnsearched ("fixme:source_uid", this.source.Uid);
			this.queryable.RemovePropertyIndexable (prop);

			this.cal_view.Dispose ();
			this.cal.Dispose ();
		}

		private void OnObjectsAdded (object o, Evolution.ObjectsAddedArgs args)
		{
			foreach (CalComponent cc in CalUtil.ICalToCalComponentArray (args.Objects.Handle, this.cal_view.Client))
				AddCalComponent (cc);
		}

		private void OnObjectsModified (object o, Evolution.ObjectsModifiedArgs args)
		{
			foreach (CalComponent cc in CalUtil.ICalToCalComponentArray (args.Objects.Handle, this.cal_view.Client))
				AddCalComponent (cc);
		}

		[StructLayout (LayoutKind.Sequential)]
		private struct CalComponentId {
			public string Uid;
			public string Rid;
		}

		private void OnObjectsRemoved (object o, Evolution.ObjectsRemovedArgs args)
		{
			// FIXME: evolution-sharp doesn't know about
			// CalComponentId itself, so we have to workaround
			// this by re-creating the GLib.List with the right
			// type.
			GLib.List id_list = new GLib.List (args.Uids.Handle,
							   typeof (CalComponentId));

			foreach (CalComponentId id in id_list)
				RemoveCalComponent (id.Uid);
		}

		private void OnViewDone (object o, Evolution.ViewDoneArgs args)
		{
			// Now that we're done synching with the original
			// state of the calendar, switch all new changes to
			// Immediate mode
			priority = Scheduler.Priority.Immediate;
		}

		/////////////////////////////////////
			
		// URI scheme is:
		// calendar:///?source-uid=<value>&comp-uid=<value>[&comp-rid=value]
		//
		// The Uri class sucks SO MUCH ASS.  It shits itself
		// on foo:///?bar so we have to insert something in
		// before "?bar".  This is filed as Ximian bug #76146.
		// Hopefully it is just a bug in Mono and not a
		// fundamental problem of the Uri class.  Fortunately
		// Evolution can handle the horribly mangled URIs
		// that come out of it.

		private Uri GetComponentUri (CalComponent cc)
		{
			return GetComponentUri (cc.Uid);
		}

		private Uri GetComponentUri (string id)
		{
			string protocol;
			switch (cal_source_type) {
			case CalSourceType.Todo:
				protocol = "task";
				break;

			default:
				// FIXME: Could not find the protocol for Journal
				protocol = "calendar";
				break;
			}

			return new Uri (String.Format ("{2}://uri-class-sucks/?source-uid={0}&comp-uid={1}",
						       this.source.Uid, id, protocol));
		}

		/////////////////////////////////////

		private void AddCalComponent (CalComponent cc)
		{
			Indexable indexable = CalComponentToIndexable (cc);
			this.queryable.ScheduleIndexable (indexable, this.priority);
		}

		private void RemoveCalComponent (string id)
		{
			Indexable indexable = new Indexable (GetComponentUri (id));
			indexable.Type = IndexableType.Remove;
			this.queryable.ScheduleIndexable (indexable, Scheduler.Priority.Immediate);
		}

		/////////////////////////////////////

		private Indexable CalComponentToIndexable (CalComponent cc)
		{
			switch (cal_source_type) {
			case CalSourceType.Event:
				return EventToIndexable (cc);
				break;

			case CalSourceType.Todo:
				return TodoToIndexable (cc);
				break;

			case CalSourceType.Journal:
				return MemoToIndexable (cc);
				break;
			}			

			return null;
		}

		private Indexable MemoToIndexable (CalComponent cc)
		{
			Indexable indexable = new Indexable (GetComponentUri (cc));
			indexable.Timestamp = cc.Dtstart;
			indexable.HitType = "Note";
			indexable.Filtering = IndexableFiltering.AlreadyFiltered;

			indexable.AddProperty (Property.NewUnsearched ("fixme:application","evolution"));

			indexable.AddProperty (Property.New ("dc:title", cc.Summary));

			// We remember the note's text so that we can stuff it in
			// the TextCache later.
			// This is here form compability with Tomboy notes.
			foreach (string description in cc.Descriptions) {
				queryable.IndexableTextCache [indexable.Uri] = description;

				StringReader reader = new StringReader (description);
				indexable.SetTextReader (reader);
			}

			return indexable;
		}

		private Indexable TodoToIndexable (CalComponent cc)
		{
			Indexable indexable = new Indexable (GetComponentUri (cc));
			indexable.Timestamp = cc.Dtstart;
			indexable.HitType = "Task";

			indexable.AddProperty (Property.NewUnsearched ("fixme:source_uid", this.source.Uid));
			indexable.AddProperty (Property.NewUnsearched ("fixme:uid", cc.Uid));

			indexable.AddProperty (Property.NewDate ("fixme:starttime", cc.Dtstart.ToUniversalTime ()));

			if (cc.Dtend != DateTime.MinValue)
				indexable.AddProperty (Property.NewDate ("fixme:endtime", cc.Dtend.ToUniversalTime ()));

			foreach (string description in cc.Descriptions)
				indexable.AddProperty (Property.New ("fixme:description", description));

			indexable.AddProperty (Property.New ("fixme:summary", cc.Summary));

			foreach (string category in cc.Categories)
				indexable.AddProperty (Property.NewUnsearched ("fixme:category", category));

			return indexable;
		}

		private Indexable EventToIndexable (CalComponent cc)
		{
			Indexable indexable = new Indexable (GetComponentUri (cc));
			indexable.Timestamp = cc.Dtstart;
			indexable.HitType = "Calendar";

			indexable.AddProperty (Property.NewUnsearched ("fixme:source_uid", this.source.Uid));
			indexable.AddProperty (Property.NewUnsearched ("fixme:uid", cc.Uid));

			indexable.AddProperty (Property.NewDate ("fixme:starttime", cc.Dtstart.ToUniversalTime ()));

			if (cc.Dtend != DateTime.MinValue)
				indexable.AddProperty (Property.NewDate ("fixme:endtime", cc.Dtend.ToUniversalTime ()));

			foreach (CalComponentAttendee attendee in cc.Attendees)
				indexable.AddProperty (Property.New ("fixme:attendee", attendee.value));

			foreach (string comment in cc.Comments)
				indexable.AddProperty (Property.New ("fixme:comment", comment));
			
			foreach (string description in cc.Descriptions)
				indexable.AddProperty (Property.New ("fixme:description", description));

			indexable.AddProperty (Property.New ("fixme:summary", cc.Summary));

			foreach (string category in cc.Categories)
				indexable.AddProperty (Property.NewUnsearched ("fixme:category", category));

			indexable.AddProperty (Property.New ("fixme:location", cc.Location));

			return indexable;
		}
	}
}
