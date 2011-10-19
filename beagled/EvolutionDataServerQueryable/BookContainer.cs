//
// BookContainer.cs
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
using System.Globalization;
using System.IO;

using Beagle.Util;

using Evolution;

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	public class BookContainer : Container {

		private Book book;
		private BookView book_view;
		private bool ignore_first_batch = false;
		private Scheduler.Priority priority = Scheduler.Priority.Delayed;

		public BookContainer (Evolution.Source source, EvolutionDataServerQueryable queryable, string fingerprint) : base (source, queryable, fingerprint) { }

		public override bool OpenClient ()
		{
			if (!this.source.IsLocal ()) {
				Logger.Log.Debug ("Skipping remote addressbook {0}", this.source.Uri);
				return false;
			}

			try {
				this.book = new Book (this.source);
				this.book.Open (true);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Unable to open addressbook {0}:", this.source.Uri);
				return false;
			}

			return true;
		}

		public override void OpenView ()
		{
			this.book_view = this.book.GetBookView (BookQuery.AnyFieldContains (""),
								new object [0],
								-1);

			this.book_view.ContactsAdded += OnContactsAdded;
			this.book_view.ContactsRemoved += OnContactsRemoved;
			this.book_view.ContactsChanged += OnContactsChanged;
			this.book_view.SequenceComplete += OnSequenceComplete;

			this.book_view.Start ();
		}

		public override void IndexAll ()
		{
			// We don't need to do anything here.  All of the
			// contacts will be added as a result of
			// OnContactsAdded being called for every item
			// after setting up the BookView.
		}

		public override void IndexChanges ()
		{
			// Ignore the torrent of add events that will come in
			// after the BookView is set up.
			ignore_first_batch = true;

			Contact[] added, changed;
			string[] removed;

			Logger.Log.Debug ("Getting addressbook changes for {0}", this.source.Uri);

			try {
				this.book.GetChanges ("beagle-" + this.fingerprint, out added, out changed, out removed);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Unable to get changes for {0}:", this.source.Uri);
				return;
			}

			Logger.Log.Debug ("Addressbook {0}: {1} added, {2} changed, {3} removed",
					  this.book.Uri, added.Length, changed.Length, removed.Length);

			foreach (Contact contact in added)
				AddContact (contact);

			foreach (Contact contact in changed)
				AddContact (contact);

			foreach (string id in removed)
				RemoveContact (id);
		}

		public override void Remove ()
		{
			Logger.Log.Debug ("Removing addressbook source {0}", this.source.Uid);

			Property prop = Property.NewUnsearched ("fixme:source_uid", this.source.Uid);
			this.queryable.RemovePropertyIndexable (prop);

			this.book_view.Stop ();
			this.book_view.Dispose ();

			this.book.Dispose ();
		}

		private void OnContactsAdded (object o, Evolution.ContactsAddedArgs args)
		{
			if (ignore_first_batch)
				return;

			foreach (Evolution.Contact contact in args.Contacts)
				AddContact (contact);
		}

		private void OnContactsChanged (object o, Evolution.ContactsChangedArgs args)
		{
			if (ignore_first_batch)
				return;

			foreach (Evolution.Contact contact in args.Contacts)
				AddContact (contact);
		}

		private void OnContactsRemoved (object o, Evolution.ContactsRemovedArgs args)
		{
			if (ignore_first_batch)
				return;

			// FIXME: This is a temporary workaround for the
			// fact that the evolution bindings return a
			// GLib.List with an object type, but there are
			// really strings in there.

			GLib.List id_list = new GLib.List (args.Ids.Handle,
							   typeof (string));

			foreach (string id in id_list)
				RemoveContact (id);
		}

		private void OnSequenceComplete (object o, Evolution.SequenceCompleteArgs args)
		{
			ignore_first_batch = false;

			// Now that we're done synching with the original
			// state of the addressbook, switch all new changes to
			// Immediate mode
			priority = Scheduler.Priority.Immediate;

			Logger.Log.Debug ("Sequence complete!");
		}

		/////////////////////////////////////

		// URI scheme is:
		// contacts:///?source-uid=<value>&contact-uid=<value>
		//
		// The Uri class sucks SO MUCH ASS.  It shits itself
		// on foo:///?bar so we have to insert something in
		// before "?bar".  This is filed as Ximian bug #76146.
		// Hopefully it is just a bug in Mono and not a
		// fundamental problem of the Uri class.  Fortunately
		// Evolution can handle the horribly mangled URIs
		// that come out of it.
		
		private Uri GetContactUri (Evolution.Contact contact) {
			return GetContactUri (contact.Id);
		}

		private Uri GetContactUri (string id) {
			return new Uri (String.Format ("contacts://uri-class-sucks/?source-uid={0}&contact-uid={1}",
						       this.source.Uid, id));
		}

		/////////////////////////////////////

		private void AddContact (Evolution.Contact contact)
		{
			Indexable indexable = ContactToIndexable (contact);

			this.queryable.ScheduleIndexable (indexable, this.priority);
		}

		private void RemoveContact (string id)
		{
			Indexable indexable = new Indexable (GetContactUri (id));
			indexable.Type = IndexableType.Remove;

			this.queryable.ScheduleIndexable (indexable, Scheduler.Priority.Immediate);
		}

		/////////////////////////////////////

		private string GetPhotoFilename (string id)
		{
			return Path.Combine (this.queryable.PhotoDir, id);
		}

		private static DateTime RevStringToDateTime (string date_str)
		{
			if (date_str == null)
				return DateTime.MinValue;

			string[] formats = {
				"yyyy'-'MM'-'dd'T'HH':'mm':'ss'Z'",
				"yyyyMMdd'T'HHmmss'Z'"
			};

			try {
				return DateTime.ParseExact (date_str, formats,
							    CultureInfo.InvariantCulture,
							    DateTimeStyles.None);
			} catch (FormatException) {
				Logger.Log.Warn ("Unable to parse last revision string: {0}", date_str);
				return DateTime.MinValue;
			}
		}

		private Indexable ContactToIndexable (Evolution.Contact contact)
		{
			DateTime rev = RevStringToDateTime (contact.Rev);

			Indexable indexable = new Indexable (GetContactUri (contact));
			indexable.Timestamp = rev;
			indexable.HitType = "Contact";

			indexable.AddProperty (Property.NewKeyword ("fixme:client", "evolution"));
			indexable.AddProperty (Property.NewUnsearched ("fixme:source_uid", this.source.Uid));
			indexable.AddProperty (Property.NewUnsearched ("fixme:uid", contact.Id));
						
			indexable.AddProperty (Property.New ("fixme:FileAs", contact.FileAs));
			indexable.AddProperty (Property.New ("fixme:FullName", contact.FullName));
			indexable.AddProperty (Property.New ("fixme:GivenName", contact.GivenName));
			indexable.AddProperty (Property.New ("fixme:FamilyName", contact.FamilyName));
			indexable.AddProperty (Property.New ("fixme:Nickname", contact.Nickname));
			indexable.AddProperty (Property.New ("fixme:AddressLabelHome", contact.AddressLabelHome));
			indexable.AddProperty (Property.New ("fixme:AddressLabelWork", contact.AddressLabelWork));
			indexable.AddProperty (Property.New ("fixme:AddressLabelOther", contact.AddressLabelOther));
			indexable.AddProperty (Property.New ("fixme:AssistantPhone", contact.AssistantPhone));
			indexable.AddProperty (Property.New ("fixme:BusinessPhone", contact.BusinessPhone));
			indexable.AddProperty (Property.New ("fixme:BusinessPhone2", contact.BusinessPhone2));
			indexable.AddProperty (Property.New ("fixme:BusinessFax", contact.BusinessFax));
			indexable.AddProperty (Property.New ("fixme:CallbackPhone", contact.CallbackPhone));
			indexable.AddProperty (Property.New ("fixme:CarPhone", contact.CarPhone));
			indexable.AddProperty (Property.New ("fixme:CompanyPhone", contact.CompanyPhone));
			indexable.AddProperty (Property.New ("fixme:HomePhone", contact.HomePhone));
			indexable.AddProperty (Property.New ("fixme:HomePhone2", contact.HomePhone2));
			indexable.AddProperty (Property.New ("fixme:HomeFax", contact.HomeFax));
			indexable.AddProperty (Property.New ("fixme:IsdnPhone", contact.IsdnPhone));
			indexable.AddProperty (Property.New ("fixme:MobilePhone", contact.MobilePhone));
			indexable.AddProperty (Property.New ("fixme:OtherPhone", contact.OtherPhone));
			indexable.AddProperty (Property.New ("fixme:OtherFax", contact.OtherFax));
			indexable.AddProperty (Property.New ("fixme:Pager", contact.Pager));
			indexable.AddProperty (Property.New ("fixme:PrimaryPhone", contact.PrimaryPhone));
			indexable.AddProperty (Property.New ("fixme:Radio", contact.Radio));
			indexable.AddProperty (Property.New ("fixme:Telex", contact.Telex));
			indexable.AddProperty (Property.NewUnsearched ("fixme:Tty", contact.Tty));
			indexable.AddProperty (Property.NewKeyword ("fixme:Email1", contact.Email1));
			indexable.AddProperty (Property.NewKeyword ("fixme:Email2", contact.Email2));
			indexable.AddProperty (Property.NewKeyword ("fixme:Email3", contact.Email3));
			indexable.AddProperty (Property.NewUnsearched ("fixme:Mailer", contact.Mailer));
			indexable.AddProperty (Property.New ("fixme:Org", contact.Org));
			indexable.AddProperty (Property.New ("fixme:OrgUnit", contact.OrgUnit));
			indexable.AddProperty (Property.New ("fixme:Office", contact.Office));
			indexable.AddProperty (Property.New ("fixme:Title", contact.Title));
			indexable.AddProperty (Property.New ("fixme:Role", contact.Role));
			indexable.AddProperty (Property.New ("fixme:Manager", contact.Manager));
			indexable.AddProperty (Property.New ("fixme:Assistant", contact.Assistant));
			indexable.AddProperty (Property.NewKeyword ("fixme:HomepageUrl", contact.HomepageUrl));
			indexable.AddProperty (Property.NewKeyword ("fixme:BlogUrl", contact.BlogUrl));
			indexable.AddProperty (Property.NewUnsearched ("fixme:Categories", contact.Categories));
			indexable.AddProperty (Property.NewUnsearched ("fixme:Caluri", contact.Caluri));
			indexable.AddProperty (Property.NewUnsearched ("fixme:Icscalendar", contact.Icscalendar));
			indexable.AddProperty (Property.New ("fixme:Spouse", contact.Spouse));
			indexable.AddProperty (Property.New ("fixme:Note", contact.Note));
			
			Evolution.ContactPhoto photo = contact.Photo;

			if (photo.Data != null && photo.Data.Length > 0) {
				string photo_filename = GetPhotoFilename (contact.Id);
				Stream s = new FileStream (photo_filename, FileMode.Create, FileAccess.Write, FileShare.ReadWrite);
				BinaryWriter w = new BinaryWriter (s);
				w.Write (photo.Data);
				w.Close ();
				s.Close ();

				indexable.AddProperty (Property.NewUnsearched ("beagle:Photo", photo_filename));
			}

			foreach (string im in contact.ImAim)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImAim", im));
			foreach (string im in contact.ImIcq)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImIcq", im));
			foreach (string im in contact.ImJabber)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImJabber", im));
			foreach (string im in contact.ImMsn)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImMsn", im));
			foreach (string im in contact.ImYahoo)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImYahoo", im));
			foreach (string im in contact.ImGroupwise)
				indexable.AddProperty (Property.NewUnsearched ("fixme:ImGroupwise", im));

			String name = "";
			if (contact.GivenName != null && contact.GivenName != "")
				name = contact.GivenName;
			if (contact.FamilyName != null && contact.FamilyName != "")
				name += " " + contact.FamilyName;
			if (name.Length > 0)
				indexable.AddProperty (Property.New ("fixme:Name", name));
		
			if (contact.Email1 != null)
				indexable.AddProperty (Property.NewKeyword ("fixme:Email",
									    contact.Email1));
			return indexable;
		}

	}
}
