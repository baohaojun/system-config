//
// TomboyQueryable.cs
//
// Copyright (C) 2004 Christopher Orr
// Copyright (C) 2004-2006 Novell, Inc.
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

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.TomboyQueryable {

	[QueryableFlavor (Name="Tomboy", Domain=QueryDomain.Local, RequireInotify=false)]
	public class TomboyQueryable : LuceneFileQueryable, IIndexableGenerator  {

		string tomboy_dir;
		Hashtable note_text_cache = UriFu.NewHashtable ();

		public TomboyQueryable () : base ("TomboyIndex")
		{
			tomboy_dir = Environment.GetEnvironmentVariable ("TOMBOY_PATH");
			if (tomboy_dir == null)
				tomboy_dir = Path.Combine (PathFinder.HomeDir, ".tomboy");
		}

		public override void Start () 
		{
                        base.Start ();

                        ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (!Directory.Exists (tomboy_dir) ) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			if (Inotify.Enabled) {			
				Inotify.EventType mask = Inotify.EventType.Delete | 
					Inotify.EventType.MovedTo |
					Inotify.EventType.MovedFrom;

				Inotify.Subscribe (tomboy_dir, OnInotifyEvent, mask);
			} else {
				FileSystemWatcher fsw = new FileSystemWatcher ();
				fsw.Path = tomboy_dir;
				fsw.Filter = "*.note";

				fsw.Changed += new FileSystemEventHandler (OnChanged);
				fsw.Created += new FileSystemEventHandler (OnChanged);
				fsw.Deleted += new FileSystemEventHandler (OnDeleted);

				fsw.EnableRaisingEvents = true;
			}

			// Start our crawler process
			Scheduler.Task task;
			task = NewAddTask (this);
			task.Tag = "Crawling Tomboy Notes";
			task.Source = this;

			ThisScheduler.Add (task);

			Logger.Log.Info ("Tomboy backend started");
		}

		private bool CheckForExistence ()
		{
			if (!Directory.Exists (tomboy_dir))
				return true;
			
			this.Start ();

			return false;
		}

		/////////////////////////////////////////////////

		// Modified/Created/Deleted event using Inotify
		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;

			if (Path.GetExtension (subitem) != ".note")
				return;

			if ((type & Inotify.EventType.MovedTo) != 0) {
				IndexNote (new FileInfo (Path.Combine (path, subitem)), Scheduler.Priority.Immediate);
			}

			if ((type & Inotify.EventType.MovedFrom) != 0 ||
					((type & Inotify.EventType.Delete) != 0 &&
					 (type & Inotify.EventType.IsDirectory) == 0))
				RemoveNote (subitem);
		}

		// Modified/Created event using FSW
		private void OnChanged (object o, FileSystemEventArgs args)
		{
			IndexNote (new FileInfo (args.FullPath), Scheduler.Priority.Immediate);
		}

		// Deleted event using FSW
		private void OnDeleted (object o, FileSystemEventArgs args)
		{
			RemoveNote (args.FullPath);
		}

		/////////////////////////////////////////////////
		
		private Indexable NoteToIndexable (FileInfo file, Note note)
		{
			Indexable indexable = new Indexable (note.Uri);

			indexable.ContentUri = UriFu.PathToFileUri (file.FullName);
			indexable.Timestamp = note.timestamp;
			indexable.HitType = "Note";
			indexable.Filtering = IndexableFiltering.AlreadyFiltered;

			indexable.AddProperty (Property.New ("dc:title", note.subject));
			indexable.AddProperty (Property.NewUnsearched ("fixme:application","tomboy"));

			// FIXME: tagging is disabled in Tomboy-0.8.x and is planned for 0.10.0
			foreach( string s in note.tags)
				indexable.AddProperty (Property.New ("note:tag", s));
			
			// We remember the note's text so that we can stuff it in
			// the TextCache later.
			note_text_cache [note.Uri] = note.text;

			StringReader reader = new StringReader (note.text);
			indexable.SetTextReader (reader);
			
			return indexable;
		}

		private void IndexNote (FileInfo file, Scheduler.Priority priority)
		{
			if (this.IsUpToDate (file.FullName))
				return;

			// Try and parse a Note from the given path
			Note note = TomboyNote.ParseNote (file);
			if (note == null)
				return;
			
			// A Note was returned; add it to the index
			Indexable indexable = NoteToIndexable (file, note);
			
			Scheduler.Task task = NewAddTask (indexable);
			task.Priority = priority;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		private void RemoveNote (string file)
		{
			Uri uri = Note.BuildNoteUri (file, "tomboy");
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		override protected Uri PostAddHook (Indexable indexable, IndexerAddedReceipt receipt)
		{
			base.PostAddHook (indexable, receipt);
			
			// Store the note's text in the text cache.
			// By doing this in the PostAddHook, we ensure that
			// the TextCache is not modified until we are
			// sure that the note was actually indexed.
			string text;
			text = (string) note_text_cache [indexable.Uri];
			// If text == null, this is equivalent to
			// calling Delete (receipt.Uri)
			TextCache.UserCache.WriteFromString (indexable.Uri, text);
			note_text_cache.Remove (indexable.Uri);

			return indexable.Uri;
		}

		override protected bool HitFilter (Hit hit)
		{
			Uri uri = hit.Uri;
			string note = Path.Combine (tomboy_dir, uri.Segments [1] + ".note");

			if (File.Exists (note))
				return true;

			return false;
		}

		// IIndexableGenerator implementation
		public string StatusName {
			get { return "TomboyQueryable"; }
		}
		
		private IEnumerator note_files = null;
		
		public void PostFlushHook () { }

		public bool HasNextIndexable ()
		{
			if (note_files == null)
				note_files = DirectoryWalker.GetFileInfos (tomboy_dir).GetEnumerator ();

			return note_files.MoveNext ();
		}

		public Indexable GetNextIndexable ()
		{
			FileInfo file = (FileInfo) note_files.Current;

			if (! file.Exists)
				return null;

			if (IsUpToDate (file.FullName))
				return null;

			Note note = TomboyNote.ParseNote (file);

			if (note == null)
				return null;

			Indexable indexable = NoteToIndexable (file, note);
			
			return indexable;
		}

	}
}
