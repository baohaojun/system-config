//
// IndexingServiceQueryable.cs
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

// The IndexingService has two modes of operation: one is through the standard
// message-passing system and one where a slightly-structured file is dropped
// into a known location on the filesystem.
//
// (1) Messaging: An IndexingServiceRequest message is sent containing URIs of
// items to remove and Indexables to add.  This is more reliable, and is best
// for clients which will also be utilizing Beagle for searching.
//
// (2) Files: The file to be indexed is dropped into the ~/.beagle/ToIndex
// directory.  Another file with the same name prepended with a period is
// also dropped into the directory.  In that file is the metadata for the
// file being indexed.  The first line is the URI of the data being indexed.
// The second line is the hit type.  The third line is the mime type.  Then
// there are zero or more properties in the form "type:key=value", where
// "type" is either 't' for text or 'k' for keyword.  This method is a lot
// easier to use, but requires that Beagle have inotify support enabled to
// work.
//
// Any text property with the name beagle:inuri would be used for inuri: queries.
//

using System;
using System.Collections;
using System.IO;
using System.Threading;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.IndexingServiceQueryable {

	[QueryableFlavor (Name="IndexingService", Domain=QueryDomain.Local, RequireInotify=false)]
	public class IndexingServiceQueryable : LuceneQueryable {

		public IndexingServiceQueryable () : base ("IndexingServiceIndex")
		{
			Server.RegisterRequestMessageHandler (typeof (IndexingServiceRequest), new Server.RequestMessageHandler (HandleMessage));
		}

		public override void Start ()
		{
			base.Start ();

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			string index_path = Path.Combine (PathFinder.StorageDir, "ToIndex");

			if (!Directory.Exists (index_path))
				Directory.CreateDirectory (index_path);

			if (Inotify.Enabled)
				Inotify.Subscribe (index_path, OnInotifyEvent, Inotify.EventType.CloseWrite);

			Logger.Log.Info ("Setting up an initial crawl of the IndexingService directory");

			IndexableGenerator generator = new IndexableGenerator (GetIndexables (index_path));
			Scheduler.Task task = NewAddTask (generator);
			task.Tag = "IndexingService initial crawl";
			ThisScheduler.Add (task);
		}

		private IEnumerable GetIndexables (string path)
		{
			foreach (FileInfo file in DirectoryWalker.GetFileInfos (path)) {
				if (file.Name [0] == '.')
					continue;

				if (File.Exists (Path.Combine (file.DirectoryName, "." + file.Name)))
					yield return FileToIndexable (file);
			}

			yield break;
		}

		private Indexable FileToIndexable (FileInfo data_file)
		{
			FileInfo meta_file = new FileInfo (Path.Combine (data_file.DirectoryName, "." + data_file.Name));
			FileStream meta_stream;

			try {
				meta_stream = meta_file.Open (FileMode.Open, FileAccess.Read, FileShare.Read);
			} catch (FileNotFoundException) {
				// The meta file disappeared before we could
				// open it.
				return null;
			}

			StreamReader reader = new StreamReader (meta_stream);
			
			// First line of the file is a URI
			string line = reader.ReadLine ();
			Uri uri;

			try {
				uri = new Uri (line);
			} catch (Exception e) {
				Logger.Log.Warn (e, "IndexingService: Unable to parse URI in {0}:", meta_file.FullName);
				meta_stream.Close ();
				return null;
			}

			Indexable indexable = new Indexable (uri);
			indexable.Timestamp = data_file.LastWriteTimeUtc;
			indexable.ContentUri = UriFu.PathToFileUri (data_file.FullName);
			indexable.DeleteContent = true;
			indexable.AddProperty( Property.New("fixme:host",uri.Host));

			// Second line is the hit type
			line = reader.ReadLine ();
			if (line == null) {
				Logger.Log.Warn ("IndexingService: EOF reached trying to read hit type from {0}",
						 meta_file.FullName);
				meta_stream.Close ();
				return null;
			} else if (line != String.Empty)
				indexable.HitType = line;

			// Third line is the mime type
			line = reader.ReadLine ();
			if (line == null) {
				Logger.Log.Warn ("IndexingService: EOF reached trying to read mime type from {0}",
						 meta_file.FullName);
				meta_stream.Close ();
				return null;
			} else if (line != String.Empty)
				indexable.MimeType = line;

			// Following lines are properties in "t:key=value" format
			do {
				line = reader.ReadLine ();

				if (line != null && line != String.Empty) {
					bool keyword = false;

					if (line[0] == 'k')
						keyword = true;
					else if (line[0] != 't') {
						Logger.Log.Warn ("IndexingService: Unknown property type: '{0}'", line[0]);
						continue;
					}

					int i = line.IndexOf ('=');

					if (i == -1) {
						Logger.Log.Warn ("IndexingService: Unknown property line: '{0}'", line);
						continue;
					}
					
					// FIXME: We should probably handle date types
					if (keyword) {
						indexable.AddProperty (Property.NewUnsearched (line.Substring (2, i - 2),
											    line.Substring (i + 1)));
					} else {
						indexable.AddProperty (Property.New (line.Substring (2, i - 2),
										     line.Substring (i + 1)));
					}
				}
			} while (line != null);

			indexable.LocalState ["MetaFile"] = meta_file;
			
			// Ok, we're finished with the meta file.  It will be
			// deleted in PostAddHook ().
			meta_stream.Close ();

			return indexable;
		}

		// Bleh, we need to keep around a list of pending items to be
		// indexed so that we don't actually index it twice because
		// the order of the creation of the data file and meta file
		// isn't defined.
		private ArrayList pending_files = new ArrayList ();

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;
			
			if (subitem[0] == '.') {
				string data_file = Path.Combine (path, subitem.Substring (1));

				lock (pending_files) {
					if (File.Exists (data_file) && ! pending_files.Contains (data_file)) {
						pending_files.Add (data_file);
						IndexFile (new FileInfo (data_file));
					}
				}
			} else {
				string meta_file = Path.Combine (path, "." + subitem);
				string data_file = Path.Combine (path, subitem);

				lock (pending_files) {
					if (File.Exists (meta_file) && ! pending_files.Contains (data_file)) {
						pending_files.Add (data_file);
						IndexFile (new FileInfo (data_file));
					}
				}
			}
		}

		private void IndexFile (FileInfo data_file)
		{
			Indexable indexable = FileToIndexable (data_file);

			if (indexable == null) // The file disappeared
				return;

			Scheduler.Task task = NewAddTask (indexable);
			task.Priority = Scheduler.Priority.Immediate;
			ThisScheduler.Add (task);
		}

		protected override Uri PostAddHook (Indexable indexable, IndexerAddedReceipt receipt)
		{
			FileInfo meta_file = indexable.LocalState ["MetaFile"] as FileInfo;
			if (meta_file == null)
				return indexable.Uri;

			meta_file.Delete ();

			lock (pending_files)
				pending_files.Remove (indexable.ContentUri.LocalPath);

			return indexable.Uri;
		}

		private class IndexableGenerator : IIndexableGenerator {
			private IEnumerator to_add_enumerator = null, to_remove_uris_enumerator = null;
			private int count = -1, done_count = 0;
			// FIXME: Unused. Use this to store the submitter queryable in the localstate
			// of the indexables and receive PostHooks() from LuceneQueryable
			private LuceneQueryable submitter_queryable = null;

			public IndexableGenerator (IEnumerable to_add)
			{
				this.to_add_enumerator = to_add.GetEnumerator ();
			}

			public IndexableGenerator (ICollection to_add) : this (to_add, null, null)
			{
			}

			public IndexableGenerator (ICollection to_add, ICollection to_remove_uris) :
				this (to_add, to_remove_uris, null)
			{
			}

			public IndexableGenerator (ICollection to_add, ICollection to_remove_uris, LuceneQueryable submitter_queryable)
			{
				this.count = 0;
				this.submitter_queryable = submitter_queryable;

				if (to_add != null) {
					this.to_add_enumerator = to_add.GetEnumerator ();
					this.count += to_add.Count;
				}

				if (to_remove_uris != null) {
					this.to_remove_uris_enumerator = to_remove_uris.GetEnumerator ();
					this.count += to_remove_uris.Count;
				}
			}

			public LuceneQueryable SubmitterQueryable {
				set { submitter_queryable = value; }
			}

			public Indexable GetNextIndexable ()
			{
				if (to_remove_uris_enumerator != null) {
					Uri uri_to_remove = to_remove_uris_enumerator.Current as Uri;
					if (uri_to_remove == null)
						return null;

					return new Indexable (IndexableType.Remove, uri_to_remove);
				}

				if (to_add_enumerator == null)
					return null;

				return to_add_enumerator.Current as Indexable;
			}

			public bool HasNextIndexable ()
			{
				++done_count;

				if (to_remove_uris_enumerator != null) {
					if (to_remove_uris_enumerator.MoveNext ())
						return true;
					else
						to_remove_uris_enumerator = null;
				}

				if (to_add_enumerator == null)
					return false;

				return to_add_enumerator.MoveNext ();
			}

			public string StatusName {
				get { 
					if (count == -1)
						return String.Format ("IndexingService: {0}", done_count);
					else
						return String.Format ("IndexingService: {0} of {1}", done_count, count);
				}
			}

			public void PostFlushHook ()
			{ }
		}

		private ResponseMessage HandleMessage (RequestMessage msg)
		{
			IndexingServiceRequest isr = (IndexingServiceRequest) msg;

			LuceneQueryable backend = this;

			if (isr.Source != null) {
				Queryable target = QueryDriver.GetQueryable (isr.Source);

				if (target == null) {
					string err = String.Format ("Unable to find backend matching '{0}'", isr.Source);

					Log.Error (err);
					return new ErrorResponse (err);
				}

				if (! (target.IQueryable is LuceneQueryable)) {
					string err = String.Format ("Backend '{0}' is not an indexed backend", isr.Source);

					Log.Error (err);
					return new ErrorResponse (err);
				}

				backend = (LuceneQueryable) target.IQueryable;
				Log.Debug ("Found backend for IndexingServiceRequest: {0}", backend.IndexName);
			}

			// FIXME: There should be a way for the request to control the
			// scheduler priority of the task.

			if (isr.ToAdd.Count > 0 || isr.ToRemove.Count > 0) {
				Log.Debug ("IndexingService: Adding {0} indexables, removing {1} indexables.", isr.ToAdd.Count, isr.ToRemove.Count);

				IndexableGenerator ind_gen;
				ind_gen = new IndexableGenerator (isr.ToAdd, isr.ToRemove, this);
				Scheduler.Task task = backend.NewAddTask (ind_gen);
				task.Priority = Scheduler.Priority.Immediate;
				ThisScheduler.Add (task);
			}

			// FIXME: There should be an asynchronous response  (fired by a Scheduler.Hook)
			// that fires when all of the items have been added to the index.
			
			// No response
			return new EmptyResponse ();
		}

		protected override QueryPart QueryPartHook (QueryPart part)
		{
			if (part is QueryPart_Property) {
				QueryPart_Property prop_part = (QueryPart_Property) part;
				if (prop_part.Key == "inuri") { // special case
					QueryPart_Property new_part = new QueryPart_Property ();
					new_part.Logic = prop_part.Logic;
					new_part.Key = "beagle:inuri";
					new_part.Type = PropertyType.Text;
					new_part.Value = prop_part.Value;

					return new_part;
				}
			}

			return part;
		}
	}

}
