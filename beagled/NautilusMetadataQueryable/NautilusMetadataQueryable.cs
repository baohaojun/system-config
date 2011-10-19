//
// NautilusMetadataQueryable.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Xml;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.NautilusMetadataQueryable {

	[QueryableFlavor (Name="NautilusMetadata", Domain=QueryDomain.Local, RequireInotify=false, DependsOn="Files")]
	public class NautilusMetadataQueryable : ExternalMetadataQueryable, IIndexableGenerator  {

		private string nautilus_dir;
		private FileSystemQueryable.FileSystemQueryable target_queryable;

		public NautilusMetadataQueryable ()
		{
			nautilus_dir = Path.Combine (Path.Combine (PathFinder.HomeDir, ".nautilus"), "metafiles");
		}

		public override void Start () 
		{
                        base.Start ();

			// The FSQ
			Queryable queryable = QueryDriver.GetQueryable ("Files");
			this.target_queryable = (FileSystemQueryable.FileSystemQueryable) queryable.IQueryable;

			string fsq_fingerprint = target_queryable.IndexFingerprint;
			InitFileAttributesStore ("NautilusMetadata", fsq_fingerprint);

			if (! Directory.Exists (nautilus_dir))
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
			else
				ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (Inotify.Enabled) {
				// Nautilus creates a temporary file, writes
				// out the content, and moves it on top of any
				// previous file.  Files are never removed.  So
				// we only need to watch the MovedTo event.
				Inotify.EventType mask = Inotify.EventType.MovedTo;
				Inotify.Subscribe (nautilus_dir, OnInotifyEvent, mask);
			}

			// Start our crawler process
			Scheduler.Task task;
			task = this.target_queryable.NewAddTask (this);
			task.Tag = "Crawling Nautilus Metadata";
			task.Source = this;

			ThisScheduler.Add (task);

			Log.Info ("Nautilus metadata backend started");
		}

		private bool CheckForExistence ()
		{
			if (!Directory.Exists (nautilus_dir))
				return true;
			
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));

			return false;
		}

		/////////////////////////////////////////////////

		public Indexable GetIndexable (NautilusTools.NautilusMetadata nm)
		{
			Indexable indexable = new Indexable (nm.Uri);
			indexable.Type = IndexableType.PropertyChange;

			Property prop;

			// Reset the notes property.
			if (nm.Notes == null)
				nm.Notes = String.Empty;

			prop = Property.New ("nautilus:notes", nm.Notes);
			prop.IsMutable = true;
			prop.IsPersistent = true;
			indexable.AddProperty (prop);

			foreach (string emblem in nm.Emblems) {
				prop = Property.NewKeyword ("nautilus:emblem", emblem);
				prop.IsMutable = true;
				prop.IsPersistent = true;
				indexable.AddProperty (prop);
			}

			// We add an empty keyword so that the property is reset
			if (nm.Emblems.Count == 0) {
				prop = Property.NewKeyword ("nautilus:emblem", String.Empty);
				prop.IsMutable = true;
				prop.IsPersistent = true;
				indexable.AddProperty (prop);
			}

			return indexable;

		}

		/////////////////////////////////////////////////

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;

			if (Path.GetExtension (subitem) != ".xml")
				return;

			// We're only handling MovedTo events here.
			string file = Path.Combine (path, subitem);

			DateTime last_checked = DateTime.MinValue;

			FileAttributes attr;
			attr = FileAttributesStore.Read (file);
			if (attr != null)
				last_checked = attr.LastWriteTime;

			foreach (NautilusTools.NautilusMetadata nm in NautilusTools.GetMetadata (file, last_checked)) {
				Indexable indexable = GetIndexable (nm);

				Scheduler.Task task;
				task = this.target_queryable.NewAddTask (indexable);
				task.Priority = Scheduler.Priority.Immediate;

				ThisScheduler.Add (task);
			}
		}

		/////////////////////////////////////////////////

		// IIndexableGenerator implementation
		public string StatusName {
			get { return "NautilusMetadataQueryable"; }
		}

		private IEnumerator metafiles = null;
		private IEnumerator metadata = null;
		
		public void PostFlushHook () { }

		static private bool IsXmlFile (string path, string name)
		{
			return (Path.GetExtension (name) == ".xml" && File.Exists (Path.Combine (path, name)));
		}

		private DirectoryWalker.FileFilter is_xml_file = new DirectoryWalker.FileFilter (IsXmlFile);

		public bool HasNextIndexable ()
		{
			if (metadata != null) {
				if (metadata.MoveNext ())
					return true;
				else {
					metadata = null;
					FileAttributesStore.AttachLastWriteTime ((string) metafiles.Current, DateTime.UtcNow);
				}
			}

			while (metadata == null) {
				if (metafiles == null)
					metafiles = DirectoryWalker.GetItems (nautilus_dir, is_xml_file).GetEnumerator ();

				if (! metafiles.MoveNext ())
					return false;

				string file = (string) metafiles.Current;

				if (FileAttributesStore.IsUpToDate (file))
					continue;

				metadata = NautilusTools.GetMetadata ((string) metafiles.Current).GetEnumerator ();

				if (metadata.MoveNext ())
					return true;
				else {
					metadata = null;
					FileAttributesStore.AttachLastWriteTime (file, DateTime.UtcNow);
				}
			}

			return false; // Makes the compiler happy
		}

		public Indexable GetNextIndexable ()
		{
			NautilusTools.NautilusMetadata nm = (NautilusTools.NautilusMetadata) metadata.Current;

			return GetIndexable (nm);
		}

	}
}
