//
// KonqBookmarkQueryable.cs
//
// Copyright (C) 2006 Debajyoti Bera
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
using System.Collections;
using System.Threading;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using System.Globalization;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.KBookmarkQueryable {

	[QueryableFlavor (Name="KonqBookmark", Domain=QueryDomain.Local, RequireInotify=false)]
	public class KonqBookmarkQueryable : LuceneFileQueryable {

		private string konq_dir;
		private string bookmark_file;
		private Hashtable last_modified_table;

		// construct a serializer and keep it handy for indexablegenerator to use
		private XmlSerializer serializer = null;
		public XmlSerializer Serializer {
			get {
				if (serializer == null)
					serializer = new XmlSerializer (typeof (Bookmark));
				return serializer;
			}
		}

		// 1: Store URL as text
		const int MINOR_VERSION = 1;

		public KonqBookmarkQueryable () : base ("KonqBookmarkIndex", MINOR_VERSION)
		{
			konq_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			konq_dir = Path.Combine (konq_dir, "share");
			konq_dir = Path.Combine (konq_dir, "apps");
			konq_dir = Path.Combine (konq_dir, "konqueror");

			bookmark_file = Path.Combine (konq_dir, "bookmarks.xml");

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
			if (!Directory.Exists (konq_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
                                return;
			}
				
			if (Inotify.Enabled) {
				Inotify.EventType mask = Inotify.EventType.CloseWrite 
							| Inotify.EventType.MovedTo;

				Inotify.Subscribe (konq_dir, OnInotifyEvent, mask);
			} else {
                                FileSystemWatcher fsw = new FileSystemWatcher ();
                                fsw.Path = konq_dir;
				fsw.Filter = bookmark_file;

                                fsw.Changed += new FileSystemEventHandler (OnChanged);
                                fsw.Created += new FileSystemEventHandler (OnChanged);
                                fsw.Renamed += new RenamedEventHandler (OnChanged);

                                fsw.EnableRaisingEvents = true;
			}

			if (File.Exists (bookmark_file)) {
				if (! FileAttributesStore.IsUpToDate (bookmark_file))
					Index ();
				else
					ScanBookmarkInitial ();
			}
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (konq_dir))
                                return true;

                        this.Start ();

                        return false;
                }

		// We need to scan the bookmark file nitially to fill up last_modified_table
		// Otherwise, we might miss deletions that occur before any addition
		private void ScanBookmarkInitial ()
		{
			BookmarkIndexableGenerator generator = new BookmarkIndexableGenerator (this, bookmark_file, last_modified_table, true);

			// just a dummy scan
			while (generator.HasNextIndexable ())
				generator.GetNextIndexable ();
		}

		/////////////////////////////////////////////////

                // Modified/Created event using Inotify

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (Path.Combine (path, subitem) != bookmark_file)
				return;

			Index ();
		}

		// Modified/Created event using FSW
		
		private void OnChanged (object o, FileSystemEventArgs args)
		{
			Index ();
		}
		
		/////////////////////////////////////////////////
		
		private void Index ()
		{
			if (ThisScheduler.ContainsByTag ("KonqBookmark")) {
				Log.Debug ("Not adding task for already running KonqBookmark task");
				return;
			}

			BookmarkIndexableGenerator generator = new BookmarkIndexableGenerator (this, bookmark_file, last_modified_table, false);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = "KonqBookmark";
			task.Priority = Scheduler.Priority.Delayed;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

		internal void RemoveDeletedBookmarks (Hashtable deleted_bookmarks)
		{
			ArrayList to_delete = new ArrayList ();
			lock (last_modified_table) {
				foreach (string uid in last_modified_table.Keys) {
					if (! deleted_bookmarks.Contains (uid) ||
					    (bool)deleted_bookmarks [uid] == true) {
						to_delete.Add (uid);
					}
				}
			}

			foreach (string uid in to_delete) {
				RemoveBookmark (uid);
				last_modified_table.Remove (uid);
			}
		}

		private void RemoveBookmark (string uid)
		{
			Uri uri = new Uri (uid);
			Log.Debug ("Removing contact {0}", uri);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}	

	/**
	 * Indexable generator for Konqueror Bookmarks
	 */
	public class BookmarkIndexableGenerator : IIndexableGenerator {
		private string bookmark_file;
		private KonqBookmarkQueryable queryable;
		private XmlTextReader reader;
		private bool is_valid_file = true;
		private bool initial_scan = false;
		private Hashtable last_modified_table;
		private Hashtable deleted_bookmarks;
		private DateTime file_last_write_time;

		private ArrayList folder_stack; // simulate a stack
		private string current_folder = String.Empty;
		private string current_title = String.Empty;
		private string current_folder_title = String.Empty;
		private DateTime current_dt;

		private string current_bookmark_id = String.Empty;
		private Bookmark current_bookmark;
		private XmlSerializer serializer;
		
		public BookmarkIndexableGenerator (KonqBookmarkQueryable queryable,
						   string bookmark_file,
						   Hashtable last_modified_table,
						   bool initial_scan)
		{
			this.queryable = queryable;
			this.bookmark_file = bookmark_file;
			this.initial_scan = initial_scan;

			ReadBookmarkHeader ();
			if (! is_valid_file)
				return;

			this.serializer = queryable.Serializer;
			this.last_modified_table = last_modified_table;
			this.folder_stack = new ArrayList ();

			lock (last_modified_table) {
				this.deleted_bookmarks = new Hashtable (last_modified_table.Count);
				foreach (string bookmark_path in last_modified_table.Keys)
					this.deleted_bookmarks [bookmark_path] = true;
			}

			// cache the last write time
			file_last_write_time = FileSystem.GetLastWriteTimeUtc (bookmark_file);
		}

		public void PostFlushHook ()
		{
		}

		public string StatusName {
			get { return current_bookmark_id; }
		}

		private bool IsUpToDate (string path)
		{
			return queryable.FileAttributesStore.IsUpToDate (path);
		}

		private void ReadBookmarkHeader () {
			if ( (! initial_scan) && IsUpToDate (bookmark_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Log.Debug ("Opening bookmark file: {0}", bookmark_file);
				reader = new XmlTextReader (bookmark_file);
				reader.WhitespaceHandling = WhitespaceHandling.None;
				
				is_valid_file = true;
				
				// move to beginning of document
				reader.MoveToContent();
				// move to <xbel> node
				reader.ReadStartElement ("xbel");
			} catch (XmlException ex) {
				Logger.Log.Warn (ex, "Caught exception parsing bookmark file:");
				is_valid_file = false;
				reader.Close ();
			}
		}

		public bool HasNextIndexable ()
		{	
			if (!is_valid_file || reader == null)
				return false;

			current_bookmark = null;
			string bookmark_string = "";
			try {
				while (! reader.EOF) {
					if (ReadNextBookmark ())
						return true;
				}
			} catch (XmlException) {
				// probably no more <item>
			}

			is_valid_file = false;
			reader.Close ();
			if (! initial_scan)
				queryable.RemoveDeletedBookmarks (deleted_bookmarks);
			return false;
		}

		private bool ReadNextBookmark ()
		{
			while (reader.NodeType == XmlNodeType.EndElement) {
			        if (reader.Name == "folder") {
					folder_stack.RemoveAt (folder_stack.Count - 1);
					ResetCurrentFolder ();
					//Log.Debug ("Resetting folder to [{0}]", current_folder);
				}
			        reader.Read ();
			}
			
			while (!reader.EOF && reader.NodeType == XmlNodeType.Element) {
				string elementname = reader.Name;
				if (elementname == "title") {
					string title = reader.ReadString ();
					current_folder_title = title;
					folder_stack.Add (title);
					ResetCurrentFolder ();
					reader.ReadEndElement ();
					continue;
				} else if (elementname == "bookmark") {
					string bookmark_string = reader.ReadOuterXml ();
					current_bookmark = (Bookmark) serializer.Deserialize (new StringReader (bookmark_string));
					if (SetLastModifiedTable ())
						return true;
				} else {
					reader.Read ();
				}
			}

			return false;
		}

		private bool SetLastModifiedTable ()
		{
			current_bookmark_id = String.Format ("kbookmark:///{0};title={1}", current_folder, current_bookmark.Title);

			if (current_bookmark.Info != null &&
			    current_bookmark.Info.Metadata != null &&
			    current_bookmark.Info.Metadata.TimeLastVisited != 0) {
				DateTime date = DateTimeUtil.UnixToDateTimeUtc (0);
				current_dt = date.AddSeconds (current_bookmark.Info.Metadata.TimeLastVisited);
			} else
				current_dt = file_last_write_time;

			// Mark bookmark seen ('undeleted')
			deleted_bookmarks [current_bookmark_id] = false;

			lock (last_modified_table) {
				if (last_modified_table.Contains (current_bookmark_id)) {
					DateTime old_dt = (DateTime) last_modified_table [current_bookmark_id];
					// Address up-to-date
					if (current_dt == old_dt) {
						//Log.Debug ("Datetime unchanged [{0}] = {1}", current_bookmark_id, current_dt);	
						return false;
					} else {
						//Log.Debug ("Updating last_mod_date [{0}] = {1}", current_bookmark_id, current_dt);
						last_modified_table [current_bookmark_id] = current_dt;
					}
				} else {
					//Log.Debug ("Adding last_mod_date [{0}] = {1}", current_bookmark_id, current_dt);
					last_modified_table [current_bookmark_id] = current_dt;
				}
			}
			return true;
		}

		private void ResetCurrentFolder ()
		{
			// FIXME: Joining using '/' isn't safe since
			// Konqueror allows '/' in the path name
			// But we need to create a unique URI from the folder path
			// and href
			StringBuilder sb = new StringBuilder ();
			foreach (string folder in folder_stack) {
				sb.Append (folder);
				sb.Append ('/');
			}
			current_folder = sb.ToString ();
		}

		public Indexable GetNextIndexable ()
		{
			if (current_bookmark != null)
				return BookmarkToIndexable ();

			return null;
		}

		private Indexable BookmarkToIndexable ()
		{
			Indexable indexable = new Indexable (new Uri (current_bookmark_id));
			indexable.ParentUri = UriFu.PathToFileUri (bookmark_file);
			indexable.HitType = "Bookmark";
			indexable.Timestamp = current_dt;
			indexable.NoContent = true;

			indexable.AddProperty (Property.New ("dc:title", current_bookmark.Title));
			indexable.AddProperty (Property.New ("dc:identifier", current_bookmark.Href));
			indexable.AddProperty (Property.NewUnsearched ("fixme:icon", current_bookmark.Icon));
			if (current_bookmark.Info != null &&
			    current_bookmark.Info.Metadata != null &&
			    current_bookmark.Info.Metadata.NumVisited != 0)
				indexable.AddProperty (Property.NewUnsearched ("fixme:visit_count", current_bookmark.Info.Metadata.NumVisited));
			foreach (string folder in folder_stack)
				indexable.AddProperty (Property.New ("fixme:folder", folder));

			return indexable;
		}

	}

	// we will deserialize XML fragments, so there wont be any <? xml ... ?>
	[System.Xml.Serialization.XmlRoot("bookmark", Namespace="", IsNullable=false)]
	[System.Xml.Serialization.XmlType("bookmark", Namespace="")]
	public class Bookmark {
		[XmlAttribute ("icon")] public string Icon = "";
		[XmlAttribute ("href")] public string Href = "";
		[XmlElement ("title")] public string Title; 
		[XmlElement ("info")] public Info Info;
	}
	
	public class Info {
		[XmlElement ("metadata")] public Metadata Metadata;
	}

	public class Metadata {
		[XmlElement ("time_added")] public long TimeAdded = 0;
		[XmlElement ("time_visited")] public long TimeLastVisited = 0;
		[XmlElement ("visit_count")] public int NumVisited = 0;
	}
}
