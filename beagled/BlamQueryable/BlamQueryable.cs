//
// BlamQueryable.cs
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
using System.IO;
using System.Collections;
using System.Threading;

using System.Xml;
	
using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.BlamQueryable {

	[QueryableFlavor (Name="Blam", Domain=QueryDomain.Local, RequireInotify=false)]
	public class BlamQueryable : LuceneFileQueryable {

		string blam_dir;
		FileInfo blam_file;

		// add versioning
		// v1: changed property names to match DC element names
		// v2: remove dc:date, use Timestamp property.
		private const int INDEX_VERSION = 2;

		public BlamQueryable () : base ("BlamIndex", INDEX_VERSION)
		{
			blam_dir = Path.Combine (Path.Combine (PathFinder.HomeDir, ".gnome2"), "blam");
			blam_file = new FileInfo (Path.Combine (blam_dir, "collection.xml"));
		}

		/////////////////////////////////////////////////

		public override void Start () 
		{			
			base.Start ();

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (!Directory.Exists (blam_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			if (Inotify.Enabled) {
				Inotify.EventType mask = Inotify.EventType.CloseWrite;
				Inotify.Subscribe (blam_dir, OnInotifyEvent, mask);
			} else {
				FileSystemWatcher fsw = new FileSystemWatcher ();
			       	fsw.Path = blam_dir;
				fsw.Filter = blam_file.Name;

				fsw.Changed += new FileSystemEventHandler (OnChangedEvent);
				fsw.Created += new FileSystemEventHandler (OnChangedEvent);
				
				fsw.EnableRaisingEvents = true;
			}

			if (File.Exists (blam_file.FullName))
				Index ();
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (blam_dir))
                                return true;

                        this.Start ();

                        return false;
                }

		/////////////////////////////////////////////////

		// Modified event using Inotify
		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem != blam_file.Name)
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
			if (ThisScheduler.ContainsByTag ("Blam")) {
				Logger.Log.Debug ("Not adding task for already running Blam task");
				return;
			}

			ItemIndexableGenerator generator = new ItemIndexableGenerator (this, blam_dir, blam_file.FullName);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = "Blam";
			ThisScheduler.Add (task);
		}

	}

	/**
	 * Indexable generator for Blam Feeds
	 */
	public class ItemIndexableGenerator : IIndexableGenerator {
		private string feed_file;
		private string blam_dir;
		private BlamQueryable queryable;
		private int indexed_count;
		
		private XmlTextReader reader;
		private bool is_valid_file = true;

		private string channel_url, channel_name;
		
		public ItemIndexableGenerator (BlamQueryable queryable, string blam_dir, string feed_file)
		{
			this.blam_dir = blam_dir;
			this.queryable = queryable;
			this.feed_file = feed_file;
			ReadFeedHeader ();
		}

		public void PostFlushHook ()
		{
			//queryable.FileAttributesStore.AttachLastWriteTime (feed_file, DateTime.UtcNow);
		}

		public string StatusName {
			get { return feed_file; }
		}

		private bool IsUpToDate (string path)
		{
			return queryable.FileAttributesStore.IsUpToDate (path);
		}

		private void ReadFeedHeader () {
			
			if (IsUpToDate (feed_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Logger.Log.Debug ("Opening blam collection file: {0}", feed_file);
				reader = new XmlTextReader (feed_file);
				reader.WhitespaceHandling = WhitespaceHandling.None;
				
				is_valid_file = true;
				// move to beginning of document
				reader.MoveToContent();
				// move to <ChannelCollection> node
				reader.ReadStartElement ("ChannelCollection");
				channel_name = null;
				channel_url = null;
			} catch (XmlException ex) {
				Logger.Log.Warn (ex, "Caught exception parsing feed file:");
				is_valid_file = false;
				reader.Close ();
			}
		}

		public bool HasNextIndexable ()
		{	
			if (!is_valid_file || reader == null)
				return false;
			string elementname = null;

			while (! reader.EOF) {
				elementname = reader.Name;

				if (reader.NodeType == XmlNodeType.Element &&
				    elementname == "Item" &&
				    reader.IsStartElement ())
					break;

				// Assuming the structure of tags is flat i.e.
				// <channel> (<item>...</item>)* </channel>
				// and <channel> tags are not nested
				// If later the file format changes,
				// and channel tags become nested, need to make sure
				// that when a nested channel ends, channel_name,
				// channel_url are reset to the parent values
				if (reader.NodeType == XmlNodeType.Element &&
				    elementname == "Channel") {

					channel_name = reader.GetAttribute ("Name");
					channel_url = reader.GetAttribute ("Url");
				}
				reader.Read ();
			}

			if (elementname == "Item") {
				return true;
			} else {
				reader.Close ();
				return false;
			}
			
		}

		public Indexable GetNextIndexable ()
		{
			string id = reader.GetAttribute ("Id");
			string title = reader.GetAttribute ("Title");
			string author = reader.GetAttribute ("Author");
			// FIXME stupid mono bug; DateTime.ParseExact ("0001-01-01T00:00:00.0000000+00:00", ...)
			// http://bugzilla.ximian.com/show_bug.cgi?id=76082
			// Still present in 1.1.9.2
			DateTime pub_date;
			try {
				pub_date = DateTime.ParseExact (
						reader.GetAttribute ("PubDate"),
						"yyyy-MM-ddTHH:mm:ss.fffffffzzz",
						null);
			} catch (Exception e) {
				pub_date = DateTime.MinValue;
			}
			string link = reader.GetAttribute ("Link");
			string text = reader.GetAttribute ("Text");
			reader.Read ();

			Uri uri = new Uri (String.Format ("feed:{0};item={1}", channel_url, id));
			Logger.Log.Debug ("BlamQ: Indexing [" + channel_name + "] " + title);
			
			Indexable indexable = new Indexable (uri);
			indexable.ParentUri = UriFu.PathToFileUri (feed_file);
			indexable.MimeType = "text/html";
			indexable.HitType = "FeedItem";
			indexable.Timestamp = pub_date.ToUniversalTime ();
					
			// change property names to DC names, as far as allowed
			indexable.AddProperty (Property.New ("dc:title", title));
			indexable.AddProperty (Property.New ("dc:creator", author));
			indexable.AddProperty (Property.NewKeyword ("dc:identifier", link));
			indexable.AddProperty (Property.NewKeyword ("dc:source", channel_url));
			indexable.AddProperty (Property.New ("dc:publisher", channel_name));

			string img = null;
			int i = text.IndexOf ("<img src=\"");
			if (i != -1) {
				i += "<img src=\"".Length;
				int j = text.IndexOf ("\"", i);
				if (j != -1)
					img = text.Substring (i, j-i);
			}

			if (img != null) {
				string path = Path.Combine (Path.Combine (blam_dir, "Cache"),
							    img.GetHashCode ().ToString ());
				indexable.AddProperty (Property.NewUnsearched ("fixme:cachedimg", path));
			}

			StringReader string_reader = new StringReader (text);
			indexable.SetTextReader (string_reader);

			return indexable;
		}

	}

}
