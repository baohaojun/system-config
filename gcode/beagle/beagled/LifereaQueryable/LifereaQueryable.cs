//
// LifereaQueryable.cs
//
// Copyright (C) 2005 Carl-Emil Lagerstedt
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
using System.IO;
using System.Collections;
using System.Threading;

using System.Xml;
using System.Xml.Serialization;
	
using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.LifereaQueryable {

	[QueryableFlavor (Name="Liferea", Domain=QueryDomain.Local, RequireInotify=false)]
	public class LifereaQueryable : LuceneFileQueryable {

		string liferea_dir;
		internal string icon_dir;

		private XmlSerializer serializer = null;
		public XmlSerializer Serializer {
			get {
				if (serializer == null)
					serializer = new XmlSerializer (typeof (Item));
				return serializer;
			}
		}

		// add versioning info
		// v1: change property names to match DC element names
		// v2: remove dc:date, use Timestamp property.
		private const int INDEX_VERSION = 2;
		
		public LifereaQueryable () : base ("LifereaIndex", INDEX_VERSION)
		{
		}

		/////////////////////////////////////////////////

		private bool CheckForDirectory ()
		{
			string dir = Path.Combine (PathFinder.HomeDir, ".liferea_1.2");

			if (! Directory.Exists (dir))
				dir = Path.Combine (PathFinder.HomeDir, ".liferea");

			if (! Directory.Exists (dir))
				return false;

			liferea_dir = Path.Combine (dir, "cache");
			icon_dir = Path.Combine (liferea_dir, "favicons");
			liferea_dir = Path.Combine (liferea_dir, "feeds");

			Log.Debug ("Found Liferea directory '{0}'", liferea_dir);

			return true;
		}

		/////////////////////////////////////////////////

		public override void Start () 
		{			
			base.Start ();

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (! CheckForDirectory ()) {
				Log.Debug ("Watching for creation of Liferea directory");
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
                                return;
			}
				
			if (Inotify.Enabled) {
				Inotify.EventType mask = Inotify.EventType.CloseWrite 
							| Inotify.EventType.Delete;

				Inotify.Subscribe (liferea_dir, OnInotifyEvent, mask);
			} else {
                                FileSystemWatcher fsw = new FileSystemWatcher ();
                                fsw.Path = liferea_dir;

                                fsw.Changed += new FileSystemEventHandler (OnChanged);
                                fsw.Created += new FileSystemEventHandler (OnChanged);

                                fsw.EnableRaisingEvents = true;
			}

                        Log.Info ("Scanning Liferea feeds...");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

                        DirectoryInfo dir = new DirectoryInfo (liferea_dir);
			int count = 0;
			foreach (FileInfo file in DirectoryWalker.GetFileInfos (dir)) {
				IndexSingleFeed (file.FullName);
			}

			stopwatch.Stop ();
                        Log.Info ("{0} files will be parsed (scanned in {1})", count, stopwatch);
		}

		private bool CheckForExistence ()
                {
                        if (! CheckForDirectory ())
                                return true;

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));

                        return false;
                }

		/////////////////////////////////////////////////

                // Modified/Created event using Inotify

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			// someone reported that backup files with abcd~
			// were being generated
			if (subitem == "" || subitem.EndsWith ("~"))
				return;

			if ((type & Inotify.EventType.CloseWrite) != 0)
				IndexSingleFeed (Path.Combine (path, subitem));
			else if ((type & Inotify.EventType.Delete) != 0)
				Removefeed_file (Path.Combine (path, subitem));
		}

		// Modified/Created event using FSW
		
		private void OnChanged (object o, FileSystemEventArgs args)
		{
			IndexSingleFeed (args.FullPath);
		}
		
		/////////////////////////////////////////////////

		private void IndexSingleFeed (string filename) {
			if (ThisScheduler.ContainsByTag (filename)) {
				Log.Debug ("Not adding task for already running task: {0}", filename);
				return;
			}

			FeedIndexableGenerator generator = new FeedIndexableGenerator (this, filename);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = filename;
			task.Source = this;
			ThisScheduler.Add (task);
		}

		private void Removefeed_file (string file) {
			Log.Debug ("Removing Liferea feed_file:" + file);
			Uri uri = UriFu.PathToFileUri (file);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}

	
	/**
	 * Indexable generator for Liferea Feeds
	 */
	public class FeedIndexableGenerator : IIndexableGenerator {
		private string feed_file;
		private string icon_file = null;
		private LifereaQueryable queryable;
		
		private XmlTextReader reader;
		private bool is_valid_file = true;

		private string feed_source = "";
		private string publisher = "";
		private Item current_item;
		private XmlSerializer serializer;
		
		public FeedIndexableGenerator (LifereaQueryable queryable, string feed_file)
		{
			this.queryable = queryable;
			this.feed_file = feed_file;
			this.serializer = queryable.Serializer;
			ReadFeedHeader ();

			// Set icon file
			string file_name = Path.GetFileNameWithoutExtension (feed_file);
			this.icon_file = this.queryable.icon_dir;
			this.icon_file = Path.Combine (icon_file, file_name);
			this.icon_file = Path.ChangeExtension (icon_file, "png");
		}

		public void PostFlushHook ()
		{
			current_item = null;
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
				Log.Debug ("Opening liferea feed file: {0}", feed_file);
				reader = new XmlTextReader (feed_file);
				reader.WhitespaceHandling = WhitespaceHandling.None;
				
				is_valid_file = true;
				
				// move to beginning of document
				reader.MoveToContent();
				// move to <feed> node
				reader.ReadStartElement ("feed");
				
				do {
					string elementName = reader.Name;
					if (elementName == "item")
						break;
					switch (elementName) {
					case "feedSource":
						reader.ReadStartElement ("feedSource");
						feed_source = reader.ReadString ();
						reader.ReadEndElement ();
						break;
					case "feedTitle":
						reader.ReadStartElement ("feedTitle");
						publisher = reader.ReadString ();
						reader.ReadEndElement ();
						break;	
					// ignore other elements
					default:
						reader.ReadOuterXml ();
						break;
					}
				} while (!reader.EOF && reader.NodeType == XmlNodeType.Element);
			} catch (XmlException ex) {
				Log.Warn (ex, "Caught exception parsing feed file:");
				is_valid_file = false;
				reader.Close ();
			}
		}

		public bool HasNextIndexable ()
		{	
			current_item = null;
			if (!is_valid_file || reader == null)
				return false;
			string itemString = "";
			try {
				// check if the reader is at the startnode
				if (reader.NodeType == XmlNodeType.Element) {
					itemString = reader.ReadOuterXml ();
					// form node object from the <node>...</node> string
					// FIXME Deserialize(...) is expensive - remove it altogether
					current_item = (Item) serializer.Deserialize (new StringReader (itemString));
				}
			} catch (XmlException ex) {
				// probably no more <item>
			}

			if (current_item == null) {
				//Log.Debug ("LifereaQ: Probably no more feeds left in " + feed_file);
				//Log.Debug ("Causing string = " + itemString);
				current_item = null;
				is_valid_file = false;
				reader.Close ();
			}
			return is_valid_file;
		}

		public Indexable GetNextIndexable ()
		{
			if (current_item != null)
				return current_itemToIndexable ();
			else
				return null;
		}

		private Indexable current_itemToIndexable ()
		{
			Indexable indexable;
			try {
				indexable = new Indexable (new Uri (String.Format ("{0};item={1}", feed_source, current_item.Source)));
			} catch (System.UriFormatException) {
				indexable = new Indexable (new Uri (String.Format ("liferea://dummy?{0};item={1}", feed_source, current_item.Source)));
			}
			indexable.ParentUri = UriFu.PathToFileUri (feed_file);
			indexable.MimeType = "text/html";
			indexable.HitType = "FeedItem";

			DateTime date = DateTimeUtil.UnixToDateTimeUtc (0);
			date = date.AddSeconds (current_item.Timestamp);
			indexable.Timestamp = date;				

			// cleaning up the property names as far as possible
			// this way querying for specific field is possible
			// following DC element names wherever applicable

			indexable.AddProperty (Property.New ("dc:title", current_item.Title));
			Attribute[] attribs = current_item.Attribs.AttribArray;
			if (attribs != null) {
				foreach (Attribute attrib in attribs) {
					if (attrib.Name != "author")
						continue;
					indexable.AddProperty (Property.New ("dc:creator", attrib.Value));
				}
			}
			indexable.AddProperty (Property.NewKeyword ("dc:identifier", current_item.Source));
			indexable.AddProperty (Property.NewKeyword ("dc:source", feed_source));
			indexable.AddProperty (Property.New ("dc:publisher", publisher));

			if (File.Exists (icon_file))
				indexable.AddProperty (Property.NewUnsearched ("fixme:cachedimg", icon_file));

			StringReader reader = new StringReader (current_item.Description);
			indexable.SetTextReader (reader);

			return indexable;
		}
	}

	[System.Xml.Serialization.XmlRoot("item", Namespace="", IsNullable=false)]
	[System.Xml.Serialization.XmlType("item", Namespace="")]
	public class Item {
		[XmlElement ("title")] public string Title = "";
		[XmlElement ("description")] public string Description ="";
		[XmlElement ("source")] public string Source="";
		[XmlElement ("attributes")] public Attributes Attribs;
		[XmlElement ("time")] public long Timestamp;
	}

	public class Attributes {
		[XmlElement ("attribute")] public Attribute[] AttribArray;
	}
	
	public class Attribute{
		[XmlAttribute ("name")] public string Name = "";
		[XmlTextAttribute] public string Value = "";
	}
	
}
