//
// AkregatorQueryable.cs
//
// Copyright (C) 2005 Debajyoti Bera
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

namespace Beagle.Daemon.AkregatorQueryable {

	[QueryableFlavor (Name="Akregator", Domain=QueryDomain.Local, RequireInotify=false)]
	public class AkregatorQueryable : LuceneFileQueryable {

		string akregator_dir;

		// construct a serializer and keep it handy for indexablegenerator to use
		private XmlSerializer serializer = null;
		public XmlSerializer Serializer {
			get {
				if (serializer == null)
					serializer = new XmlSerializer (typeof (Item));
				return serializer;
			}
		}

		// store the file size indexed by the filenames
		// akregator unnecessarily saves files
		private Hashtable file_sizes;
		public long GetFileSize (string name)
		{
			if (! file_sizes.Contains (name))
				return -1;
			return (long)file_sizes [name];
		}
		public void SetFileSize (string name, long size)
		{
			file_sizes [name] = size;
		}
		
		// add versioning of index
		// v1: change property names to DC names,
		//	store feed_file as ParentUri
		// v2: remove dc:date, use Timestamp property.
		private const int INDEX_VERSION = 2;
		
		public AkregatorQueryable () : base ("AkregatorIndex", INDEX_VERSION)
		{
			akregator_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			akregator_dir = Path.Combine (akregator_dir, "share");
			akregator_dir = Path.Combine (akregator_dir, "apps");
			akregator_dir = Path.Combine (akregator_dir, "akregator");
			akregator_dir = Path.Combine (akregator_dir, "Archive");

			file_sizes = new Hashtable ();
		}

		/////////////////////////////////////////////////

		public override void Start () 
		{			
			base.Start ();

			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (!Directory.Exists (akregator_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
                                return;
			}
				
			if (Inotify.Enabled) {
				Inotify.EventType mask = Inotify.EventType.CloseWrite 
							| Inotify.EventType.Delete;

				Inotify.Subscribe (akregator_dir, OnInotifyEvent, mask);
			} else {
                                FileSystemWatcher fsw = new FileSystemWatcher ();
                                fsw.Path = akregator_dir;

                                fsw.Changed += new FileSystemEventHandler (OnChanged);
                                fsw.Created += new FileSystemEventHandler (OnChanged);

                                fsw.EnableRaisingEvents = true;
			}

                        Log.Info ("Scanning Akregator feeds...");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

                        DirectoryInfo dir = new DirectoryInfo (akregator_dir);
			int count = 0;
			foreach (FileInfo file in DirectoryWalker.GetFileInfos (dir)) {
				if (file.Extension == ".xml") {
					IndexSingleFeed (file.FullName, true);
					count ++;
				}
			}

			stopwatch.Stop ();
                        Log.Info ("{0} files will be parsed (scanned in {1})", count, stopwatch);
		}

		private bool CheckForExistence ()
                {
                        if (!Directory.Exists (akregator_dir))
                                return true;

                        this.Start ();

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
			if (subitem == "" || !subitem.EndsWith (".xml"))
				return;

			if ((type & Inotify.EventType.CloseWrite) != 0)
				IndexSingleFeed (Path.Combine (path, subitem), false);
			else if ((type & Inotify.EventType.Delete) != 0)
				RemoveFeedFile (Path.Combine (path, subitem));
		}

		// Modified/Created event using FSW
		
		private void OnChanged (object o, FileSystemEventArgs args)
		{
			IndexSingleFeed (args.FullPath, false);
		}
		
		/////////////////////////////////////////////////
		
		// Parse and index a single feed

		private void IndexSingleFeed (string filename, bool initial_scan) {
			if (! filename.EndsWith (".xml"))
				return;
			if (ThisScheduler.ContainsByTag (filename)) {
				Log.Debug ("Not adding task for already running task: {0}", filename);
				return;
			}

			FeedIndexableGenerator generator = new FeedIndexableGenerator (this, filename, initial_scan);
			Scheduler.Task task;
			task = NewAddTask (generator);
			task.Tag = filename;
			ThisScheduler.Add (task);
		}

		private void RemoveFeedFile (string file) {
			Log.Debug ("Removing Akregator feedfile:" + file);
			Uri uri = UriFu.PathToFileUri (file);
			Scheduler.Task task = NewRemoveTask (uri);
			task.Priority = Scheduler.Priority.Immediate;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
		}

	}	

	/**
	 * Indexable generator for Akregator Feeds
	 */
	public class FeedIndexableGenerator : IIndexableGenerator {
		private string feed_file;
		private AkregatorQueryable queryable;
		
		private XmlTextReader reader;
		private bool is_valid_file = true;
		private bool initial_scan = false;

		private string channel_title;
		private string channel_link;
		private string channel_description;
		
		private Item current_item;
		private XmlSerializer serializer;
		
		public FeedIndexableGenerator (AkregatorQueryable queryable, string feed_file, bool initial_scan)
		{
			this.queryable = queryable;
			this.feed_file = feed_file;
			this.serializer = queryable.Serializer;
			this.initial_scan = initial_scan;
			ReadFeedHeader ();
		}

		public void PostFlushHook ()
		{
		}

		public string StatusName {
			get { return feed_file; }
		}

		private bool IsUpToDate (string path)
		{
			// first check the file date
			if (queryable.FileAttributesStore.IsUpToDate (path))
				return true;
			// if not up to date and initial scan, then we should index
			if (initial_scan)
				return false;
			// next check the size - its really unlucky if the file is changed
			// and yet the size is same
			// FIXME: Maybe store the md5-hash of the file - that is less expensive
			// than indexing all the feeds in the file!
			FileInfo file = new FileInfo (path);
			if (queryable.GetFileSize (path) != file.Length)
				return false;
			return true;
		}

		private void ReadFeedHeader () {
			
			if (IsUpToDate (feed_file)) {
				is_valid_file = false;
				return;
			}
			try {
				Log.Debug ("Opening feed file: {0}", feed_file);
				reader = new XmlTextReader (feed_file);
				reader.WhitespaceHandling = WhitespaceHandling.None;
				
				is_valid_file = true;
				
				// move to beginning of document
				reader.MoveToContent();
				// move to <rss ...> node
				reader.ReadStartElement ("rss");
				// move to <channel> node
				reader.ReadStartElement ("channel");
				
				// read <title>
				
				do {
					string elementName = reader.Name;
					if (elementName == "item")
						break;
					switch (elementName) {
					case "title":
						reader.ReadStartElement ("title");
						channel_title = reader.ReadString ();
						reader.ReadEndElement ();
						break;
						
					case "link":
						reader.ReadStartElement ("link");
						channel_link = reader.ReadString ();
						reader.ReadEndElement ();
						break;
						
					case "description":
						reader.ReadStartElement ("description");
						channel_description = reader.ReadString ();
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
					// FIXME Deserialize is expensive - remove it altogether
					current_item = (Item) serializer.Deserialize (new StringReader (itemString));
				}
			} catch (XmlException ex) {
				// probably no more <item>
			}

			if (current_item == null) {
				//Log.Debug ("AkregatorQ: Probably no more feeds left in " + feed_file);
				//Log.Debug ("Causing string = " + itemString);
				current_item = null;
				is_valid_file = false;
				reader.Close ();
			}
			if (! is_valid_file)
				StoreFileSize ();
			return is_valid_file;
		}

		private void StoreFileSize ()
		{
			// cache the file size
			FileInfo file = new FileInfo (feed_file);
			queryable.SetFileSize (feed_file, file.Length);
		}

		public Indexable GetNextIndexable ()
		{
			if (current_item != null || !current_item.IsDeleted)
				return current_itemToIndexable ();
			else
				return null;
		}

		private Indexable current_itemToIndexable ()
		{
			// sanity check
			if (current_item == null)
				return null;

			//Log.Debug ("Indexing " + channel_link + ":" + current_item.Link);
			Indexable indexable = new Indexable (new Uri (String.Format ("feed:{0};item={1}", channel_link, current_item.Link)));
			indexable.ParentUri = UriFu.PathToFileUri (feed_file);
			indexable.MimeType = "text/html";
			indexable.HitType = "FeedItem";

			string RFC822 = "ddd, dd MMM yyyy HH:mm:ss zzz";
			DateTime date = DateTime.ParseExact(current_item.PubDate, RFC822, DateTimeFormatInfo.InvariantInfo, DateTimeStyles.AdjustToUniversal);
			indexable.Timestamp = date;

			// replace property names with Dublin Core names
			indexable.AddProperty (Property.New ("dc:title", current_item.Title));
			indexable.AddProperty (Property.NewKeyword ("dc:identifier", current_item.Link));
			indexable.AddProperty (Property.NewKeyword ("dc:source", channel_link));
			indexable.AddProperty (Property.New ("dc:publisher", channel_title));
				
			StringReader reader = new StringReader (current_item.Description);
			indexable.SetTextReader (reader);

			return indexable;
		}

	}

	public class MetaInfo {
		[XmlText]
		public string value = "";
		[XmlAttribute ("type")] public string Type = "";
	}

	// we will deserialize XML fragments, so there wont be any <? xml ... ?>
	[System.Xml.Serialization.XmlRoot("item", Namespace="", IsNullable=false)]
	[System.Xml.Serialization.XmlType("item", Namespace="")]
	public class Item {
		[XmlElement ("pubDate")] public string PubDate; 
		[XmlElement ("title")] public string Title = "";
		[XmlElement ("description")] public string Description ="";
		[XmlElement ("link")] public string Link="";
		[XmlElement ("meta", typeof (MetaInfo), Namespace="http://foobar")]
		public ArrayList MetaList {
		    get { return metaList; }
		    set { metaList = value; }
		}
		private ArrayList metaList = new ArrayList ();

		public bool IsDeleted {
			get {
			    for (int i=0; i<metaList.Count; ++i) {
				    MetaInfo meta = (MetaInfo)metaList[i];
				    if (meta.Type == "deleted" && meta.value == "true") {
					return true;
				    }
			    }
			    return false;
			}
		}
	}
	
}
