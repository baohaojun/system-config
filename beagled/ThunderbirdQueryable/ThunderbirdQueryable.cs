//
// ThunderbirdQueryable.cs: This is the Thunderbird backend
//
// Copyright (C) 2007 Pierre Östlund
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.IO;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Collections;
using Beagle.Util;
using Mono.Unix.Native;
using GMime;
using Mime = GMime.Utils;

[assembly: Beagle.Daemon.IQueryableTypes (typeof (Beagle.Daemon.ThunderbirdQueryable.ThunderbirdQueryable))]

namespace Beagle.Daemon.ThunderbirdQueryable {
	
	[QueryableFlavor (Name = "Thunderbird", Domain = QueryDomain.Local, RequireInotify = true)]
	public class ThunderbirdQueryable : LuceneQueryable {
	
		// History:
		// 1: Initial version used to force reindex due to changes in URI scheme
		// 2: Client and folder properties have been changed to Keyword instead of Unsearched
		private const int MINOR_VERSION = 2;
	
		private ThunderbirdIndexer indexer = null;
		private string overriden_toindex = null;
		
		public ThunderbirdQueryable () : base ("ThunderbirdIndex", MINOR_VERSION)
		{
			// In case we use another directory
			overriden_toindex = Environment.GetEnvironmentVariable ("TOINDEX_DIR");
			
			GMime.Global.Init ();
		}
		
		public override void Start ()
		{
			base.Start ();

			// delay everything till the backend is actually started
			Inotify.Subscribe (IndexDirectory, 
				OnInotifyEvent, 
				Inotify.EventType.Create | 
				Inotify.EventType.Delete |
				Inotify.EventType.DeleteSelf);
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}
		
		private void StartWorker ()
		{
			if (indexer != null)
				return; // already started

			Logger.Log.Debug ("Starting Thunderbird backend");
			Stopwatch watch = new Stopwatch ();
			watch.Start ();
			
			string toindex_dir = ToIndexDirectory;
			if (!Directory.Exists (toindex_dir)) {
				Logger.Log.Debug ("No Thunderbird data to index in {0}", toindex_dir);
				return;
			}
			
			indexer = new ThunderbirdIndexer (this);
			indexer.Start ();
			
			watch.Stop ();
			Logger.Log.Debug ("Thunderbird backend done in {0}s", watch.ElapsedTime);
		}
		
		private void OnInotifyEvent (
				Inotify.Watch watch,
				string path,
				string subitem,
				string srcpath,
				Inotify.EventType type)
		{
			// Stop watching if we were deleted
			if ((type & Inotify.EventType.DeleteSelf) != 0) {
				watch.Unsubscribe ();
				return;
			}
			
			// We want a directory
			if ((type & Inotify.EventType.IsDirectory) == 0)
				return;
			
			// We are only watching one directory, so we only have to check for ToIndex
			// as subitem
			if (subitem != "ToIndex")
				return;
			
			// We only have to watch for creation of the ToIndex directory here, so that
			// the indexing process can be started. The Indexer will automatically clean
			// up if the ToIndex diretory is deleted (we still display a status message
			// here though)
			if ((type & Inotify.EventType.Create) != 0)
				ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
			else if ((type & Inotify.EventType.Delete) != 0) 
				Logger.Log.Debug ("Stopping the Thunderbird indexing process; ToIndex disappeared");
		}
		
		// This is the directory where all metafiles goes
		public string ToIndexDirectory {
			get {
				if (overriden_toindex != null)
					return overriden_toindex;
				else
					return Path.Combine (IndexDirectory, "ToIndex");
			}
		}
	}
	
	public class ThunderbirdIndexer {	
		private ThunderbirdQueryable queryable;
		private ThunderbirdIndexableGenerator indexable_generator;
		
		private const string TAG = "ThunderbirdIndexer";

		public ThunderbirdIndexer (ThunderbirdQueryable queryable)
		{
			this.queryable = queryable;
			this.indexable_generator = null;
		}
		
		public void Start ()
		{
			// Make sure we catch file system changes
			Inotify.Subscribe (queryable.ToIndexDirectory, 
				OnInotifyEvent, 
				Inotify.EventType.Create | 
				Inotify.EventType.DeleteSelf);
			
			// Start the indexable generator and begin adding things to the index
			LaunchIndexable ();
		}
		
		private void LaunchIndexable ()
		{
			// Cancel running task before adding a new one
			CancelIndexable ();
			
			// Add the new indexable generator
			indexable_generator = new ThunderbirdIndexableGenerator (this, queryable.ToIndexDirectory);
			
			Scheduler.Task task = queryable.NewAddTask (indexable_generator);
			task.Tag = TAG;
			queryable.ThisScheduler.Add (task);
		}
		
		private void CancelIndexable ()
		{
			if (indexable_generator != null && queryable.ThisScheduler.ContainsByTag (TAG))
				queryable.ThisScheduler.GetByTag (TAG).Cancel ();
		}

		public void RemoveFolder (string folderFile)
		{
			if (queryable.ThisScheduler.ContainsByTag (folderFile)) {
				Logger.Log.Debug ("Not adding task for already running {0}", folderFile);
				return;
			}
			
			Property prop = Property.NewUnsearched ("ParentUri", folderFile);
			Scheduler.Task task = queryable.NewRemoveByPropertyTask (prop);
			task.Tag = folderFile;
			task.Priority = Scheduler.Priority.Immediate;
			queryable.ThisScheduler.Add (task);
		}

		private void OnInotifyEvent (
				Inotify.Watch watch,
				string path,
				string subitem,
				string srcpath,
				Inotify.EventType type)
		{
			// Stop watching if we are deleted. We should unsubscribe to the Inotify watcher
			// here to, but that gives an exception. Why?
			if ((type & Inotify.EventType.DeleteSelf) != 0) {
				CancelIndexable ();
				return;
			}
		
			// We need to have a filename
			if (subitem == null)
				return;
			
			// If the running indexable generator has more items, we don't have to care here.
			// Otherwise launch a new indexable generator.
			if (indexable_generator != null && !indexable_generator.Done)
				return;
			
			LaunchIndexable ();
		}
	}
	
	public class ThunderbirdIndexableGenerator : IIndexableGenerator {
		private ThunderbirdIndexer indexer;
		private Queue processed_files;
		private string path = null;
		private bool done = false;
		private int max_obj = -1, min_obj = -1, current_obj = 0, obj_count = 0;
		
		private const string ADD_MAILMESSAGE = "MailMessage";
		private const string ADD_RSS = "FeedItem";
		private const string REMOVE_MESSAGE = "DeleteHdr";
		private const string REMOVE_FOLDER = "DeleteFolder";
		
		public ThunderbirdIndexableGenerator (ThunderbirdIndexer indexer, string path)
		{
			Logger.Log.Debug ("New Thunderbird indexable generator launched for {0}", path);
			
			this.indexer = indexer;
			this.path = path;
			this.processed_files = new Queue ();
			ReadToIndexDirectory ();
		}
		
		private void ReadToIndexDirectory ()
		{
			// Reset values before we do anything
			max_obj = -1;
			min_obj = -1;
			current_obj = 0;
			obj_count = 0;
			
			foreach (string filename in DirectoryWalker.GetFiles (path)) {
				int cur_index;
				
				try {
					cur_index = Convert.ToInt32 (Path.GetFileName (filename));
				} catch {
					continue;
				}
				
				if (max_obj == -1 || min_obj == -1)
					max_obj = min_obj = cur_index;
				else if (cur_index < min_obj)
					min_obj = cur_index;
				else if (cur_index > max_obj)
					max_obj = cur_index;
				else
					continue; // This should _never_ happen
				
				obj_count++;
			}
			
			current_obj = min_obj;
		}
		
		private XmlDocument OpenCurrent ()
		{
			while (current_obj <= max_obj) {
				string filename = Path.Combine (path, Convert.ToString (current_obj));
				
				// Try to open file
				try {
					current_obj++;
					processed_files.Enqueue (filename);
					
					// We need to use the StreamReader to get full UTF-8 support
					XmlDocument document = new XmlDocument ();
					StreamReader reader = new StreamReader (filename);
					document.Load (reader);
					
					return document;
				} catch (Exception e) {
					Logger.Log.Debug (e, "Failed to parse file {0}", filename);
				}
			}
			current_obj++;
			
			return null;
		}
		
		private static string GetText (XmlDocument doc, string child)
		{
			if (doc == null || doc.DocumentElement == null || String.IsNullOrEmpty (child))
				return string.Empty;
			
			try {
				return doc.DocumentElement [child].InnerText;
			} catch {
			}
			
			return string.Empty;
		}
		
		private static bool ToBool (string str)
		{
			string lower = str.ToLower ();
			
			if (lower == "false")
				return false;
			else if (lower == "true")
				return true;
			
			return Convert.ToBoolean (str);
		}
		
		// We cannot use the URIs provided by Thunderbird due to limitations  in the URI-class 
		// implementation. We have to make up something unique and use another property with 
		// the correct URI (fixme:uri).
		private static Uri GenerateUniqueUri (XmlDocument document)
		{
			return new Uri (String.Format ("{0}/?id={1}",
				GetText (document, "FolderFile"), 
				GetText (document, "MessageKey")));
		}

		private GMime.Message GetGMimeMessage (string file, int offset, int size)
		{
			GMime.Message msg = null;
			
			if (!File.Exists (file))
				return null;
				
			StreamFs stream = null;
			Parser parser = null;
			try {
				int fd = Syscall.open (file, OpenFlags.O_RDONLY);
				stream = new StreamFs (fd, offset, offset + size);
				parser = new Parser (stream);
				msg = parser.ConstructMessage ();
			} catch {
			} finally {
				if (stream != null)
					stream.Dispose ();
				if (parser != null)
					parser.Dispose ();
			}
			
			return msg;
		}
		
		private GMime.Message GetStubMessage (XmlDocument document)
		{
			GMime.Message message = new GMime.Message (true);
			
			message.Subject = Mime.HeaderDecodeText (GetText (document, "Subject"));
			message.Sender = Mime.HeaderDecodePhrase (GetText (document, "Author"));
			message.MessageId = GetText (document, "MessageId");
			message.SetDate (DateTimeUtil.UnixToDateTimeUtc (Convert.ToInt64 (GetText (document, "Date"))), 0);
			message.AddRecipientsFromString ("To", Mime.HeaderDecodePhrase (GetText (document, "Recipients")));

			return message;
		}
		
		private Indexable ToAddMailMessageIndexable (XmlDocument document)
		{
			GMime.Message message = null;
			
			// Check if the entire message is available
			if (ToBool (GetText (document, "HasOffline"))) {
				// We must make sure we don't get an exception here since we can fallback to
				// other information
				try {
					int offset = Convert.ToInt32 (GetText (document, "MessageOffset")),
						size = Convert.ToInt32 (GetText (document, "OfflineSize"));
					message = GetGMimeMessage (GetText (document, "FolderFile"), offset, size);
				} catch (Exception e) {
					Logger.Log.Debug (e, "Failed to parse GMime message");
				}
			}

			if (message == null)
				message = GetStubMessage (document);
			
			Indexable indexable = new Indexable (GenerateUniqueUri (document));
			indexable.HitType = "MailMessage";
			indexable.MimeType = "message/rfc822";
			indexable.Timestamp = DateTimeUtil.UnixToDateTimeUtc (Convert.ToInt64 (GetText (document, "Date")));
			indexable.CacheContent = true;
			indexable.FlushBufferCache = true;
			indexable.SetBinaryStream (message.Stream);

			indexable.AddProperty (Property.NewKeyword ("fixme:client", "thunderbird"));
			indexable.AddProperty (Property.NewKeyword ("fixme:folder", GetText (document, "Folder")));
			indexable.AddProperty (Property.NewUnsearched ("ParentUri", GetText (document, "FolderFile")));
			indexable.AddProperty (Property.NewUnsearched ("fixme:uri", GetText (document, "Uri")));

			message.Dispose ();

			return indexable;
		}

		// ReadLine in stream requires the more "fancy" line ending "\r\n", which we do't have. So we use this
		// implementation instead.
		private static string ReadEncodingLine (System.IO.Stream stream)
		{
			// We usually only need to cover UTF-8 and iso-8859-1, so we initially make the buffer big
			// enough for this (and expand later if needed)
			byte[] str = new byte [10];
			int pos = 0, c = stream.ReadByte ();
			
			while (c != -1 && c != 10) {
				// Do we need to grow?
				if (pos == str.Length) {
					// We grow with 50% of current size
					byte[] tmp = new byte [str.Length + (int) 0.5 * str.Length];
					Array.Copy (str, 0, tmp, 0, str.Length);
				}

				str [pos++] = (byte) c;
				c = stream.ReadByte ();
			}
			
			// We _know_ that the stream comes from a StreamReader, which uses UTF8 by
			// default. So we use that here when parsing our string.
			return (str != null ? Encoding.UTF8.GetString (str, 0, pos) : string.Empty);
		}
		
		// This spell "charset="
		private static readonly int [] CHARSET = new int [] { 99, 104, 97, 114, 115, 101, 116 };
		private static StringReader GetRssBody (string file, int position, int len, out string encoding_str)
		{
			// We must assure at least this happens before we leave
			encoding_str = null;
			
			// Check if file exists to begin with
			if (!File.Exists (file))
				return null;
			
			//FileStream stream = new FileStream (file, FileMode.Open);
			StreamReader stream = new StreamReader (file);
			stream.BaseStream.Position = position;
			
			// We want to skip http headers since we are not interested in those. The normal scenario is
			// that a content begins once two newlines have been found. This is a bit different in 
			// Thunderbird though. Thunderbird specific headers are added first, then two newlines 
			// followed by http headers and then another three newlines followed by the content. So we
			// want to find the three newlines and read from there.
			//
			// Extra note here: We need to pull charset used here, otherwise we might end up with ASCII
			// instead of UTF-8 in some cases and that will really mess things up.
			byte[] buffer = null;
			int c, header_length = 0, newlines = 0, charset_pos = 0;
			Encoding enc = Encoding.UTF8;
			try {
				do {
					c = stream.BaseStream.ReadByte ();
					newlines = (c == 10 ? ++newlines : 0);
					header_length++;
					
					// This is a way to avoid using a lot of string allocations. We compare
					// current character with the ones in CHARSET. If we match all characters
					// in order, we know that what follows is the encoding.
					if (charset_pos == CHARSET.Length) {
						encoding_str = ReadEncodingLine (stream.BaseStream);
						newlines++; // We must compensate this
						header_length += encoding_str.Length+1; // ...and this
						charset_pos = 0;
					} else if (charset_pos > CHARSET.Length-1)
						charset_pos = 0; // Just in case
					else if (CHARSET [charset_pos] == c)
						charset_pos++;
					else
						charset_pos = 0;
					
				} while (c != -1 && newlines != 3);
				
				// We now know what to read
				buffer = new byte [len - header_length];
				stream.BaseStream.Read (buffer, 0, buffer.Length);
				
				// We need to use correct encoding
				enc = Encoding.GetEncoding (encoding_str);
			} catch {
			} finally {
				stream.Close ();
			}
			
			
			return new StringReader (enc.GetString (buffer));
		}
		
		private string ExtractUrl (string url)
		{
			if (url == null)
				return string.Empty;
			
			return url.Substring (0, url.IndexOf ('@'));
		}
		
		private Indexable ToAddRssIndexable (XmlDocument document)
		{
			string encoding_str = null;
			StringReader reader = null;
			
			if (ToBool (GetText (document, "HasOffline"))) {
				try {
					// RSS does not use OfflineSize but MessageSize instead (for some reason...)
					int offset = Convert.ToInt32 (GetText (document, "MessageOffset")),
						size = Convert.ToInt32 (GetText (document, "MessageSize"));
					reader = GetRssBody (GetText (document, "FolderFile"), offset, size, out encoding_str);
				} catch (Exception e) {
					Logger.Log.Debug (e, "Failed to parse RSS body");
				}
			}
			
			Indexable indexable = new Indexable (GenerateUniqueUri (document));
			indexable.HitType = "FeedItem";
			indexable.MimeType = "text/html";
			indexable.Timestamp = DateTimeUtil.UnixToDateTimeUtc (Convert.ToInt64 (GetText (document, "Date")));
			indexable.CacheContent = true;
			indexable.FlushBufferCache = true;
			
			indexable.AddProperty (Property.NewKeyword ("fixme:client", "thunderbird"));
			indexable.AddProperty (Property.NewKeyword ("fixme:folder", GetText (document, "Folder")));
			indexable.AddProperty (Property.NewUnsearched ("ParentUri", GetText (document, "FolderFile")));
			indexable.AddProperty (Property.NewUnsearched ("fixme:uri", GetText (document, "Uri")));
			
			indexable.AddProperty (Property.NewKeyword ("dc:identifier", ExtractUrl (GetText (document, "MessageId"))));
			indexable.AddProperty (Property.NewKeyword ("dc:source", GetText (document, "FeedURL")));
			indexable.AddProperty (Property.New ("dc:publisher", Mime.HeaderDecodePhrase (GetText (document, "Author"))));
			
			// The title will be added by the filter. In case we add it twice we will just get
			// an empty tile in the search tool (a bug maybe?).
			if (reader != null) {
				// If we got an encoding, make sure we use that
				if (!String.IsNullOrEmpty (encoding_str)) {
					indexable.AddProperty (Property.New (
						String.Format ("{0}encoding", StringFu.UnindexedNamespace), encoding_str));
				}
				
				indexable.SetTextReader (reader);
			} else
				indexable.AddProperty (Property.New ("dc:title", Mime.HeaderDecodePhrase (GetText (document, "Subject"))));
			
			return indexable;
		}
		
		private Indexable ToRemoveMessageIndexable (XmlDocument document)
		{
			Uri uri = GenerateUniqueUri (document);

			return new Indexable (IndexableType.Remove, uri);
		}
		
		public Indexable GetNextIndexable ()
		{
			XmlDocument document = OpenCurrent ();
			if (document == null || document.DocumentElement == null)
				return null;
			
			// Compare our document element type to expected ones
			string name = document.DocumentElement.Name;
			if (name.Equals (ADD_MAILMESSAGE)) 
				return ToAddMailMessageIndexable (document);
			else if (name.Equals (ADD_RSS))
				return ToAddRssIndexable (document);
			else if (name.Equals (REMOVE_MESSAGE))
				return ToRemoveMessageIndexable (document);
			else if (name.Equals (REMOVE_FOLDER)) 
				indexer.RemoveFolder (GetText (document, "FolderFile"));
				
			return null;
		}
		
		public bool HasNextIndexable ()
		{
			if (obj_count == 0 || current_obj > max_obj) {
				// We need to flush old files here. Otherwise we might end up in an eternal loop.
				PostFlushHook ();

				// Read directory information again since content could have been added since we
				// read last time
				ReadToIndexDirectory ();
				
				// If we are still zero here, then we're done
				if (obj_count == 0) {
					done = true;
					return false;
				} else
					return true;
			}
			
			if (current_obj <= max_obj) 
				return true;
				
			done = true;
			return false;
		}
		
		public void PostFlushHook ()
		{
			// Remove the files we just processed
			while (processed_files.Count > 0) {
				string filename = processed_files.Dequeue () as string;
				
				try {
					File.Delete (filename);
				} catch (Exception e) {
					Logger.Log.Warn (e, "Failed to remove metafile: {0}", filename);
				}
			}
		}
		
		public string StatusName {
			get {
				return String.Format ("Thunderbird object {0} of {1}", current_obj, obj_count);
			}
		}
		
		public bool Done {
			get {
				return done;
			}
		}
	}
}

