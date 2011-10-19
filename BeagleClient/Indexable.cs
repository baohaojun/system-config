//
// Indexable.cs
//
// Copyright (C) 2004 Novell, Inc.
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;
using Beagle.Util;

namespace Beagle {

	public enum IndexableType {
		Add,
		Remove,
		PropertyChange
	}

	public enum IndexableFiltering {
		Never,           // Never try to filter this indexable, it contains no content
		AlreadyFiltered, // The readers promise to return nice clean text, so do nothing
		Automatic,       // Try to determine automatically if this needs to be filtered
		Always           // Always try to filter this indexable
	}

	public class Indexable : Versioned, IComparable {

		static private bool Debug = Beagle.Util.Debug.Enabled ("Indexable");

		// This is the type of indexing operation represented by
		// this Indexable object.  We default to Add, for historical
		// reasons.
		private IndexableType type = IndexableType.Add;

		// Used to uniquely determine any indexable
		private int indexable_id = 0;

		// The URI of the item being indexed.
		private Uri uri = null;

		// The URI of the parent indexable, if any.
		private Uri parent_uri = null;

		// The URI of the contents to index
		private Uri contentUri = null;

		// The URI of the hot contents to index
		private Uri hotContentUri = null;

		// Whether the content should be deleted after indexing
		private bool deleteContent = false;
		
		// File, WebLink, MailMessage, IMLog, etc.
		private String hit_type = null;

		// If applicable, otherwise set to null.
		private String mimeType = null;

		// The source backend that generated this indexable
		private string source = null;
		
		// List of Property objects
		private ArrayList properties = new ArrayList ();

		// Is this being indexed because of crawling or other
		// background activity?
		// If set, then the underlying file will be flushed
		// from buffer cache as soon as it is indexed.
		// Set it to true when the buffer cache should not
		// be disturbed due to this indexable (e.g. during
		// crawling).
		private bool flush_buffer_cache = false;

		// Is this object inherently contentless?
		private bool no_content = false;

		// If necessary, should we cache this object's content?
		// The cached version is used to generate snippets.
		private bool cache_content = true;

		// Is this indexable a child of another indexable ?
		// If true, then parent_uri points to the uri of the parent
		// However, an indexable can have parent_uri set but may not be a child
		private bool is_child = false;

		// A stream of the content to index
		private TextReader textReader;

		// A stream of the hot content to index
		private TextReader hotTextReader;

		// A stream of binary data to filter
		private Stream binary_stream;

		// When should we try to filter this indexable?
		private IndexableFiltering filtering = IndexableFiltering.Automatic;

		// Local state: these are key/value pairs that never get serialized
		// into XML
		Hashtable local_state = new Hashtable ();

		//////////////////////////

		static private XmlSerializer our_serializer;

		static Indexable ()
		{
			our_serializer = new XmlSerializer (typeof (Indexable));
		}

		//////////////////////////

		public Indexable (IndexableType type,
				  Uri           uri)
		{
			this.type = type;
			this.uri = uri;
			this.hit_type = String.Empty;
		}

		public Indexable (Uri uri) : this (IndexableType.Add, uri)
		{ }

		public Indexable ()
		{
			// Only used when reading from xml
		}

		public static Indexable NewFromXml (string xml)
		{
			StringReader reader = new StringReader (xml);
			return (Indexable) our_serializer.Deserialize (reader);
		}

		//////////////////////////

		[XmlAttribute ("Type")]
		public IndexableType Type {
			get { return type; }
			set { type = value; }
		}

		[XmlAttribute ("Id")]
		public int Id {
			get { return indexable_id; }
			set { indexable_id = value; }
		}

		[XmlIgnore]
		public Uri Uri { 
			get { return uri; }
			set { uri = value; }
		}

		[XmlAttribute ("Uri")]
		public string UriString {
			get { return UriFu.UriToEscapedString (uri); }
			set { uri = UriFu.EscapedStringToUri (value); }
		}

		[XmlIgnore]
		public Uri ParentUri { 
			get { return parent_uri; }
			set { parent_uri = value; }
		}

		[XmlAttribute ("ParentUri")]
		public string ParentUriString {
			get {
				if (parent_uri == null)
					return null;

				return UriFu.UriToEscapedString (parent_uri);
			}

			set {
				if (value == null)
					parent_uri = null;
				else
					parent_uri = UriFu.EscapedStringToUri (value);
			}
		}

		[XmlIgnore]
		public Uri ContentUri {
			get { return contentUri != null ? contentUri : Uri; }
			set { contentUri = value; }
		}

		[XmlAttribute ("ContentUri")]
		public string ContentUriString {
			get { return UriFu.UriToEscapedString (ContentUri); }
			set { contentUri = UriFu.EscapedStringToUri (value); } 
		}

		[XmlIgnore]
		private Uri HotContentUri {
			get { return hotContentUri; }
			set { hotContentUri = value; }
		}
		
		[XmlAttribute ("HotContentUri")]
		public string HotContentUriString {
			get { return HotContentUri != null ? UriFu.UriToEscapedString (HotContentUri) : ""; }
			set { hotContentUri = (value != "") ? UriFu.EscapedStringToUri (value) : null; }
		}

		private Uri display_uri = null;

		[XmlIgnore]
		public Uri DisplayUri {
			get { return display_uri != null ? display_uri : Uri; }
			set { display_uri = value; }
		}

		[XmlAttribute ("DisplayUri")]
		public string DisplayUriString {
			get { return UriFu.UriToEscapedString (DisplayUri); }
			set { DisplayUri = UriFu.EscapedStringToUri (value); }
		}

		[XmlAttribute]
		public bool DeleteContent {
			get { return deleteContent; }
			set { deleteContent = value; }
		}

		[XmlAttribute]
		public String HitType {
			get { return  hit_type; }
			set { hit_type = value; }
		}

		[XmlAttribute]
		public String MimeType {
			get { return mimeType; }
			set { mimeType = value; }
		}

		[XmlAttribute]
		public string Source {
			get { return source; }
			set { source = value; }
		}

		[XmlIgnore]
		public bool IsNonTransient {
			/* Not transient if
			 *  - content should not be deleted after indexing and
			 *  - actual source of data (data might be stored in temporary files for indexing) is a file and
			 *  - there is no parent uri set.
			 */
			get { return ! DeleteContent && ContentUri.IsFile && ParentUri == null; }
		}

		[XmlAttribute ("Crawled")]
		public bool FlushBufferCache {
			get { return flush_buffer_cache; }
			set { flush_buffer_cache = value; }
		}

		[XmlAttribute]
		public bool NoContent {
			get { return no_content; }
			set { no_content = value; }
		}

		[XmlAttribute]
		public bool CacheContent {
			get { return cache_content; }
			set { cache_content = value; }
		}

		[XmlAttribute]
		public IndexableFiltering Filtering {
			get { return filtering; }
			set { filtering = value; }
		}

		[XmlIgnore]
		public IDictionary LocalState {
			get { return local_state; }
		}

		[XmlAttribute]
		public bool IsChild {
			get { return is_child; }
			set { is_child = value; }
		}

		//////////////////////////

		public void Cleanup ()
		{
			if (DeleteContent) {
				if (contentUri != null) {
					if (Debug)
						Logger.Log.Debug ("Cleaning up {0} ({1})", contentUri.LocalPath, Uri);

					try {
						FileSystem.PosixDelete (contentUri.LocalPath);
					} catch { 
						// It might be gone already, so catch the exception.
					}

					contentUri = null;
				}

				if (hotContentUri != null) {
					if (Debug)
						Logger.Log.Debug ("Cleaning up {0} ({1})", hotContentUri.LocalPath, Uri);

					try {
						FileSystem.PosixDelete (hotContentUri.LocalPath);
					} catch {
						// Ditto
					}

					hotContentUri = null;
				}
			}
		}

		private Stream StreamFromUri (Uri uri)
		{
			Stream stream = null;

			if (uri != null && uri.IsFile && ! no_content) {
				stream = new FileStream (uri.LocalPath,
							 FileMode.Open,
							 FileAccess.Read,
							 FileShare.Read);
			}

			return stream;
		}

		private TextReader ReaderFromUri (Uri uri)
		{
			Stream stream = StreamFromUri (uri);

			if (stream == null)
				return null;

			return new StreamReader (stream);
		}
				

		public TextReader GetTextReader ()
		{
			if (NoContent)
				return null;

			if (textReader == null)
				textReader = ReaderFromUri (ContentUri);

			return textReader;
		}
		
		public void SetTextReader (TextReader reader)
		{ 
			textReader = reader;
		}

		public TextReader GetHotTextReader ()
		{
			if (NoContent)
				return null;

			if (hotTextReader == null)
				hotTextReader = ReaderFromUri (HotContentUri);
			return hotTextReader;
		}

		public void SetHotTextReader (TextReader reader)
		{
			hotTextReader = reader;
		}

		public Stream GetBinaryStream ()
		{
			if (NoContent)
				return null;

			if (binary_stream == null)
				binary_stream = StreamFromUri (ContentUri);

			return binary_stream;
		}

		public void SetBinaryStream (Stream stream)
		{
			binary_stream = stream;
		}


		[XmlArrayItem (ElementName="Property", Type=typeof (Property))]
		public ArrayList Properties {
			get { return properties; }
		}

		public void AddProperty (Property prop) {
			if (prop != null) {
				
				if (type == IndexableType.PropertyChange && ! prop.IsMutable)
					throw new ArgumentException ("Non-mutable properties aren't allowed in this indexable");

				// If this is a mutable property, make sure that
				// we don't already contain another mutable property
				// with the same name.  If we do, replace it.
				if (prop.IsMutable) {
					for (int i = 0; i < properties.Count; ++i) {
						Property other_prop = properties [i] as Property;
						if (other_prop.IsMutable && prop.Key == other_prop.Key) {
							properties [i] = prop;
							return;
						}
					}
				}

				properties.Add (prop);
			}
		}

		public bool HasProperty (string keyword) {
			foreach (Property property in properties)
				if (property.Key == keyword)
					return true;

			return false;
		}

		// This doesn't check if it makes sense to actually
		// merge the two indexables: it just does it.
		public void Merge (Indexable other)
		{
			if (other.Timestamp > this.Timestamp)
				this.Timestamp = other.Timestamp;

			foreach (Property prop in other.Properties)
				this.AddProperty (prop);

			foreach (DictionaryEntry entry in other.local_state)
				this.local_state [entry.Key] = entry.Value;
		}

		//////////////////////////

		public void SetChildOf (Indexable parent)
		{
			this.IsChild = true;
			if (parent.IsChild)
				this.ParentUri = parent.ParentUri;
			else
				this.ParentUri = parent.Uri;

			if (!this.ValidTimestamp)
				this.Timestamp = parent.Timestamp;

			if (string.IsNullOrEmpty (this.HitType))
				this.HitType = parent.HitType;

			this.Source = parent.Source;

			// FIXME: Set all of the parent's properties on the
			// child so that we get matches against the child
			// that otherwise would match only the parent, at
			// least until we have proper RDF support.

			if (parent.IsChild)
				CopyPropertyChildToChild (parent);
			else
				CopyPropertyParentToChild (parent);
		}

		// FIXME: Copying the correct properties from parent to child:
		// (This is not perfect yet)
		// It does not make sense to have parent:parent:parent:...:parent:foo
		// for property names of a nested child
		// Moreover, if indexable a.mbox has child b.zip which has child c.zip,
		// then upon matching c.zip, we would like to get the information from
		// a.mbox (i.e. the toplevel indexable) only. Intermediate parent information
		// is not necessary for displaying results; in fact, storing them would cause
		// confusion during display.
		// E.g. storing parent:beagle:filename for all parents
		// would cause, parent:beagle:filename=a.mbox, parent.beagle.filename=b.zip
		// whereas we are only interested in toplevel parent:beagle:filename=a.mbox
		// For indexables which need to store the intermediate/immediate parent info
		// separately, explicitly store them.
		// Another problem is, toplevel indexable might want to store information
		// which should not be matched when searching for its child. Copying those
		// properties in all children will incorrectly match them.
		//

		private void CopyPropertyChildToChild (Indexable parent)
		{
			// If parent itself is a child,
			// then only copy parents' parent:xxx and _private:xxx properties
			foreach (Property prop in parent.Properties) {

				if (prop.Key.StartsWith ("parent:") ||
				    prop.Key.StartsWith (Property.PrivateNamespace)) {

					Property new_prop = (Property) prop.Clone ();
					this.AddProperty (new_prop);
				} else {
					
					Property new_prop = (Property) prop.Clone ();
					new_prop.IsStored = false;
					this.AddProperty (new_prop);
				}
			}
		}

		private void CopyPropertyParentToChild (Indexable parent)
		{
			// Parent is a top level indexable
			// Copy all properties
			foreach (Property prop in parent.Properties) {

				Property new_prop = (Property) prop.Clone ();
				// Add parent: to property names ONLY IF
				// - not private property (these are not properties of the file content)
				// - property name does not already start with parent:
				if (! new_prop.Key.StartsWith (Property.PrivateNamespace) &&
				    ! new_prop.Key.StartsWith ("parent:"))
					new_prop.Key = "parent:" + new_prop.Key;

				this.AddProperty (new_prop);
			}
		}

		//////////////////////////

		public override string ToString () 
		{
			StringWriter writer = new StringWriter ();
			our_serializer.Serialize (writer, this);
			writer.Close ();
			return writer.ToString ();
		}

		//////////////////////////

		const int BUFFER_SIZE = 8192;

		private static char [] GetCharBuffer ()
		{
			LocalDataStoreSlot slot;
			slot = Thread.GetNamedDataSlot ("Char Buffer");

			object obj;
			char [] buffer;
			obj = Thread.GetData (slot);
			if (obj == null) {
				buffer = new char [BUFFER_SIZE];
				Thread.SetData (slot, buffer);
			} else {
				buffer = (char []) obj; 
			}

			return buffer;
		}

		private static byte [] GetByteBuffer ()
		{
			LocalDataStoreSlot slot;
			slot = Thread.GetNamedDataSlot ("Byte Buffer");

			object obj;
			byte [] buffer;
			obj = Thread.GetData (slot);
			if (obj == null) {
				buffer = new byte [BUFFER_SIZE];
				Thread.SetData (slot, buffer);
			} else {
				buffer = (byte []) obj; 
			}

			return buffer;
		}

		//////////////////////////

		private static Uri TextReaderToTempFileUri (TextReader reader)
		{
			if (reader == null)
				return null;

			string filename = Path.GetTempFileName ();
			FileStream fileStream = File.OpenWrite (filename);

			// When we dump the contents of an indexable into a file, we
			// expect to use it again soon.
			FileAdvise.PreLoad (fileStream);

			// Make sure the temporary file is only readable by the owner.
			// FIXME: There is probably a race here.  Could some malicious program
			// do something to the file between creation and the chmod?
			Mono.Unix.Native.Syscall.chmod (filename, Mono.Unix.Native.FilePermissions.S_IRUSR);

			BufferedStream bufferedStream = new BufferedStream (fileStream);
			StreamWriter writer = new StreamWriter (bufferedStream);


			char [] buffer;
			buffer = GetCharBuffer ();

			int read;
			do {
				read = reader.Read (buffer, 0, buffer.Length);
				if (read > 0)
					writer.Write (buffer, 0, read);
			} while (read > 0);
			
			writer.Close ();

			return UriFu.PathToFileUri (filename);
		}

		private static Uri BinaryStreamToTempFileUri (Stream stream)
		{
			if (stream == null)
				return null;

			string filename = Path.GetTempFileName ();
			FileStream fileStream = File.OpenWrite (filename);

			// When we dump the contents of an indexable into a file, we
			// expect to use it again soon.
			FileAdvise.PreLoad (fileStream);

			// Make sure the temporary file is only readable by the owner.
			// FIXME: There is probably a race here.  Could some malicious program
			// do something to the file between creation and the chmod?
			Mono.Unix.Native.Syscall.chmod (filename, Mono.Unix.Native.FilePermissions.S_IRUSR);

			BufferedStream bufferedStream = new BufferedStream (fileStream);

			byte [] buffer;
			buffer = GetByteBuffer ();

			int read;
			do {
				read = stream.Read (buffer, 0, buffer.Length);
				if (read > 0)
					bufferedStream.Write (buffer, 0, read);
			} while (read > 0);

			bufferedStream.Close ();

			return UriFu.PathToFileUri (filename);
		}

		public void StoreStream () {
			if (textReader != null) {
				ContentUri = TextReaderToTempFileUri (textReader);

				if (Debug)
					Logger.Log.Debug ("Storing text content from {0} in {1}", Uri, ContentUri);

				DeleteContent = true;
			} else if (binary_stream != null) {
				ContentUri = BinaryStreamToTempFileUri (binary_stream);

				if (Debug)
					Logger.Log.Debug ("Storing binary content from {0} in {1}", Uri, ContentUri);

				DeleteContent = true;
			}

			if (hotTextReader != null) {
				HotContentUri = TextReaderToTempFileUri (hotTextReader);

				if (Debug)
					Logger.Log.Debug ("Storing hot content from {0} in {1}", Uri, HotContentUri);

				DeleteContent = true;
			}
		}

		public void CloseStreams ()
		{
			if (textReader != null)
				textReader.Close ();
			else if (binary_stream != null)
				binary_stream.Close ();

			if (hotTextReader != null)
				hotTextReader.Close ();
		}

		//////////////////////////

		public override int GetHashCode ()
		{
			return (uri != null ? uri.GetHashCode () : 0) ^ type.GetHashCode ();
		}

		public int CompareTo (object obj)
		{
			Indexable other = (Indexable) obj;
			return DateTime.Compare (this.Timestamp, other.Timestamp);
		}
	}
}
