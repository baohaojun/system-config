//
// XmpFile.cs
// Handler for xmp sidecar files. If a generic external metadata store
// is implemented, this should be made part of that.
//
// This also contains the xmp parser. We don't handle xmp files outside file system backend !!!
// That is why this is here and not in Util/ or Filters/
//
// Copyright (C) 2007 Alexander McDonald
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
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Daemon.FileSystemQueryable {

	public class XmpSidecarStore {
		static public bool Debug = FileSystemQueryable.Debug;

		private UidManager uid_manager;
		// FIXME: Do not expose the queryable if not needed to. Currently, this is solely needed to
		// access the RegisterFile method, which should be abstracted out.
		private FileSystemQueryable queryable;

		public XmpSidecarStore (UidManager uid_manager, FileSystemQueryable queryable)
		{
			this.uid_manager = uid_manager;
			this.queryable = queryable;
		}

		// MergeXmpData can return an entirely new indexable or just change the passed indexable
		public Indexable MergeXmpData (ref Indexable indexable,
					       string path,
					       Guid id,
					       DirectoryModel parent,
					       bool crawling)
		{
			// In crawl mode, whenever xmp file is encountered, it either schedules the basefile or notices that
			// the basefile is already scheduled. In short, it is properly taken care of. So, during
			// crawling, we can return the indexable created at this point instead of checking the
			// existence of foo.xmp for each crawled file foo.
			if (crawling)
				return null;
			else
				return GetNewXmpIndexable (ref indexable, path, id, parent);
		}

		private Indexable GetNewXmpIndexable (ref Indexable indexable,
						      string path,
					       	      Guid id,
					       	      DirectoryModel parent)
		{
			// In non-crawl mode, check if a corresponding xmp file is present and not already scheduled and index it.
			// If file.xmp and file are rapidly written/updated (in that order), this does the right thing.
			// If file and file.xmp are rapidly written/updated (in that order), either
			// - file.xmp is present during FileToIndexable(file): in which case xmp properties are
			//   added to file; and when file.xmp is indexed, it will replace the xmp properties
			// - file.xmp is not present during FileToIndexable(file): when the xmp file is later indexed
			//   it will add the xmp properties
			// since the uid file will still be in the uid-cache, correct uid will be used for xmp prop-change indexable
			string possible_xmp_file_path = string.Concat (path, ".xmp");
			if (! File.Exists (possible_xmp_file_path))
				return null;

			Guid xmp_id = queryable.RegisterFile (parent, (Path.GetFileName (possible_xmp_file_path)));
			if (xmp_id == Guid.Empty)
				return null;

			XmpFile xmp_file = null;
			try {
				xmp_file = new XmpFile (possible_xmp_file_path);
			} catch {
				uid_manager.ForgetNewId (possible_xmp_file_path);
				return null;
			}

			// FIXME: Should also delete previous xmp properties!
			foreach (Property p in xmp_file.Properties) {
				p.IsMutable = true;
				indexable.AddProperty (p);
			}
			xmp_file.Close ();

			// Also need to save some local states for PostAddHook,
			// namely, path to the xmp file, path to basefile and generated uid
			indexable.LocalState ["XmpFilePath"] = possible_xmp_file_path;
			indexable.LocalState ["BaseFilePath"] = path;
			indexable.LocalState ["XmpGuid"] = GuidFu.ToShortString (xmp_id);
			if (Debug)
				Log.Debug ("Adding properties from {0}({2}) to {1}({3})", possible_xmp_file_path, path, GuidFu.ToShortString (xmp_id), GuidFu.ToShortString (id));

			return null;
		}

		public Indexable GetXmpQueryable (string path, Guid id, DirectoryModel parent)
		{
			Log.Debug ("Asked to create xmp indexable for ({0}) {1}", GuidFu.ToShortString (id), path);
			// Should be at least 6 characters /<...>.xmp
			if (path.Length < 6)
				return null;

			string basefile_path = Path.ChangeExtension (path, null);
			// Ignore xmp files by itself
			// FIXME: To support indexing independent xmp files will require even greater trouble
			if (! File.Exists (basefile_path))
				return null;

			XmpFile xmp_file = null;
			try {
				xmp_file = new XmpFile (path);
			} catch {
				Log.Warn ("Cannot create xmpfile from {0}", path);
				return null;
			}

			// Try to get the correct uid for the basefile
			// First we need to see if basefile is already scheduled (yet to be dispatched)
			Uri basefile_uri = null;
			Indexable base_indexable;

			if (uid_manager.HasNewId (basefile_path)) {
				// Since uid_manager has a new id for this basefile, so basefile is already scheduled
				// Get basefile uid from there
				Guid basefile_id = uid_manager.GetNewId (basefile_path);
				basefile_uri = GuidFu.ToUri (basefile_id);
				Log.Debug ("{0} is already scheduled with uri {1}", basefile_path, basefile_uri);
			} else {
				// Basefile is not scheduled in the current batch
				string basefile_name = Path.GetFileName (basefile_path);
				// Try to schedule it for addition
				base_indexable = queryable.GetCrawlingFileIndexable (parent, basefile_name);

				if (base_indexable == null) {
					// GetCrawlingFileIndexable returns null if file does not need to be indexed
					// So basefile is up-to-date
					// Need to figure out id from uid manager
					Guid basefile_id = uid_manager.GetIdByNameAndParentId (basefile_name, parent.UniqueId);
					basefile_uri = GuidFu.ToUri (basefile_id);
					Log.Debug ("{0} is not scheduled and need not be, uri is {1}", basefile_path, basefile_uri);
				} else {
					Log.Debug ("Need to index {0}", basefile_path);
					// basefile needs to be indexed
					// FIXME: Move the task business out of handler and into FSQ.cs
					Scheduler.Task task;
					task = queryable.NewAddTask (base_indexable);
					// FIXME: What is the correct priority ?
					// If should have similar priority to the one that this xmp-indexable will be a part of
					task.Priority = Scheduler.Priority.Immediate;
					queryable.ThisScheduler.Add (task);

					// Get the basefile uri from the indexable
					basefile_uri = base_indexable.Uri;
				}
			}

			Log.Debug ("Adding xmp-indexable for {0} (basefile uri {1}) with uid {2}",
				path,
				basefile_uri,
				GuidFu.ToShortString (id));

			Indexable indexable = new Indexable (IndexableType.PropertyChange, basefile_uri);
			// Set the timestamp of the indexable as the timestamp of the basefile
			// It could have also been skipped, the original Indexable.Add would anyway have it
			indexable.Timestamp = File.GetLastWriteTimeUtc (basefile_path);
			indexable.DisplayUri = UriFu.PathToFileUri (path);

			// If the file was somehow deleted before this point, bail out.
			if (! FileSystem.ExistsByDateTime (indexable.Timestamp)) {
				xmp_file.Close ();
				return null;
			}

			// Save some local states for PostAddHook, namely, path to the xmp file, path to basefile and generated uid
			indexable.LocalState ["XmpFilePath"] = path;
			indexable.LocalState ["BaseFilePath"] = basefile_path;
			indexable.LocalState ["XmpGuid"] = GuidFu.ToShortString (id);

			// FIXME: Should also delete previous xmp properties!
			foreach (Property p in xmp_file.Properties) {
				p.IsMutable = true;
				indexable.AddProperty (p);
			}
			xmp_file.Close ();

			return indexable;
		}

	}

	// XMP Parser
	public class XmpFile
	{
		private const string RdfNamespace = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
		private const string XmpNamespace = "adobe:ns:meta/";
		
		public const string FilterName = "FilterFileSystemXmp";
		public const int FilterVersion = 0;

		private string xmpfile = null;
		private List<Beagle.Property> properties = null;
		private XmlReader reader;
		
#if FALSE
		public static void Main(string[] args)
		{
			Console.WriteLine("Properties:");
			
			foreach( Beagle.Property bp in new xmp(args[0]).GetProperties())
			{
				Console.WriteLine(bp.Key + " --- " + bp.Value);
			}
		}
#endif	

		public XmpFile (string path)
		{
			xmpfile = path;
			XmlReaderSettings ReaderSettings = new XmlReaderSettings ();
			reader = XmlReader.Create (xmpfile, ReaderSettings);
			// FIXME: Check here if the preamble matches correct xmp preamble
		}

		public void Close ()
		{
			properties = null;
			reader.Close ();
		}
		
		private void AddProperty(string Name, string Value)
		{
			if (Name.Contains("Date")) {
				try {
					properties.Add (Beagle.Property.NewDate (Name, System.Convert.ToDateTime (Value)));
				} catch (FormatException) {
					properties.Add (Beagle.Property.New (Name, Value));
				}
			} else {
				properties.Add (Beagle.Property.New (Name, Value));
			}
		}

		public IList<Property> Properties {
			get {
				if (properties == null) {
					properties = new List<Beagle.Property>();
					Parse ();
				}

				return properties;
			}
		}
					
		// FIXME: Change it to a pulling (returning IEnumerable) method
		private void Parse()
		{
			StringBuilder sb = new StringBuilder ();
			
			while (reader.Read()) {
				if (reader.NodeType == XmlNodeType.Element) {
					switch (reader.NamespaceURI) {
					case XmpNamespace:
					case RdfNamespace:
						break;
					default:
						if (reader.GetAttribute("rdf:parseType") == "Resource") {
							HandleResource (reader, sb);
						} else {
							HandleElement (reader, sb, null);
						}
						break;
					}
				}
			}
		}

		protected void HandleElement (XmlReader r, StringBuilder sb, string prefix)
		{
			int StartDepth = r.Depth;
			int ListNum = 0;
			sb.Length = 0;
	
			string Name;
			if (prefix == null)
				Name = r.Name;
			else
				Name = String.Format ("{0}:{1}-{2}", r.Prefix, prefix, r.LocalName);
	
			string ns = Name.ToLower ();
			// Only add some properties and not all, they are a huge list!!!
			// FIXME: Which properties to choose to add ?
			if (! ns.StartsWith ("exif") && 
			    ! ns.StartsWith ("dc") &&
			    ! ns.StartsWith ("tiff") &&
			    ! ns.StartsWith ("aux") &&
			    ! ns.StartsWith ("Iptc4xmpCore"))
				return;
			
			while (r.Read() && r.Depth > StartDepth) {
				switch (r.NodeType) {
				case XmlNodeType.Text:
					sb.Append (r.Value);
					break;
				case XmlNodeType.Element:
					if (r.NamespaceURI == RdfNamespace && r.LocalName == "li") {
						if (ListNum > 0) {
							sb.Append (", ");
						}
						ListNum++;
					}
					break;
				}
			}
			
			AddProperty (Name, sb.ToString());
		}
		
		protected void HandleResource (XmlReader r, StringBuilder sb)
		{
			int StartDepth = r.Depth;
			string prefix = r.LocalName;
	
			while (r.Read() && r.Depth > StartDepth) {
				if (r.NodeType == XmlNodeType.Element) {
					HandleElement (r, sb, prefix);
				}
			}
		}
	}
}

