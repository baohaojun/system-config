//
// Hit.cs
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
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Xml;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {
    
	public class Hit: Versioned, IComparable {

		// A URI we can use to locate the source of this match.
		private Uri uri = null;

		// A URI of this Hit's container element
		private Uri parent_uri = null;

		// This is used to hold a copy of the Queryable in the
		// server-side copy of the Hit.  It is always null
		// on the client-side.
		private object source_object = null;

		// High scores imply greater relevance.
		private double score = 0.0;

		private ArrayList properties = new ArrayList ();

		private enum SpecialType {
			Unknown,
			None,
			Invalid,
			File,
			Directory
		}

		SpecialType special = SpecialType.Unknown;

		private string path;
		private FileInfo fileInfo = null;
		private DirectoryInfo directoryInfo = null;

		//////////////////////////

		[XmlIgnore]
		public Uri Uri {
			get { return uri; }
			set { uri = value; }
		}

		[XmlAttribute ("Uri")]
		public string EscapedUri {
			get {
				return UriFu.UriToEscapedString (uri);
			}

			set {
				uri = UriFu.EscapedStringToUri (value);
			}
		}

		[XmlIgnore]
		public Uri ParentUri {
			get { return parent_uri; }
			set { parent_uri = value; }
		}

		
		[XmlAttribute ("ParentUri")]
		public string EscapedParentUri {
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

		/// <value>
		/// File, WebHistory, MailMessage, IMLog, etc.
		/// </value>
		[XmlIgnore]
		public string Type {
			get { return GetFirstProperty ("beagle:HitType"); }
		}

		/// <value>
		/// If applicable otherwise can be null.
		/// </value>
		[XmlIgnore]
		public string MimeType {
			get { return GetFirstProperty ("beagle:MimeType"); }
		}
	
		
		/// <value>
		/// IndexUser, IndexSystem, Google, Addressbook, iFolder, etc.
		/// </value>
		[XmlIgnore]
		public string Source {
			get { return GetFirstProperty ("beagle:Source"); }
		}

		/// <summary>
		/// document, archive, image etc.
		/// </summary>
		[XmlIgnore]
		public string FileType {
			get { return GetFirstProperty ("beagle:FileType"); }
		}

		[XmlIgnore]
		public object SourceObject {
			get { return source_object; }
			set { source_object = value; }
		}

		[XmlAttribute]
		public double Score {
			get { return score; }
			set { score = value; }
		}

		//////////////////////////
		
		private void SpecialHandling ()
		{
			if (special != SpecialType.Unknown)
				return;
			
			if (uri.IsFile) {
				path = uri.LocalPath;
				if (File.Exists (path))
					special = SpecialType.File;
				else if (Directory.Exists (path))
					special = SpecialType.Directory;
				else
					special = SpecialType.Invalid;
			}
			
			if (special == SpecialType.Unknown)
				special = SpecialType.None;
		}

		public bool IsValid {
			get { SpecialHandling (); return special != SpecialType.Invalid; }
		}

		public bool IsFile {
			get { SpecialHandling (); return special == SpecialType.File; }
		}

		public bool IsDirectory {
			get { SpecialHandling (); return special == SpecialType.Directory; }
		}

		public bool IsFileSystem {
			get { return IsFile || IsDirectory; }
		}

		public string Path {
			get { SpecialHandling (); return path; }
		}

		public string PathQuoted {
			get { return Path.Replace (" ", "\\ "); }
		}

		public string FileName {
			get { return Path != null ? System.IO.Path.GetFileName (Path) : null; }
		}

		public string DirectoryName {
			get { return Path != null ? System.IO.Path.GetDirectoryName (Path) : null; }
		}

		[XmlIgnore]
		public FileSystemInfo FileSystemInfo {
			get {
				if (IsFile)
					return (FileSystemInfo) FileInfo;
				else if (IsDirectory)
					return (FileSystemInfo) DirectoryInfo;
				else
					return null;
			}
		}

		[XmlIgnore]
		public FileInfo FileInfo {
			get { 
				if (fileInfo == null && IsFile)
					fileInfo = new FileInfo (Path);
				return fileInfo;
			}
		}

		[XmlIgnore]
		public DirectoryInfo DirectoryInfo {
			get {
				if (directoryInfo == null && IsDirectory)
					directoryInfo = new DirectoryInfo (Path);
				return directoryInfo;
			}
		}

		//////////////////////////

		[XmlIgnore]
		public ArrayList Properties {
			get {  return properties; }
		}

		[XmlArray (ElementName="Properties")]
		[XmlArrayItem (ElementName="Property", Type=typeof (Property))]
		public PropertyList PropertyList {
			get { return new PropertyList (properties); }
			set {
				foreach (Property prop in value)
					properties.Add (prop);
			}
		}

		public void AddProperty (Property prop)
		{
			if (prop == null)
				return;

			int loc = properties.BinarySearch (prop);

			// If the value is not in the array we get its position
			// by taking bitwise complement.
			if (loc < 0)
				loc = ~loc;

			properties.Insert (loc, prop);
		}

		public void AddProperty (ICollection props)
		{
			if (props == null)
				return;

			properties.AddRange (props);
			properties.Sort ();
		}


		private bool FindProperty (string key, out int first, out int top)
		{
			first = 0;
			top = 0;
			
			int range = properties.Count - 1;
			if (range < 0)
				return false;

			Property prop;
			// O(log n + |range|)-time algorithm for 1-d range query
			while (range > 1) {
				// find middle index
				int mid = first + (range/2);
				
				prop = properties [mid] as Property;
				// Properties are sorted first by Key, then by Value
				// We only need to compare the key of the middle element to the key in parameter
				if (String.Compare (key, prop.Key) > 0) {
					// minimum item in right subtree is smaller
					// move right
					first = mid;
					range -= (range/2);
				} else
					// move left
					range = (range/2);
			}

			// first points to 
			// - either the previous item in the BST
			// - or the next to the previous item in the BST
			prop = (Property) properties [first];
			if (prop.Key != key)
				first = first + 1;
			if (first >= properties.Count)
				return false;
			prop = (Property) properties [first];
			if (prop.Key != key)
				return false;

			top = first + 1;
			// Since range will be small, do a linear scan from here.
			// We could do another BST traversal at the cost of O(log-n),
			// but its not worth it.
			while (top < properties.Count) {
				prop = properties [top] as Property;
				if (prop.Key != key)
					break;
				++top;
			}

			return true;
		}

		public string this [string key] {
			get {
				int first, top;
				if (! FindProperty (key, out first, out top))
					return null;

				if (top - first != 1) {
					Logger.Log.Warn ("Accessed multi-property key '{0}' with Hit's indexer.", key);
					return null;
				}

				Property prop;
				prop = properties [first] as Property;
				return prop.Value;
			}

			set {
				int first = 0, top = 0;

				// If we've never heard of this property, add it.
				if (! FindProperty (key, out first, out top)) {
					AddProperty (Property.New (key, value));
					return;
				}

				// If it has appeared once before, clobber the existing
				// value.  This emulates the previous (broken) semantics.

				if (top - first == 1) {
					properties [first] = Property.New (key, value);
					return;
				}
				
				// Otherwise throw an exception (which sort of sucks,
				// but we don't really know what to do there)
				throw new Exception (String.Format ("Attempt to re-set multi-property '{0}' via the indexer", key));
			}
		}

		public string GetFirstProperty (string key)
		{
			int first, top;

			if (! FindProperty (key, out first, out top))
				return null;

			Property prop = properties [first] as Property;
			return prop.Value;
		}

		public string[] GetProperties (string key)
		{
			int first, top;
			if (! FindProperty (key, out first, out top))
				return null;

			string[] values = new string [top - first];

			for (int i = 0; first + i < top; i++) {
				Property prop = properties [first + i] as Property;
				values [i] = prop.Value;
			}

			return values;
		}			

		//////////////////////////

		public override int GetHashCode ()
		{
			string type = Type;
			string source = Source;
			return (uri != null ? uri.ToString().GetHashCode () : 0)
				^ (type != null ? type.GetHashCode () : 0)
				^ (source != null ? source.GetHashCode () : 0);
		}

		//////////////////////////

		public int CompareTo (object obj)
		{
			Hit otherHit = (Hit) obj;
			// Notice that we sort by time from most to least recent
			return DateTime.Compare (otherHit.Timestamp, this.Timestamp);
		}
	}
}
