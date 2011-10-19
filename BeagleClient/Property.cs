//
// Property.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.IO;
using System.Text;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

	public enum PropertyType {
		Internal = 0,
		Text     = 1,
		Keyword  = 2,
		Date     = 3
	}

	/// <summary>
	///  IEnumerable class to serialize properties with non-private namespace.
	/// </summary>
	public class PropertyList : IEnumerable {
		private ArrayList property_list;
		
		public PropertyList ()
		{
		        property_list = new ArrayList ();
		}
		
		public PropertyList (ArrayList list)
		{
		        property_list = list;
		}
		
		public IEnumerator GetEnumerator ()
		{
		        return new PropertyListEnumerator (property_list);
		}
		
		public void Add (object o)
		{
		        property_list.Add (o);
		}
	}
	
	internal class PropertyListEnumerator : IEnumerator {
		private ArrayList property_list;
		int position = -1; // Position is before the first element initially
		
		public PropertyListEnumerator (ArrayList list)
		{
		        property_list = list;
		}
		
		public bool MoveNext ()
		{
		        position ++;
		        while (position < property_list.Count) {
		    	    Property prop = (Property) property_list [position];
		    	    if (prop != null && ! prop.Key.StartsWith (Property.PrivateNamespace))
		    		    break;
		    	    position ++;
		        }
		
		        return (position < property_list.Count);
		}
		
		public void Reset ()
		{
		        position = -1;
		}
		
		public object Current {
		        get {
				try {
					return property_list [position];
				} catch (IndexOutOfRangeException) {
					throw new InvalidOperationException();
				}
			}
		}
	}

	public class Property : IComparable, ICloneable {
		
		PropertyType type;
		string       key;
		string       value;
		bool         is_searched;
		bool         is_mutable;
		bool	     is_stored;
		bool         is_persistent;

		// Commonly used property keys
		public const string PrivateNamespace = "_private:";
		public const string SplitFilenamePropKey = "beagle:SplitFilename";
		public const string ExactFilenamePropKey = "beagle:ExactFilename";
		public const string TextFilenamePropKey = "beagle:Filename";
		public const string NoPunctFilenamePropKey = "beagle:NoPunctFilename";
		public const string FilenameExtensionPropKey = "beagle:FilenameExtension";
		public const string ParentDirUriPropKey = Property.PrivateNamespace + "ParentDirUri";
		public const string IsDirectoryPropKey = Property.PrivateNamespace + "IsDirectory";
		public const string IsChildPropKey = "beagle:IsChild";


		[XmlAttribute]
		public PropertyType Type {
			get { return type; }
			set { type = value; }
		}
		
		[XmlAttribute]
		public string Key {
			get { return key; }
			set { this.key = StringFu.CleanupInvalidXmlCharacters (value); }
		}

		[XmlAttribute]
		public string Value {
			get { return value; }
			set { this.value = StringFu.CleanupInvalidXmlCharacters (value); }
		}

		
		/// <value>
		/// If IsSearched is true, this property will can be matched by a
		/// general match-any-propety query.
		/// You can always query against the specific property, even if
		/// IsSearched is false.
		/// </value>
		[XmlAttribute]
		public bool IsSearched {
			get { return is_searched; }
			set { is_searched = value; }
		}

		
		/// <value>
		///  When IsMutable is true, the property is stored in the secondary
		/// index so that it can more efficiently be changed later on.
		/// </value>
		[XmlAttribute]
		public bool IsMutable {
			get { return is_mutable; }
			set { is_mutable = value; }
		}

		/// <value>
		///  When IsStored is false, the property will be stored as an
		///  "unstored lucene field".
		/// </value>
		[XmlAttribute]
		public bool IsStored {
			get { return is_stored; }
			set { is_stored = value; }
		}

		
		/// <value>
		/// When true, this property is persisted across documents being
		/// readded, for instance if a file is touched on disk.
		/// </value>
		[XmlAttribute]
		public bool IsPersistent {
			get { return is_persistent; }
			set { is_persistent = value; }
		}

		/////////////////////////////////////

		public Property () { }

		public int CompareTo (object other)
		{
			// By convention, a non-null object always
			// compares greater than null.
			if (other == null)
				return 1;

			Property other_property = other as Property;

			// If the other object is not a Property, compare the
			// two objects by their hash codes.
			if (other_property == null)
				return this.GetHashCode ().CompareTo (other.GetHashCode ());

			int rv;
			rv = String.Compare (this.Key, other_property.Key);
			if (rv != 0)
				return rv;

			return String.Compare (this.Value, other_property.Value);
		}

		public object Clone ()
		{
			return this.MemberwiseClone ();
		}

		static public Property New (string key, string value)
		{
			if (value == null)
				return null;

			Property p = new Property ();
			p.type = PropertyType.Text;
			p.Key = key;
			p.Value = value;
			p.is_searched = true;
			p.is_stored = true;
			return p;
		}

		static public Property NewKeyword (string key, object value)
		{
			if (value == null)
				return null;

			Property p = new Property ();
			p.type = PropertyType.Keyword;
			p.Key = key;
			p.Value = value.ToString ();
			p.is_searched = true;
			p.is_stored = true;
			return p;
		}

		static public Property NewUnsearched (string key, object value)
		{		
			if (value == null)
				return null;

			Property p = new Property ();
			p.type = PropertyType.Keyword;
			p.Key = key;
			p.Value = value.ToString ();
			p.is_searched = false;
			p.is_stored = true;
			return p;
		}

		static public Property NewUnstored (string key, object value)
		{		
			if (value == null)
				return null;

			Property p = new Property ();
			p.type = PropertyType.Text;
			p.Key = key;
			p.Value = value.ToString ();
			p.is_searched = true;
			p.is_stored = false;
			return p;
		}

		static public Property NewBool (string key, bool value)
		{
			return Property.NewUnsearched (key, value ? "true" : "false");
		}

		static public Property NewFlag (string key)
		{
			return NewBool (key, true);
		}

		static public Property NewDate (string key, DateTime dt)
		{
			Property p = new Property ();
			p.type = PropertyType.Date;
			p.Key = key;
			p.Value = StringFu.DateTimeToString (dt);
			p.is_searched = true;
			p.is_stored = true;
			return p;
		}

		static public Property NewDateFromString (string key, string value)
		{
			if (value == null)
				return null;

			Property p = new Property ();
			p.type = PropertyType.Date;
			p.Key = key;
			// FIXME: Should probably check that value is a valid date string.
			p.Value = value;
			p.is_searched = true;
			p.is_stored = true;
			return p;
		}

		override public string ToString ()
		{
			return String.Format ("{0}={1}", Key, Value);
		}

		/// <summary>
		/// Standard properties for files
		/// Used by FileSystem backend and filters which produce file child-indexables
		/// </summary>
		/// <param name="name">
		/// A <see cref="System.String"/>
		/// </param>
		/// <param name="mutable">
		/// A <see cref="System.Boolean"/>
		/// </param>
		/// <returns>
		/// A <see cref="IEnumerable"/>
		/// </returns>
		public static IEnumerable StandardFileProperties (string name, bool mutable)
		{
			StringBuilder sb;
			sb = new StringBuilder ();

			string no_ext, ext, no_punct;
			no_ext = Path.GetFileNameWithoutExtension (name);
			ext = Path.GetExtension (name).ToLower ();
			
			sb.Append (no_ext);
			for (int i = 0; i < sb.Length; ++i)
				if (! Char.IsLetterOrDigit (sb [i]))
					sb [i] = ' ';
			no_punct = sb.ToString ();


			Property prop;

			prop = Property.NewKeyword (ExactFilenamePropKey, name);
			prop.IsMutable = mutable;
			yield return prop;

			prop = Property.New (TextFilenamePropKey, no_ext);
			prop.IsMutable = mutable;
			yield return prop;

			prop = Property.New (NoPunctFilenamePropKey, no_punct);
			prop.IsMutable = mutable;
			yield return prop;

			prop = Property.NewUnsearched (FilenameExtensionPropKey, ext);
			prop.IsMutable = mutable;
			yield return prop;

			string str;
			str = StringFu.FuzzyDivide (no_ext);
			prop = Property.New (SplitFilenamePropKey, str);
			prop.IsMutable = mutable;
			yield return prop;
		}
	}
}
