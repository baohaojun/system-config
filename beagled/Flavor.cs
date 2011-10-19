//
// Flavor.cs
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

using Beagle.Util;

namespace Beagle.Daemon {

	public class FilterFlavor {

		private string uri = null;
		private string mime_type = null;
		private string extension = null;

		private int priority = 0;

		public string Uri { 
			get { return uri; }
		}

		public string Extension {
			get { return extension; }
		}

		public string MimeType {
			get { return mime_type; }
		}

		public int Priority {
			get { return priority; }
			set { priority = value; }
		}

		public FilterFlavor (string uri, string extension, string mime_type, int priority) 
		{
			this.uri = uri;
			this.extension = extension;
			this.mime_type = mime_type;
			this.priority = priority;
		}

		private bool IsWild (string str)
		{
			if (str == null)
				return true;
			if (str == "")
				return false;
			foreach (char c in str)
				if (c != '*')
					return false;
			return true;
		}

		public bool IsMatch (Uri uri, string extension, string mime_type)
		{
			if (Uri != null && (uri == null || !StringFu.GlobMatch (Uri, uri.ToString ())))
				return false;

			if (Extension != null && (extension == null || !StringFu.GlobMatch (Extension, extension)))
				return false;

			if (MimeType != null && (mime_type == null || !StringFu.GlobMatch (MimeType, mime_type)))
				return false;

			return true;
		}

		public int Weight 
		{
			get {
				int weight = priority;

				/* Uri matches are very important, next are extensions and then mimetype.
				 * This allows filters to override everything else by specifying matching Uris,
				 * and override mimetype by matching extensions.
				 */
				if (Uri != null)
					weight += 3;
				if (Extension != null)
					weight += 2;
				if (MimeType != null)
					weight += 1;

				return weight;
			}
		}

		////////////////////////////////////////////

		public override string ToString ()
		{
			string ret = String.Empty;

			if (Uri != null)
				ret += String.Format ("Uri: {0}", Uri);

			if (Extension != null)
				ret += String.Format ("{1}Extension: {0}", Extension, (ret == String.Empty ? "": ", "));

			if (MimeType != null && ! MimeType.StartsWith ("beagle"))
				ret += String.Format ("{1}MimeType: {0}", MimeType, (ret == String.Empty ? "": ", "));

			return ret;
		}

		public class FlavorComparer : IComparer 
		{
			// flav [larger wt] < flav [smaller wt]
			// for same wt, use hashcode (never return obj1 == obj2 unless they are actually same)
			public int Compare (object obj1, object obj2) 
			{
				FilterFlavor flav1 = (FilterFlavor) obj1;
				FilterFlavor flav2 = (FilterFlavor) obj2;

				int ret = flav2.Weight.CompareTo (flav1.Weight);
				if (ret != 0)
					return ret;
				else
					return obj1.GetHashCode () - obj2.GetHashCode ();
			} 
		}

		static FlavorComparer flavor_comparer = new FlavorComparer ();

		public static SortedList NewHashtable ()
		{
			return new SortedList (flavor_comparer);
		}

		////////////////////////////////////////////

		private static Hashtable filter_types_by_flavor = new Hashtable ();
		
		public static Hashtable FilterTable {
			get { return filter_types_by_flavor; }
		}

		public static ICollection Flavors {
			get { return filter_types_by_flavor.Keys; }
		}

		public static FilterFlavor NewFromMimeType (string mime_type) {
			return new FilterFlavor (null, null, mime_type, 0);
		}

		public static FilterFlavor NewFromExtension (string extension) {
			return new FilterFlavor (null, extension, null, 0);
		}
	}
}
