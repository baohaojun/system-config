//
// PropertyKeywordFu.cs
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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;
using System.Xml.Serialization;

using Beagle;
using Beagle.Util;

namespace Beagle.Daemon {

	public static class PropertyKeywordFu {
		// mapping
		private static Hashtable property_table;

		public static IEnumerable Keys {
			get { return property_table.Keys; }
		}

		public static IEnumerable Properties (string keyword) {
			if (! property_table.Contains (keyword))
				yield break;

			object o = property_table [keyword];
			if (o is QueryKeywordMapping)
				yield return o;
			else if (o is ArrayList) {
				foreach (QueryKeywordMapping mapping in ((ArrayList) o))
					yield return mapping;
			}
		}

		////////////////////////////////////////////////////////

		public static void ReadKeywordMappings ()
		{
			property_table = new Hashtable ();

			// FIXME: No need for a SerializerFactory here, since we need the serializer
			// only once
			XmlSerializerFactory xsf = new XmlSerializerFactory();
			XmlSerializer xs = xsf.CreateSerializer (typeof (QueryMapping), new Type[] { typeof (QueryKeywordMapping)});

			QueryMapping query_mapping = null;

			// <keyword name, can override>
			Dictionary<string, bool> mapping_override = new Dictionary<string, bool> ();

			using (Stream s = File.OpenRead (Path.Combine (PathFinder.ConfigDataDir, "query-mapping.xml"))) {
				try {			
					query_mapping = (QueryMapping) xs.Deserialize (s);
					foreach (QueryKeywordMapping mapping in query_mapping.Mappings) {
						PropertyKeywordFu.RegisterMapping (mapping);
						mapping_override [mapping.Keyword] = true;
					}
				} catch (XmlException e) {
					Logger.Log.Error (e, "Unable to parse global query-mapping.xml");
				}
			}

			// Override global mappings by local mappings

			if (! File.Exists (Path.Combine (PathFinder.StorageDir, "query-mapping.xml")))
				return;

			using (Stream s = File.OpenRead (Path.Combine (PathFinder.StorageDir, "query-mapping.xml"))) {
				try {			
					query_mapping = (QueryMapping) xs.Deserialize (s);
					foreach (QueryKeywordMapping mapping in query_mapping.Mappings) {
						if (mapping_override.ContainsKey (mapping.Keyword)) {
							property_table.Remove (mapping.Keyword);
							mapping_override [mapping.Keyword] = false;
						}

						PropertyKeywordFu.RegisterMapping (mapping);
					}
				} catch (XmlException e) {
					Logger.Log.Error (e, "Unable to parse local query-mapping.xml");
				}
			}
		}

		private static void RegisterMapping (QueryKeywordMapping mapping)
		{
			// If multiple mapping as registered, create an OR query for them
			// Store the multiple matchings in a list
			if (property_table.Contains (mapping.Keyword)) {
				object o = property_table [mapping.Keyword];
				if (o is ArrayList) {
					((ArrayList)o).Add (mapping);
				} else if (o is QueryKeywordMapping) {
					ArrayList list = new ArrayList (2);
					list.Add (o);
					list.Add (mapping);
					property_table [mapping.Keyword] = list;
				}
				return;
			}

			property_table.Add (mapping.Keyword, mapping);
		}

		////////////////////////////////////////////////////////

		// return false if property not found!
		public static bool GetMapping (string keyword, out int num, out string[] name, out PropertyType[] type) {
			num = 0;
			name = null;
			type = null;

			if (! property_table.Contains (keyword))
				return false;

			object o = property_table [keyword];
			if (o is ArrayList) {
				ArrayList list = (ArrayList) o;
				num = list.Count;
				name = new string [num];
				type = new PropertyType [num];

				for (int i = 0; i < num; ++i) {
					QueryKeywordMapping mapping = (QueryKeywordMapping) (list [i]);
					name [i] = mapping.PropertyName;
					type [i] = (mapping.IsKeyword ? PropertyType.Keyword : PropertyType.Text);
				}
			} else if (o is QueryKeywordMapping) {
				num = 1;
				name = new string [num];
				type = new PropertyType [num];
				name [0] = ((QueryKeywordMapping) o).PropertyName;
				type [0] = (((QueryKeywordMapping) o).IsKeyword ? PropertyType.Keyword : PropertyType.Text);
			}
			return true;
		}
	}

	public class QueryMapping
	{
		private List<QueryKeywordMapping> property_mappings = new List<QueryKeywordMapping> ();

		[XmlArray]
		[XmlArrayItem (ElementName="Mapping", Type=typeof (QueryKeywordMapping))]
		public List<QueryKeywordMapping> Mappings {
			get { return property_mappings; }
			set { property_mappings = value; }
		}

		public QueryMapping () { }
	}
}
