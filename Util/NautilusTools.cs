//
// NautilusTools.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.Xml;
using System.Collections;

namespace Beagle.Util {

	public static class NautilusTools {

		public class NautilusMetadata {
			public Uri Uri;
			public string Notes;
			public ArrayList Emblems;
		}

		static public IEnumerable GetMetadata (string metafile)
		{
			return GetMetadata (metafile, DateTime.MinValue);
		}

		static public IEnumerable GetMetadata (string metafile, DateTime newer_than)
		{
			string dir_name = GetDirectory (metafile);

			if (dir_name == null)
				yield break;

			XmlDocument doc = new XmlDocument ();
			StreamReader reader = null;
			
			try {
				reader = new StreamReader (metafile);
				doc.Load (reader);
			} catch (Exception e) {
				Log.Error (e, "Error while trying to parse nautilus metadata file {0}", metafile);

				if (reader != null)
					reader.Close ();
				reader = null;
			}

			if (reader == null)
				yield break;

			foreach (XmlNode node in doc.SelectNodes ("/directory/file[@name]")) {
				XmlNode timestamp_node = node.Attributes.GetNamedItem ("timestamp");
				long time_t;
				DateTime timestamp;

				// Intentionally give non-timestamped entries a
				// timestamp of the Unix epoch rather than
				// DateTime.MinValue, so that they are
				// processed when newer_than is.
				if (timestamp_node == null)
					time_t = 0;
				else
					time_t = Int64.Parse ((string) timestamp_node.Value);

				timestamp = DateTimeUtil.UnixToDateTimeUtc (time_t);
 
				if (timestamp <= newer_than)
					continue;
				
				NautilusMetadata nm = new NautilusMetadata ();

				string filename = Path.Combine (dir_name, (string) node.Attributes.GetNamedItem ("name").Value);

				// The filename is already escaped for us.
				nm.Uri = new Uri (filename, true);

				XmlNode notes_node = node.Attributes.GetNamedItem ("annotation");

				if (notes_node != null)
					nm.Notes = (string) notes_node.Value;

				nm.Emblems = new ArrayList ();

				foreach (XmlNode emblem_node in node.SelectNodes ("keyword"))
					nm.Emblems.Add (emblem_node.Attributes.GetNamedItem ("name").Value);

				// If we don't have a timestamp, and have
				// neither notes nor an emblem, this isn't an
				// interesting node.
				if (time_t == 0 && String.IsNullOrEmpty (nm.Notes) && nm.Emblems.Count == 0)
					continue;

				yield return nm;
			}

			reader.Close ();

			yield break;
		}

		static private string GetDirectory (string metafile)
		{
			string directory;

			// Get only the name of the file, which represents
			// the directory.  Also drop the .xml extension
			directory = Path.GetFileNameWithoutExtension (metafile);

			// We only care about file URIs right now.
			if (! directory.StartsWith ("file:%2F%2F%2F"))
				return null;

			// Unescape this mess.
			directory = directory.Replace ("%25", "%");
			directory = directory.Replace ("%2F", "/");

			return directory;
		}

		public static void Main (string[] args)
		{
			foreach (string a in args) {
				Console.WriteLine ("{0}: {1}", a, GetDirectory (a));
				
				foreach (NautilusMetadata nm in GetMetadata (a)) {
					Console.WriteLine ("  uri: {0}", nm.Uri);
					Console.WriteLine ("  notes: {0}", nm.Notes);
					Console.Write ("  emblems: ");

					foreach (string e in nm.Emblems)
						Console.Write (e + " ");

					Console.WriteLine ();
				}
			}
			
		}
	}
}
