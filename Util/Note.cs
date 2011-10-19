//
// Note.cs
//
// Copyright (C) 2004 Christopher Orr
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
using System.Text;
using System.Xml;
using System.Collections.Generic;

namespace Beagle.Util {

	public abstract class Note {

		private string type;
		private string filename;
		public  string subject;
		public  string text;
		public  DateTime timestamp;
		public  List<string> tags;

		//////////////////////////

		protected Note (string _type, string _filename) {
			this.type = _type;
			this.filename = _filename;
			this.tags = new List<string>();
		}

		// FIXME: this is a mess
		public Uri Uri {
			get {
				return BuildNoteUri (filename, type);
			}
		}

		public static Uri BuildNoteUri (string _filename, string _type)
		{
			string name = Path.GetFileNameWithoutExtension (_filename);
			string uri  = String.Format ("note://{0}/{1}", _type, name);
			return new Uri (uri, true);
		}
	}

	///////////////////////////////////////////////////////////////////////////////

	//
	// Parse Tomboy notes
	//

	public class TomboyNote : Note {

		private TomboyNote (string type, string filename) : base ("tomboy", filename)
		{ }

		private TomboyNote (string filename) : base ("tomboy", filename)
		{ }

 		public static Note ParseNote (FileInfo file)
		{
			// Tomboy uses .note files.
			// Don't index the backup folder.
			if (file.Extension != ".note" 
			    || file.DirectoryName.EndsWith ("/Backup")) {
				return null;
			}

			// Create new note instance
			Note note = new TomboyNote(file.FullName);
			note.timestamp = file.LastWriteTimeUtc;

			// Parse XML info from file
			StreamReader reader = new StreamReader (file.FullName, System.Text.Encoding.UTF8);
			XmlTextReader doc = new XmlTextReader (reader);
			doc.Namespaces = false;

			bool read_text = false;
			StringBuilder sb = new StringBuilder ();

			while (doc.Read ()) {
				// FIXME: Extract more information than mere text from the tags in note-content
				// for hottext, linking notes etc.
				if (doc.NodeType == XmlNodeType.Element && doc.Name == "note-content") {
					read_text = true;
					continue;
				}

				if (doc.NodeType == XmlNodeType.EndElement && doc.Name == "note-content") {
					read_text = false;
					continue;
				}

				if (doc.NodeType == XmlNodeType.Element && doc.Name == "title") {
					note.subject = doc.ReadString ();
					continue;
				}
				if (doc.NodeType == XmlNodeType.Element && doc.Name == "tag"){
					note.tags.Add(doc.ReadString());
					continue;
				}

				if (doc.NodeType == XmlNodeType.Text) {
					if (read_text)
						sb.Append (doc.Value);
					continue;
				}
			}

			doc.Close ();
			note.text = sb.ToString ();

			return note;
		}

	}
}

