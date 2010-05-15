//
// FilterLabyrinth.cs
//
// Copyright (C) 2006 Kevin Kubasik <kevin@kubasik.net>
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

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {
	
	public class FilterLabyrinth : Beagle.Daemon.Filter {


		public FilterLabyrinth ()
		{
			SnippetMode = true;
			OriginalIsText = false;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("x-beagle/x-labyrinth-note"));
		}


		protected override void DoPullProperties ()
		{
			StreamReader reader = new StreamReader (FileInfo.FullName, System.Text.Encoding.UTF8);
			XmlTextReader doc = new XmlTextReader (reader);
			try {
				 do {
					// FIXME: Extract more information than mere text from the tags in note-content
					// for hottext, linking notes etc.

					if (doc.GetAttribute("title") != null) {
						AddProperty (Property.New ("dc:title", doc.GetAttribute("title")));
						continue;
					}

					if (doc.NodeType == XmlNodeType.Text) {
						AppendText (doc.Value);
						AppendStructuralBreak ();
						continue;
					}
				} while (doc.Read ());
				Finished();
			} catch (Exception e) {
				Log.Warn (e, "Unable to index Labyrinth Note {0}", FileInfo.FullName);
				Error ();
			} 
			doc.Close();					
			
		}


	}
}
