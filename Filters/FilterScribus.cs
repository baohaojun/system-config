//
// FilterScribus.cs
//
// Copyright (C) 2006 Alexander Macdonald <alex@alexmac.cc>
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
using System.Text;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {
	public class FilterScribus : Beagle.Daemon.Filter {
		// These two arrays contain the mapping from attributes in a scribus
		// file to actual beagle property names
		private static string[] scribus_attributes = { "TITLE", "KEYWORDS", "COMMENTS", "DOCSOURCE", "DOCCONTRIB", "DOCRELATION", "DOCIDENT", "PUBLISHER", "AUTHOR", "DOCCOVER", "DOCRIGHTS" };
		private static string[] scribus_properties = { "dc:title", "dc:keywords", "dc:description", "fixme:source", "dc:contributers", "fixme:relation", "dc:identifier", "dc:publisher", "dc:author", "fixme:coverage", "dc:rights" };
		
		private StringBuilder sb = new StringBuilder();

		public FilterScribus ()
		{
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-scribus"));
			AddSupportedFlavor (FilterFlavor.NewFromExtension (".sla"));
		}

		override protected void DoPullProperties ()
		{
			XmlTextReader reader = new XmlTextReader (Stream);

			string text = String.Empty;
			int pagecount = 0;
			
			bool join_text_mode = false;

			try {
				while (reader.Read ()) {
					switch (reader.NodeType) {
					case XmlNodeType.Element:
						switch (reader.LocalName) {
						case "SCRIBUSUTF8":
							join_text_mode = true;
							AddProperty (Property.New ("fixme:scribus-version" , reader.GetAttribute("Version")));
							break;
						case "SCRIBUSUTF8NEW":
							AddProperty (Property.New ("fixme:scribus-version" , reader.GetAttribute("Version")));
							break;
						case "DOCUMENT":
							for (int i=0; i<scribus_attributes.Length; i++) {
								text = reader.GetAttribute(scribus_attributes[i]);
								AddProperty (Property.New (scribus_properties[i] , text));
							}
							
							// treat the DOCDATE attribute in a special way
							text = reader.GetAttribute("DOCDATE");
							if (text != null) {
								try {
									AddProperty (Property.NewDate ("dc:date", System.Convert.ToDateTime (text)));
								} catch (FormatException) {
									AddProperty (Property.New ("dc:date", text));
								}
							}
							break;
						case "ITEXT":
							text = reader.GetAttribute("CH");
							if (text == null)
								break;
							
							if (join_text_mode) {
								sb.Append(text);
							} else {
								AppendText (text);
								AppendStructuralBreak ();
							}
							break;
						case "PAGE":
							pagecount++;
							break;
						}
						break;
					case XmlNodeType.Comment:
						AppendText (reader.Value.Trim ());
						AppendStructuralBreak ();
						break;
					}
				}

				AddProperty (Property.New ("fixme:pagecount", pagecount.ToString ()));
				
				if (join_text_mode)
					AppendText (sb.ToString());

				Finished ();
			} catch (System.Xml.XmlException e) {
				Logger.Log.Error ("Error parsing xml file {0}", FileInfo.FullName);
				Logger.Log.Debug (e);
				Error ();
			}
		}
	}
}
