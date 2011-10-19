//
// FilterSVG.cs
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
	public class FilterSvg : Beagle.Daemon.Filter {
		private StringBuilder sb = new StringBuilder ();

		// List of keys that should be ignored when adding to content.
		// For example, dc:format is the mime type, so it's not interesting text.
		static private string [] ignore_strings = { "format"};
		
		static private string dcnamespace = "http://purl.org/dc/elements/1.1/";
		static private string ccnamespace = "http://creativecommons.org/ns#";
		
		public FilterSvg ()
		{
			SetVersion (2);
			// FIXME: Should really set FileType as "image"
			// But this does not extract common image properties,
			// so will confuse everybody
			// Version 2: Add file license metadata to properties.
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/svg+xml"));
			AddSupportedFlavor (FilterFlavor.NewFromExtension (".svg"));
		}

		override protected void DoPullProperties ()
		{
			XmlReaderSettings reader_settings = new XmlReaderSettings ();
			reader_settings.ProhibitDtd = false;
			
			XmlNamespaceManager mngr = new XmlNamespaceManager (new NameTable ());
			// SVG created by batik has does not declare xlink
			mngr.AddNamespace ("xlink", "http://www.w3.org/1999/xlink");
			XmlParserContext pc = new XmlParserContext (null, mngr, null, XmlSpace.Default);

			XmlReader reader = XmlReader.Create (Stream, reader_settings, pc);

			string current_tag_name = String.Empty;
			bool grab_property = false;
			bool grab_text = false;

			
			while (reader.ReadState == ReadState.Initial || reader.ReadState == ReadState.Interactive) {
				try {
					while (reader.Read ()) {
						switch (reader.NodeType) {
						case XmlNodeType.Element:
							if (grab_text)
								break;
							if (reader.IsEmptyElement)
								break;
							
							if (Array.IndexOf<string> (ignore_strings, reader.LocalName) != -1)
								break;
							else if (reader.LocalName == "title") {
								grab_property = true;
								current_tag_name = "title";
							} else if (reader.LocalName == "desc") {
								grab_property = true;
								current_tag_name = "description";
							} else if (reader.LocalName == "text") {
								grab_text = true;
								current_tag_name = "text";
							} else if (reader.LocalName == "RDF") {
								PullRdfProperties (reader, reader.Depth);
							}
							break;

						case XmlNodeType.Text:
							if (grab_property)
								AddProperty (Property.New ("dc:" + current_tag_name , reader.Value));
							else if (grab_text) {
								sb.Append (reader.Value);
							}
							break;

						case XmlNodeType.Comment:
							AppendText (reader.Value.Trim ());
							AppendStructuralBreak ();
							break;

						case XmlNodeType.EndElement:
							if (reader.LocalName != current_tag_name)
								break;

							if (grab_text) {
								AppendText (sb.ToString());
								AppendStructuralBreak ();
								sb.Length = 0;
							}

							grab_text = grab_property = false;
							break;
						}
					}
				} catch (System.Xml.XmlException e) {
					Logger.Log.Error ("Fatal error parsing xml file {0}", FileInfo.FullName);
					Logger.Log.Debug (e);
					Error ();
					return;
				}
			}
			
			Finished ();
		}

		protected void PullRdfProperties (XmlReader reader, int depth)
		{
			string current_tag_name = String.Empty;
			bool grab_text = false;
			bool grab_date = false;

			while (reader.ReadState == ReadState.Initial || reader.ReadState == ReadState.Interactive) {
				try {
					while (reader.Read ()) {
						if (depth == reader.Depth)
							return;			

						switch (reader.NodeType) {
						case XmlNodeType.Element:
							if (grab_text)
								break;
							
							if (reader.IsEmptyElement){
 								if (reader.NamespaceURI == ccnamespace) {
									if ( reader.LocalName == "license" ) {
										AddProperty (Property.NewKeyword ("fixme:license", reader.GetAttribute("resource","http://www.w3.org/1999/02/22-rdf-syntax-ns#")));
									}
								}
 								break;
							}

							if (Array.IndexOf<string> (ignore_strings, reader.LocalName) != -1)
								break;
							else if (reader.NamespaceURI == dcnamespace) {
								if (reader.LocalName == "date")
									grab_date = true;
								else
									grab_text = true;
							}

							current_tag_name = reader.LocalName;
							break;

						case XmlNodeType.Text:
							if (grab_text)
								AddProperty (Property.New ("dc:" + current_tag_name, reader.Value));
							else if (grab_date) {
								try {
									AddProperty (Property.NewDate ("dc:date", System.Convert.ToDateTime (reader.Value.Trim())));
								} catch (FormatException) {
									AddProperty (Property.New ("dc:date", reader.Value));
								}
							}
							break;

						case XmlNodeType.EndElement:
							if(reader.LocalName == current_tag_name)
								grab_text = grab_date = false;
							break;
						}
					}
				} catch (System.Xml.XmlException e) {
					if (reader.ReadState == ReadState.Error) {
						Logger.Log.Error ("Fatal error parsing embedded RDF {0}", FileInfo.FullName);
						Logger.Log.Debug (e);
						Error ();
						return;
					} else {
						Logger.Log.Debug ("Non-Fatal error parsing embedded RDF {0}", FileInfo.FullName);
						Logger.Log.Debug (e.Message);
					}
				}
			}
		}
	}
}
