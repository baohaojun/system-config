//
// FilterXSLT.cs
//
// Copyright (C) 2006,2007 Alexander Macdonald <alex@alexmac.cc>
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
using System.Text.RegularExpressions;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters 
{
	public class FilterXslt : Filter 
	{
		static Regex xpath_variables_regex = new Regex ("\\$(?<name>[\\wS][\\wS\\d-:]*)", RegexOptions.Compiled);
		static Regex xpath_functions_regex = new Regex ("(?<name>[\\wS][\\wS\\d-:]*)\\(", RegexOptions.Compiled);

		public FilterXslt ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
//			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-xslt"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xslt+xml"));
		}

		override protected void DoPull()
		{
			XmlReaderSettings reader_settings = new XmlReaderSettings ();
			reader_settings.ProhibitDtd = false;
			XmlReader reader = XmlReader.Create (Stream, reader_settings);

			try {
				while (reader.Read ()) {
					switch (reader.NodeType) {

					case XmlNodeType.Element:
						HandleElement (reader);
						break;

					case XmlNodeType.Text:
						AppendText (reader.Value.Trim ());
						AppendStructuralBreak ();
						break;

					case XmlNodeType.CDATA:
						AppendText (reader.Value.Trim ());
						AppendStructuralBreak ();
						break;

					case XmlNodeType.Comment:
						AppendText (reader.Value.Trim ());
						AppendStructuralBreak ();
						break;
					}
				}
			} catch (System.Xml.XmlException e) {
				Logger.Log.Error ("Fatal error parsing xml file {0}", FileInfo.FullName);
				Logger.Log.Debug (e);
				Error ();
				return;
			}
			
			Finished ();
		}

		protected void HandleElement(XmlReader reader)
		{
			for (int i = 0; i < reader.AttributeCount; i++) {
				reader.MoveToAttribute (i);

				switch (reader.LocalName) {

				case "name":
					AppendText (reader.Value);
					AppendStructuralBreak ();
					break;

				case "select":
					foreach (Match m in xpath_variables_regex.Matches (reader.Value)) {
						AppendText (m.Groups ["name"].Value);
						AppendStructuralBreak ();
					}

					foreach (Match m in xpath_functions_regex.Matches (reader.Value)) {
						AppendText (m.Groups ["name"].Value);
						AppendStructuralBreak ();
					}

					break;
				}
			}

			reader.MoveToElement ();
		}
	}
}
