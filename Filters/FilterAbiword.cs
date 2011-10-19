//
// FilterAbiword.cs : Trivial implementation of a Abiword-document filter.
//
// Author: Veerapuram Varadhan <vvaradhan@novell.com>
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
using System.IO;
using System.Text;
using System.Xml;

using Beagle.Util;
using Beagle.Daemon;

using ICSharpCode.SharpZipLib.GZip;

namespace Beagle.Filters {
    
	public class FilterAbiWord : Beagle.Daemon.Filter {

		Hashtable hotStyles;
		bool is_gzipped;
				
		public FilterAbiWord () 
		{
			SnippetMode = true;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-abiword"));
		}

		// Process the <styles> ... </styles> nodes.
		void StudyStyleNode (XmlTextReader reader)
		{
			string styleName = null;
			int original_depth = reader.Depth;

			if (!reader.IsEmptyElement) {
				reader.Read ();
				while (reader.Depth > original_depth) {
					if (reader.NodeType == XmlNodeType.Element 
					    && reader.Name == "s") {
						styleName = reader.GetAttribute ("name");
						if (styleName != null && 
						    (styleName.ToLower().IndexOf ("head") > -1 ||
						     styleName.ToLower().IndexOf ("note") > -1))
							hotStyles [styleName] = true;
					}
					reader.Read ();
				}
			} 
		}

		// Process the props="blah:blah; blah:blah;" values
		bool StudyPropsAttribute (string props)
		{
			string[] propsTokens = null; 
			string[] propAndValue = null;
			bool retVal = false;

			if (props == null)
				return false;

			propsTokens = props.Split (';');

			if (propsTokens.Length > 0) {
				for (int i = 0; i < propsTokens.Length; i++) {
					
					propAndValue = propsTokens[i].Split (':');
					switch (propAndValue[0].Trim()) {
					case "font-weight": 
						if (propAndValue[1] == "bold")
							retVal = true;
						break;

					case "font-style":
						if (propAndValue[1] == "italic")
							retVal = true;
						break;

					case "text-decoration":
						if (propAndValue[1] == "underline")
							retVal = true;
						break;

					case "bgcolor":
						return retVal = true;
					}
				}
			}
			return retVal;
		}
		
		static bool NodeIsFreezing (String nodeName)
		{
			return nodeName == "text:footnote-citation";
		}

		static bool NodeBreaksTextAfter (String nodeName)
		{
			return nodeName == "p";
		}

		private Stack hot_nodes = new Stack ();
		private bool inSection = false;

		// Walk through the <section> ... </section> nodes
		// and extract the texts.
		bool WalkContentNodes (XmlTextReader reader)
		{
			// total number of elements to read per-pull
			const int total_elements = 10;
			int num_elements = 0;
			while (reader.Read ()) {
				if (reader.Name == "styles" && 
				    reader.NodeType == XmlNodeType.Element) {
					StudyStyleNode (reader); 
					continue;
				} else if (!inSection && reader.Name != "section")
					continue;
				
				switch (reader.NodeType) {
				case XmlNodeType.Element:
					// A node/text is hot if:
					// (1) It is flagged with a hot style (header, footer and 
					// other styles)
					// (2) It contains "hot" styled attributes.
					bool isHot = false;
					if (reader.Name == "section") {
						string type = reader.GetAttribute ("type");
						if (type == "header" || 
						    type == "footer")
							isHot = true;
						inSection = true;

					} else 	if (reader.IsEmptyElement) {
						if (NodeBreaksTextAfter (reader.Name)) {
							AppendWhiteSpace ();
							AppendStructuralBreak ();
						}
						continue;
					}

					// <c ....> text blah blah </c> overrides the 
					// formatting at the paragraph level.
					if (reader.Name == "c") {
						string val = reader.GetAttribute ("props");
						isHot = StudyPropsAttribute (val);
						//Console.WriteLine ("{0} is hot? {1}", val, isHot);
					} else {
					
						bool has_attr = reader.MoveToFirstAttribute ();
						while (has_attr) {
							if (reader.Name == "style") {
								if (hotStyles.Contains (reader.Value))
									isHot = true;
								break;
							}
							has_attr = reader.MoveToNextAttribute ();
						
						}
						reader.MoveToElement();
					}				
				
					hot_nodes.Push (isHot);
				
					if (isHot)
						HotUp ();
				
					if (NodeIsFreezing (reader.Name))
						FreezeUp ();
				
					break;
				case XmlNodeType.Text:
					string text = reader.Value;
					AppendText (text);
					break;
				case XmlNodeType.EndElement:
					if (NodeBreaksTextAfter (reader.Name)) {
						AppendWhiteSpace ();
						AppendStructuralBreak ();
					}

					bool is_hot = (bool) hot_nodes.Pop ();
					if (is_hot)
						HotDown ();
				
					if (NodeIsFreezing (reader.Name))
						FreezeDown ();
					if (reader.Name == "section")
						inSection = false;
					break;
				}
				num_elements++;
				if (num_elements >= total_elements) {
					return false;
				}
			}
			return true;
		}

		private void ExtractMetadata (XmlTextReader reader)
		{
			string key = null;
			bool found = false;
			int depth = -1;

			while (reader.Read()) {
				if (!found && reader.Name == "metadata" && reader.NodeType == XmlNodeType.Element) {
					found = true;
					depth = reader.Depth;
					continue;
				}
				
				if (found && reader.Name == "metadata" && reader.NodeType == XmlNodeType.EndElement)
					break;

				if (found && reader.Name == "m" && reader.Depth > depth) {
					key = reader.GetAttribute ("key");
					switch (key) {
					case "abiword.generator":
						reader.Read ();
						AddProperty (Beagle.Property.New ("fixme:appname", reader.Value ));
						break;

					case "dc.description":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:description", reader.Value ));
						break;

					case "abiword.keywords":
						reader.Read ();
						AddProperty (Beagle.Property.New ("fixme:keywords", reader.Value ));
						break;

					case "dc.relation":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:relation", reader.Value ));
						break;

					case "dc.rights":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:rights", reader.Value ));
						break;

					case "dc.source":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:source", reader.Value ));
						break;

					case "dc.contributor":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:contributor", reader.Value ));
						break;

					case "dc.subject":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:subject", reader.Value ));
						break;

					case "dc.creator":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:creator", reader.Value ));
						break;

					case "dc.coverage":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:coverage", reader.Value ));
						break;

					case "dc.type":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:type", reader.Value ));
						break;

					case "dc.language":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:language", reader.Value ));
						break;

					case "dc.title":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:title", reader.Value ));
						break;
						
					case "dc.publisher":
						reader.Read ();
						AddProperty (Beagle.Property.New ("dc:publisher", reader.Value ));
						break;
					}
				}
			}
		}

		private XmlTextReader BuildReader (string path)
		{
			Stream s;
			s = new FileStream (path,
					    FileMode.Open,
					    FileAccess.Read,
					    FileShare.Read);

			if (is_gzipped)
				s = new GZipInputStream (s);

			return new XmlTextReader (s);
		}

		XmlTextReader reader = null;
		override protected void DoOpen (FileInfo info)
		{
			// Try to open the file as if it is gzip.
			// If that fails, we conclude that it must
			// just be a regular text file full of xml.
			is_gzipped = true;
			try {
				Stream s;
				s = new FileStream (info.FullName, FileMode.Open, FileAccess.Read, FileShare.Read);
				Stream z;
				z = new GZipInputStream (s);
				z.ReadByte ();
				z.Close ();
				s.Close ();
			} catch (Exception) {
				is_gzipped = false;
			}

			hotStyles = new Hashtable ();
			reader = BuildReader (info.FullName);
		}

		override protected void DoPullProperties ()
		{
			XmlTextReader metaReader = BuildReader (FileInfo.FullName);
			try {
				ExtractMetadata (metaReader);
				metaReader.Close ();
			} catch (Exception e) {
				metaReader.Close ();
				Finished ();
				Logger.Log.Error ("Exception occurred while reading meta-data from {0}",
						 FileInfo.FullName);
				Logger.Log.Debug (e);
			}
		}

		override protected void DoPull ()
		{
			if (reader == null) {
				Finished ();
				return;
			}
			try {
				if (WalkContentNodes (reader)) {
					reader.Close ();
					Finished ();
				}
			} catch (Exception e) {
				reader.Close ();
				Finished ();
				Logger.Log.Error ("Exception occurred while reading contents from {0}",
						  FileInfo.FullName);
				Logger.Log.Debug (e);
			}
		}
	}
}
