//
// FilterOpenOffice.cs
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

using ICSharpCode.SharpZipLib.Zip;

namespace Beagle.Filters {
    
	public class FilterOpenOffice : Beagle.Daemon.Filter {

		private Hashtable hotStyles;

		private Hashtable attr_to_index = null;
		private Hashtable InterestingAttribute {
			get {
				if (attr_to_index != null)
					return attr_to_index;

				attr_to_index = new Hashtable (1 /* Number of attributes */);
				// Add interesting node-attribute pairs to this hashtable
				// attr_to_index [node_name] = attribute_name
				attr_to_index ["table:table"] = "table:name";

				return attr_to_index;
			}
		}

		public FilterOpenOffice () 
		{
			SnippetMode = true;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			// OO 1.0 mime types
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.writer"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.writer.template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.calc"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.calc.template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.impress"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.impress.template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.draw"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.sun.xml.draw.template"));

			// OO 2.0 mime types
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.text"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.text-template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.spreadsheet"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.spreadsheet-template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.presentation"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.presentation-template"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.graphics"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.oasis.opendocument.graphics-template"));
		}
		
		// Parse the "style" nodes and mark appropriate styles as *HOT*
		// FIXME: Identify and ADD more *HOT* styles. ;)
		void StudyStyleNode (XmlReader reader)
		{
			string style_name = reader.GetAttribute ("style:name");
			string style_parent = reader.GetAttribute ("style:parent-style-name");

			string weight = null;
			string underline = null;
			string italic = null;
			int original_depth = reader.Depth;

			if (!reader.IsEmptyElement) {
				reader.Read ();
				while (reader.Depth > original_depth) {
					if (reader.NodeType == XmlNodeType.Element
					    && (reader.Name == "style:properties" ||
						reader.Name == "style:text-properties")) {  /* ODT changes */
						weight = reader.GetAttribute ("fo:font-weight");
						italic = reader.GetAttribute ("fo:font-style");
						underline = reader.GetAttribute ("style:text-underline");
					}
					reader.Read ();
				}
			}

			if ((style_parent != null && style_parent.StartsWith("Heading"))
			    || (style_name != null && ((String.Compare (style_name, "Footnote") == 0)
						       || (String.Compare (style_name, "Endnote") == 0)
						       || (String.Compare (style_name, "Header") == 0)
						       || (String.Compare (style_name, "Footer") == 0)))
			    || (weight != null && weight == "bold")
			    || (italic != null && italic == "italic")
			    || (underline != null && underline != "none"))
				hotStyles[style_name] = true;
		}

		static bool NodeIsLink (String nodeName)
		{
			return nodeName == "text:a";
		}

		static bool NodeIsHot (String nodeName)
		{
			return nodeName == "text:h";
		}

		// These container tags allows multiple-lines of texts and 
		// all of them should be marked *HOT* and hence called Container ;-)
		static bool NodeIsHotContainer (String nodeName)
		{
			return nodeName == "office:annotation" ||
				nodeName == "text:footnote" ||
				nodeName == "text:endnote" ||
				nodeName == "text:note"; // "ODT format"
		}

		static bool NodeIsFreezing (String nodeName)
		{
			return nodeName == "text:footnote-citation"
				|| nodeName == "text:endnote-citation"
				|| nodeName == "text:note-citation";  // "ODT format"

		}

		static bool NodeBreaksTextBefore (String nodeName)
		{
			return nodeName == "text:footnote"
				|| nodeName == "text:endnote"
				|| nodeName == "office:annotation"
				|| nodeName == "table:table"
				|| nodeName == "text:note";       // "ODT format"
		}

		static bool NodeBreaksTextAfter (String nodeName)
		{
			return  nodeName == "text:line-break"
				|| nodeName == "text:s"
				|| nodeName == "text:tab-stop"
				|| nodeName == "table:table-cell";
		}

		static bool NodeBreaksStructureAfter (String nodeName)
		{
			return nodeName == "text:p"
				|| nodeName == "text:h"
				|| nodeName == "text:footnote"
				|| nodeName == "text:endnote"
				|| nodeName == "office:annotation"
				|| nodeName == "text:note";       // "ODT format"
		}

		private Stack hot_nodes = new Stack ();
		private string strPartText = String.Empty;
		private bool bPartHotStyle = false;
		private Stack hot_container_nodes = new Stack ();

		void AddTextForIndexing (string paramStr)
		{
			int sindex = 0;
			string strTemp;
			bool wasHot = false;
			int index = paramStr.LastIndexOf (' ');

			if (index > -1) {
				// During the previous-parsing, a word got terminatted partially,
				// find the remaining part of the word, concatenate it and add it to 
				// the respective pools and reset the HOT status, if required.
				if (strPartText.Length > 0) {
					sindex = paramStr.IndexOf (' ');
					strTemp = strPartText + paramStr.Substring (0, sindex);
					if (!IsHot) {
						if (bPartHotStyle)
							HotUp ();
					}
					else
						wasHot = true;

					AppendText (strTemp);
					if (!wasHot && bPartHotStyle)
						HotDown ();
					bPartHotStyle = false;
				}
				paramStr = paramStr.Substring (sindex);
				index = paramStr.LastIndexOf (' ');
				sindex = 0;
			}

			if (index > -1) {
				strPartText = paramStr.Substring (index);
				paramStr = paramStr.Substring (sindex, index);
			} else {
				strTemp = strPartText + paramStr;
				strPartText = strTemp;
				paramStr = String.Empty;
				strTemp = String.Empty;
			}
					
			// Enable *HOT* just before appending the text
			// because, there can be some *Partial Texts* without
			// *HOT* styles that needs to be appended.
			if (hot_nodes.Count > 0 && (bool) hot_nodes.Peek() == true) {
				if (!IsHot)
					HotUp ();
				bPartHotStyle = true;
			} else 
				bPartHotStyle |= false;

			AppendText (paramStr);

			if (strPartText.Trim().Length < 1)
				bPartHotStyle = false;
		}

		void IndexAttribute (string node_name, string attr_name, string attr_value)
		{
			if (attr_value == null || attr_value.Length == 0)
				return;

			if ((string)InterestingAttribute [node_name] == attr_name)
				AppendText (attr_value);	
		}

		void FlushPartialText () 
		{
			if (strPartText.Length > 0) {
				if (bPartHotStyle && !IsHot) 
					HotUp ();
				AppendText (strPartText);
				if (IsHot)
					HotDown ();
				strPartText = String.Empty;
			}
			bPartHotStyle = false;

		}

		bool WalkContentNodes (XmlReader reader)
		{
			// total number of elements to read per-pull
			const int total_elements = 10;
			int num_elements = 0;

			while (reader.Read ()) {
				switch (reader.NodeType) {
				case XmlNodeType.Element:
					if (reader.IsEmptyElement) {
						FlushPartialText ();
						if (NodeBreaksStructureAfter (reader.Name))
							AppendStructuralBreak ();
						else {
							if (NodeBreaksTextBefore (reader.Name))
								AppendWhiteSpace ();

							if (NodeBreaksTextAfter (reader.Name))
								AppendWhiteSpace ();
						}
						continue;
					}

					// FIXME: Allow adding more style nodes
					if (reader.Name == "style:style"
					    || reader.Name == "number:date-style"
					    || reader.Name == "style:font-decl") {
						StudyStyleNode (reader); 
						continue;
					}

					// A node is hot if:
					// (1) It's name is hot
					// (2) It is flagged with a hot style
					// (3) annotations are always hot.

					bool isHot = false;

					if (NodeIsHot (reader.Name)) {
						isHot = true;
					} else if (NodeIsHotContainer (reader.Name)) {
						hot_container_nodes.Push (reader.Name);
					} else if (NodeIsLink (reader.Name)) {
						string attr = reader.GetAttribute ("xlink:href");
						AppendText (attr);
						AppendWhiteSpace ();
						isHot = false;
					} else {
						string node_name = reader.Name;
						bool has_attr = reader.MoveToFirstAttribute ();
						while (has_attr) {
							if (reader.Name.EndsWith(":style-name")) {
								if (hotStyles.Contains (reader.Value))
									isHot = true;
								break;
							} else
								IndexAttribute (node_name, reader.Name, reader.Value);

							has_attr = reader.MoveToNextAttribute ();
						}
						reader.MoveToElement();
					} 
					
					hot_nodes.Push (isHot);

					// Call *HotUp* iff
					//   i) Its already in *HOT* mode and
					//  ii) there is a hot style/hot container tag
					if (!IsHot && (isHot || hot_container_nodes.Count > 0))
						HotUp ();
				
					if (NodeIsFreezing (reader.Name)) {
						FreezeUp ();
					}
				
					if (NodeBreaksTextBefore (reader.Name)) {
						FlushPartialText ();
						AppendWhiteSpace ();
					}
					break;

				case XmlNodeType.Text:
					string text = reader.Value;
					if (text.Length < 1)
						continue;
					if (!IsFrozen)
						AddTextForIndexing (text);
					break;

				case XmlNodeType.EndElement:
					if (NodeBreaksStructureAfter (reader.Name)) {
						FlushPartialText ();
						AppendStructuralBreak ();
					}
					else if (NodeBreaksTextAfter (reader.Name))
						AppendWhiteSpace ();

					bool is_hot = false;
					if (hot_nodes.Count > 0)
						is_hot = (bool) hot_nodes.Pop ();
					else
						Logger.Log.Debug ("FilterOpenOffice: hot_nodes underflow in {0}", 
								  reader.Name);
					if (hot_container_nodes.Count > 0) {
						string hot_container_tag = (string) hot_container_nodes.Peek ();
						if (hot_container_tag == reader.Name) {
							hot_container_nodes.Clear ();
							HotDown ();
						}
					}
					
					if (is_hot)
						HotDown ();

					if (NodeIsFreezing (reader.Name)) {
						FreezeDown ();
					}
					break;
				}
				num_elements++;
				if (num_elements >= total_elements) {
					return false;
				}
			} 
			return true;
		}

		// SlideCount is not stored in meta.xml rather we need to 
		// parse the whole of content.xml to find out the count of
		// slides present in an .sxi.
		private void ExtractSlideCount (XmlReader reader)
		{
			string slideCount = null;
			reader.Read ();
			do {
				reader.Read ();

				// Do not parse the whole file if it is not a
				// presentation (impress document)
				if (reader.Name == "office:document-content" 
				    && reader.NodeType == XmlNodeType.Element) {
					string docClass = reader.GetAttribute ("office:class");
					if (docClass != "presentation")
						return;
				}
			} while (reader.Depth < 2);
			
			while (reader.Depth >= 1) {
				if (reader.Depth != 2 || reader.NodeType != XmlNodeType.Element) {
					reader.Read ();
					continue;
				}
				switch (reader.Name) {
				case "draw:page":
					slideCount = reader.GetAttribute ("draw:id");
					break;
				}
				reader.Read ();
			}

			AddProperty (Beagle.Property.NewUnsearched ("fixme:slide-count", slideCount));
		}

		private void ExtractMetadata (XmlReader reader)
		{
			while (reader.Read () && reader.Depth < 2)
				;

			while (reader.Depth >= 2) {
				if (reader.Depth != 2 || reader.NodeType != XmlNodeType.Element) {
					reader.Read ();
					continue;
				}
				switch (reader.Name) {
				case "dc:title":
					reader.Read ();
					AddProperty (Beagle.Property.New ("dc:title",
									  reader.Value));
					break;

				case "dc:description":
					reader.Read ();
					
					AddProperty (Beagle.Property.New ("dc:description",
									  reader.Value));
					break;

				case "dc:subject":
					reader.Read ();

					AddProperty (Beagle.Property.New ("dc:subject",
									  reader.Value));
					break;
					
				case "meta:document-statistic":
					string attr = reader.GetAttribute ("meta:page-count");
					AddProperty (Beagle.Property.NewUnsearched ("fixme:page-count", attr));
					attr = reader.GetAttribute ("meta:word-count");
					AddProperty (Beagle.Property.NewUnsearched ("fixme:word-count", attr));

					// Both writer and calc uses this attribute.  writer stores the
					// count of tables in a sxw whereas calc stores the count of
					// spreadsheets in a sxc.
					attr = reader.GetAttribute ("meta:table-count");
					if (attr != null && Convert.ToInt32 (attr) > 0 
					    && MimeType == "application/vnd.sun.xml.calc")
						AddProperty (Beagle.Property.NewUnsearched ("fixme:spreadsheet-count", attr));
					break;

				case "meta:user-defined":
					string name = reader.GetAttribute ("meta:name");
					reader.Read ();

					AddProperty (Beagle.Property.New ("fixme:UserDefined-" + name,
										  reader.Value));
					break;

				case "meta:keyword":
					reader.Read ();
					AddProperty (Beagle.Property.New ("fixme:keywords",
									  reader.Value));
					break;
				}
				
				reader.Read ();
			}
		}

		ZipFile zip = null;

		override protected void DoOpen (FileInfo info)
		{
			hotStyles = new Hashtable ();
			try {
				zip = new ZipFile (info.FullName);

				//if (MimeType.StartsWith ("application/vnd.oasis.opendocument."))
				//	odtFormat = true;

			} catch (Exception) {
				Logger.Log.Error ("Unable to open {0}.  Probably an invalid OpenOffice document.", 
						  info.FullName);
				Finished ();
			}
		}

		override protected void DoPullProperties ()
		{
			if (zip != null) {
				ZipEntry entry = zip.GetEntry ("meta.xml");
				if (entry != null) {
					Stream meta_stream = zip.GetInputStream (entry);
					XmlReader reader = new XmlTextReader (meta_stream);
					ExtractMetadata (reader);
				} else {
					Logger.Log.Error ("No meta.xml!");
				}
				entry = zip.GetEntry ("content.xml");
				if (entry != null) {
					Stream contents_stream = zip.GetInputStream (entry);
					XmlReader reader = new XmlTextReader (contents_stream);
					ExtractSlideCount (reader);
				} else {
					Logger.Log.Error ("No content.xml!");
				}
			}
		}

		XmlReader content_reader = null;
		XmlReader style_reader = null;
		override protected void DoPull ()
		{
			if (zip != null) {
				// We need both styles.xml and content.xml as 
				// "Header", "Footer" are stored in styles.xml and
				// "[Foot/End]Notes are stored in content.xml
				if ((content_reader == null) && (style_reader == null)) {

					ZipEntry entry = zip.GetEntry ("content.xml");
					ZipEntry entry1 = zip.GetEntry ("styles.xml");

					if ((entry != null) && (entry1 != null)) {
						Stream content_stream = zip.GetInputStream (entry);
						Stream style_stream = zip.GetInputStream (entry1);
						content_reader = new XmlTextReader (content_stream);
						style_reader = new XmlTextReader (style_stream);
					}
				}			
			}	
			if ((content_reader == null) && (style_reader == null)) {
				Finished ();
				return;
			}

			// Note: Do not change the order.
			// we need to populate our hotStyles table with all posible hot styles.
			// Since, "footnotes" and "endnotes" gets stored in content.xml and these
			// styles needs to be marked as *HOT*, they need to be processed before contents.
			if ((WalkContentNodes (style_reader)) && (WalkContentNodes (content_reader)))
				Finished ();
		}

		override protected void DoClose ()
		{
			if (zip != null)
				zip.Close ();
		}
	}
}
