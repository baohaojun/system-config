//
// FilterHtml.cs
//
// Copyright (C) 2005 Debajyoti Bera <dbera.web@gmail.com>
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
using SW=System.Web;

using Beagle.Daemon;
using Beagle.Util;

using HtmlAgilityPack;

namespace Beagle.Filters {

	public class FilterHtml : Beagle.Daemon.Filter {

		// When see <b> push "b" in the stack
		// When see </b> pop from the stack
		// For good error checking, we should compare
		// current element with what was popped
		// Currently, we just count the start and end tags using HotUp and HotDown
		// This allows unmatched elements to pass through

		// Do similar thing for ignore elements
		private int ignore_level;

		private bool building_text;
		private StringBuilder builder;
		protected Encoding enc;
		private HtmlDocument doc;

		// delegate types
		public delegate bool AppendTextCallback (string s);
		public delegate void AddPropertyCallback (Beagle.Property p);
		public delegate bool AppendSpaceCallback ();
		public delegate void HotCallback ();
		public delegate void AddLinkCallback (string s);

		// delegates
		private new AppendTextCallback AppendText;
		private new AppendTextCallback AppendWord;
		private new AddPropertyCallback AddProperty;
		private new AppendSpaceCallback AppendWhiteSpace;
		private new AppendSpaceCallback AppendStructuralBreak;
		private new HotCallback HotUp;
		private new HotCallback HotDown;
		private new AddLinkCallback AddLink;

		// 1: Add meta keyword fields as meta:key
		private int version = 1;

		public FilterHtml (bool register_filter)
		{
			if (register_filter) {
				base.SetVersion (version);
				SnippetMode = true;
				SetFileType ("document");

				AppendText = new AppendTextCallback (base.AppendText);
				AppendWord = new AppendTextCallback (base.AppendWord);
				AddProperty = new AddPropertyCallback (base.AddProperty);
				AppendWhiteSpace = new AppendSpaceCallback (base.AppendWhiteSpace);
				AppendStructuralBreak = new AppendSpaceCallback (base.AppendStructuralBreak);
				HotUp = new HotCallback (base.HotUp);
				HotDown = new HotCallback (base.HotDown);
#if ENABLE_RDF_ADAPTER
				AddLink = new AddLinkCallback (base.AddLink);
#endif
			}

			ignore_level = 0;
			building_text = false;
			builder = new StringBuilder ();
		}

		public FilterHtml () : this (true) {}

		protected new void SetVersion (int version)
		{
			this.version += version;
			base.SetVersion (this.version);
		}

#if ENABLE_RDF_ADAPTER
		public void SetAddLinkHandler (AddLinkCallback link_handler)
		{
			if (link_handler != null)
				AddLink = link_handler;
		}
#endif

		protected bool NodeIsHot (String nodeName) 
		{
			return nodeName == "b"
				|| nodeName == "u"
				|| nodeName == "em"
				|| nodeName == "strong"
				|| nodeName == "big"
				|| nodeName == "h1"
				|| nodeName == "h2"
				|| nodeName == "h3"
				|| nodeName == "h4"
				|| nodeName == "h5"
				|| nodeName == "h6"
				|| nodeName == "i"
				|| nodeName == "th";
		}

		protected static bool NodeBreaksText (String nodeName) 
		{
			return nodeName == "td"
				|| nodeName == "a"
				|| nodeName == "div"
				|| nodeName == "option";
		}

		protected static bool NodeBreaksStructure (string nodeName)
		{
			return nodeName == "p"
				|| nodeName == "br"
				|| nodeName == "h1"
				|| nodeName == "h2"
				|| nodeName == "h3"
				|| nodeName == "h4"
				|| nodeName == "h5"
				|| nodeName == "h6";
		}
		
		protected static bool NodeIsContentFree (String nodeName) 
		{
			return nodeName == "script"
				|| nodeName == "map"
				|| nodeName == "style";
		}

		protected static bool NodeIsNonBody (string nodeName)
		{
			return nodeName == "html"
				|| nodeName == "head"
				|| nodeName == "meta"
				|| nodeName == "style"
				|| nodeName == "title"
				|| nodeName == "link"
				|| nodeName == "script";
		}

		protected static bool NodeIsInBody (string nodeName)
		{
			return (nodeName == "script" || (! NodeIsNonBody (nodeName)));
		}

		protected void HandleTitleNode (HtmlNode node)
		{
			if (node.StartTag) {
				builder.Length = 0;
				building_text = true;
			} else {
				String title = HtmlEntity.DeEntitize (builder.ToString ());
				AddProperty (Beagle.Property.New ("dc:title", title));
				builder.Length = 0;
				building_text = false;
			}
		}

		protected void HandleMetaNode (HtmlNode node)
		{
	   		string name = node.GetAttributeValue ("name", String.Empty);
           		string content = node.GetAttributeValue ("content", String.Empty);
			if (name != String.Empty)
				AddProperty (Beagle.Property.New ("meta:" + name.ToLower (), content));
		}

		protected void HandleContentFreeNode (HtmlNode node)
		{
			// so node is a content-free node
			// ignore contents of such node
			if (node.StartTag)
				ignore_level ++;
			else
				ignore_level = (ignore_level > 1 ? ignore_level - 1 : 0);
		}

		protected bool HandleContentRichNode (HtmlNode node)
		{
			bool isHot = NodeIsHot (node.Name);
			bool breaksText = NodeBreaksText (node.Name);
			bool breaksStructure = NodeBreaksStructure (node.Name);

			bool ret = true;

			if (breaksText)
				ret = AppendWhiteSpace ();

			if (node.StartTag) {
				if (isHot) {
					HotUp ();
				}
				if (node.Name == "img") {
					string attr = node.GetAttributeValue ("alt", String.Empty);
					if (attr != String.Empty) {
						string s = HtmlEntity.DeEntitize (attr);
						AppendWord (s);
						ret = AppendWhiteSpace ();
					}
				} else if (node.Name == "a") {
					string attr = node.GetAttributeValue ("href", String.Empty);
					if (attr != String.Empty) {
						string s = HtmlEntity.DeEntitize (
							    SW.HttpUtility.UrlDecode (attr, enc));
						AppendWord (s);
#if ENABLE_RDF_ADAPTER
						// Add valid and global URLs to special field "Link"
						if (s.StartsWith ("http://") || s.StartsWith ("mailto:") || s.StartsWith ("ftp://"))
							AddLink (s);
#endif
						ret = AppendWhiteSpace ();
					}
				} else if (node.Name == "br") // both <br> and </br> are used - special case
					ret = AppendStructuralBreak ();
			} else { // (! node.StartTag)
				if (isHot) {
					HotDown ();
				}	
				if (breaksStructure)
					ret = AppendStructuralBreak ();
			}

			if (breaksText)
				ret = AppendWhiteSpace ();

			return ret;
		}

		protected bool HandleTextNode (HtmlNode node)
		{
			bool ret = true;
			String text = ((HtmlTextNode)node).Text;

			if (ignore_level != 0)
				return true; // still ignoring ...
			if (building_text)
				builder.Append (text);
			else {
				string s = HtmlEntity.DeEntitize (text);
				ret = AppendText (s);
			}

			//if (hot_stack.Count != 0)
			//Console.WriteLine (" TEXT:" + text + " ignore=" + ignore_level);
			return ret;
		}

		protected bool HandleNodeEventHead (HtmlNode node)
		{
			//Log.Debug ("HandleNodeEventHead (<{0}{1}>)", (node.StartTag ? "" : "/"), node.Name);
			switch (node.NodeType) {
				
			case HtmlNodeType.Document:
			case HtmlNodeType.Element:
				if (! NodeIsNonBody (node.Name)) {
					doc.PauseLoad ();
				} else if (node.Name == "title") {
					HandleTitleNode (node);
				} else if (node.Name == "meta") {
					HandleMetaNode (node);
				} else if (NodeIsContentFree (node.Name)) {
					HandleContentFreeNode (node);
				}
				break;
				
			case HtmlNodeType.Text:
				HandleTextNode (node);
				break;
			}

			return true;
		}

		protected bool HandleNodeEventBody (HtmlNode node)
		{
			//Log.Debug ("HandleNodeEventBody (<{0}{1}>)", (node.StartTag ? "" : "/"), node.Name);
			bool pull = true;
			switch (node.NodeType) {
				
			case HtmlNodeType.Document:
			case HtmlNodeType.Element:
				if (! NodeIsInBody (node.Name)) {
					break;
				} else if (! NodeIsContentFree (node.Name)) {
					pull = HandleContentRichNode (node);
				} else {
					HandleContentFreeNode (node);
				}
				break;
				
			case HtmlNodeType.Text:
				pull = HandleTextNode (node);
				break;
			}

			if (! pull)
				doc.PauseLoad ();

			return true;
		}

		// Combined handler when you do not need separate head and body events
		// No idea of pause and resume implemented here
		protected bool HandleNodeEvent (HtmlNode node)
		{
			switch (node.NodeType) {
				
			case HtmlNodeType.Document:
			case HtmlNodeType.Element:
				if (node.Name == "title") {
					HandleTitleNode (node);
				} else if (node.Name == "meta") {
					HandleMetaNode (node);
				} else if (! NodeIsContentFree (node.Name)) {
					HandleContentRichNode (node);
				} else {
					HandleContentFreeNode (node);
				}
				break;
				
			case HtmlNodeType.Text:
				HandleTextNode (node);
				break;
			}

			return true;
		}

		override protected void DoPullProperties ()
		{
			enc = null;

			try {
				foreach (Property prop in Indexable.Properties) {
					if (prop.Key != StringFu.UnindexedNamespace + "encoding")
						continue;

					enc = Encoding.GetEncoding ((string) prop.Value);
					break;
				}

				if (enc == null) {
					// we need to tell the parser to detect encoding,
					HtmlDocument temp_doc = new HtmlDocument ();
					enc = temp_doc.DetectEncoding (Stream);
					temp_doc = null;
					Stream.Seek (0, SeekOrigin.Begin);
				}
			} catch (NotSupportedException) {
				// Encoding passed in isn't supported
			}

			// Default
			if (enc == null)
				enc = Encoding.ASCII;

			doc = new HtmlDocument ();
			doc.ReportNode += HandleNodeEventHead;
			doc.StreamMode = true;
			// we already determined encoding
			doc.OptionReadEncoding = false;
	
			try {
				if (enc == null)
					doc.Load (Stream);
				else
					doc.Load (Stream, enc);
			} catch (NotSupportedException) {
				enc = Encoding.ASCII;
				doc.Load (Stream, enc);
			} catch (Exception e) {
				Log.Debug (e, "Exception while filtering HTML file " +FileInfo.FullName);
			}
		}

		override protected void DoPullSetup ()
		{
			doc.ReportNode -= HandleNodeEventHead;
			doc.ReportNode += HandleNodeEventBody;
		}

		override protected void DoPull ()
		{
			doc.ResumeLoad ();

			if (doc.DoneParsing)
				Finished ();
		}

		public void ExtractText (string html_string,
					 AppendTextCallback append_text_cb,
					 AddPropertyCallback add_prop_cb,
					 AppendSpaceCallback append_white_cb,
					 AppendSpaceCallback append_break_cb,
					 HotCallback hot_up_cb,
					 HotCallback hot_down_cb)
		{
			AppendText = append_text_cb;
			AppendWord = append_text_cb;
			AddProperty = add_prop_cb;
			AppendWhiteSpace = append_white_cb;
			AppendStructuralBreak = append_break_cb;
			HotUp = hot_up_cb;
			HotDown = hot_down_cb;

			HtmlDocument doc = new HtmlDocument ();
			doc.ReportNode += HandleNodeEvent;

			doc.StreamMode = true;
	
			try {
				doc.LoadHtml (html_string);
			} catch (Exception e) {
				Log.Debug (e, "Exception while filtering html string [{0}]", html_string);
			}

		}

		override protected void RegisterSupportedTypes () 
		{
		}

		public static TextReader GetHtmlReader (Stream stream, string charset)
		{
			return GetHtmlReader (stream, charset, null);
		}

		public static TextReader GetHtmlReader (Stream stream, string charset, AddLinkCallback link_handler)
		{
			if (stream == null)
				throw new ArgumentNullException ("stream");

			FilterHtml html_filter = new FilterHtml ();
			html_filter.SnippetMode = false;
#if ENABLE_RDF_ADAPTER
			html_filter.SetAddLinkHandler (link_handler);
#endif

			html_filter.Indexable = new Indexable (); // fake an indexable
			html_filter.AddProperty (Property.NewUnsearched (StringFu.              UnindexedNamespace + "encoding", charset));

			if (! html_filter.Open (stream, false))
				throw new Exception ("Cannot open html");

			TextReader pr = html_filter.GetTextReader ();
			return pr;
		}
	}
}
