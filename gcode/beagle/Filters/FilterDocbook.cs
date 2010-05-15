//
// FilterDocbook.cs
//
// Copyright (C) 2005 Novell, Inc.
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
using System.Collections;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters 
{
	public class FilterDocbook : Filter 
	{
		protected XmlTextReader reader;

		protected string base_path;
		protected string base_title;
		protected string base_language;

		protected Stack entries_stack = new Stack ();

		protected class DocbookEntry {
			public string Id = null;
			public string Title = null;
			public string Language = null;
			public int Depth = -1;
			public StringBuilder Content = new StringBuilder ();
		}

		//////////////////////////////////////////////////

		public FilterDocbook ()
		{
			SnippetMode = false;
			SetVersion (5);
			SetFileType ("documentation");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/docbook+xml"));
			AddSupportedFlavor (FilterFlavor.NewFromExtension (".docbook"));

			// Hack for detecting Docbook files in certain
			// well-known locations.  These roughly map to the
			// directories in crawl-rules/crawl-documentation
			//
			// xdgmime/shared-mime-info don't dive into XML files
			// to see what they really are, so the mime type is
			// just application/xml.  We know most of these are
			// docbook.
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/doc/*",              ".xml", null, 0));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/local/share/doc/*",        ".xml", null, 0));
			AddSupportedFlavor (new FilterFlavor ("file:///opt/kde3/share/doc/*",         ".xml", null, 0));
			AddSupportedFlavor (new FilterFlavor ("file:///opt/gnome/share/gnome/help/*", ".xml", null, 0));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/gnome/help/*",       ".xml", null, 0));
		}

		///////////////////////////////////////////////////

		override protected void DoOpen (FileInfo info)
		{
			if (info.Name == "legal.xml") {
				Error ();
				return;
			}

			base_path = info.FullName;		
			reader = new XmlTextReader (Stream);
			reader.XmlResolver = null;
		}

		override protected void DoPullProperties ()
		{
			Stopwatch watch = new Stopwatch ();
			
			watch.Start ();

			while (reader.Read ()) {
				switch (reader.NodeType) {
				case XmlNodeType.Element:
					if (reader.Name.StartsWith ("sect") || reader.Name.StartsWith ("chapter")) {
						string id = reader.GetAttribute ("id");

						if (id != null && id != String.Empty) {
							DocbookEntry entry = new DocbookEntry ();
							entry.Id = id;
							entry.Depth = reader.Depth;

							string language = reader.GetAttribute ("lang");
							
							if (language != null && language != String.Empty)
								entry.Language = language;

							entries_stack.Push (entry);
						}
					} else if (reader.Name == "article" || reader.Name == "book") {
						string language = reader.GetAttribute ("lang");

						if (language != null && language != String.Empty)
							base_language = language;
					} else if (reader.Name == "title") {
						reader.Read (); // Go to the text node

						if (entries_stack.Count == 0 && base_title == null) {
							// This is probably the book title
							base_title = reader.Value;
						} else if (entries_stack.Count > 0) {
							DocbookEntry entry = (DocbookEntry) entries_stack.Peek ();

							if (entry.Title == null)
								entry.Title = reader.Value;
						}
					} else if (reader.Name == "keyword") {
						reader.Read (); // read the text node
						AddProperty (Property.NewKeyword ("dc:subject", reader.Value));
					}
					break;
					
				case XmlNodeType.Text:
					// Append text to the child indexable
					if (entries_stack.Count > 0)
						((DocbookEntry) entries_stack.Peek ()).Content.Append (reader.Value);

					// Append text to the main indexable
					else
						AppendWord (reader.Value);
					break;
					
				case XmlNodeType.EndElement:
					if (entries_stack.Count > 0 &&
					    ((DocbookEntry) entries_stack.Peek ()).Depth == reader.Depth) {
						DocbookEntry entry, parent_entry = null;

						entry = (DocbookEntry) entries_stack.Pop ();
						
						if (entries_stack.Count > 0)
							parent_entry = (DocbookEntry) entries_stack.Peek ();
						
						Indexable indexable;
						indexable = new Indexable (UriFu.AddFragment (Indexable.Uri, entry.Id, false));
						indexable.HitType = "DocbookEntry";
						indexable.MimeType = "text/x-docbook-entry";
						indexable.AddProperty (Property.NewKeyword ("beagle:FileType", "documentation"));
						indexable.Filtering = IndexableFiltering.AlreadyFiltered;

						indexable.AddProperty (Property.NewUnsearched ("fixme:id", entry.Id));
						indexable.AddProperty (Property.New ("dc:title", entry.Title));

						// Add the docbook book title
						indexable.AddProperty (Property.NewUnsearched ("fixme:base_title", base_title));

						// Add the child language (or docbook language if none is specified)
						if (entry.Language != null)
							indexable.AddProperty (Property.NewUnsearched ("fixme:language", entry.Language));
						else if (base_language != null)
							indexable.AddProperty (Property.NewUnsearched ("fixme:language", base_language));
						
						// Add any parent (as in docbook parent entry, not beagle) data if we have it
						if (parent_entry != null) {
							indexable.AddProperty (Property.NewUnsearched ("fixme:parent_id", parent_entry.Id));
							indexable.AddProperty (Property.NewUnsearched ("fixme:parent_title", parent_entry.Title));
						}


						StringReader content_reader = new StringReader (entry.Content.ToString ());
						indexable.SetTextReader (content_reader);
						indexable.SetChildOf (this.Indexable);

						AddIndexable (indexable);
					}
					break;
				}
			}

			// Add the common properties to the top-level
			// file item such as Title, Language etc.

			AddProperty (Property.New ("dc:title", base_title));
			AddProperty (Property.NewUnsearched ("fixme:language", base_language));

			watch.Stop ();
			
			// If we've successfully crawled the file but haven't 
			// found any indexables, we shouldn't consider it
			// successfull at all (unless we have a title, which
			// means that it's actually a docbook file, just without
			// sections.
			if (! HasGeneratedIndexable && base_title == null) {
				Log.Error ("Probably not a docbook. Ignoring {0}!", base_path);
				Error ();
				return;
			}

			Logger.Log.Debug ("Parsed docbook file in {0}", watch);

			Finished ();
		}
	}
}
