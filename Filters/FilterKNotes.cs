//
// FilterKnotes.cs
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
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

using Beagle.Daemon;
using Beagle.Util;
using HtmlAgilityPack;

namespace Beagle.Filters {

	public class FilterKnotes : Beagle.Filters.FilterKCal {

		public FilterKnotes ()
		{
			SnippetMode = true;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType (ICalParser.KnotesMimeType));
		}

		private static Hashtable vCard_property_mapping = null;
		override protected Hashtable KCalPropertyMapping {
			get { return vCard_property_mapping; }
		}

		private void SetupPropertyMapping () {
			vCard_property_mapping = new Hashtable ();
			// KCalProperty (name, comma_sep, keyword, text_or_date)
			vCard_property_mapping ["SUMMARY"] = new KCalProperty ("dc:title", false, false, KCalType.Text);
			vCard_property_mapping ["PRIORITY"] = new KCalProperty ("fixme:priority", true, false, KCalType.Text);
			vCard_property_mapping ["DESCRIPTION"] = new KCalProperty (KCalType.Special);
			vCard_property_mapping ["CLASS"] = new KCalProperty ("fixme:class", false, true, KCalType.Text);
			vCard_property_mapping ["LAST-MODIFIED"] = new KCalProperty (KCalType.Special);
			// Open KNotes notes by
			// dcop knotes KNotesIface text <UID>
			vCard_property_mapping ["UID"] = new KCalProperty ("fixme:uid", false, true, KCalType.Text);
			vCard_property_mapping ["X-KDE-KNotes-RichText"] = new KCalProperty (KCalType.Special);
		}

		private string description = null;
		private bool is_rich_text = false;

		override protected void ProcessPropertySpecial (string prop_name,
							      ArrayList paramlist,
							      string prop_value)
		{
			//Log.Debug ("Handling special property {0}=[{1}]", prop_name, prop_value);
			if (prop_name == "DESCRIPTION") {
				prop_value = prop_value.Replace ("\\,", ",");
				prop_value = prop_value.Replace ("\\\\", "\\");
				prop_value = prop_value.Replace ("\\n", "\n");

				description = prop_value;
			} else if (prop_name == "X-KDE-KNotes-RichText")
				is_rich_text = (prop_value == "true");
			else if (prop_name == "LAST-MODIFIED") {
				DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
					Indexable.Timestamp = dt;
			}
		}

		override protected void DoOpen (FileInfo fi)
		{
			if (vCard_property_mapping == null)
				SetupPropertyMapping ();
		}

		override protected void DoPull ()
		{
			if (! is_rich_text)
				AppendText (description);
			else {
				FilterHtml.AppendTextCallback append_text_cb = new FilterHtml.AppendTextCallback (AppendText);
				FilterHtml.AddPropertyCallback add_prop_cb = new FilterHtml.AddPropertyCallback (delegate(Beagle.Property p) {});
				FilterHtml.AppendSpaceCallback append_white_cb = new FilterHtml.AppendSpaceCallback (AppendWhiteSpace);
				FilterHtml.AppendSpaceCallback append_break_cb = new FilterHtml.AppendSpaceCallback (AppendStructuralBreak);
				FilterHtml.HotCallback hot_up_cb = new FilterHtml.HotCallback (HotUp);
				FilterHtml.HotCallback hot_down_cb = new FilterHtml.HotCallback (HotDown);

				FilterHtml html_filter = new FilterHtml (false);
				html_filter.ExtractText (description,
							 append_text_cb,
							 add_prop_cb,
							 append_white_cb,
							 append_break_cb,
							 hot_up_cb,
							 hot_down_cb);
			}

			Finished ();
		}

	}

}
