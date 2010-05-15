//
// FilterKOrganizer.cs
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2007 Stephan Binner <binner@kde.org>
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

	public class FilterKOrganizer : Beagle.Filters.FilterKCal {

		public FilterKOrganizer ()
		{
			SnippetMode = true;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType (ICalParser.KOrganizerEventMimeType));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType (ICalParser.KOrganizerTodoMimeType));
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
			vCard_property_mapping ["CREATED"] = new KCalProperty (KCalType.Special);
			vCard_property_mapping ["DUE"] = new KCalProperty (KCalType.Special);
			vCard_property_mapping ["DTSTART"] = new KCalProperty (KCalType.Special);
			vCard_property_mapping ["DTEND"] = new KCalProperty (KCalType.Special);
			vCard_property_mapping ["UID"] = new KCalProperty ("fixme:uid", false, true, KCalType.Text);
			vCard_property_mapping ["LOCATION"] = new KCalProperty ("fixme:location", false, true, KCalType.Text);
			vCard_property_mapping ["PERCENT-COMPLETE"] = new KCalProperty ("fixme:percent-complete", false, true, KCalType.Text);
		}

		private string description = null;

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
			}
			else if (prop_name == "LAST-MODIFIED") {
				DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
					Indexable.Timestamp = dt;
			} else if (prop_name == "CREATED") {
				DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
                                        AddProperty (Beagle.Property.NewDate ("fixme:created", dt));
			} else if (prop_name == "DUE") {
                                string property = "fixme:duetime";
                                if (prop_value.Length == 8) {
                                        // property was DUE;VALUE=DATE
                                        prop_value = prop_value + "T000000Z";
                                        property = "fixme:duedate";
                                }
		                DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
                                        AddProperty (Beagle.Property.NewDate (property, dt));
			} else if (prop_name == "DTSTART") {
                                string property = "fixme:starttime";
                                if (prop_value.Length == 8) {
                                        // property was DTSTART;VALUE=DATE
                                        prop_value = prop_value + "T000000Z";
                                        property = "fixme:startdate";
                                }
		                DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
                                        AddProperty (Beagle.Property.NewDate (property, dt));
			} else if (prop_name == "DTEND") {
                                string property = "fixme:endtime";
                                if (prop_value.Length == 8) {
                                        // property was DTEND;VALUE=DATE
                                        prop_value = prop_value + "T000000Z";
                                        property = "fixme:enddate";
                                }
		                DateTime dt = ProcessKCalDate (prop_value);
				if (dt != DateTime.MinValue)
                                        AddProperty (Beagle.Property.NewDate (property, dt));
                        }
		}

		override protected void DoOpen (FileInfo fi)
		{
			if (vCard_property_mapping == null)
				SetupPropertyMapping ();
		}

		override protected void DoPull ()
		{
			AppendText (description);

			Finished ();
		}

	}

}
