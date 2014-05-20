//
// QueryPart.cs
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
using System.Collections;
using System.Text;
using System.Xml.Serialization;

using Beagrep.Util;

namespace Beagrep {

        public enum QueryPartLogic {
                Required = 1,
                Prohibited = 2
        }

        [XmlInclude (typeof (QueryPart_Text)),
         XmlInclude (typeof (QueryPart_Property)),
         XmlInclude (typeof (QueryPart_DateRange)),
         XmlInclude (typeof (QueryPart_Human)),
         XmlInclude (typeof (QueryPart_Wildcard)),
         XmlInclude (typeof (QueryPart_Or)),
         XmlInclude (typeof (QueryPart_Uri))]
        abstract public class QueryPart {

                private QueryPartLogic logic = QueryPartLogic.Required;

                public QueryPart ()
                { }

                public QueryPartLogic Logic {
                        get { return logic; }
                        set { logic = value; }
                }

                public override string ToString ()
                {
                        return String.Format ("{0}:\n  Logic: {1}\n", this.GetType (), Logic);
                }
        }

        public class QueryPart_Text : QueryPart {

                private string text;
                private bool search_full_text = true;
                private bool search_properties = true;

                public QueryPart_Text ()
                { }

                public string Text {
                        get { return text; }
                        set { text = value; }
                }

                public bool SearchFullText {
                        get { return search_full_text; }
                        set { search_full_text = value; }
                }

                public bool SearchTextProperties {
                        get { return search_properties; }
                        set { search_properties = value; }
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  Text: {0}\n" +
                                "  Search full text: {1}\n" +
                                "  Search text properties: {2}",
                                Text, SearchFullText, SearchTextProperties);
                }
        }

        // AllProperties queries are not allowed on keywords.
        public class QueryPart_Property : QueryPart {

                public const string AllProperties = "_all";

                private PropertyType type;
                private string key;
                private string value;

                public QueryPart_Property ()
                { }

                public PropertyType Type {
                        get { return type; }
                        set { type = value; }
                }

                public string Key {
                        get { return key; }
                        set { key = value; }
                }

                public string Value {
                        get { return value; }
                        set { this.value = value; } // ugh
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  Type: {0}\n" +
                                "  Key: {1}\n" +
                                "  Value: {2}",
                                Type, Key, Value);
                }
        }

        public class QueryPart_DateRange : QueryPart {

                public const string AllPropertiesKey = "_all";
                public const string TimestampKey = "Timestamp";

                private string key = AllPropertiesKey;
                private DateTime start_date;
                private DateTime end_date;

                public QueryPart_DateRange ()
                { }

                public string Key {
                        get { return key; }
                        set { key = value; }
                }

                [XmlIgnore]
                public DateTime StartDate {
                        get { return start_date; }
                        set { start_date = value; }
                }

                [XmlElement ("StartDate")]
                public string StartDateAsString {
                        get { return StringFu.DateTimeToString (start_date); }
                        set { start_date = StringFu.StringToDateTime (value); }
                }

                [XmlIgnore]
                public DateTime EndDate {
                        get { return end_date; }
                        set { end_date = value; }
                }

                [XmlElement ("EndDate")]
                public string EndDateAsLocal {
                        get { return StringFu.DateTimeToString (end_date); }
                        set { end_date = StringFu.StringToDateTime (value); }
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  Key: {0}\n" +
                                "  Start date: {1}\n" +
                                "  End date: {2}",
                                Key, StartDate, EndDate);
                }
        }

        public class QueryPart_Human : QueryPart {

                private string query_string;

                public QueryPart_Human ()
                { }

                public string QueryString {
                        get { return query_string; }
                        set { query_string = value; }
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  QueryString: {0}",
                                QueryString);
                }
        }

        public class QueryPart_Wildcard : QueryPart {

                private string query_string;
                private bool property_only;

                public QueryPart_Wildcard ()
                    { property_only = false;}

                public string QueryString {
                        get { return query_string; }
                        set { query_string = value; }
                }

                public bool PropertyOnly {
                    get { return property_only; }
                    set { property_only = value; }
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  QueryString: {0}",
                                QueryString);
                }
        }

        public class QueryPart_Or : QueryPart {

                private ArrayList sub_parts = new ArrayList ();

                public QueryPart_Or ()
                { }

                [XmlArray ("SubParts")]
                [XmlArrayItem (ElementName="Part", Type=typeof (QueryPart))]
                public ArrayList SubParts_ShouldBePrivateSoPleaseDontUseThis {
                        get { return sub_parts; }
                }

                [XmlIgnore]
                public ICollection SubParts {
                        get { return sub_parts; }
                }

                // FIXME: Really the only thing that is allowed as a subpart
                // of an 'Or' part are required (not prohibited) text or
                // property queries.  We should be clearer about the rules,
                // and enforce them.
                public void Add (QueryPart part)
                {
                        sub_parts.Add (part);
                }

                public override string ToString ()
                {
                        StringBuilder sb = new StringBuilder (base.ToString ());

                        foreach (QueryPart p in sub_parts)
                                sb.Append ("  " + p.ToString () + "\n");

                        return sb.ToString ();
                }
        }

        /* Get all indexed data about some uri. */
        public class QueryPart_Uri : QueryPart {

                private Uri uri;

                public QueryPart_Uri ()
                { }

                [XmlIgnore]
                public Uri Uri {
                        get { return uri; }
                        set { uri = value; }
                }

                [XmlElement ("Uri")]
                public string UriString {
                        get { return UriFu.UriToEscapedString (uri); }
                        set { uri = UriFu.EscapedStringToUri (value); }
                }

                public override string ToString ()
                {
                        return String.Format (
                                base.ToString () +
                                "  Uri: {0}",
                                Uri);
                }
        }

}
