//
// QueryStringParser.cs
//
// Copyright (C) 2004-2005 Novell, Inc.
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
using System.Text.RegularExpressions;

using Beagrep.Util;

namespace Beagrep.Daemon {

        public static class QueryStringParser {

                private static Regex query_string_regex;
                private static Regex extension_re;

                static QueryStringParser () {
                        /* This is our regular Expression Pattern:
                         * we expect something like this:
                         * -key:"Value String"
                         * key:Value
                         * or
                         * Value
                                ([+-]?)         # Required or Prohibited (optional)
                                (\w+:)?         # Key  (optional)
                                (               # Query Text
                                 (\"([^\"]*)\"?)#  quoted
                                 |              #  or
                                 (([^\s\"]+)(\"([^\"]*)\"?)?) # unquoted followed by optional quoted
                                )
                                ";
                         */
                        string Pattern = "(?<pm>[+-]?) ( (?<key>\\w+) :)? ( (\"(?<quote>[^\"]*)\"?) | ((?<midquote1>[^\\s\"]+) (\"(?<midquote2>[^\"]*)\"?)?) )";

                        query_string_regex = new Regex (Pattern, RegexOptions.IgnorePatternWhitespace | RegexOptions.Compiled);

                        extension_re = new Regex (@"^\**\.\w*$", RegexOptions.Compiled);
                }

                // Returns an ICollection of QueryPart objects.
                static public ICollection Parse (string query_string)
                {

                        Match m = query_string_regex.Match (query_string);

                        ArrayList parts;
                        parts = new ArrayList ();

                        ArrayList or_list = null;

                        while (m.Success) {

                                QueryPart query_part = MatchToQueryPart (m);

                                if (or_list != null) {
                                        or_list.Add (query_part);
                                        query_part = null;
                                }

                                Match next = m.NextMatch ();

                                // Trap the OR operator
                                // If the next match is an or, start an or_list
                                // (if we don't have one already) and skip
                                // ahead to the next part.
                                if ( next.Success
                                    && (next.Groups ["key"].ToString () == "")
                                    && (next.Groups ["midquote1"].ToString ().ToUpper () == "OR") ) {

                                        if (or_list == null) {
                                                or_list = new ArrayList ();
                                                or_list.Add (query_part);
                                        }

                                        m = next.NextMatch();
                                        continue;
                                }

                                // If we have a non-empty or-list going,
                                // Create the appropriate QueryPart and add it
                                // to the list.
                                if (or_list != null) {

                                        QueryPart_Or or_part = new QueryPart_Or ();
                                        or_part.Logic = QueryPartLogic.Required;

                                        foreach (QueryPart sub_part in or_list)
                                                or_part.Add (sub_part);

                                        parts.Add (or_part);
                                        or_list = null;
                                }

                                // Add the next text part
                                if (query_part != null)
                                        parts.Add (query_part);

                                m=next;
                        }

                        // If we ended with an or_parts list, do the right thing.
                        if (or_list != null) {

                                QueryPart_Or or_part = new QueryPart_Or ();
                                or_part.Logic = QueryPartLogic.Required;

                                foreach (QueryPart sub_part in or_list)
                                        or_part.Add (sub_part);
                        }

                        return parts;
                }

                static private QueryPart StringToQueryPart (string text, bool is_prohibited)
                {
                        QueryPart part;

                        if (text.IndexOf ('*') != -1) {
                                part = new QueryPart_Wildcard ();
                                ((QueryPart_Wildcard) part).QueryString = text;
                        } else {
                                part = new QueryPart_Text ();
                                ((QueryPart_Text) part).Text = text;
                        }

                        part.Logic = (is_prohibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);
                        return part;
                }

                static private QueryPart MatchToQueryPart (Match m)
                {
                        // Looping over all Matches we have got:
                        // m.Groups["pm"]       plus or minus sign
                        // m.Groups["key"]      keyname
                        // m.Groups["quote"]    quoted string
                        // m.Groups["midquote1"] + m.Groups["midquote2"] quoted midway string also represents unquoted string

                        string query = m.ToString ();
                        // Either quote is set or midquote1 and (optionally) midquote2 is set
                        string text = m.Groups ["quote"].ToString () + m.Groups ["midquote1"].ToString () + m.Groups ["midquote2"].ToString ();
                        string key = m.Groups ["key"].ToString ();

                        bool IsProhibited = (m.Groups ["pm"].ToString () == "-");


                        // check for file extensions
                        // if match starts with *. or . and only contains letters we assume it's a file extension
                        if (extension_re.Match (text).Success || key.ToLower () == "ext" || key.ToLower () == "extension") {

                                QueryPart_Property query_part = new QueryPart_Property ();

                                query_part.Key = Property.FilenameExtensionPropKey;

                                if (text.StartsWith ("*."))
                                        query_part.Value = text.Substring (1).ToLower ();
                                else if (text.StartsWith ("."))
                                        query_part.Value = text.ToLower ();
                                else
                                        query_part.Value = "." + text.ToLower ();

                                query_part.Type = PropertyType.Keyword;
                                query_part.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);

                                Logger.Log.Debug ("Extension query: {0}", query_part.Value);

                                return query_part;
                        }

                        if (key == String.Empty) {

                                Logger.Log.Debug ("Parsed query '{0}' as text_query", text);

                                return StringToQueryPart (text, IsProhibited);
                        }

                        // FIXME: i18n-izing "date"
                        if (key == "date") {
                                try {
                                        QueryPart part = DateQueryToQueryPart (text);
                                        part.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);
                                        return part;
                                } catch (FormatException) {
                                        Log.Warn ("Could not parse [{0}] as date query. Assuming text.", text);
                                        return StringToQueryPart (text, IsProhibited);
                                }
                        }

                        // FIXME: i18n-izing "uri"
                        if (key == "uri") {
                                try {
                                        QueryPart_Uri part = new QueryPart_Uri ();
                                        part.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);
                                        part.Uri = UriFu.UserUritoEscapedUri (text);
                                        return part;
                                } catch (System.UriFormatException) {
                                        Log.Warn ("Could not parse [{0}] as uri query. Assuming text.", text);
                                        return StringToQueryPart (text, IsProhibited);
                                }
                        }

                        // Special case
                        if (key == "inuri") {
                                QueryPart_Property inuri_part = new QueryPart_Property ();
                                inuri_part.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);
                                inuri_part.Key = "inuri";
                                inuri_part.Value = text;
                                inuri_part.Type = PropertyType.Keyword;
                                Log.Debug ("Handing special query 'inuri:{0}'", text);
                                return inuri_part;
                        }

                        // Non-keyword queries by directly using property names
                        // Query of form property:namespace:name=value
                        // which is translated to a non-keyword query
                        // namespace:name=value
                        int pos;
                        if (key == "property" && ((pos = text.IndexOf ('=')) != -1)) {
                                QueryPart_Property part = new QueryPart_Property ();
                                part.Key = text.Substring (0, pos);
                                part.Value = text.Substring (pos + 1);
                                part.Type = PropertyType.Text;
                                part.Logic = (IsProhibited ?      QueryPartLogic.Prohibited : QueryPartLogic.Required);
                                Logger.Log.Debug ("Parsed query '"          + query +
                                                  "' as prop query:key="    + part.Key +
                                                  ", value="                + part.Value +
                                                  " and property type="     + part.Type);

                                return part;
                        }

                        // keyword queries by directly using property names
                        // Query of form keyword:namespace:name=value
                        // which is translated to a keyword query
                        // namespace:name=value
                        if (key == "keyword" && ((pos = text.IndexOf ('=')) != -1)) {
                                QueryPart_Property part = new QueryPart_Property ();
                                part.Key = text.Substring (0, pos);
                                part.Value = text.Substring (pos + 1);
                                part.Type = PropertyType.Keyword;
                                part.Logic = (IsProhibited ?      QueryPartLogic.Prohibited : QueryPartLogic.Required);
                                Logger.Log.Debug ("Parsed query '"          + query +
                                                  "' as prop query:key="    + part.Key +
                                                  ", value="                + part.Value +
                                                  " and property type="     + part.Type);

                                return part;
                        }

                        if ((pos = text.IndexOf('*')) >= 0) {
                            QueryPart_Wildcard wild = new QueryPart_Wildcard();
                            wild.QueryString = text;
                            wild.PropertyOnly = true;
                            return wild;
                        }

                        string[] prop_string = null;
                        bool is_present;
                        PropertyType[] prop_type;
                        int num;

                        is_present = PropertyKeywordFu.GetMapping (key, out num, out prop_string, out prop_type);
                        // if key is not present in the mapping, assume the query is a text query
                        // i.e. if token is foo:bar and there is no mappable property named foo,
                        // assume "foo:bar" as text query
                        // FIXME the analyzer changes the text query "foo:bar" to "foo bar"
                        // which might not be the right thing to do

                        if (!is_present) {

                                Logger.Log.Warn ("Could not find property, parsed query '{0}' as text_query", query);

                                return StringToQueryPart (query, IsProhibited);
                        }

                        if (num == 1) {
                                QueryPart_Property query_part_prop = new QueryPart_Property ();
                                query_part_prop.Key = prop_string [0];
                                query_part_prop.Value = text;
                                query_part_prop.Type = prop_type [0];
                                query_part_prop.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);

                                Logger.Log.Debug ("Parsed query '"          + query +
                                                  "' as prop query:key="    + query_part_prop.Key +
                                                  ", value="                + query_part_prop.Value +
                                                  " and property type="     + query_part_prop.Type);

                                return query_part_prop;
                        }

                        // Multiple property queries are mapped to this keyword query
                        // Create an OR query from them
                        // FIXME: Would anyone want an AND query ?

                        QueryPart_Or query_part_or = new QueryPart_Or ();
                        query_part_or.Logic = (IsProhibited ? QueryPartLogic.Prohibited : QueryPartLogic.Required);

                        Logger.Log.Debug ("Parsed query '{0}' as OR of {1} queries:", query, num);

                        for (int i = 0; i < num; ++i) {
                                QueryPart_Property query_part_prop = new QueryPart_Property ();
                                query_part_prop.Key = prop_string [i];
                                query_part_prop.Value = text;
                                query_part_prop.Type = prop_type [i];
                                query_part_prop.Logic = QueryPartLogic.Required;

                                Log.Debug ("\t:key={0}, value={1} and property type={2}", query_part_prop.Key, query_part_prop.Value, query_part_prop.Type);
                                query_part_or.Add (query_part_prop);
                        }

                        return query_part_or;
                }

                private static QueryPart DateQueryToQueryPart (string query)
                {
                        // Format
                        // query := date[-date]
                        // date := empty | year | year.month | year.month.date
                        // FIXME: Do we really want to query time too ? They are too long!

                        if (query == String.Empty)
                                throw new FormatException ();

                        int next_date_index = query.IndexOf ('-');
                        if (next_date_index == -1)
                                return DateToQueryPart (query);

                        // We have a range query - get the ranges
                        DateTime start_date, end_date;

                        try {
                                int y, m, d;
                                ParseDateQuery (query.Substring (0, next_date_index), out y, out m, out d);
                                start_date = CreateDateTime (y, m, d, true);
                        } catch (FormatException) {
                                start_date = DateTime.MinValue;
                        }

                        try {
                                int y, m, d;
                                ParseDateQuery (query.Substring (next_date_index + 1), out y, out m, out d);
                                end_date = CreateDateTime (y, m, d, false);
                        } catch (FormatException) {
                                // FIXME: Should the default end_date be DateTime.Now ?
                                end_date = DateTime.MinValue;
                        }

                        if (start_date == DateTime.MinValue && end_date == DateTime.MinValue)
                                throw new FormatException ();

                        QueryPart_DateRange range_query = new QueryPart_DateRange ();
                        range_query.Key = QueryPart_DateRange.TimestampKey;
                        range_query.StartDate = start_date;
                        range_query.EndDate = end_date;
                        Log.Debug ("Parsed date range query [{0}] as {1}", query, range_query);

                        return range_query;
                }

                private static void ParseDateQuery (string dt_string, out int year, out int month, out int date)
                {
                        year = month = date = -1;

                        if (dt_string.Length != 4 && dt_string.Length != 6 && dt_string.Length != 8)
                                throw new FormatException ();

                        if (dt_string.Length >= 4)
                                year = Convert.ToInt32 (dt_string.Substring (0, 4));

                        if (dt_string.Length >= 6)
                                month = Convert.ToInt32 (dt_string.Substring (4, 2));

                        if (dt_string.Length == 8)
                                date = Convert.ToInt32 (dt_string.Substring (6, 2));
                }

                private static DateTime CreateDateTime (int y, int m, int d, bool start_date)
                {
                        if (m == -1)
                                m = (start_date ? 1 : 12);
                        if (d == -1)
                                d = (start_date ? 1 : DateTime.DaysInMonth (y, m));

                        int hour = (start_date ? 0 : 23);
                        int min  = (start_date ? 0 : 59);
                        int sec  = (start_date ? 0 : 59);

                        // Create the date in local time
                        DateTime dt = new DateTime (y, m, d, hour, min, sec, DateTimeKind.Local);

                        // Dates could be in local or UTC
                        // Convert them to UTC
                        return dt.ToUniversalTime ();
                }

                private static QueryPart_DateRange DateToQueryPart (string dt_string)
                {
                        int y, m, d;
                        ParseDateQuery (dt_string, out y, out m, out d);

                        QueryPart_DateRange dt_query = new QueryPart_DateRange ();
                        dt_query.Key = QueryPart_DateRange.TimestampKey;
                        dt_query.StartDate = CreateDateTime (y, m, d, true);
                        dt_query.EndDate =  CreateDateTime (y, m, d, false);
                        Log.Debug ("Parsed date query [{0}] as {1}", dt_string, dt_query);

                        return dt_query;
                }

#if false
// gmcs QueryStringParser.cs -r:../Util/Util.dll -r:../BeagrepClient/Beagrep.dll PropertyKeywordFu.cs
                public static void Main ()
                {
                        PropertyKeywordFu.ReadKeywordMappings ();

                        while (true) {
                                string input = Console.ReadLine ();
                                if (input == String.Empty)
                                        continue;

                                Console.WriteLine ("Parsing query string '{0}'", input);
                                foreach (QueryPart part in Parse (input))
                                        Console.WriteLine (part.ToString ());
                        }
                }
#endif
        }
}
