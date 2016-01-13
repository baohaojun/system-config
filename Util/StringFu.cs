//
// StringFu.cs
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
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;
using System.Xml;
using System.Text.RegularExpressions;

using Mono.Unix;

namespace Beagrep.Util {

        public class StringFu {

                private StringFu () { } // class is static

                public const string UnindexedNamespace = "_unindexed:";

                private const String TimeFormat = "yyyyMMddHHmmss";

                public static DateTime MinValueUtc = new DateTime (0, DateTimeKind.Utc);
                public static DateTime MaxValueUtc = new DateTime (DateTime.MaxValue.Ticks, DateTimeKind.Utc);

                /// <summary>
                /// We use this instead of DateTime.ToUniversalTime() because
                /// we want to assume DateTimeKind.Unspecified dates are UTC
                /// </summary>
                /// <param name="dt">
                /// A <see cref="DateTime"/>
                /// </param>
                /// <returns>
                /// A <see cref="DateTime"/>
                /// </returns>
                static private DateTime ToUniversalTime (DateTime dt)
                {
                        switch (dt.Kind) {

                        case DateTimeKind.Utc:
                                return dt;

                        case DateTimeKind.Local:
                                return dt.ToUniversalTime ();

                        case DateTimeKind.Unspecified:
                                // FIXME: We should fix all the instances of unspecified
                                // DateTimes to avoid any potential comparison issues.
                                //Log.Warn ("Possibly incorrect unspecified date ({0}): {1}", dt, new System.Diagnostics.StackTrace (true).ToString ());
                                return dt;
                        }

                        // We'll never hit this, but otherwise the compiler
                        // complains about not all codepaths returning...
                        throw new Exception ("Unreachable code reached");
                }

                static public string DateTimeToString (DateTime dt)
                {
                        return ToUniversalTime (dt).ToString (TimeFormat);
                }

                static public string DateTimeToYearMonthString (DateTime dt)
                {
                        return ToUniversalTime (dt).ToString ("yyyyMM");
                }

                static public string DateTimeToDayString (DateTime dt)
                {
                        return ToUniversalTime (dt).ToString ("dd");
                }

                static public DateTime StringToDateTime (string str)
                {
                        if (String.IsNullOrEmpty (str) || str.Length != 14)
                                return new DateTime ();

                        int year, month, day, hour, min, sec;

                        // Pretty simple format, parse it ourself
                        year  = int.Parse (str.Substring (0, 4));
                        month = int.Parse (str.Substring (4, 2));
                        day   = int.Parse (str.Substring (6, 2));
                        hour  = int.Parse (str.Substring (8, 2));
                        min   = int.Parse (str.Substring (10, 2));
                        sec   = int.Parse (str.Substring (12, 2));

                        DateTime dt = new DateTime (year, month, day, hour, min, sec, DateTimeKind.Utc);
                        return dt;
                }

                static public string DateTimeToFuzzy (DateTime dt)
                {
                        DateTime today = DateTime.Today;
                        TimeSpan sinceToday = today - dt;

                        string date = null, time = null;

                        if (sinceToday.TotalDays <= 0)
                                date = Catalog.GetString ("Today");
                        else if (sinceToday.TotalDays < 1)
                                date = Catalog.GetString ("Yesterday");
                        else if (today.Year == dt.Year)
                                /* Translators: Example output: Aug 9 */
                                date = dt.ToString (Catalog.GetString ("MMM d"));
                        else
                                /* Translators: Example output: Aug 9, 2000 */
                                date = dt.ToString (Catalog.GetString ("MMM d, yyyy"));

                        /* Translators: Example output: 11:05 AM  (note h = 12-hour time) */
                        time = dt.ToString (Catalog.GetString ("h:mm tt"));

                        string fuzzy;

                        if (date != null && time != null)
                                /* Translators: {0} is a date (e.g. 'Today' or 'Apr 23'), {1} is the time */
                                fuzzy = String.Format (Catalog.GetString ("{0}, {1}"), date, time);
                        else if (date != null)
                                fuzzy = date;
                        else
                                fuzzy = time;

                        return fuzzy;
                }

                public static string DateTimeToPrettyString (DateTime date)
                {
                        DateTime now = DateTime.Now;
                        string short_time = date.ToShortTimeString ();

                        if (date.Year == now.Year) {
                                if (date.DayOfYear == now.DayOfYear) {
                                        /* To translators: {0} is the time of the day, eg. 13:45 */
                                        return String.Format (Catalog.GetString ("Today, {0}"), short_time);
                                } else if (date.DayOfYear == now.DayOfYear - 1) {
                                        /* To translators: {0} is the time of the day, eg. 13:45 */
                                        return String.Format (Catalog.GetString ("Yesterday, {0}"), short_time);
                                } else if (date.DayOfYear > now.DayOfYear - 6 && date.DayOfYear < now.DayOfYear) {
                                        int days = now.DayOfYear - date.DayOfYear;

                                        /* To translators: {0} is the number of days that have passed, {1} is the time of the day, eg. 13:45 */
                                        return String.Format (Catalog.GetPluralString ("{0} day ago, {1}",
                                                                                       "{0} days ago, {1}",
                                                                                       days),
                                                              days, short_time);
                                } else {
                                        /* Translators: Example output: January 3, 3:45 PM */
                                        return date.ToString (Catalog.GetString ("MMMM d, h:mm tt"));
                                }
                        }

                        /* Translators: Example output: March 23 2001, 10:04 AM */
                        return date.ToString (Catalog.GetString ("MMMM d yyyy, h:mm tt"));
                }

                public static string DurationToPrettyString (DateTime end_time, DateTime start_time)
                {
                        TimeSpan span = end_time - start_time;

                        string span_str = "";

                        if (span.Hours > 0) {
                                span_str = String.Format (Catalog.GetPluralString ("{0} hour", "{0} hours", span.Hours), span.Hours);

                                if (span.Minutes > 0)
                                        span_str += ", ";
                        }

                        if (span.Minutes > 0) {
                                span_str += String.Format (Catalog.GetPluralString ("{0} minute", "{0} minutes", span.Minutes), span.Minutes);
                        }


                        return span_str;
                }

                static public string TimeSpanToString (TimeSpan span)
                {
                        StringBuilder sb = new StringBuilder ();

                        if (span.Days > 0)
                                sb.Append (span.Days + "d");

                        if (span.TotalHours > 1.0)
                                sb.Append (span.Hours + "h");

                        if (span.TotalMinutes > 1.0)
                                sb.Append (span.Minutes + "m");

                        sb.Append (span.Seconds + "s");

                        return sb.ToString ();
                }

                static public string FileLengthToString (long len)
                {
                        const long oneMb = 1024*1024;

                        if (len < 0)
                                return "*BadLength*";

                        if (len < 1024)
                                /* Translators: {0} is a file size in bytes */
                                return String.Format (Catalog.GetPluralString ("{0} byte", "{0} bytes", (int) len), len);

                        if (len < oneMb)
                                /* Translators: {0} is a file size in kilobytes */
                                return String.Format (Catalog.GetString ("{0:0.0} KB"), len/(double)1024);

                        /* Translators: {0} is a file size in megabytes */
                        return String.Format (Catalog.GetString ("{0:0.0} MB"), len/(double)oneMb);
                }

                /// <summary>
                ///      Match strings against patterns that are allowed to contain
                ///      glob-style * and ? wildcards.
                /// </summary>
                /// <param name="pattern">
                /// A <see cref="System.String"/>
                /// </param>
                /// <param name="str">
                /// A <see cref="System.String"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.Boolean"/>
                /// </returns>
                static public bool GlobMatch (string pattern, string str)
                {
                        if (pattern == null || str == null)
                                return false;
                        else if (pattern == "*")
                                return true;
                        else if (str == String.Empty && pattern != String.Empty)
                                return false;
                        else if (pattern.IndexOf ('*') == -1)
                                return pattern == str;
                        else
                                return WildcardEquals (pattern, 0, str, 0);
                }

                private const char WILDCARD_STRING = '*';

                /// <summary>
                /// Copied from beagrepd/Lucene.Net/Search/WildcardTermEnum.cs
                /// Simple string matching algorithm with wildcards
                /// '*' matches 0 or more characters
                /// </summary>
                /// <param name="pattern">
                /// A <see cref="System.String"/>
                /// </param>
                /// <param name="patternIdx">
                /// A <see cref="System.Int32"/>
                /// </param>
                /// <param name="text">
                /// A <see cref="System.String"/>
                /// </param>
                /// <param name="stringIdx">
                /// A <see cref="System.Int32"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.Boolean"/>
                /// </returns>
                private static bool WildcardEquals(System.String pattern, int patternIdx, System.String text, int stringIdx)
                {
                        int p = patternIdx;

                        for (int s = stringIdx; ; ++p, ++s) {
                                // End of string yet?
                                bool sEnd = (s >= text.Length);
                                // End of pattern yet?
                                bool pEnd = (p >= pattern.Length);

                                // If we're looking at the end of the string...
                                if (sEnd) {
                                        // Assume the only thing left on the pattern is/are wildcards
                                        bool justWildcardsLeft = true;

                                        // Current wildcard position
                                        int wildcardSearchPos = p;
                                        // While we haven't found the end of the pattern,
                                        // and haven't encountered any non-wildcard characters
                                        while (wildcardSearchPos < pattern.Length && justWildcardsLeft) {
                                                // Check the character at the current position
                                                char wildchar = pattern[wildcardSearchPos];

                                                // If it's not a wildcard character, then there is more
                                                // pattern information after this/these wildcards.
                                                if (wildchar != WILDCARD_STRING)
                                                        justWildcardsLeft = false;
                                                else
                                                        // Look at the next character
                                                        wildcardSearchPos++;
                                        }

                                        // This was a prefix wildcard search, and we've matched, so
                                        // return true.
                                        if (justWildcardsLeft) {
                                                return true;
                                        }
                                }

                                // If we've gone past the end of the string, or the pattern,
                                // return false.
                                if (sEnd || pEnd) {
                                        break;
                                }

                                if (pattern[p] == WILDCARD_STRING) {
                                        // Look at the character beyond the '*'.
                                        ++p;
                                        // Examine the string, starting at the last character.
                                        for (int i = text.Length; i >= s; --i) {
                                                if (WildcardEquals(pattern, p, text, i)) {
                                                        return true;
                                                }
                                        }
                                        break;
                                }
                                if (pattern[p] != text[s]) {
                                        break;
                                }
                        }
                        return false;
                }

                // FIXME: how do we do this operation in a culture-neutral way?
                static public string[] SplitQuoted (string str)
                {
                        char[] specialChars = new char [2] { ' ', '"' };

                        ArrayList array = new ArrayList ();

                        int i;
                        while ((i = str.IndexOfAny (specialChars)) != -1) {
                                if (str [i] == ' ') {
                                        if (i > 0)
                                                array.Add (str.Substring (0, i));
                                        str = str.Substring (i+1);
                                } else if (str [i] == '"') {
                                        int j = str.IndexOf ('"', i+1);
                                        if (i > 0)
                                                array.Add (str.Substring (0, i));
                                        if (j == -1) {
                                                if (i+1 < str.Length)
                                                        array.Add (str.Substring (i+1));
                                                str = "";
                                        } else {
                                                if (j-i-1 > 0)
                                                array.Add (str.Substring (i+1, j-i-1));
                                                str = str.Substring (j+1);
                                        }
                                }
                        }
                        if (str != "")
                                array.Add (str);

                        string [] retval = new string [array.Count];
                        for (i = 0; i < array.Count; ++i)
                                retval [i] = (string) array [i];
                        return retval;
                }

                static public bool ContainsWhiteSpace (string str)
                {
                        foreach (char c in str)
                                if (char.IsWhiteSpace (c))
                                        return true;
                        return false;
                }

                static char[] CharsToQuote = { ';', '?', '#', ' ' };

                static public string HexEscape (string str)
                {
                        int index = -1;
                        if ((index = str.IndexOfAny (CharsToQuote)) == -1)
                                return str;

                        StringBuilder builder = new StringBuilder (str, 0, index, str.Length << 1);

                        for (; index < str.Length; ++ index) {
                                char c = str [index];

                                if (Array.IndexOf<char> (CharsToQuote, c) != -1)
                                        builder.Append (Uri.HexEscape (c));
                                else if (c < 128)
                                        builder.Append (c);
                                else {
                                        byte[] utf8_bytes;

                                        utf8_bytes = Encoding.UTF8.GetBytes (new char [] { c });

                                        foreach (byte b in utf8_bytes)
                                                builder.AppendFormat ("%{0:X}", b);
                                }
                        }

                        return builder.ToString ();
                }

                /// <summary>
                /// Translate all %xx codes into real characters
                /// </summary>
                /// <param name="str">
                /// A <see cref="System.String"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.String"/>
                /// </returns>
                static public string HexUnescape (string str)
                {
                        int i, pos = 0;
                        if ((i = str.IndexOf ('%')) == -1)
                                return str;

                        List<byte> bytes = new List<byte> (str.Length);
                        byte[] sub_bytes;

                        do {
                                sub_bytes = Encoding.UTF8.GetBytes (str.Substring (pos, i - pos));
                                bytes.AddRange (sub_bytes);

                                pos = i;
                                char unescaped = Uri.HexUnescape (str, ref pos);
                                bytes.Add (Convert.ToByte (unescaped));
                        } while ((i = str.IndexOf ('%', pos)) != -1);

                        sub_bytes = Encoding.UTF8.GetBytes (str.Substring (pos, str.Length - pos));
                        bytes.AddRange (sub_bytes);

                        return Encoding.UTF8.GetString (bytes.ToArray ());
                }

                // These strings should never be exposed to the user.
                static int uid = 0;
                static object uidLock = new object ();
                static public string GetUniqueId ()
                {
                        lock (uidLock) {
                                if (uid == 0) {
                                        Random r = new Random ();
                                        uid = r.Next ();
                                }
                                ++uid;

                                return string.Format ("{0}-{1}-{2}-{3}",
                                                      Environment.GetEnvironmentVariable ("USER"),
                                                      Environment.GetEnvironmentVariable ("HOST"),
                                                      DateTime.Now.Ticks,
                                                      uid);
                        }
                }

                static string [] replacements = new string [] {
                        "&amp;", "&lt;", "&gt;", "&quot;", "&apos;",
                        "&#xD;", "&#xA;"};

                static private StringBuilder cachedStringBuilder;
                static private char QuoteChar = '\"';

                private static bool IsInvalid (int ch)
                {
                        switch (ch) {
                        case 9:
                        case 10:
                        case 13:
                                return false;
                        }
                        if (ch < 32)
                                return true;
                        if (ch < 0xD800)
                                return false;
                        if (ch < 0xE000)
                                return true;
                        if (ch < 0xFFFE)
                                return false;
                        if (ch < 0x10000)
                                return true;
                        if (ch < 0x110000)
                                return false;
                        else
                                return true;
                }

                static public string EscapeStringForHtml (string source, bool skipQuotations)
                {
                        int start = 0;
                        int pos = 0;
                        int count = source.Length;
                        char invalid = ' ';
                        for (int i = 0; i < count; i++) {
                                switch (source [i]) {
                                case '&':  pos = 0; break;
                                case '<':  pos = 1; break;
                                case '>':  pos = 2; break;
                                case '\"':
                                        if (skipQuotations) continue;
                                        if (QuoteChar == '\'') continue;
                                        pos = 3; break;
                                case '\'':
                                        if (skipQuotations) continue;
                                        if (QuoteChar == '\"') continue;
                                        pos = 4; break;
                                case '\r':
                                        if (skipQuotations) continue;
                                        pos = 5; break;
                                case '\n':
                                        if (skipQuotations) continue;
                                        pos = 6; break;
                                default:
                                        if (IsInvalid (source [i])) {
                                                invalid = source [i];
                                                pos = -1;
                                                break;
                                        }
                                        else
                                                continue;
                                }
                                if (cachedStringBuilder == null)
                                        cachedStringBuilder = new StringBuilder
                                                ();
                                cachedStringBuilder.Append (source.Substring (start, i - start));
                                if (pos < 0) {
                                        cachedStringBuilder.Append ("&#x");
                                        if (invalid < (char) 255)
                                                cachedStringBuilder.Append (((int) invalid).ToString ("X02", CultureInfo.InvariantCulture));
                                        else
                                                cachedStringBuilder.Append (((int) invalid).ToString ("X04", CultureInfo.InvariantCulture));
                                        cachedStringBuilder.Append (";");
                                }
                                else
                                        cachedStringBuilder.Append (replacements [pos]);
                                start = i + 1;
                        }
                        if (start == 0)
                                return source;
                        else if (start < count)
                                cachedStringBuilder.Append (source.Substring (start, count - start));
                        string s = cachedStringBuilder.ToString ();
                        cachedStringBuilder.Length = 0;
                        return s;
                }

                static public string CleanupInvalidXmlCharacters (string str)
                {
                        if (str == null)
                                return null;

                        int len = str.Length;

                        // Find the first invalid character in the string
                        int i = 0;
                        while (i < len && ! IsInvalid (str [i]))
                                ++i;

                        // If the string doesn't contain invalid characters,
                        // just return it.
                        if (i >= len)
                                return str;

                        // Otherwise copy the first chunk, then go through
                        // character by character looking for more invalid stuff.

                        char [] char_array = new char[len];

                        for (int j = 0; j < i; ++j)
                                char_array [j] = str [j];
                        char_array [i] = ' ';

                        for (int j = i+1; j < len; ++j) {
                                char c = str [j];
                                if (IsInvalid (c))
                                        char_array [j] = ' ';
                                else
                                        char_array [j] = c;
                        }

                        return new string (char_array);
                }

                /// <summary>
                /// Words of less than min_word_length characters are not counted
                /// </summary>
                /// <param name="str">
                /// A <see cref="System.String"/>
                /// </param>
                /// <param name="max_words">
                /// A <see cref="System.Int32"/>
                /// </param>
                /// <param name="min_word_length">
                /// A <see cref="System.Int32"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.Int32"/>
                /// </returns>
                static public int CountWords (string str, int max_words, int min_word_length)
                {
                        if (str == null)
                                return 0;

                        bool last_was_white = true;
                        int words = 0;
                        int word_start_pos = -1;

                        for (int i = 0; i < str.Length; ++i) {
                                if (Char.IsWhiteSpace (str [i])) {
                                        // if just seen word is too short, ignore it
                                        if (! last_was_white && (i - word_start_pos < min_word_length))
                                                --words;
                                        last_was_white = true;
                                } else {
                                        if (last_was_white) {
                                                ++words;
                                                word_start_pos = i;
                                                if (max_words > 0 && words >= max_words)
                                                        break;
                                        }
                                        last_was_white = false;
                                }
                        }

                        return words;
                }

                static public int CountWords (string str, int max_words)
                {
                        return CountWords (str, max_words, -1);
                }

                static public int CountWords (string str)
                {
                        return CountWords (str, -1);
                }

                /// <summary>
                /// Strip trailing slashes and make sure we only have 1 leading slash
                /// </summary>
                /// <param name="path">
                /// A <see cref="System.String"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.String"/>
                /// </returns>
                static public string SanitizePath (string path)
                {
                        if (path.StartsWith ("//")) {
                                int pos;
                                for (pos = 2; pos < path.Length; pos++)
                                        if (path [pos] != '/')
                                                break;

                                path = path.Substring (pos - 1);
                        }
                        if (!(path.Length == 1 && path [0] == '/'))
                                path = path.TrimEnd ('/');

                        return path;
                }

                /// <summary>
                /// This method will translate an email address like
                /// "john.doe+spamtrap@foo.com" to "john doe spamtrap foo"
                ///
                /// FIXME: Maybe we should only do the username part?  Ie,
                /// "john doe spamtrap"?  That way searching for "foo" won't
                /// turn up *everything*
                /// </summary>
                /// <param name="email">
                /// A <see cref="System.String"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.String"/>
                /// </returns>
                static public string SanitizeEmail (string email)
                {
                        char[] replace_array = { '@', '.', '-', '_', '+' };
                        string[] tlds = { "com", "net", "org", "edu", "gov", "mil" }; // Just the Big Six

                        if (email == null)
                                return null;

                        email = email.ToLower ();

                        string[] tmp = email.Split (replace_array);
                        email = String.Join (" ", tmp);

                        foreach (string tld in tlds) {
                                if (email.EndsWith (" " + tld)) {
                                        email = email.Substring (0, email.Length - 4);
                                        break;
                                }
                        }

                        return email;
                }


                /// <summary>
                /// expands environment variables in a string e.g.
                /// folders=$HOME/.kde/share/...
                /// </summary>
                /// <param name="path">
                /// A <see cref="System.String"/>
                /// </param>
                /// <returns>
                /// A <see cref="System.String"/>
                /// </returns>
                public static string ExpandEnvVariables (string path)
                {
                        int dollar_pos = path.IndexOf ('$');
                        if (dollar_pos == -1)
                                return path;

                        System.Text.StringBuilder sb =
                                new System.Text.StringBuilder ( (dollar_pos == 0 ? "" : path.Substring (0, dollar_pos)));

                        bool env_var_found = false;

                        while (dollar_pos != -1 && dollar_pos + 1 < path.Length) {
                                // FIXME: kconfigbase.cpp contains an additional case, $(expression)/.kde/...
                                // Ignoring such complicated expressions for now. Volunteers ;) ?
                                int end_pos = dollar_pos;
                                if (path [dollar_pos + 1] != '$') {
                                        string var_name;
                                        end_pos ++;
                                        if (path [end_pos] == '{') {
                                                while ((end_pos < path.Length) &&
                                                       (path [end_pos] != '}'))
                                                        end_pos ++;
                                                end_pos ++;
                                                var_name = path.Substring (dollar_pos + 2, end_pos - dollar_pos - 3);
                                        } else {
                                                while ((end_pos < path.Length) &&
                                                       (Char.IsNumber (path [end_pos]) ||
                                                        Char.IsLetter (path [end_pos]) ||
                                                        path [end_pos] == '_'))
                                                        end_pos ++;
                                                var_name = path.Substring (dollar_pos + 1, end_pos - dollar_pos - 1);
                                        }

                                        string value_env = null;
                                        if (var_name != String.Empty)
                                                value_env = Environment.GetEnvironmentVariable (var_name);
                                        if (value_env != null) {
                                                sb.Append (value_env);
                                                env_var_found = true;
                                        } else {
                                                env_var_found = false;
                                        }
                                        // else, no environment variable with that name exists. ignore
                                }else // else, ignore the first '$', second one will be expanded
                                        end_pos ++;
                                if (end_pos >= path.Length)
                                        break;
                                dollar_pos = path.IndexOf ('$', end_pos);
                                if (dollar_pos == -1) {
                                        sb.Append (path.Substring (end_pos));
                                } else {
                                        sb.Append (path.Substring (end_pos, dollar_pos - end_pos));
                                }
                        }

                        if (! env_var_found)
                                return null;

                        return sb.ToString ();
                }

                public static string StripTags (string line, StringBuilder builder)
                {
                        int first = line.IndexOf ('<');
                        if (first == -1)
                                return line;

                        builder.Length = 0;

                        int i = 0;
                        while (i < line.Length) {

                                int j;
                                if (first == -1) {
                                        j = line.IndexOf ('<', i);
                                } else {
                                        j = first;
                                        first = -1;
                                }

                                int k = -1;
                                if (j != -1) {
                                        k = line.IndexOf ('>', j);

                                        // If a "<" is unmatched, preserve it, and the
                                        // rest of the line
                                        if (k == -1)
                                                j = -1;
                                }

                                if (j == -1) {
                                        builder.Append (line, i, line.Length - i);
                                        break;
                                }

                                builder.Append (line, i, j-i);

                                i = k+1;
                        }

                        return builder.ToString ();
                }

                public static string StripTags (string line)
                {
                        StringBuilder sb = new StringBuilder ();
                        return StripTags (line, sb);
                }

                public static string ConvertSpecialEntities (string line)
                {
                        line = line.Replace ("&lt;", "<");
                        line = line.Replace ("&gt;", ">");
                        line = line.Replace ("&quot;", "\"");
                        line = line.Replace ("&amp;", "&");
                        line = line.Replace ("&nbsp;", " ");
                        line = line.Replace ("&apos;", "\'");

                        return line;
                }

                public static Regex GetPatternRegex (IEnumerable patterns)
                {
                        if (patterns == null)
                                return null;

                        StringBuilder sb = new StringBuilder ();
                        bool first = true;
                        Regex tmp_regex = null;
                        foreach (string pattern in patterns) {
                                try {
                                        string regex_pattern;
                                        if (pattern.Length >= 2 && pattern [0] == '/' && pattern.EndsWith ("/")) {
                                                regex_pattern = pattern.Substring (1, pattern. Length - 2);
                                        } else {
                                                // Make *.foo to .*\.foo
                                                regex_pattern = pattern.Replace (".", @"\.");
                                                regex_pattern = regex_pattern.Replace ("*", ".*");
                                        }
                                        tmp_regex = new Regex (regex_pattern);
                                        sb.Append (String.Format (@"{0}^(?:{1})$", first ? String.Empty : "|", regex_pattern));
                                } catch (System.ArgumentException) {
                                        Log.Warn ("Ignoring invalid regex: [{0}]", pattern);
                                }
                                first = false;
                        }

                        // If no patterns are there, prevent generating a regex that will match everything
                        if (sb.Length == 0)
                                return null;

                        //Log.Debug ("regex: [{0}]", sb.ToString());
                        return new Regex (sb.ToString (), RegexOptions.Compiled);
                }

                public class OrdinalComparer : IComparer {

                        public static OrdinalComparer Instance = new OrdinalComparer ();

                        public int Compare (object a, object b)
                        {
                                return String.CompareOrdinal ((string) a, (string) b);
                        }
                }
        }
}
