//
// UriFu.cs
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
// Copyright (C) 2004 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Collections;
using System.IO;
using System.Text;

namespace Beagrep.Util {

        /* Beagrep URIs : A bunch of hacks
         * ------------------------------
         *
         * Every indexed document is identified by a URI.
         * In general URI format is not very flexible so we
         * construct our own URI format and convince System.Uri
         * to use that. Also, we do our own URI<->string conversion
         * and use that during XML serialization/deserialization.
         *
         * The format is similar to the Uri RFC:
         *
         * Scheme
         * SchemeDelimiter
         * Userinfo
         * Host (case sensitive - different from the RFC)
         * Absolute path and query (escaped)
         * Zero or more fragments: each fragment is
         *   - '#' (unescaped)
         *   - fragment path (escaped)
         *
         * The difference is the presence of multiple fragments
         * and case-sensitive hostname.
         *
         * Also there are a bunch of functions for converting a
         * file path to a file:/// URI. This is needed because
         * sometimes path can contain characters prohibited
         * in the path component of a URI. These paths first need
         * to be escaped. Otherwise Uri() will think these are
         * special characters!
         *
         * Note: Beagrep internal uris are of the form "uid:xyz".
         *
         */

        public static class UriFu {

                static public Uri PathToFileUri (string path)
                {
                        return new Uri (PathToFileUriString (path));
                }

                // DEPRECATED! path is always escaped but escaping of fragment is
                // context dependent.
                //static public Uri PathToFileUri (string path, string fragment)
                //

                // Whether fragment should be escaped or not is crucial and depends on the context.
                // Hence the caller of the method should set it accordingly and no default
                // value for escape_fragment is provided.
                static public Uri AddFragment (Uri uri, string fragment, bool dont_escape_fragment)
                {
                        if (fragment [0] == '#')
                                fragment = fragment.Substring (1);

                        if (! dont_escape_fragment)
                                fragment = StringFu.HexEscape (fragment);

                        fragment = String.Concat (uri.Fragment,
                                                  '#',
                                                  fragment); // Append to existing fragment

                        return new Uri (uri, fragment, true /* dont escape*/);
                }

                static public string PathToFileUriString (string path)
                {
                        return String.Concat (Uri.UriSchemeFile,
                                              Uri.SchemeDelimiter,
                                              StringFu.HexEscape (Path.GetFullPath (path)));
                }

                static public Uri EscapedStringToUri (string uri_string)
                {
                        return new Uri (uri_string, true);
                }

                static public string UriToEscapedString (Uri uri)
                {
                        if (uri.UserEscaped)
                                return uri.OriginalString;

                        // Do not use uri.ToString(), it actually unescapes certain characters.
                        // uri.AbsoluteUri also returns "uid:xxx" from the beagrep internal
                        // uid:xxx URIs.
                        // If AbsoluteUri is found to skip any information in the URI, then
                        // manually the string needs to be constructed.
                        return uri.AbsoluteUri;
                }

                // Get a uri of our liking from a user-entered uri
                // Basically hex-escape the path, query and the fragment
                static public Uri UserUritoEscapedUri (string user_uri)
                {
                        Uri uri;
                        try {
                                uri = new Uri (user_uri);
                        } catch {
                                return null;
                        }

                        UriBuilder new_uri = new UriBuilder ();
                        new_uri.Scheme = uri.Scheme;
                        new_uri.Host = uri.Host;

                        if (uri.UserInfo != String.Empty) {
                                int index = uri.UserInfo.IndexOf (":");
                                if (index == -1)
                                        new_uri.UserName = uri.UserInfo;
                                else {
                                        new_uri.UserName = uri.UserInfo.Substring (0, index);
                                        index ++;
                                        if (index < uri.UserInfo.Length)
                                                new_uri.Password = uri.UserInfo.Substring (index);
                                }
                        }

                        if (! uri.IsDefaultPort)
                                new_uri.Port = uri.Port;
                        new_uri.Path = StringFu.HexEscape (uri.AbsolutePath);
                        new_uri.Query = uri.Query; // FIXME: escape ?
                        new_uri.Fragment = StringFu.HexEscape (uri.Fragment);

                        return new_uri.Uri;
                }

                //////////////////////////////////

                // Uri.ToString() should not be used. Use Uri.AbsoluteUri

                static public bool Equals (Uri uri1, Uri uri2)
                {
                        return uri1.ToString () == uri2.ToString ();
                }

                static public int Compare (Uri uri1, Uri uri2)
                {
                        return String.Compare (uri1.ToString (), uri2.ToString ());
                }

                //////////////////////////////////

                public class EqualityComparer : IEqualityComparer
                {
                        public new bool Equals (object uri1, object uri2)
                        {
                                return String.Equals (uri1.ToString (), uri2.ToString ());
                        }

                        public int GetHashCode (object o)
                        {
                                return o.ToString ().GetHashCode ();
                        }
                }

                static EqualityComparer equality_comparer = new EqualityComparer ();

                // Returns a hash table that does the right thing when
                // the key is a Uri.
                static public Hashtable NewHashtable ()
                {
                        return new Hashtable (equality_comparer);
                }

        }

}
