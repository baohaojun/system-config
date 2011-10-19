//
// GuidFu.cs
//
// Copyright (C) 2005 Novell, Inc.
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

namespace Beagle.Util {

	public class GuidFu {

		public const string UriScheme = "uid";

		static private string UriPrefix = UriScheme + ":";

		static public string ToShortString (Guid uid)
		{
			// When converting 16 bytes to base64, the final two characters
			// are always "==".
			string str = Convert.ToBase64String (uid.ToByteArray (), 0, 16);
			str = str.Substring (0, 22);
			str = str.Replace ('/', '_'); // this avoids problems w/ Uris
			return str;
		}

		static public Guid FromShortString (string str)
		{
			// We need to add back the padding before converting the
			// string back to a byte array.
			str = str.Replace ('_', '/') + "==";
			return new Guid (Convert.FromBase64String (str));
		}

		static public Uri ToUri (Guid uid)
		{
			return new Uri (ToUriString (uid), true);
		}

		static public Uri FromShortStringToUri (string str)
		{
			return new Uri (UriPrefix + str , true);	
		}

		static public Guid FromUri (Uri uri)
		{
			return FromShortString (uri.LocalPath);
		}

		static public string ToUriString (Guid uid)
		{
			return UriPrefix + ToShortString (uid);
		}

		static public Guid FromUriString (string str)
		{
			return FromShortString (str.Substring (UriPrefix.Length));
		}

	}
}
