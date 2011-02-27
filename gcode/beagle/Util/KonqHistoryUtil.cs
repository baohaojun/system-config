//
// KonqHistoryUtil.cs
//
// Copyright (C) 2005 Debajyoti Bera <dbera.web@gmail.com>
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

namespace Beagle.Util {
	public class KonqHistoryUtil {
		public const string KonqCacheMimeType = "beagle/x-konq-cache";

		public static bool IsGZipCache (Stream stream)
		{
			byte id1 = (byte) stream.ReadByte ();
			byte id2 = (byte) stream.ReadByte ();
			stream.Position = 0;

			return (id1 == (byte) 0x1f && id2 == (byte) 0x8b);
		}

		public static bool ShouldIndex (StreamReader reader,
					  out string url,
					  out string creation_date,
					  out string mimetype,
					  out string charset,
					  out string revision)
		{
			url = null;
			creation_date = null;
			mimetype = null;
			charset = null;

			// format from kdelibs/kioslave/http/http.cc
			// line-1: Cache revision
			// FIXME: What happens when cache revision changes ???
			revision = reader.ReadLine ().Trim ();
			if (revision == "7")
				return ShouldIndexRev7 (reader, out url, out creation_date, out mimetype, out charset);
			else if (revision == "9")
				return ShouldIndexRev9 (reader, out url, out creation_date, out mimetype, out charset);

			return false;
		}

		public static bool ShouldIndexRev7 (StreamReader reader,
					  out string url,
					  out string creation_date,
					  out string mimetype,
					  out string charset)
		{
			// format from kdelibs/kioslave/http/http.cc

			// line-2: URL
			url = reader.ReadLine ();

			// line-3: creation date
			creation_date = reader.ReadLine ();

			// line-4: expiry date
			// dont need
			reader.ReadLine ();

			// line-5: ETag
			// dont need
			reader.ReadLine ();

			// line-6: last-modified
			// dont need
			reader.ReadLine ();

			// line-7: mimetype for the data
			// important stuff
			// we only index text/plain and text/html - anything else ?
			mimetype = reader.ReadLine ();

			// line-8: charset for the rest data
			// important stuff
			charset = reader.ReadLine ();
			
			/*
			Console.WriteLine ("FilterKonqHistory:" + 
					    " url=" + url +
					    " date=" + creation_date +
					    " mimetype=" + mimetype +
					    " charset=" + charset);
			*/
			// rest is data ...
			return (mimetype == "text/html");
		}

		public static bool ShouldIndexRev9 (StreamReader reader,
					  out string url,
					  out string creation_date,
					  out string mimetype,
					  out string charset)
		{
			bool ret = ShouldIndexRev7 (reader, out url, out creation_date, out mimetype, out charset);

			// Revision 9 is mostly same as revisiom 7
			// 7 lines followed by HTTP response headers.
			// http://websvn.kde.org/trunk/KDE/kdelibs/kioslave/http/http.cpp

			// The charset line is at a different place. What the above function read was the HTTP response headers.
			// Keep reading the HTTP response headers till an empty line is found or the charset line is found
			string line;
			while ((line = reader.ReadLine ().ToLower ()) != String.Empty) {
				if (! line.StartsWith ("content-type: "))
					continue;

				int pos = line.IndexOf ("charset=");
				if (pos != -1)
					charset = line.Substring (pos + 8);
				else
					charset = "utf-8";
			}

			return ret;
		}
	}
}
