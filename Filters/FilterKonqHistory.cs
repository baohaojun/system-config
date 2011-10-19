//
// FilterKonqHistory.cs
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
using System.Collections;
using System.IO;
using System.Text;
using ICSharpCode.SharpZipLib.GZip;

using Beagle.Daemon;
using Beagle.Util;

using HtmlAgilityPack;

namespace Beagle.Filters {

	public class FilterKonqHistory : Beagle.Filters.FilterHtml {

		// use a static buffer to prevent constant alloc and de-alloc
		private static byte[] buf = null;

		public FilterKonqHistory ()
		{
		}

		override protected void DoOpen (FileInfo info)
		{
			if (buf == null)
				buf = new byte [1024];

			// read the charset hint from indexable
			string charset = null;
			string gzipped = null;
			string revision = "7";

			foreach (Property property in Indexable.Properties) {
				if (property.Key == (StringFu.UnindexedNamespace + "charset"))
					charset = (string) property.Value;
				else if (property.Key == (StringFu.UnindexedNamespace + "revision"))
					revision = (string) property.Value;
				else if (property.Key == (StringFu.UnindexedNamespace + "gzipped"))
					gzipped = (string) property.Value.ToLower ();
				//Console.WriteLine ("charset hint accepted: " + charset);
			}

			if (revision != "7" && revision != "9") {
				Error ();
				return;
			}

			Stream stream;
			if (gzipped == "true")
				stream = new GZipInputStream (Stream);
			else
				stream = Stream;

			// count past 8 lines ... Streams suck!
			int c = 0; // stores the number of newlines read
			int b = 0;
			int lines_to_skip = 0;
			if (revision == "7")
				lines_to_skip = 8;
			else if (revision == "9")
				lines_to_skip = 7;

			while (c < lines_to_skip && (b = stream.ReadByte ()) != -1) {
				if (b == '\n')
					c ++;
			}

			if (revision == "9") {
				// skip HTTP response headers i.e. keep reading till a blank line is found
				long last_pos = 0; // GZipInputStream.Position can be fake, keep our own count
				long cur_pos = 0;

				while ((b = stream.ReadByte ()) != -1) {
					cur_pos ++;
					if (b != '\n')
						continue;

					if (cur_pos == last_pos + 1)
						break;
					else
						last_pos = cur_pos;
				}
			}

			// copy the rest of the file to a memory stream
			MemoryStream mem_stream = new MemoryStream ();
			while ((b = stream.Read (buf, 0, 1024)) != 0)
				mem_stream.Write (buf, 0, b);
			mem_stream.Seek (0, SeekOrigin.Begin);

			HtmlDocument doc = new HtmlDocument ();
			doc.ReportNode += HandleNodeEvent;
			doc.StreamMode = true;
			// we already determined encoding
			doc.OptionReadEncoding = false;
			Encoding enc = Encoding.UTF8;
			if (charset != null && charset != String.Empty)
			    enc = Encoding.GetEncoding (charset);
	
			try {
				if (enc == null)
					doc.Load (mem_stream);
				else
					doc.Load (mem_stream, enc);
				Finished ();
			} catch (NotSupportedException) {
				doc.Load (mem_stream, Encoding.ASCII);
				Finished ();
			} catch (Exception e) {
				Log.Warn (e, "Unable to parse Konqueror History");
				Error ();
			}
		}

		override protected void RegisterSupportedTypes () 
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType (KonqHistoryUtil.KonqCacheMimeType));
		}
	}

}
