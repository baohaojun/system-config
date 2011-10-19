//
// FilterInfo.cs
//
// Copyright (C) 2008 D Bera<dbera.web@gmail.com>
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
using System.Text;
using System.Text.RegularExpressions;

using Beagle.Util;
using Beagle.Daemon;

using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpZipLib.BZip2;
using Decoder = SevenZip.Compression.LZMA.Decoder;

namespace Beagle.Filters {

	public class FilterInfo : Beagle.Daemon.Filter {

		public FilterInfo ()
		{
			SetFileType ("documentation");
		}

		protected override void RegisterSupportedTypes ()
		{
			// common paths
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/info/*", ".lzma", null, 1));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/info/*", ".gz", null, 1));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/info/*", ".bz2", null, 1));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/info/*", ".info", null, 1));
		}

		const char c = ''; // Info node separator
		private bool skipping = false; // use this to skip certain nodes
		static readonly char[] node_line_sep = new char[] {'*', ':', '.', ','};

		private TextReader reader;

		protected override void DoOpen (FileInfo info)
		{
			if (Extension == ".gz" || Extension == ".bz2" || Extension == ".lzma")
				GetCompressedInfoReader ();
			else
				reader = base.TextReader;
		}

		override protected void DoPullProperties ()
		{
			// start reading lines till the first node is hit
			string line;
			while ((line = reader.ReadLine ()) != null) {
				if (line.Length == 0)
					continue;

				if (line [0] == c) {
					// no title node
					skipping = ReportNewNode ();
					return;
				}

				if (line [0] != '*')
					continue;

				string[] words = line.Split (node_line_sep, StringSplitOptions.RemoveEmptyEntries);
				AddProperty (Beagle.Property.New ("dc:title", words [0].Trim ()));
				skipping = true;
				return;
			}

			if (line == null) {
				Finished ();
				return;
			}
		}

		override protected void DoPull ()
		{
			string line;
						
			line = reader.ReadLine ();
			if (line == null) {
				Finished ();
				return;
			}

			if (line.Length == 0)
				return;

			if (skipping && line [0] != c)
				return;

			if (line [0] != c) {
				if (! skipping && ! line.StartsWith ("*"))
					AppendLine (line);
				return;
			}

			skipping = ReportNewNode ();
		}

		// Returns true if the node should be skipped
		private bool ReportNewNode ()
		{
			// Starting a new node
			// Read the next line
			string line = reader.ReadLine ();
			if (line == null) {
				Finished ();
				return true;
			}

			if (line.StartsWith ("Indirect:") ||
			    line.StartsWith ("Tag Table:") ||
			    line.Contains ("Node: Index") ||
			    line.Contains ("Node: Top")) {
				return true;
			}

			return false;
		}

		private void GetCompressedInfoReader ()
		{
			StreamReader compressed_reader = null;

			try {
				Stream stream = null;
				if (Extension == ".gz")
					stream = new GZipInputStream (Stream);
				else if (Extension == ".bz2")
					stream = new BZip2InputStream (Stream);
				else if (Extension == ".lzma")
					stream = GetLzmaStream (Stream);

				compressed_reader = new StreamReader (stream);
			} catch (Exception e) {
				Log.Error (e, "Error in opening compressed man page");
				if (compressed_reader != null)
					compressed_reader.Close ();
				Error ();
				return;
			}

			reader = compressed_reader;
		}

		protected override void DoClose ()
		{
			if (Extension == ".gz" || Extension == ".bz2" || Extension == ".lzma")
				if (reader != null)
					reader.Close ();
		}

		private Stream GetLzmaStream (Stream in_stream)
		{
			// From LzmaAlone.cs
			byte[] properties = new byte [5];
			if (in_stream.Read (properties, 0, 5) != 5)
				throw new Exception ("input .lzma is too short");

			Decoder decoder = new Decoder ();
			decoder.SetDecoderProperties (properties);

			long out_size = 0;
			for (int i = 0; i < 8; i++)
			{
				int v = in_stream.ReadByte ();
				if (v < 0)
					throw new Exception ("LZMA: Can't Read 1");
				out_size |= ((long)(byte)v) << (8 * i);
			}
			long compressed_size = in_stream.Length - in_stream.Position;

			// FIXME: Man pages are small enough to use a MemoryStream to store the
			// entire uncompressed file.
			// Still, a proper stream based approach would be good. Unfortunately,
			// LZMA does not provide a streaming interface. Current hacks involve
			// a separate synchronized thread.
			MemoryStream out_stream = new MemoryStream ((int) out_size); // outsize is long but this constructor is resizable
			decoder.Code (in_stream, out_stream, compressed_size, out_size, null);
			//Log.Debug ("Decoded {0} bytes to {1} bytes", compressed_size, out_size);
			out_stream.Position = 0;
			return out_stream;
		}
	}
}

