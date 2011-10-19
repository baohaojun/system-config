//
// DocExtractor.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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

// A simple little tool to extract text from a MS Word document and
// print it to stdout.  This is used by FilterDoc because wv is far too
// unreliable to do it in-proc.

using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

using Beagle.Util;

class DocExtractor {

	private delegate void TextHandlerCallback (IntPtr byte_array, int len, 
						   IntPtr hot_byte_array, int hot_len,
						   bool append_break);
		
	[DllImport ("libbeagleglue")]
	private static extern int wv1_glue_init_doc_parsing (string filename, TextHandlerCallback callback);

	[DllImport ("libbeagleglue")]
	private static extern int wv1_init ();

	//////////////////////////////////////////////////////////

	static private string filename;

	static private void ExtractText (IntPtr byte_array, int len,
					 IntPtr hot_byte_array, int hot_len,
					 bool append_break)
	{
		try {
			byte[] data;
			string str, hot_str;
			bool printed = false;

			if (len > 0) {
				data = new byte [len]; // At most 1023
				Marshal.Copy (byte_array, data, 0, len);

				str = Encoding.UTF8.GetString (data, 0, len).Trim ();

				if (! String.IsNullOrEmpty (str)) {
					Console.WriteLine (str);
					printed = true;
				}
			}

			if (hot_len > 0) {
				data = new byte [hot_len]; // At most 1023
				Marshal.Copy (hot_byte_array, data, 0, hot_len);
				hot_str = Encoding.UTF8.GetString (data, 0, hot_len).Trim ();

				if (! String.IsNullOrEmpty (hot_str)) {
					Console.WriteLine ("**HOT**" + hot_str);
					printed = true;
				}
			}

			if (append_break && printed)
				Console.WriteLine ("**BREAK**");
		} catch (Exception e) {
			Console.Error.WriteLine ("Exception occurred extracting text from word doc {0}", filename);
			Console.Error.WriteLine (e);
		}
	}

	static public int Main (string[] args)
	{
		SystemInformation.SetProcessName ("beagle-doc-extractor");

		if (args.Length != 1) {
			Console.Error.WriteLine ("Usage: beagle-doc-extractor <file>");
			return 1;
		}

		filename = args [0];

		wv1_init ();

		int ret = wv1_glue_init_doc_parsing (filename, ExtractText);

		switch (ret) {
		case -1:
			Console.Error.WriteLine ("Unable to read {0}", filename);
			return 1;
			
		case -2:
			Console.Error.WriteLine ("File is password protected: {0}", filename);
			return 1;

		case -3:
			Console.Error.WriteLine ("Unable to start the parser for {0}", filename);
			return 1;

		default:
			return 0;
		}
	}
}