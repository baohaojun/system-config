//
// FilterMan.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2004 Michael Levy <mlevy@wardium.homeip.net>
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
using System.Text;
using System.Text.RegularExpressions;

using Beagle.Util;
using Beagle.Daemon;

using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpZipLib.BZip2;
using Decoder = SevenZip.Compression.LZMA.Decoder;

namespace Beagle.Filters {

	// FIXME: Right now we don't handle pages with just one line like:
	//   .so man3/strcpy.3
	// Which is in strncpy.3.gz and points to strcpy.3.gz

	public class FilterMan : Beagle.Daemon.Filter {

		// The regular expression for a complete man header line is built to allow a suite of 
		// non-spaces, or words separated by spaces which are encompassed in quotes
		// The regexp should be :
		//
		// Regex header_re = new Regex (@"^\.TH\s+" +
		//			     @"(?<title>(\S+|(""(\S+\s*)+"")))\s+" +
		//			     @"(?<section>\d+)\s+" + 
		//			     @"(?<date>(\S+|(""(\S+\s*)+"")))\s+" +
		//			     @"(?<source>(\S+|(""(\S+\s*)+"")))\s+" +
		//			     @"(?<manual>(\S+|(""(\S+\s*)+"")))\s*" +
		//			    "$");
		//
		// But there seem to be a number of broken man pages, and the current filter can be used 
		// for general troff pages.

		private static Regex header_regex = new Regex (@"^\.TH\s+(?<title>(\S+|(""(\S+\s*)+"")))\s*", RegexOptions.Compiled);

		private StreamReader compressed_reader = null;

		public FilterMan ()
		{
			// 1:Separate compressed man page filter
			SetVersion (1);

			SnippetMode = true;
			SetFileType ("documentation");
		}

		protected override void RegisterSupportedTypes ()
		{
			// Make this a general troff filter.
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-troff-man"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-troff-man"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-troff"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-troff"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/troff"));

			// Compressed man pages

			// FIXME: Hardcoded path is ok ?
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/man/*", ".gz", null, 1));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/man/*", ".bz2", null, 1));
			AddSupportedFlavor (new FilterFlavor ("file:///usr/share/man/*", ".lzma", null, 1));
		}

		protected void ParseManFile (TextReader reader)
		{
			string line = null;

			string section_property_name = null;
						    
			while ((line = reader.ReadLine ()) != null) {

				// Comment in man page
				if (line.StartsWith (".\\\""))
					continue;

				if (line.StartsWith (".TH ")) {
					HandleTH (line);
					continue;
				} else if (line.StartsWith (".SH")) {
					section_property_name = HandleSH (line);

					// Did not find useful property, so treat this is a section
					if (section_property_name == null)
						AppendStructuralBreak ();

					continue;
				} else if (section_property_name != null) {
					AddProperty (Beagle.Property.New (section_property_name, ProcessMacros (line)));
					continue;
                      		}

				if (! line.StartsWith (".")) {
					AppendLine (ProcessMacros (line));
					continue;
				}

				// macro line
				// From http://www.mamiyami.com/document/oreilly/unix3/unixnut/ch16_01.htm
				int index = line.IndexOf (' ');
				if (index == -1)
					continue; // processing macro

				string macro = line.Substring (0, index);
				macro = macro.ToUpper ();
				switch (macro) {
				case ".B":
					goto case ".hottext";
				case ".BI":
					goto case ".hottext";
				case ".BR":
					goto case ".hottext";
				case ".DT":
					break;
				case ".HP":
					reader.ReadLine (); // eat next line
					AppendStructuralBreak (); // new para
					break;
				case ".I":
					goto case ".hottext";
				case ".IB":
					goto case ".hottext";
				case ".IP":
					AppendStructuralBreak (); // new para
					break;
				case ".IR":
					goto case ".hottext";
				case ".LP":
					AppendStructuralBreak ();
					break;
				case ".P":
					AppendStructuralBreak ();
					break;
				case ".PD":
					break;
				case ".PP":
					AppendStructuralBreak ();
					break;
				case ".RB":
					goto case ".hottext";
				case ".RE":
					break;
				case ".RI":
					goto case ".hottext";
				case ".RS":
					break;
				case ".SB":
					goto case ".hottext";
				case ".SM":
					goto case ".hottext";
				case ".SS":
					AppendStructuralBreak ();
					break;
				case ".TP":
					AppendStructuralBreak ();
					break;
				case ".UR":
					AppendWord (line.Substring (index));
					break;

				case ".hottext":
					string text = ProcessMacros (line.Substring (index));
					AppendText (text, text);
					AppendWhiteSpace ();
					break;
				default:
					//Log.Error ("Unsupported man-page macro: '{0}'", line);
					break;
				}
			} 

			Finished ();
		}

		private void HandleTH (string line)
		{
			MatchCollection matches = header_regex.Matches (line);
			
			if (matches.Count != 1) {
				Log.Error ("In title Expected 1 match but found {0} matches in '{1}'",
					   matches.Count, line);
				return;
			}

			foreach (Match match in matches) {
				string title = ProcessMacros (match.Groups ["title"].ToString ());
				AddProperty (Beagle.Property.New ("dc:title", title));
			}
		}

		private string HandleSH (string line)
		{
			if (line == ".SH NAME")
				return "dc:subject";
			if (line == ".SH AUTHOR")
				return "dc:creator";
			if (line == ".SH COPYRIGHT")
				return "dc:rights";
			return null;
		}

		private string ProcessMacros (string line)
		{
			line = line.Replace (@"\-", "-");
			line = line.Replace (@"\ ", " ");
			line = line.Replace (@"\fB", String.Empty);
			line = line.Replace (@"\fI", String.Empty);
			line = line.Replace (@"\fP", String.Empty);
			line = line.Replace (@"\fR", String.Empty);
			line = line.Replace (@"\(co", "(C)");

			return line;
		}

		protected override void DoPullProperties ()
		{
			if (Extension == ".gz" || Extension == ".bz2" || Extension == ".lzma")
				ParseComressedManFile ();
			else
				ParseManFile (base.TextReader);
		}

		private void ParseComressedManFile ()
		{
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

			ParseManFile (compressed_reader);
		}

		protected override void DoClose ()
		{
			if (compressed_reader != null)
				compressed_reader.Close ();
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
