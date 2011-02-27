//
// FilterPidginLog.cs
//
// Copyright (C) 2007 Novell, Inc.
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

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterPidginLog : Beagle.Daemon.Filter {

		public FilterPidginLog ()
		{
			SnippetMode = true;
			OriginalIsText = true;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("beagle/x-pidgin-log"));
		}

		private static DateTime ParseStartingDate (FileInfo file)
		{
			DateTime start_time;

			// Parse what we can from the file path
			try {
				string str;

				// Character at position 17 will be either a dot, indicating the beginning
				// of the extension for old gaim logs, or a plus or minus indicating a
				// timezone offset for new gaim logs.
				if (file.Name [17] == '+' || file.Name [17] == '-') {
					// New gaim 2.0.0 format, including timezone.
					//
					// Ugly hack time: DateTime's format specifiers only know how to
					// deal with timezones in the format "+HH:mm" and not "+HHmm",
					// which is how UNIX traditionally encodes them.  I have no idea
					// why; it would make RFC 822/1123 parsing a hell of a lot easier.
					// Anyway, in this case, we're going to insert a colon in there so
					// that DateTime.ParseExact can understand it.
					//
					// 2006-02-21-160424-0500EST.html
					//                     ^
					//                     offset 20

					str = file.Name.Substring (0, 20) + ':' + file.Name.Substring (20, 2);
					start_time = DateTime.ParseExact (str, "yyyy-MM-dd.HHmmsszzz", null);
				} else if (file.Name [17] == '.') {
					// Older gaim format.
					//
					// 2006-02-21-160424.html

					str = Path.GetFileNameWithoutExtension (file.Name);
					start_time = DateTime.ParseExact (str, "yyyy-MM-dd.HHmmss", null);
				} else {
					throw new FormatException ();
				}
			} catch {
				Logger.Log.Warn ("Could not parse date/time from filename '{0}'", file.Name);
				start_time = file.CreationTime;
			}

			return start_time;
		}

		protected override void DoPullProperties ()
		{
			DateTime start_time = ParseStartingDate (FileInfo);
			AddProperty (Beagle.Property.NewDate ("fixme:starttime", start_time));

			// FIXME: We are dropping endtime in favor of performance
			// for the time being, until we can figure out how to get
			// it out of the log file more efficiently.
			//AddProperty (Beagle.Property.NewDate ("fixme:endtime", log.EndTime)); 

			AddProperty (Beagle.Property.NewUnsearched ("fixme:client", "Pidgin"));

			string protocol = FileInfo.Directory.Parent.Parent.Name;
			AddProperty (Beagle.Property.NewUnsearched ("fixme:protocol", protocol));

			// FIXME: Should the following properties use Property.NewKeyword and be searched?		       

			// Gaim likes to represent many characters in hex-escaped %xx form
			string speaking_to = StringFu.HexUnescape (FileInfo.Directory.Name);			
			AddProperty (Beagle.Property.NewKeyword ("fixme:speakingto", speaking_to));

			string identity = StringFu.HexUnescape (FileInfo.Directory.Parent.Name);
			AddProperty (Beagle.Property.NewUnsearched ("fixme:identity", identity));
		}

		protected override void DoPull ()
		{
			string line = TextReader.ReadLine ();

			if (String.IsNullOrEmpty (line)) {
				Error ();
				return;
			}

			TextReader reader = base.TextReader;

			// If this is a HTML log, use our totally awesome HTML
			// removing TextReader :-)
			if (line [0] == '<')
				reader = new HtmlRemovingReader (TextReader);

			while ((line = reader.ReadLine ()) != null) {
				string text = null;

				try {
					text = ProcessLine (line);
				} catch (Exception e) {
					Logger.Log.Error ("Could not parse line in: '{0}'", FileInfo.FullName);
					Logger.Log.Error (e);
				}

				if (!String.IsNullOrEmpty (text)) {
					AppendText (text);
					AppendWhiteSpace ();
				}
			}

			reader.Close ();
			Finished ();
		}

		private string ProcessLine (string line)
		{
			if (line.Length == 0)
				return null;

			// Sample line:
			// (13:50:07) LukasLipka: I wish I was a cowboy!

			int i = line.IndexOf (')');

			// If this is true, then the line probably spills
			// from the previous one
			if (line [0] != '(' || i == -1 || line.Length <= i + 2) {				
				return line;
			}

			// Cut off the date and continue with extracting the
			// alias and text
			line = line.Substring (i + 2);

			// This is an "action message"
			if (line.StartsWith ("***")) {
				i = line.IndexOf (' ');
				return line.Substring (i + 1);
			}

			// Figure out where the alias ends so that we can cut it off.
			// Also pay attention so that we don't break on aliases that
			// have a ':' in their name.
			i = line.IndexOf (": ");
			if (i == -1 || line.Length <= i + 2)
				return null;
			
			return line.Substring (i + 2);
		}
	}
}
