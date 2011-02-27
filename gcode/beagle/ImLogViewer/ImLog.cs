//
// ImLog.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using Mono.Unix.Native;

using Beagle.Util;

namespace ImLogViewer {

	public enum ImClient {
		Pidgin,
		Kopete,
		Konversation
	}

	public abstract class ImLog {

		public delegate void Sink (ImLog imLog);

		public string Client;
		public FileInfo File;
		public TextReader TextReader;
		public string Protocol;

		public DateTime StartTime;
		public DateTime EndTime;

		public string SpeakingTo;
		public string Identity;

		private Hashtable speakerHash = new Hashtable ();
		
		public class Utterance {
			private long timestamp;

			public DateTime Timestamp {
				get { return NativeConvert.ToDateTime (timestamp); }
				set { timestamp = NativeConvert.FromDateTime (value); }
			}
			
			public String Who;
			public String Text;
		}
		private ArrayList utterances = new ArrayList ();

		//////////////////////////

		protected ImLog (string client, FileInfo file, TextReader reader)
		{
			Client = client;
			TextReader = reader;
			File = file;
		}

		public ICollection Speakers {
			get { return speakerHash.Keys; }
		}

		public IList Utterances {
			get { return utterances; }
		}

		protected void AddUtterance (DateTime timestamp, string who, string text)
		{
			Utterance utt = new Utterance ();
			utt.Timestamp = timestamp;
			utt.Who = who;
			utt.Text = text.Trim ();

			if (StartTime.Ticks == 0 || StartTime > timestamp)
				StartTime = timestamp;

			if (EndTime.Ticks == 0 || EndTime < timestamp)
				EndTime = timestamp;

			speakerHash [who] = true;

			utterances.Add (utt);
		}

		protected void AppendToPreviousUtterance (string text)
		{
			if (utterances.Count > 0) {
				Utterance utt = (Utterance) utterances [utterances.Count - 1];
				utt.Text += "\n" + text;
			}
		}

		protected void ClearUtterances ()
		{
			utterances.Clear ();
		}

		protected abstract void Load ();
	}

	///////////////////////////////////////////////////////////////////////////////

	//
	// Pidgin Logs (formerly Gaim)
	//

	public class PidginLog : ImLog {

		public PidginLog (FileInfo file, TextReader reader) : base ("Pidgin", file, reader)
		{
			string filename = file.Name;

			// Parse what we can from the file path
			try {
				string str;

				// Character at position 17 will be either a dot, indicating the beginning
				// of the extension for old gaim logs, or a plus or minus indicating a
				// timezone offset for new gaim logs.
				if (filename [17] == '+' || filename [17] == '-') {
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

					str = filename.Substring (0, 20) + ':' + filename.Substring (20, 2);
					StartTime = DateTime.ParseExact (str, "yyyy-MM-dd.HHmmsszzz", null);
				} else if (filename [17] == '.') {
					// Older gaim format.
					//
					// 2006-02-21-160424.html

					str = Path.GetFileNameWithoutExtension (filename);
					StartTime = DateTime.ParseExact (str, "yyyy-MM-dd.HHmmss", null);
				} else {
					throw new FormatException ();
				}
			} catch {
				Logger.Log.Warn ("Could not parse date/time from filename '{0}'", file.Name);
				StartTime = DateTime.Now;
			}

			// Gaim likes to represent many characters in hex-escaped %xx form
			SpeakingTo = StringFu.HexUnescape (file.Directory.Name);
			Identity = StringFu.HexUnescape (file.Directory.Parent.Name);

			Protocol = file.Directory.Parent.Parent.Name;

			Load ();
		}

		// Return true if a new utterance is now available,
		// and false if the previous utterance was changed.
		private void ProcessLine (string line)
		{
			if (line.Length == 0)
				return;

			if (line [0] != '(') {
				AppendToPreviousUtterance (line);
				return;
			}
			int j = line.IndexOf (')');
			if (j == -1) {
				AppendToPreviousUtterance (line);
				return;
			}

			// Gaim 2.0
			// The new version of Gaim adds AM or PM right after the time
			// 1.x: (19:07:07)
			// 2.0: (19:07:07 AM)

			string when = line.Substring (1, j-1);
			DateTime timestamp;
			
			try {
				DateTime time = DateTime.Parse (when);
				
				timestamp = new DateTime (StartTime.Year, StartTime.Month, StartTime.Day,
						time.Hour, time.Minute, time.Second);

				// Try to deal with time wrapping around.
				if (timestamp < EndTime)
					timestamp.AddDays (1);
			} catch {
				// If something goes wrong, this line probably
				// spills over from the previous one.
				AppendToPreviousUtterance (line);
				return;
			}

			line = line.Substring (j+2);

			// Extract the alias
			string alias, text;
			int i;

			if (line.StartsWith ("***")) {
				i = line.IndexOf (' ');

				alias = line.Substring (3, i - 3);
				text = line.Substring (i + 1);
			} else {
				i = line.IndexOf (": ");
				if (i == -1 || line.Length < i + 2)
					return;

				alias = line.Substring (0, i);
				text = line.Substring (i + 2);
			}

			AddUtterance (timestamp, alias, text);

			return;
		}

		protected override void Load ()
		{
			ClearUtterances ();

			string line = TextReader.ReadLine (); // throw away first line

			if (line == null)
				return;

			TextReader reader = base.TextReader;

			// Could the second line ever start w/ < in a non-html log?
			// I hope not!
			if (line.Length > 0 && line [0] == '<')
				reader = new HtmlRemovingReader (TextReader);
				
			while ((line = reader.ReadLine ()) != null) {
				try {
					ProcessLine (line);
				} catch (Exception e) {
					Logger.Log.Warn ("Could not parse line in '{0}'", File.FullName);
					Logger.Log.Warn (e);
				}
			}
		}
	}

	///////////////////////////////////////////////////////////////////////////////

	//
	// Kopete Logs
	//
	public class KopeteLog : ImLog {

		public KopeteLog (FileInfo file, TextReader reader) : base ("Kopete", file, reader)
		{
			// FIXME: Artificially split logs into conversations depending on the
			// amount of time elapsed betweet messages?
			
			// Figure out the protocol from the parent.parent or parent foldername
			if (file.Directory.Parent.Name.EndsWith ("Protocol"))
				Protocol = file.Directory.Parent.Name.Substring (0, file.Directory.Parent.Name.Length - 8).ToLower ();
			else if (file.Directory.Name.EndsWith ("Protocol"))
				Protocol = file.Directory.Name.Substring (0, file.Directory.Name.Length - 8).ToLower ();
			else
				Protocol = file.Directory.Name;
			Identity = file.Directory.Name;

			// FIXME: This is not safe for all kinds of file/screennames
			string filename = Path.GetFileNameWithoutExtension (file.Name);
			if (filename.LastIndexOf ('.') > 0)
				SpeakingTo = filename.Substring (0, filename.LastIndexOf ('.'));
			else if (filename.LastIndexOf ('_') > 0)
				SpeakingTo = filename.Substring (0, filename.LastIndexOf ('_'));
			else
				SpeakingTo = filename;
			Logger.Log.Debug ("Speakingto for " + file.Name + " is " + SpeakingTo + ", protocol is " + Protocol);
			Load ();
		}
		
		private const string date_format = "yyyy M d H:m:s";

		protected override void Load ()
		{
			if (File.Length == 0)
				return;

			ClearUtterances ();

			XmlReader reader;
			DateTime base_date = DateTime.MinValue;

			try {
				reader = new XmlTextReader (File.Open(
									     FileMode.Open,
									     FileAccess.Read,
									     FileShare.Read));
			} catch (Exception e) {
				Log.Debug (e, "Could not open '{0}'", File.FullName);
				return;
			}
			
			while (reader.Read ()) {
				if (reader.NodeType != XmlNodeType.Element)
					continue;
				
				switch (reader.Name) {
				case "date":
					base_date = new DateTime (Convert.ToInt32 (reader.GetAttribute ("year")),
								  Convert.ToInt32 (reader.GetAttribute ("month")),
								  1);
					break;
					
				case "msg":
					// Parse the timestamp of the message
					string timestamp = String.Format ("{0} {1} {2}",
									  base_date.Year,
									  base_date.Month,
									  reader.GetAttribute ("time"));
					int time_separator_count = 0;
					foreach (int the_char in timestamp)
						if (the_char == ':')
							time_separator_count++;
					if (time_separator_count < 2)
						timestamp = timestamp + ":00";

					DateTime msg_date = DateTime.MinValue;

					try {
						msg_date = DateTime.ParseExact (timestamp,
										date_format,
										null);
					} catch {
						Logger.Log.Error ("Couldn't parse Kopete timestamp: {0}", timestamp);
						break;
					}
					
					string who = reader.GetAttribute ("nick");
					if (who == null || who == "")
						who = reader.GetAttribute ("from");
					if (who == null || who == "")
						break;
					
					// Advance to the text node for the actual message
					reader.Read ();
					
					AddUtterance (msg_date, who, reader.Value);
					break;
				}
			}
			
			reader.Close ();
		}

	}

	///////////////////////////////////////////////////////////////////////////////

	//
	// Konversation Logs
	//
	public class KonversationLog : ImLog {

		public KonversationLog (FileInfo file) : base ("Konversation", file, null)
		{
			Protocol = "IRC";
			Identity = "Me"; // FIXME - read from config file

			SpeakingTo = Path.GetFileNameWithoutExtension (file.Name);

			Logger.Log.Debug ("Speakingto for " + file.Name + " is " + SpeakingTo);
			Load ();
		}
		
		public const string LogTimeFormatString = "[ddd MMM d yyyy] [HH:mm:ss]";

		protected override void Load ()
		{
			ClearUtterances ();

			StringBuilder line_sb = null;
			LineReader reader = new ReencodingLineReader (File.FullName, Encoding.Default);

			string dt_string, text;
			string speaker = String.Empty;

			try {
				while ((line_sb = reader.ReadLineAsStringBuilder ()) != null) {
					if (! ProcessLine (line_sb, out text, out dt_string, ref speaker))
						continue;

					try {
						DateTime dt = DateTime.ParseExact (
							dt_string,
							LogTimeFormatString,
							CultureInfo.CurrentCulture,
							DateTimeStyles.AssumeLocal);

						AddUtterance (dt, speaker, text);
					} catch (FormatException) { }
				}
			} catch {
				if (line_sb != null)
					Log.Debug ("Caught exception while parsing line [{0}] from {1}", line_sb.ToString (), File.FullName);
				else
					Log.Debug ("Caught exception while parsing (null) line from {0}", File.FullName);
			} finally {
				reader.Close ();
			}
		}

		// Returns true if the line is a correct chat line, false otherwise
		// If the line is correct, text contains the text of the chat line,
		// dt_string contains the date string line and if speaker was set to not-null,
		// speaker contains the name of the speaker
		public static bool ProcessLine (StringBuilder log_line_sb, out string text, out string dt_string, ref string speaker)
		{
			text = dt_string = null;

			// Proper log line looks like
			//
			//[Mon Nov 1 2005] [14:09:32] <dBera>    can yo...

			int bracket_begin_index, bracket_end_index;

			// Ignore empty lines or non-chat lines
			if (log_line_sb.Length == 0 || log_line_sb [0] != '[')
				return false;
			bracket_begin_index = 0;

			bracket_end_index = IndexOfSB (log_line_sb, ']', bracket_begin_index + 1);
			if (bracket_end_index == -1)
				return false;

			bracket_end_index += 2;
			if (bracket_end_index >= log_line_sb.Length || log_line_sb [bracket_end_index] != '[')
				return false;

			bracket_end_index += 9;
			if (bracket_end_index >= log_line_sb.Length || log_line_sb [bracket_end_index] != ']')
				return false;

			// Ignore lines like '[Tue Nov 8 2005] [17:53:14]   * joe nods'
			// Good line should have name of speaker '<foobar>' after the time
			bracket_begin_index = bracket_end_index + 2;
			if (bracket_begin_index >= log_line_sb.Length || log_line_sb [bracket_begin_index] != '<')
				return false;

			dt_string = log_line_sb.ToString (0, bracket_end_index + 1);

			// Search for name of speaker '<foobar>'
			bracket_end_index = IndexOfSB (log_line_sb, '>', bracket_begin_index + 1);
			if (bracket_end_index == -1)
				return false;

			if (speaker != null)
				speaker = log_line_sb.ToString (bracket_begin_index + 1, bracket_end_index - bracket_begin_index - 1);

			text = log_line_sb.ToString (bracket_end_index + 1, log_line_sb.Length - bracket_end_index - 1);
			//Console.WriteLine ("[{0}] {1}", dt_string, text);

			return true;
		}

		private static int IndexOfSB (StringBuilder sb, char c, int begin_index)
		{
			int pos = -1;
			for (int i = begin_index; i < sb.Length; ++i) {
				if (sb [i] != c)
					continue;
				pos = i;
				break;
			}

			return pos;
		}

		public static void ParseFilename (string filename, out string server, out string channel)
		{
			server = channel = null;

			if (string.IsNullOrEmpty (filename))
				return;

			filename = Path.GetFileNameWithoutExtension (filename);
			int index_ = filename.IndexOf ('_');
			if (index_ == -1) {
				channel = filename;
			} else {
				server = filename.Substring (0, index_);
				channel = filename.Substring (index_ + 1);
			}
		}
	}
}

