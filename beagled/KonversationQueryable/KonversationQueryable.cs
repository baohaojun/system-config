//
// KonversationQueryable.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Text;
using System.Threading;
using System.Globalization;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.KonversationQueryable {

	// FIXME: Absolutely requires Inotify currently
	[QueryableFlavor (Name="Konversation", Domain=QueryDomain.Local, RequireInotify=true)]
	public class KonversationQueryable : LuceneFileQueryable {

		private string log_dir;
		private Dictionary<string, long> session_offset_table;
		private ArrayList initial_log_files;

		public KonversationQueryable () : base ("KonversationIndex")
		{
			//log_dir = Path.Combine (PathFinder.HomeDir, "konv");
			log_dir = Path.Combine (PathFinder.HomeDir, ".kde");
			log_dir = Path.Combine (log_dir, "share");
			log_dir = Path.Combine (log_dir, "apps");
			log_dir = Path.Combine (log_dir, "konversation");
			log_dir = Path.Combine (log_dir, "logs");
		}

		public override void Start () 
		{
			base.Start ();
			
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private void StartWorker ()
		{
			if (! Directory.Exists (log_dir)) {
				GLib.Timeout.Add (300000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			Log.Info ("Starting konversation backend; using log files from {0}", log_dir);

			session_offset_table = new Dictionary<string, long> ();

			if (Inotify.Enabled)
				Inotify.Subscribe (log_dir, OnInotify,
						    Inotify.EventType.Create |
						    Inotify.EventType.Modify);

			initial_log_files = new ArrayList (Directory.GetFiles (log_dir));
			Log.Debug ("Konversation backend: found {0} log files", initial_log_files.Count);

			IsIndexing = true;
			LogIndexableGenerator generator = new LogIndexableGenerator (this, log_dir);
			Scheduler.Task task = NewAddTask (generator);
			task.Tag = log_dir;
			task.Source = this;
			ThisScheduler.Add (task);
		}

		// FIXME: Improve this by storing the data on disk. Then scan the data on startup
		// and compare with the last modified time or file length to determine if a complete
		// rescan is needed.
		private void UpdateLogOffset (string log_file)
		{
			FileInfo fi = new FileInfo (log_file);
			UpdateLogOffset (log_file, fi.Length);
			lock (initial_log_files) {
				initial_log_files.Remove (log_file);
			}
		}

		internal void UpdateLogOffset (string log_file, long session_start_position)
		{
			//Log.Debug ("Asked to store offset for {0} = {1}", log_file, session_start_position);

			if (! session_offset_table.ContainsKey (log_file))
				session_offset_table [log_file] = session_start_position;

			long stored_pos = session_offset_table [log_file];
			if (stored_pos < session_start_position)
				session_offset_table [log_file] = session_start_position;
		}

		private void OnInotify (Inotify.Watch watch,
					string path, string subitem, string srcpath,
					Inotify.EventType type)
		{
			long offset = 0;
			path = Path.Combine (path, subitem);
			if (ThisScheduler.ContainsByTag (path)) {
				Log.Debug ("Not adding task for already running task: {0}", path);
				return;
			}

			lock (initial_log_files) {
				if (initial_log_files.Contains (path)) {
					Log.Debug ("{0} is already scheduled for initial indexing", path);
					return;
				}
			}

			if (session_offset_table.ContainsKey (path))
				offset = session_offset_table [path];

			SessionIndexableGenerator generator = new SessionIndexableGenerator (this, path, offset);
			Scheduler.Task task = NewAddTask (generator);
			task.Tag = path;
			task.Source = this;
			ThisScheduler.Add (task);
		}

		// Returns true if the line is a correct chat line, false otherwise
		// If the line is correct, text contains the text of the chat line,
		// dt_string contains the date string line and if speaker was set to not-null,
		// speaker contains the name of the speaker
		private static bool ProcessLine (StringBuilder log_line_sb, out string text, out string dt_string, ref string speaker)
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
		
		private static void ParseFilename (string filename, out string server, out string channel)
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

		// To balance system load, use a nested IIndexableGenerator
		// LogIndexableGenerator iterates over the log files,
		// and then for each log file, it schedules the sessions one after another
		// This will take a while to index, but your machine will be happy.
		private class LogIndexableGenerator : IIndexableGenerator {

			private KonversationQueryable queryable;
			private SessionIndexableGenerator generator;
			private string[] files;
			private int file_index;
			private string log_dir;

			public LogIndexableGenerator (KonversationQueryable q, string log_dir)
			{
				this.queryable = q;
				this.files = Directory.GetFiles (log_dir);
				this.file_index = 0;
				this.log_dir = log_dir;
				this.generator = null;
			}

			private bool MoveToNextFile ()
			{
				while (file_index < files.Length) {
					if (files [file_index] != "konversation.log") {
						if (! queryable.IsUpToDate (files [file_index]))
							break;
						else
							queryable.UpdateLogOffset (files [file_index]);
					}
					file_index ++;
				}

				if (file_index == files.Length)
					return false;

				return true;
			}

			public bool HasNextIndexable ()
			{
				if (generator != null && generator.HasNextIndexable ())
					return true;

				// Move to the next file
				if (! MoveToNextFile ()) {
					queryable.IsIndexing = false;
					return false;
				}

				generator = new SessionIndexableGenerator (queryable, files [file_index], 0);
				file_index ++;
				if (! generator.HasNextIndexable ()) {
					queryable.IsIndexing = false;
					return false;
				}

				return true;
			}

			public Indexable GetNextIndexable ()
			{
				return generator.GetNextIndexable ();
			}

			public void PostFlushHook ()
			{
				generator.PostFlushHook ();
			}

			public string StatusName {
				get {
					if (generator == null)
						return log_dir;
					else
						return generator.StatusName;
				}
			}
		}

		private class SessionIndexableGenerator : IIndexableGenerator {

			private KonversationQueryable queryable;
			private string log_file;
			private LineReader reader;
			private StringBuilder log_line_as_sb;
			private StringBuilder data_sb;
			private Dictionary<string, bool> speakers; // list of speakers in the session
			private string server_name, speaking_to;

			// Split log into 6 hour sessions or 50 lines, which ever is larger
			private DateTime session_begin_time;
			private DateTime session_end_time;
			private long session_begin_offset;
			private long prev_line_offset; // stores the offset of the previous line read by reader
			private long session_num_lines;

			private const string time_format_string = "[ddd MMM d yyyy] [HH:mm:ss]";

			public SessionIndexableGenerator (KonversationQueryable queryable, string log_file, long offset)
			{
				this.queryable = queryable;
				this.log_file = log_file;
				this.session_begin_offset = offset;
				this.prev_line_offset = offset;

				this.data_sb = new StringBuilder ();
				this.log_line_as_sb = null;
				this.reader = null;
				this.session_begin_time = DateTime.MinValue;
				this.speakers = new Dictionary<string, bool> (10); // rough default value

				ParseFilename (Path.GetFileName (log_file), out server_name, out speaking_to);
				Log.Debug ("Reading from konversation log {0} (server={1}, channel={1})", log_file, server_name, speaking_to);
			}

			public void PostFlushHook ()
			{
				//Log.Debug ("Storing reader position {0}", session_begin_offset);
				queryable.UpdateLogOffset (log_file, session_begin_offset);
			}

			public string StatusName {
				get { return log_file; }
			}

			public bool HasNextIndexable ()
			{
				data_sb.Length = 0;
				session_num_lines = 0;
				speakers.Clear ();

				if (reader == null) {
					// Log files are in system encoding
					reader = new ReencodingLineReader (log_file, Encoding.Default);
					reader.Position = session_begin_offset;
					log_line_as_sb = reader.ReadLineAsStringBuilder ();
					//Log.Debug ("Read line from {0}:[{1}]", log_file, log_line_as_sb);
				}

				if (log_line_as_sb == null) {
					reader.Close ();
					return false;
				} else {
					// Update session_begin_offset
					session_begin_offset = prev_line_offset;
				}

				return true;
			}

			public Indexable GetNextIndexable ()
			{
				DateTime line_dt = DateTime.MinValue;

				while (log_line_as_sb != null) {
					//Log.Debug ("Checking line from {0}:[{1}]", log_file, log_line_as_sb);
					bool in_session = AppendLogText (log_line_as_sb, out line_dt);
					if (! in_session)
						break;

					prev_line_offset = reader.Position;
					log_line_as_sb = reader.ReadLineAsStringBuilder ();
				}

				// Check if there is new data to index
				if (data_sb.Length == 0) {
					session_begin_time = line_dt;
					return null;
				}

				Uri uri = new Uri (String.Format ("konversation://{0}@dumb/{1}", session_begin_offset, log_file));
				Log.Debug ("Creating indexable {0}", uri);
				Indexable indexable = new Indexable (uri);
				indexable.ParentUri = UriFu.PathToFileUri (log_file);
				indexable.Timestamp = session_begin_time;
				indexable.HitType = "IMLog";
				indexable.CacheContent = false;
				indexable.Filtering = IndexableFiltering.AlreadyFiltered;

				indexable.AddProperty (Beagle.Property.NewUnsearched ("fixme:session_begin_offset", session_begin_offset));
				indexable.AddProperty (Beagle.Property.NewUnsearched ("fixme:session_end_offset", prev_line_offset));
				indexable.AddProperty (Beagle.Property.NewDate ("fixme:starttime", session_begin_time));
				indexable.AddProperty (Beagle.Property.NewUnsearched ("fixme:client", "Konversation"));
				indexable.AddProperty (Beagle.Property.NewUnsearched ("fixme:protocol", "IRC"));

				AddChannelInformation (indexable);

				foreach (string speaker in speakers.Keys)
					indexable.AddProperty (Beagle.Property.NewUnstored ("fixme:speaker", speaker));

				StringReader data_reader = new StringReader (data_sb.ToString ());
				indexable.SetTextReader (data_reader);

				// update session begin time to the date of the current line which is not in this session
				session_begin_time = line_dt;

				return indexable;
			}

			// Returns false if log_line belonged to next session and was not appended
			// line_dt is set to the timestamp of the log_line
			private bool AppendLogText (StringBuilder log_line_as_sb, out DateTime line_dt)
			{
				line_dt = DateTime.MinValue;

				string dt_string, text;
				string speaker = String.Empty;
				bool is_good_line = ProcessLine (log_line_as_sb, out text, out dt_string, ref speaker);

				if (! is_good_line)
					return true;

				try {
					line_dt = DateTime.ParseExact (
						dt_string,
						time_format_string,
						CultureInfo.CurrentCulture,
						DateTimeStyles.AssumeLocal);
				} catch (FormatException) {
					// Old log files had date strings as
					// [11:05:08] <berkus>  ...
					return true;
				}

				// On first scan, set the session_begin_time
				if (session_begin_time == DateTime.MinValue) {
					session_begin_time = new DateTime (
						line_dt.Year,
						line_dt.Month,
						line_dt.Day,
						line_dt.Hour,
						0,
						0);
					session_end_time = line_dt;
					//Log.Debug ("Adding session begin time {0}", DateTimeUtil.ToString (session_begin_time));
					AddLine (text, speaker);
					return true;
				}

				// If we are on a different day then make a new session
				if (session_begin_time.Day != line_dt.Day)
					return false;
				// or if more than 50 useful lines were seen in this session
				else if (session_num_lines > 50) {
					// Split session in 6 hour = 360 min interval
					TimeSpan ts = line_dt - session_begin_time;
					if (ts.TotalMinutes > 360)
						return false;
				}

				AddLine (text, speaker);
				session_end_time = line_dt;

				return true;
			}

			private void AddLine (string text, string speaker)
			{
				//Log.Debug ("Adding [{0}]", text);
				data_sb.Append (text);
				data_sb.Append (' ');
				session_num_lines ++;

				speakers [speaker] = true;
			}

			private void AddChannelInformation (Indexable indexable)
			{
				// FIXME: Parse identity information from konversation .config file
				//AddProperty (Beagle.Property.NewUnsearched ("fixme:identity", log.Identity));

				// Get server name, channel name from the filename and add it here
				indexable.AddProperty (Beagle.Property.NewKeyword ("fixme:server", server_name));
				indexable.AddProperty (Beagle.Property.NewKeyword ("fixme:speakingto", speaking_to));
			}
		}

		private bool CheckForExistence ()
		{
			if (!Directory.Exists (log_dir))
				return true;

			this.Start ();

			return false;
		}

		override public ISnippetReader GetSnippet (string [] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			if (hit.ParentUri == null)
				return null;

			string path = hit.ParentUri.LocalPath;

			string snippet = null;
			long begin_offset = Convert.ToInt64 (hit ["fixme:session_begin_offset"]);
			long end_offset = Convert.ToInt64 (hit ["fixme:session_end_offset"]);

			try {
				LineTextReader reader;
				reader = new LineTextReader (path, begin_offset, end_offset);

				return SnippetFu.GetSnippet (query_terms, reader, full_text, ctx_length, snp_length);
			} catch {
				return null;
			}

			return null;
		}

		internal class LineTextReader : TextReader {
			private LineReader reader;
			private long end_offset;
			private StringBuilder sb;
	    
			public LineTextReader (string path, long begin_offset, long end_offset)
			{
				this.reader = new ReencodingLineReader (path);
				this.end_offset = end_offset;
				this.reader.Position = begin_offset;
			}

			public override string ReadLine ()
			{
				if (reader.Position >= end_offset)
					return null;

				string dt_string, speaker = String.Empty, text = null;
				bool good_line = false;

				while (! good_line) {
					sb = reader.ReadLineAsStringBuilder ();
					if (sb == null)
						return null;

					good_line = ProcessLine (sb, out text, out dt_string, ref speaker);
				}

				return String.Format ("[{0}] {1}", speaker, text);
			}

			public override void Close ()
			{
				reader.Close ();
			}
		}
	}
}

