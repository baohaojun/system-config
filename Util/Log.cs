//
// Log.cs
//
// Copyright (C) 2004-2005 Novell, Inc.
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
using System.Text;
using System.IO;
using System.Diagnostics;

namespace Beagle.Util {

	public enum LogLevel {
		Always,
		Error,
		Warn,
		Debug,
		Ignored
	}
		
	static public class Log {

		static private string log_directory;
		static private string log_name_prefix;
		static private string program_identifier;
		static private string program_identifier_truncated;

		// If we don't call Log.Initialize, these defaults ensure that
		// everything will just get spewed to the console.
		static private bool running_in_foreground = true; 
		static private LogLevel cutoff_level = LogLevel.Debug;

		static private TextWriter log_writer;
		static private TextWriter exception_writer;
		static private TextWriter foreground_echo_writer;

		static public LogLevel Level {
			get { return cutoff_level; }
			set { cutoff_level = value; }
		}

		static public void Initialize (string   log_directory,
					       string   program_identifier,
					       LogLevel cutoff_level,
					       bool     running_in_foreground)
		{
			Log.log_directory = log_directory;
			Log.program_identifier = program_identifier;
			Log.cutoff_level = cutoff_level;
			Log.running_in_foreground = running_in_foreground;

			if (! running_in_foreground)
				Console.WriteLine ("beagled will run in the background.\nUse beagle-status to check progress of beagled.\nFor log files check {0}/current-Beagle.\n", Log.log_directory);

			PruneOldLogs ();

			log_name_prefix = String.Format ("{0:yyyy-MM-dd-HH-mm-ss}-", DateTime.Now);

			if (program_identifier.Length > 6)
				program_identifier_truncated = program_identifier.Substring (0, 6);
			else
				program_identifier_truncated = program_identifier;

			log_writer = NewLogWriter (program_identifier);
			exception_writer = NewDelayedLogWriter (program_identifier + "Exceptions");
			
			TextWriter console_log_writer;
			console_log_writer = NewDelayedLogWriter (program_identifier + "Console");

			TextWriter console_redirect_writer;
			if (running_in_foreground) {
				foreground_echo_writer = Console.Out;
				console_redirect_writer = new TeeTextWriter (Console.Out, console_log_writer);
			} else {
				console_redirect_writer = console_log_writer;
			}

			// Redirect the console output to a special file
			Console.SetOut (console_redirect_writer);
			Console.SetError (console_redirect_writer);

			if (! running_in_foreground) {
				// Now redirect the *actual* stdout/stderr to our main
				// log file.  This is used to catch Mono crash reports
				// in our logs.  Note that this doesn't override the
				// console redirection above, which is good.
				StreamWriter sw = (StreamWriter) log_writer;
				FileStream fs = (FileStream) sw.BaseStream;
				int fd = (int) fs.Handle;
				Mono.Unix.Native.Syscall.dup2 (fd, 1); // stdout
				Mono.Unix.Native.Syscall.dup2 (fd, 2); // stderr

				// If we are running in the background, redirect stdin to /dev/null
				FileStream dev_null_stream = new FileStream ("/dev/null",
									     FileMode.Open,
									     FileAccess.Read,
									     FileShare.ReadWrite);
				TextReader dev_null_reader = new StreamReader (dev_null_stream);
				Console.SetIn (dev_null_reader);
			}
		}

		static public void Disable ()
		{
			cutoff_level = LogLevel.Ignored;
		}

		static private void PruneOldLogs ()
		{
			DateTime magic_date = DateTime.Now.AddDays (-7);
			DirectoryInfo dir = new DirectoryInfo (log_directory);

			string current_str;
			current_str = "current-" + program_identifier;
			
			foreach (FileInfo file in dir.GetFiles ()) {

				// Clean up old symlinks
				if (file.Name.StartsWith (current_str) && FileSystem.IsSymLink (file.FullName)) {
					// Work around a Mono bug in which FileInfo.Delete()
					// doesn't delete dangling symlinks.  See
					// http://bugzilla.ximian.com/show_bug.cgi?id=78664

					//file.Delete ();
					File.Delete (file.FullName);

					continue;
				}
				
				int last_dash = file.Name.LastIndexOf ("-");
				if (last_dash == -1)
					continue; // skip strange-looking files

				string date = file.Name.Substring (0, last_dash);

				try {
					DateTime log_date;
					log_date = DateTime.ParseExact (date, "yyyy-MM-dd-HH-mm-ss", null);
					if (log_date < magic_date)
						file.Delete ();
				} catch { }
			}				
		}

		static private TextWriter NewLogWriter (string name)
		{
			string log_path;
			log_path = Path.Combine (log_directory, log_name_prefix + name);

			FileStream stream;
			stream = new FileStream (log_path,
						 FileMode.CreateNew,
						 FileAccess.Write,
						 FileShare.ReadWrite);

			StreamWriter writer;
			writer = new StreamWriter (stream);
			writer.AutoFlush = true;

			string log_link;
			log_link = Path.Combine (log_directory, "current-" + name);
			Mono.Unix.Native.Syscall.symlink (log_path, log_link);

			return writer;
		}

		private class DelayedClosure {

			string name;

			public DelayedClosure (string name)
			{
				this.name = name;
			}

			public TextWriter Build ()
			{
				return NewLogWriter (name);
			}
		}

		static private TextWriter NewDelayedLogWriter (string name)
		{
			DelayedClosure closure;
			closure = new DelayedClosure (name);
			return new DelayedTextWriter (new DelayedTextWriter.Builder (closure.Build));
		}

		/////////////////////////////////////////////////////////////////////////////////////////

		class SimpleQueue {
			int pos;		// next write position in this queue
			int size;		// size of this SimpleQueue
			string[] buffer;	// the actual queue
			
			// Keep a buffer of last 4 big messages by default
			public SimpleQueue () : this (4) { }
			
			public SimpleQueue (int size)
			{
				this.size = size;
				this.buffer = new string [size];
				this.pos = 0;
			}

			// Return false if message is in the buffer.
			// Otherwise add the message to the buffer and return true.
			public bool Add (string msg)
			{
				for (int i = 0; i < size; i ++) {
					// null values are empty buffer positions
					// which should be no problem to compare with msg
					if (buffer [i] == msg)
						return false;
				}
				
				// write the new message to the current write position
				buffer [pos] = msg;
				
				// move the write position to the next position
				pos = (pos + 1) % size;

				return true;
			}

		}

		static private SimpleQueue msg_buffer = new SimpleQueue ();

		/////////////////////////////////////////////////////////

		static object write_lock = new object ();

		static private void WriteLine (LogLevel level, string format, object [] args, Exception ex) 
		{
			if (level != LogLevel.Always && cutoff_level < level)
				return;

			string msg_string = null;
			if (format != null)
				msg_string = String.Format (format, args);
			if (ex != null)
				msg_string = String.Concat (msg_string,
							    (msg_string != null ? "\n" : String.Empty),
							    ex);

			// Use the buffer only for multiline messages, which will mainly happen with exceptions
			if (ex != null && ! msg_buffer.Add (msg_string)) {
				msg_string = "(Repeated)";
				if (format != null) {
					msg_string = String.Format (format, args);
					int newline = msg_string.IndexOf ('\n');
					if (newline != -1) {
						msg_string = msg_string.Substring (0, newline);
						msg_string += " ...";
					}
				} else {
					msg_string = ex.Message;
				}

				msg_string = "(Repeated) " + msg_string;
			}

			// This only happens if Log.Initialize was never called.
			if (running_in_foreground && foreground_echo_writer == null)
				foreground_echo_writer = Console.Out;

			if (foreground_echo_writer != null) {
				foreground_echo_writer.Write (level);
				foreground_echo_writer.Write (": ");

				if (msg_string != null)
					foreground_echo_writer.WriteLine (msg_string);
				foreground_echo_writer.Flush ();
			}

			if (log_writer == null) // i.e. if Log.Initialize has not been called
				return;

			StringBuilder prefix_builder;
			prefix_builder = new StringBuilder ();
			prefix_builder.Append ('\n'); // start w/ a newline
			prefix_builder.AppendFormat ("{0:yyyyMMdd HH:mm:ss.ffff} {1:00000} ",
						     DateTime.Now, Process.GetCurrentProcess ().Id);
			prefix_builder.Append (program_identifier_truncated);

			prefix_builder.Append (' ');
			switch (level) {
			case LogLevel.Error:
				prefix_builder.Append ("ERROR");
				break;
			case LogLevel.Warn:
				prefix_builder.Append (" WARN");
				break;
			case LogLevel.Debug:
				prefix_builder.Append ("DEBUG");
				break;
			case LogLevel.Always:
				prefix_builder.Append (" INFO");
				break;
			default:
				prefix_builder.Append (" HUH?");
				break;
			}

			if (ex != null)
				prefix_builder.Append (" EX");
			prefix_builder.Append (": ");
			
			string prefix;
			prefix = prefix_builder.ToString ();
			
			StringBuilder message;
			message = new StringBuilder ();
			message.Append (prefix);
			message.Remove (0, 1); // remove leading \n
			if (msg_string != null)
				message.Append (msg_string);
			message.Replace ("\n", prefix);

			string message_str;
			message_str = message.ToString ();
			
			lock (write_lock) {
				log_writer.WriteLine (message_str);
				if (ex != null && exception_writer != null)
					exception_writer.WriteLine (message_str);
			}
		}

		/////////////////////////////////////////////////////////////////////////////////////////

		static public void Debug (string message, params object [] args)
		{
			WriteLine (LogLevel.Debug, message, args, null);
		}

		static public void Debug (Exception ex, string message, params object [] args)
		{
			WriteLine (LogLevel.Debug, message, args, ex);
		}

		static public void Debug (Exception ex)
		{
			WriteLine (LogLevel.Debug, null, null, ex);
		}

		static public void Info (string message, params object [] args)
		{
			// The Info log level is deprecated: just map it to Debug.
			Debug (message, args);
		}

		static public void Info (Exception ex, string message, params object [] args)
		{
			Debug (ex, message, args);
		}

		static public void Info (Exception ex)
		{
			Debug (ex);
		}

		static public void Warn (string message, params object [] args)
		{
			WriteLine (LogLevel.Warn, message, args, null);
		}

		static public void Warn (Exception ex, string message, params object [] args)
		{
			WriteLine (LogLevel.Warn, message, args, ex);
		}

		static public void Warn (Exception ex)
		{
			WriteLine (LogLevel.Warn, null, null, ex);
		}

		static public void Error (string message, params object [] args)
		{
			WriteLine (LogLevel.Error, message, args, null);
		}
		
		static public void Error (Exception ex, string message, params object [] args)
		{
			WriteLine (LogLevel.Error, message, args, ex);
		}

		static public void Error (Exception ex)
		{
			WriteLine (LogLevel.Error, null, null, ex);
		}

		static public void Always (string message, params object [] args)
		{
			WriteLine (LogLevel.Always, message, args, null);
		}
		
		static public void Always (Exception ex, string message, params object [] args)
		{
			WriteLine (LogLevel.Always, message, args, ex);
		}

		static public void Always (Exception ex)
		{
			WriteLine (LogLevel.Always, null, null, ex);
		}
	}
}

