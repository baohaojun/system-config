//
// SqliteUtils.cs
//
// Copyright (C) 2004-2006 Novell, Inc.
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
using System.Threading;

using Mono.Data.Sqlite;

namespace Beagle.Util {

	public class SqliteUtils {

		// static class
		private SqliteUtils () { }

		public static int DoNonQuery (SqliteConnection connection, string command_text, string[] param_names, object[] param_args)
		{
			int ret = 0;

			using (SqliteCommand command = new SqliteCommand ()) {
				command.Connection = connection;
				command.CommandText = command_text;

				if (param_names != null) {
					if (param_args == null || param_names.Length != param_args.Length)
						throw new ArgumentException ("param_names, param_args", "param_names and param_args should have same number of items");
					for (int i = 0; i < param_names.Length; ++i)
						command.Parameters.AddWithValue (param_names [i], param_args [i]);
				}

				while (true) {
					try {
						ret = command.ExecuteNonQuery ();
						break;
					} catch (SqliteBusyException ex) {
						Thread.Sleep (50);
					} catch (Exception e) {
						Log.Error (e, "SQL that caused the exception: {0}", command_text);
						throw;
					}
				}
			}

			return ret;
		}
			
		public static int DoNonQuery (SqliteConnection connection, string command_text)
		{
			return DoNonQuery (connection, command_text, null, null);
		}

		public static int DoNonQuery (SqliteCommand command)
		{
			int ret = 0;
			while (true) {
				try {
					ret = command.ExecuteNonQuery ();
					break;
				} catch (SqliteBusyException ex) {
					Thread.Sleep (50);
				} catch (Exception e) {
					Log.Error ( e, "SQL that caused the exception: {0}", command.CommandText);
					throw;
				}
			}
			return ret;
		}
		

		public static SqliteDataReader ExecuteReaderOrWait (SqliteCommand command)
		{
			SqliteDataReader reader = null;
			while (reader == null) {
				try {
					reader = command.ExecuteReader ();
				} catch (SqliteBusyException ex) {
					Thread.Sleep (50);
				}
			}
			return reader;
		}

		public static bool ReadOrWait (SqliteDataReader reader)
		{
			while (true) {
				try {
					return reader.Read ();
				} catch (SqliteBusyException ex) {
					Thread.Sleep (50);
				}
			}
		}

		public static string Sanitize (string item)
		{
			return item.Replace ("'", "''");
		}
	}
}

		
