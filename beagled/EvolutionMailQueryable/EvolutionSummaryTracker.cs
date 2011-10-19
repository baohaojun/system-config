//
// EvolutionSummaryTracker.cs
//
// Copyright (C) 2006 Novell, Inc.
//
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

using Mono.Data.Sqlite;

using Beagle.Util;

namespace Beagle.Daemon.EvolutionMailQueryable {

	public class EvolutionSummaryTracker : IDisposable {

		private SqliteConnection connection;

		public EvolutionSummaryTracker (string directory, string account_name, string folder_name)
		{
			// Make the on-disk files for folders have sane names
			folder_name = folder_name.Replace ('/', '-');
			folder_name = folder_name.Replace (':', '_');
			folder_name = folder_name.Replace (',', ' '); // Causes problems with the ConnectionString otherwise

			string filename = Path.Combine (directory, String.Format ("SummaryTracker-{0}-{1}.db", account_name, folder_name));
			bool create_new_db = ! File.Exists (filename);
			bool purge_old_db = false;

			connection = GetConnection (filename);
			try {
				connection.Open ();
			} catch (ApplicationException) {
				purge_old_db = true;
			}

			if (! create_new_db && ! purge_old_db) {
				// Run a dummy SELECT statement to catch more errors
				// indicating sqlite version mismatches.
				using (SqliteCommand command = new SqliteCommand ()) {
					command.Connection = connection;
					command.CommandText = "SELECT flags FROM mapping WHERE uid = 'fo/ky'";

					SqliteDataReader reader;

					try {
						reader = SqliteUtils.ExecuteReaderOrWait (command);
						reader.Close ();
					} catch (ApplicationException) {
						purge_old_db = true;
					}
				}
			}

			if (purge_old_db) {
				connection.Dispose ();

				// Purge the old database and create a new one
				File.Delete (filename);
				connection = GetConnection (filename);
				connection.Open ();

				create_new_db = true;
			}

			if (create_new_db)
				CreateDatabase ();

			// Start a transaction for any updates
			SqliteUtils.DoNonQuery (connection, "BEGIN");
		}

		private static SqliteConnection GetConnection (string filename)
		{
			SqliteConnection connection = new SqliteConnection ();
			connection.ConnectionString = String.Format ("version={0},encoding=UTF-8,URI=file:{1}", 3, filename);

			return connection;
		}

		private void CreateDatabase ()
		{
			SqliteUtils.DoNonQuery (connection,
						"CREATE TABLE mapping (        " +
						"  uid       TEXT    UNIQUE,   " +
						"  flags     INTEGER NOT NULL, " +
						"  last_seen TEXT    NOT NULL  " +
						")");

			SqliteUtils.DoNonQuery (connection,
						"CREATE INDEX mapping_uid on mapping (uid)");
		}

		public void Checkpoint ()
		{
			lock (connection) {
				// Commit any outstanding changes and open a new transaction
				SqliteUtils.DoNonQuery (connection, "COMMIT");
				SqliteUtils.DoNonQuery (connection, "BEGIN");
			}
		}

		public void Close ()
		{
			lock (connection) {
				// Commit any outstanding changes
				SqliteUtils.DoNonQuery (connection, "COMMIT");
			
				connection.Close ();
				connection = null;
			}
		}

		public void Dispose ()
		{
			Close ();
			GC.SuppressFinalize (this);
		}

		public void Update (string uid, uint flags)
		{
			lock (connection) {
				SqliteUtils.DoNonQuery (connection,
							"INSERT OR REPLACE INTO mapping " +
							"  (uid, flags, last_seen) " +
							"  VALUES (@uid, @flags, @last_seen)",
							new string [] {"@uid", "@flags", "@last_seen"},
							new object [] {uid, flags, StringFu.DateTimeToString (DateTime.UtcNow)});
			}
		}

		public bool Get (string uid, out uint flags)
		{
			SqliteCommand command;
			SqliteDataReader reader;

			lock (connection) {
				command = new SqliteCommand ();
				command.Connection = connection;
				command.CommandText = String.Format (
					"SELECT flags FROM mapping WHERE uid='{0}'",
					uid.Replace ("'", "''"));

				reader = SqliteUtils.ExecuteReaderOrWait (command);

				try {
					if (SqliteUtils.ReadOrWait (reader)) {
						flags = (uint) reader.GetInt32 (0);
						return true;
					} else {
						flags = 0;
						return false;
					}
				} finally {
					reader.Close ();
					command.Dispose ();
				}
			}
		}				

		public void Remove (string uid)
		{
			lock (connection) {
				SqliteUtils.DoNonQuery (connection,
							"DELETE FROM mapping WHERE uid=@uid",
							new string [] {"@uid"},
							new object [] {uid});
			}
		}

		// FIXME: I don't really like this, the collection could be huge
		public ArrayList GetOlderThan (DateTime dt)
		{
			SqliteCommand command;
			SqliteDataReader reader;

			lock (connection) {
				command = new SqliteCommand ();
				command.Connection = connection;
				command.CommandText =
					"SELECT uid FROM mapping WHERE last_seen < " +
					StringFu.DateTimeToString (dt);
				
				reader = SqliteUtils.ExecuteReaderOrWait (command);
				
				ArrayList uids = new ArrayList ();

				while (SqliteUtils.ReadOrWait (reader))
					uids.Add (reader.GetString (0));

				reader.Close ();
				command.Dispose ();
				
				return uids;
			}
		}
	}
}
