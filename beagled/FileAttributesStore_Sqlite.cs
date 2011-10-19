//
// FileAttributesStore_Sqlite.cs
//
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
using System.Threading;

using Mono.Data.Sqlite;

using Beagle.Util;

namespace Beagle.Daemon {

	// FIXME: This class isn't multithread safe!  This class does not
	// ensure that different threads don't utilize a transaction started
	// in a certain thread at the same time.  However, since all the
	// writes to this database are handled by a single thread, this isn't
	// currently a problem, but it would have to be fixed if we wanted to
	// do this in the future.
	//
	// See http://bugzilla.gnome.org/show_bug.cgi?329022

	public class FileAttributesStore_Sqlite : IFileAttributesStore {

		// Version history:
		// 1: Original version
		// 2: Replaced LastIndexedTime with LastAttrTime
		// 3: Changed STRING to TEXT
		// 4: Use (directory, filename) as unique constraint
		const int VERSION = 4;

		private SqliteConnection connection;
		private BitArray path_flags;
		private int transaction_count = 0;
		public SqliteCommand ReadCommand;
		public SqliteCommand InsertCommand;
		public SqliteCommand DeleteCommand;
		enum TransactionState {
			None,
			Requested,
			Started
		}
		private TransactionState transaction_state;

		public FileAttributesStore_Sqlite (string directory, string index_fingerprint)
		{
			bool create_new_db = false;
			path_flags = new BitArray (65536);

			if (! File.Exists (GetDbPath (directory))) {
				create_new_db = true;
			} else {
				
				// Funky logic here to deal with sqlite versions.
				//
				// When sqlite 3 tries to open an sqlite 2 database,
				// it will throw an SqliteException with SqliteError
				// NOTADB when trying to execute a command.
				//
				// When sqlite 2 tries to open an sqlite 3 database,
				// it will throw an ApplicationException when it
				// tries to open the database.

				try {
					connection = Open (directory);
				} catch (ApplicationException) {
					Logger.Log.Warn ("Likely sqlite database version mismatch trying to open {0}.  Purging.", GetDbPath (directory));
					create_new_db = true;
				}

				if (! create_new_db) {
					SqliteCommand command;
					SqliteDataReader reader = null;
					int stored_version = 0;
					string stored_fingerprint = null;


					command = new SqliteCommand ();
					command.Connection = connection;
					command.CommandText =
						"SELECT version, fingerprint FROM db_info";
					try {
						reader = SqliteUtils.ExecuteReaderOrWait (command);
					} catch (Exception ex) {
						Logger.Log.Warn ("Likely sqlite database version mismatch trying to read from {0}.  Purging.", GetDbPath (directory));
						create_new_db = true;
					}
					if (reader != null && ! create_new_db) {
						if (SqliteUtils.ReadOrWait (reader)) {
							stored_version = reader.GetInt32 (0);
							stored_fingerprint = reader.GetString (1);
						}
						reader.Close ();
					}
					command.Dispose ();

					if (VERSION != stored_version
					    || (index_fingerprint != null && index_fingerprint != stored_fingerprint))
						create_new_db = true;
				}
			}

			if (create_new_db) {
				if (connection != null)
					connection.Dispose ();
				File.Delete (GetDbPath (directory));
				connection = Open (directory);

				SqliteUtils.DoNonQuery (connection,
							"CREATE TABLE db_info (             " +
							"  version       INTEGER NOT NULL,  " +
							"  fingerprint   TEXT NOT NULL    " +
							")");

				SqliteUtils.DoNonQuery (connection,
							"INSERT INTO db_info (version, fingerprint) VALUES (@version, @index_fingerprint)",
							new string [] {"@version", "@index_fingerprint"},
							new object [] {VERSION, index_fingerprint});

				SqliteUtils.DoNonQuery (connection,
							"CREATE TABLE file_attributes (         " +
							"  unique_id      TEXT NOT NULL,        " +
							"  directory      TEXT NOT NULL,        " +
							"  filename       TEXT NOT NULL,        " +
							"  last_mtime     TEXT NOT NULL,        " +
							"  last_attrtime  TEXT NOT NULL,        " +
							"  filter_name    TEXT NOT NULL,        " +
							"  filter_version TEXT NOT NULL,        " +
							"  UNIQUE (directory, filename)         " +
							")");
			} else {
				SqliteCommand command;
				SqliteDataReader reader;
				int count = 0;

				DateTime dt1 = DateTime.Now;

				// Select all of the files and use them to populate our bit-vector.
				command = new SqliteCommand ();
				command.Connection = connection;
				command.CommandText = "SELECT directory, filename FROM file_attributes";

				reader = SqliteUtils.ExecuteReaderOrWait (command);

				while (SqliteUtils.ReadOrWait (reader)) {

					string dir = reader.GetString (0);
					string file = reader.GetString (1);
					string path = Path.Combine (dir, file);
					SetPathFlag (path);
					++count;
				}

				reader.Close ();
				command.Dispose ();

				DateTime dt2 = DateTime.Now;

				Logger.Log.Debug ("Loaded {0} records from {1} in {2:0.000}s", 
						 count, GetDbPath (directory), (dt2 - dt1).TotalSeconds);
			}
			ReadCommand = new SqliteCommand (this.connection);
			ReadCommand.CommandText = "SELECT unique_id, directory, filename, last_mtime, last_attrtime, filter_name, filter_version " +
				"FROM file_attributes WHERE directory=@dir AND filename=@fname";
			InsertCommand = new SqliteCommand (this.connection);
			InsertCommand.CommandText = "INSERT OR REPLACE INTO file_attributes " +
							" (unique_id, directory, filename, last_mtime, last_attrtime, filter_name, filter_version) " +
							" VALUES (@unique_id, @directory, @filename, @last_mtime, @last_attrtime, @filter_name, @filter_version)";
			DeleteCommand = new SqliteCommand (this.connection);
			DeleteCommand.CommandText = "DELETE FROM file_attributes WHERE directory=@directory AND filename=@filename";
		}

		public void Dispose ()
		{
			ReadCommand.Dispose ();
			InsertCommand.Dispose ();
			DeleteCommand.Dispose ();
			connection.Dispose ();
			connection = null;
		}

		///////////////////////////////////////////////////////////////////

		private string GetDbPath (string directory)
		{
			return Path.Combine (directory, "FileAttributesStore.db");
		}

		private SqliteConnection Open (string directory)
		{
			SqliteConnection c;			
			c = new SqliteConnection ();
			c.ConnectionString = "version=3,encoding=UTF-8,URI=file:" + GetDbPath (directory);
			c.Open ();
			return c;
		}

		private FileAttributes GetFromReader (SqliteDataReader reader)
		{
			FileAttributes attr = new FileAttributes ();

			attr.UniqueId = GuidFu.FromShortString (reader.GetString (0));
			attr.Path = System.IO.Path.Combine (reader.GetString (1), reader.GetString (2));
			attr.LastWriteTime = StringFu.StringToDateTime (reader.GetString (3));
			attr.LastAttrTime = StringFu.StringToDateTime (reader.GetString (4));
			attr.FilterName = reader.GetString (5);
			attr.FilterVersion = int.Parse (reader.GetString (6));

			if (attr.FilterName == "")
				attr.FilterName = null;

			return attr;
		}

		///////////////////////////////////////////////////////////////////

		private int GetPathHash (string path)
		{
			uint hash = 0xdeadbeef;
			foreach (char c in path)
				hash = 17 * hash + (uint) c;
			// Fold the 32 bits in 16.
			return (int) ((hash & 0xffff) ^ (hash >> 16));
		}

		private bool GetPathFlag (string path)
		{
			int hash = GetPathHash (path);
			return path_flags [hash];
		}

		private void SetPathFlag (string path)
		{
			int hash = GetPathHash (path);
			path_flags [hash] = true;
		}

		///////////////////////////////////////////////////////////////////

		public FileAttributes Read (string path)
		{
			// Sanitize the path; remove the last '/'
			if (path != null && path != "/" && path.EndsWith ("/"))
				path = path.TrimEnd ('/');

			if (! GetPathFlag (path))
				return null;

			FileAttributes attr = null;

			// We need to quote any 's that appear in the strings
			// (int particular, in the path)
			string directory = FileSystem.GetDirectoryNameRootOk (path).Replace ("'", "''");
			string filename = Path.GetFileName (path).Replace ("'", "''");
			lock (connection) {
				ReadCommand.Parameters.AddWithValue ("@dir",directory);
				ReadCommand.Parameters.AddWithValue ("@fname",filename);
				using (SqliteDataReader reader = SqliteUtils.ExecuteReaderOrWait (ReadCommand)) {
					if (SqliteUtils.ReadOrWait (reader))
						attr = GetFromReader (reader);
				}
			}

			return attr;
		}

		public bool Write (FileAttributes fa)
		{
			SetPathFlag (fa.Path);
			int ret = 0;
			string filter_name;

			// We need to quote any 's that appear in the strings
			// (in particular, in the path)
			lock (connection) {
				
				// If a transaction has been requested, start it now.
				MaybeStartTransaction ();

				filter_name = fa.FilterName;
				if (filter_name == null)
					filter_name = "";
				filter_name = filter_name.Replace ("'", "''");
				string[] param= new string [] { "@unique_id", "@directory", "@filename", "@last_mtime", "@last_attrtime", "@filter_name", "@filter_version"};
				object[] vals =	new object [] {
								GuidFu.ToShortString (fa.UniqueId),
								fa.Directory.Replace ("'", "''"), fa.Filename.Replace ("'", "''"),
								StringFu.DateTimeToString (fa.LastWriteTime),
								StringFu.DateTimeToString (fa.LastAttrTime),
								filter_name,
								fa.FilterVersion};
				for (int i=0; i < param.Length; i++)
					InsertCommand.Parameters.AddWithValue (param[i], vals[i]);
				
				ret = SqliteUtils.DoNonQuery (InsertCommand);
				
			}

			return (ret != 0);
		}

		public void Drop (string path)
		{
			// Sanitize the path; remove the last '/'
			if (path != null && path != "/" && path.EndsWith ("/"))
				path = path.TrimEnd ('/');

			// We don't want to "UnSetPathFlag" here, since we have no way of knowing
			// if another path hashes to the same value as this one.

			// We need to quote any 's that appear in the strings
			// (in particular, in the path)
			string directory = FileSystem.GetDirectoryNameRootOk (path).Replace ("'", "''");
			string filename = Path.GetFileName (path).Replace ("'", "''");
			lock (connection) {

				// If a transaction has been requested, start it now.
				MaybeStartTransaction ();
				DeleteCommand.Parameters.AddWithValue ("@directory", directory);
				DeleteCommand.Parameters.AddWithValue ("@filename", filename);
				SqliteUtils.DoNonQuery (DeleteCommand);
			}
		}

		private void MaybeStartTransaction ()
		{
			if (transaction_state == TransactionState.Requested) {
				SqliteUtils.DoNonQuery (connection, "BEGIN");
				transaction_state = TransactionState.Started;
			}
		}

		public void BeginTransaction ()
		{
			if (transaction_state == TransactionState.None)
				transaction_state = TransactionState.Requested;
		}

		public void CommitTransaction ()
		{
			if (transaction_state == TransactionState.Started) {
				lock (connection)
					SqliteUtils.DoNonQuery (connection, "COMMIT");
			}
			transaction_state = TransactionState.None;
		}

		public void Flush ()
		{
			lock (connection) {
				if (transaction_count > 0) {
					Logger.Log.Debug ("Flushing requested -- committing sqlite transaction");
					SqliteUtils.DoNonQuery (connection, "COMMIT");
					transaction_count = 0;
				}
			}
		}

		///////////////////////////////////////////////////////////////////

		// Return all attributes in the attributes database, used for merging

		private ICollection ReadAllAttributes () 
		{
			ArrayList attributes = new ArrayList ();
			
			SqliteCommand command;
			SqliteDataReader reader;
				
			lock (connection) {
				command = new SqliteCommand ();
				command.Connection = connection;
				command.CommandText =
					"SELECT unique_id, directory, filename, last_mtime, last_attrtime, filter_name, filter_version " +
					"FROM file_attributes";
				
				reader = SqliteUtils.ExecuteReaderOrWait (command);
				
				while (SqliteUtils.ReadOrWait (reader)) {
					attributes.Add (GetFromReader (reader));
				}
				reader.Close ();
				command.Dispose ();
			}
			
			return attributes;
		}

		// FIXME: Might wanna do this a bit more intelligently

		public void Merge (FileAttributesStore_Sqlite fa_sqlite_store_to_merge) 
		{
			ICollection attributes = fa_sqlite_store_to_merge.ReadAllAttributes ();
				
			BeginTransaction ();

			foreach (FileAttributes attribute in attributes)
				Write (attribute);

			CommitTransaction ();
		}
	}
}
