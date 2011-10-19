//
// TextCache.cs
//
// Copyright (C) 2004 Novell, Inc.
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

using Mono.Data.Sqlite;
using ICSharpCode.SharpZipLib.GZip;

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

	public class TextCache : IDisposable {

		private const string SELF_CACHE_TAG = "*self*";
		private const string BLOB_TAG = "*blob*";
		private SqliteCommand InsertCommand;
		private SqliteCommand LookupPathCommand;
		private SqliteCommand LookupDataCommand;
		private SqliteCommand DeleteCommand;
		private string text_cache_dir;
		internal string TextCacheDir {
			get { return text_cache_dir; }
		}

		private SqliteConnection connection;

		private enum TransactionState {
			None,
			Requested,
			Started
		}
		private TransactionState transaction_state;

		private static TextCache user_cache = null;

		public static TextCache UserCache {
			get {
				if (user_cache == null)
					user_cache = new TextCache (PathFinder.StorageDir);
				
				return user_cache;
			}
		}

		public TextCache (string storage_dir) : this (storage_dir, false) { }

		public TextCache (string storage_dir, bool read_only)
		{
			text_cache_dir = Path.Combine (storage_dir, "TextCache");
			if (! Directory.Exists (text_cache_dir)) {
				Directory.CreateDirectory (text_cache_dir);
				
				// Create our cache subdirectories.
				for (int i = 0; i < 256; ++i) {
					string subdir = i.ToString ("x");
					if (i < 16)
						subdir = "0" + subdir;
					subdir = Path.Combine (text_cache_dir, subdir);
					Directory.CreateDirectory (subdir);
				}
			}
			
			// Create our Sqlite database
			string db_filename = Path.Combine (text_cache_dir, "TextCache.db");
			bool create_new_db = false;
			if (! File.Exists (db_filename))
				create_new_db = true;

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
				connection = Open (db_filename);
			} catch (ApplicationException) {
				Logger.Log.Warn ("Likely sqlite database version mismatch trying to open {0}.  Purging.", db_filename);
				create_new_db = true;
			}

			if (!create_new_db) {
				// Run a dummy query to see if we get a NOTADB error.  Sigh.
				SqliteCommand command;
				SqliteDataReader reader = null;

				command = new SqliteCommand ();
				command.Connection = connection;
				command.CommandText =
					"SELECT filename FROM textcache_data WHERE uri='blah'";

				try {
					reader = SqliteUtils.ExecuteReaderOrWait (command);
				} catch (ApplicationException ex) {
					Logger.Log.Warn ("Likely sqlite database version mismatch trying to read from {0}.  Purging.", db_filename);
					create_new_db = true;
				} catch (SqliteException ex) {
					// When the table name changed from 0.2.18 -> 0.3.0.
					Logger.Log.Warn ("Sqlite error: {0}. Purging textcache.", ex.Message);
					create_new_db = true;
				}

				if (reader != null)
					reader.Dispose ();
				command.Dispose ();
			}
			
			if (create_new_db) {
				if (connection != null)
					connection.Dispose ();

				if (read_only)
					throw new UnauthorizedAccessException (String.Format ("Unable to create read only text cache {0}", db_filename));

				File.Delete (db_filename);

				try {
					connection = Open (db_filename);
				} catch (Exception e) {
					Log.Debug (e, "Exception opening text cache {0}", db_filename);
				}

				// Database schema: uri, filename, data
				SqliteUtils.DoNonQuery (connection,
							"CREATE TABLE textcache_data (     " +
							"  uri      TEXT UNIQUE NOT NULL,  " +
							"  filename TEXT NOT NULL,         " +
							"  data     BLOB                   " +
							")");
			}
			this.InitCommands ();
		}

		private void InitCommands ()
		{
			InsertCommand = new SqliteCommand (this.connection);
			InsertCommand.CommandText = "INSERT OR REPLACE INTO textcache_data (uri, filename, data) VALUES (@uri,@filename,@data)";
			LookupPathCommand = new SqliteCommand (this.connection);
			LookupPathCommand.CommandText = "SELECT filename FROM textcache_data WHERE uri=@uri";
			LookupDataCommand = new SqliteCommand (this.connection);
			LookupDataCommand.CommandText = "SELECT filename, data FROM textcache_data WHERE uri=@uri";
			DeleteCommand = new SqliteCommand (this.connection);
			DeleteCommand.CommandText = "DELETE FROM textcache_data WHERE uri=@uri";

		}

		private SqliteConnection Open (string db_filename)
		{
			SqliteConnection connection = new SqliteConnection ();
			connection.ConnectionString = "version=3,encoding=UTF-8,URI=file:" + db_filename;
			connection.Open ();
			return connection;
		}

		public void Dispose ()
		{
			InsertCommand.Dispose ();
			LookupPathCommand.Dispose ();
			LookupDataCommand.Dispose ();
			DeleteCommand.Dispose ();
			connection.Dispose ();
			connection = null;
		}

		private static string UriToString (Uri uri)
		{
			return UriFu.UriToEscapedString (uri).Replace ("'", "''");
		}

		private SqliteCommand NewCommand (string format, params object [] args)
		{
			SqliteCommand command;
			command = new SqliteCommand ();
			command.Connection = connection;
			command.CommandText = String.Format (format, args);
			return command;
		}

		private void Insert (Uri uri, string filename, byte[] data)
		{
			lock (connection) {
				MaybeStartTransaction_Unlocked ();
				InsertCommand.Parameters.AddWithValue ("@uri",UriToString (uri));
				InsertCommand.Parameters.AddWithValue ("@filename",filename);
				InsertCommand.Parameters.AddWithValue ("@data", data);
				SqliteUtils.DoNonQuery (InsertCommand);

			}
		}

		private string LookupPathRaw (Uri uri)
		{
			lock (connection)
				return LookupPathRawUnlocked (uri);
		}

		// Returns raw path as stored in the db i.e. relative path wrt the text_cache_dir
		private string LookupPathRawUnlocked (Uri uri)
		{
			//SqliteCommand command;
			string path = null;
			LookupPathCommand.Parameters.AddWithValue ("@uri", UriToString (uri));
			using (SqliteDataReader reader = SqliteUtils.ExecuteReaderOrWait (LookupPathCommand)) {
				if (SqliteUtils.ReadOrWait (reader))
					path = reader.GetString (0);
			}

			return path;
		}

		public void MarkAsSelfCached (Uri uri)
		{
			lock (connection)
				Insert (uri, SELF_CACHE_TAG, null);
		}

		private bool world_readable = false;
		public bool WorldReadable {
			get { return this.world_readable; }
			set { this.world_readable = value; }
		}

		private class TextCacheWriteStream : Stream {
			public delegate void StreamClosedHandler (byte[] buffer);

			const int BLOB_SIZE_LIMIT = 4 * 1024; // 4 KB
			private byte[] buffer;
			private int buffer_pos;
			private Stream stream;
			private string path;
			private bool disposed = false;
			private bool world_readable = true;
			private StreamClosedHandler finished_handler;

			public TextCacheWriteStream (string path, bool world_readable, StreamClosedHandler finished_handler) {
				this.path = path;
				this.world_readable = world_readable;
				this.finished_handler = finished_handler;

				stream = null;
				buffer = new byte [BLOB_SIZE_LIMIT];
				buffer_pos = 0;
			}

			public override bool CanRead {
				get { return false; }
			}

			public override bool CanWrite {
				get { return true; }
			}

			public override bool CanSeek {
				get { return false; }
			}

			public override long Length {
				get { throw new NotSupportedException (); }
			}

			public override long Position {
				get { throw new NotSupportedException (); }
				set { throw new NotSupportedException (); }
			}

			public override void Close ()
			{
				// Initially buffer is not null and stream is null
				// Later, exactly one of them is not null
				if (stream != null)
					stream.Close ();
				else
					Array.Resize (ref buffer, buffer_pos);

				finished_handler (buffer);
				buffer = null;
				buffer_pos = 0;
				disposed = true;
			}

			public override void Flush ()
			{
				CheckObjectDisposedException ();

				if (stream != null)
					stream.Flush ();
			}

			public override long Seek (long offset, SeekOrigin origin)
			{
				throw new NotSupportedException ();
			}

			public override void SetLength (long value)
			{
				throw new NotSupportedException ();
			}

			public override int Read (byte[] array, int offset, int count)
			{
				throw new NotSupportedException ();
			}

			private void StoreOnDisk ()
			{
				if (stream != null)
					throw new Exception ("Already writing to a file on disk.");

				//Log.Debug ("Large cached text, storing in file {0}", path);
				FileStream file_stream = new FileStream (path, FileMode.Create, FileAccess.Write, FileShare.ReadWrite);
				// We don't expect to need this again in the near future.
				FileAdvise.FlushCache (file_stream);
			
				if (! world_readable) {
					// Make files only readable by the owner.
					Mono.Unix.Native.Syscall.chmod (
						path,
						Mono.Unix.Native.FilePermissions.S_IRUSR |
						Mono.Unix.Native.FilePermissions.S_IWUSR);
				}

				stream = file_stream;
				stream.Write (buffer, 0, buffer_pos);

				buffer_pos = 0;
				buffer = null;
			}

			public override void Write (byte[] array, int offset, int count)
			{
				if (stream == null) {
					if (buffer_pos + count < BLOB_SIZE_LIMIT) {
						Array.Copy (array, offset, buffer, buffer_pos, count);
						buffer_pos += count;
						return;
					}
					StoreOnDisk ();
				}

				stream.Write (array, offset, count);
			}

			private void CheckObjectDisposedException ()
			{
			        if (disposed) {
			                throw new ObjectDisposedException ("TextCacheWriteStream", "Stream is closed");
			        }
			}
		}

		public TextWriter GetWriter (Uri uri)
		{
			string local_path = LookupPathRaw (uri);

			// LookupPathRaw returns local path, if NOT self_cached
			if (local_path == SELF_CACHE_TAG)
				throw new ArgumentException ("uri", String.Format ("Cannot return TextCache writer for self-cached file {0}", uri));

			// If there is no path, create new prospective path, by called Guid.NewGuid()
			if (local_path == null || local_path == BLOB_TAG) {
				string guid = Guid.NewGuid ().ToString ();
				local_path = Path.Combine (guid.Substring (0, 2), guid.Substring (2));
			}

			// Return TextCacheWriteStream (Uri, path, handler)
			Stream stream;
			stream = new TextCacheWriteStream (Path.Combine (text_cache_dir, local_path),
							   world_readable,
							   delegate (byte[] buffer)
							   {
								CachedDataWriteFinished (uri, local_path, buffer);
							   });

			// But after wrapping it with compression
			stream = new GZipOutputStream (stream);

			StreamWriter writer;
			writer = new StreamWriter (new BufferedStream (stream));
			return writer;
		}

		private void CachedDataWriteFinished (Uri uri, string path, byte[] buffer)
		{
			// In handler,
			// - if buffer is null, write <uri, path, null> in db
			if (buffer == null) {
				Insert (uri, path, null);
				//Log.Debug ("Storing {0} on disk: {1}", uri, path);
			} else {
			// - if buffer is not null, remove earlier file at path and write <uri, ** BLOB **, buffer> in db
				File.Delete (Path.Combine (text_cache_dir, path));
				Insert (uri, BLOB_TAG, buffer);
				//Log.Debug ("Storing {0} on db with {1} bytes", uri, buffer.Length);
			}
		}

		public void WriteFromReader (Uri uri, TextReader reader)
		{
			TextWriter writer;
			writer = GetWriter (uri);
			string line;
			while ((line = reader.ReadLine ()) != null)
				writer.WriteLine (line);
			writer.Close ();
		}

		public void WriteFromString (Uri uri, string str)
		{
			if (str == null) {
				Delete (uri);
				return;
			}

			TextWriter writer;
			writer = GetWriter (uri);
			writer.WriteLine (str);
			writer.Close ();
		}

		// Returns null if no snippet for this uri
		public TextReader GetReader (Uri uri)
		{
			bool self_cache = false;
			return GetReader (uri, ref self_cache);
		}

		// If self_cache is true when called, then self_cache will be set upon return
		public TextReader GetReader (Uri uri, ref bool self_cache)
		{
			byte[] blob = null;
			string filename = null;

			lock (connection) {
				
				LookupDataCommand.Parameters.AddWithValue ("@uri", UriToString (uri));
				using (SqliteDataReader reader = SqliteUtils.ExecuteReaderOrWait (LookupDataCommand)) {
					if (! SqliteUtils.ReadOrWait (reader)) {
						if (self_cache)
							self_cache = false;
						return null;
					}

					filename = reader.GetString (0);
					if (! reader.IsDBNull (1))
						blob = reader.GetValue (1) as byte [];
				}

			}

			if (filename == SELF_CACHE_TAG) {
				if (self_cache) {
					self_cache = true;
					return null;
				}

				if (! uri.IsFile) {
					string msg = String.Format ("non-file uri {0} flagged as self-cached", uri);
					throw new Exception (msg);
				}
				return new StreamReader (uri.LocalPath);
			}

			if (self_cache)
				self_cache = false;

			if (filename == BLOB_TAG && (blob == null || blob.Length == 0))
				return null;

			Stream stream;
			if (filename == BLOB_TAG)
				stream = new MemoryStream (blob);
			else
				stream = new FileStream (Path.Combine (text_cache_dir, filename), FileMode.Open, FileAccess.Read, FileShare.ReadWrite);

			stream = new GZipInputStream (stream);
			TextReader text_reader = new StreamReader (new BufferedStream (stream));

			return text_reader;
		}

		public void Delete (Uri uri)
		{
			lock (connection) {
				string path = LookupPathRawUnlocked (uri);
				if (path != null) {
					MaybeStartTransaction_Unlocked ();
					DeleteCommand.Parameters.AddWithValue("@uri", UriToString (uri));
					SqliteUtils.DoNonQuery (DeleteCommand);
					if (path != SELF_CACHE_TAG && path != BLOB_TAG)
						File.Delete (Path.Combine (text_cache_dir, path));
				}
			}
		}


		private void MaybeStartTransaction_Unlocked ()
		{
			if (transaction_state == TransactionState.Requested)
				SqliteUtils.DoNonQuery (connection, "BEGIN");
			transaction_state = TransactionState.Started;
		}

		public void BeginTransaction ()
		{
			lock (connection) {
				if (transaction_state == TransactionState.None)
					transaction_state = TransactionState.Requested;
			}
		}

		public void CommitTransaction ()
		{
			lock (connection) {
				if (transaction_state == TransactionState.Started)
					SqliteUtils.DoNonQuery (connection, "COMMIT");
				transaction_state = TransactionState.None;
			}
		}
	}
}
