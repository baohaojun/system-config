//
// FSpotTools.cs
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
using System.IO;
using System.Threading;
using Mono.Data.Sqlite;

namespace Beagle.Util {

	public class FSpotTools {

		public class Tag {
			public uint   Id;
			public string Name;
			public uint   CategoryId;
			public bool   IsCategory;
			public int    SortPriority;
			public Tag    Category;
			// FIXME: Icon
		}
		
		public class Photo {
			public uint   Id;
			public string Path;
			public string Description;
			public Tag[]  Tags;
		}

		static private bool tried_connection;
		static private SqliteConnection connection;
		static Hashtable tagCache = null;
		static Hashtable directoryCache = null;


		static private string PhotoStorePath {
			get {
				string home = Environment.GetEnvironmentVariable ("HOME");
				return Path.Combine (home, ".gnome2/f-spot/photos.db");
			}
		}

		static private SqliteConnection PhotoStoreConnection {
			get {
				if (! tried_connection && File.Exists (PhotoStorePath)) {
					connection = new SqliteConnection ();
					connection.ConnectionString = "version=3,URI=file:" + PhotoStorePath;

					// Try to open the f-spot store.  This
					// will fail if there is a version
					// mismatch.
					try {
						connection.Open ();

						// Run a dummy SELECT statement to catch more errors
						// indicating sqlite version mismatches.
						using (SqliteCommand command = new SqliteCommand ()) {
							command.Connection = connection;
							command.CommandText = "SELECT id FROM tags WHERE name='dummy'";
							SqliteDataReader reader = null;
							while (reader == null) {
								try {
									reader = command.ExecuteReader ();
								} catch (SqliteBusyException) {
									Thread.Sleep (50);
								}
							}
							reader.Close ();
						}
					} catch (ApplicationException) {
						Logger.Log.Warn ("Unable to open F-Spot database: no sqlite3 database found");
						connection = null;
					}

					tried_connection = true;
				}
			
				return connection;
			}
		}

		static private bool HavePhotoStore {
			get {
				return PhotoStoreConnection != null;
			}
		}

		// FIXME: We should expire this cache if the underlying db has changed.
		static private Tag GetTagById (uint id)
		{
			if (! HavePhotoStore)
				return null;

			if (tagCache == null) {
				tagCache = new Hashtable ();

				SqliteCommand command = new SqliteCommand ();
				command.Connection = PhotoStoreConnection;
				command.CommandText = "SELECT id, name, category_id, is_category, sort_priority FROM tags";
				
				SqliteDataReader reader = command.ExecuteReader ();
				while (reader.Read ()) {
					Tag tag = new Tag ();
					tag.Id = Convert.ToUInt32 (reader [0]);
					tag.Name = (string) reader [1];
					tag.CategoryId = Convert.ToUInt32 (reader [2]);
					tag.IsCategory = (((string)reader [3]) == "1");
					tag.SortPriority = Convert.ToInt32 (reader [4]);
					tagCache [tag.Id] = tag;
				}

				// Walk across all tags, linking to the category's Tag
				// object.  Since the tagCache is fully populated, it is
				// safe to call GetTagById here.
				foreach (Tag tag in tagCache.Values)
					tag.Category = GetTagById (tag.CategoryId);

				command.Dispose ();
			}
			
			return (Tag) tagCache [id];
		}

		// FIXME: We should expire this cache if the underlying db has changed.
		static bool IsPossibleDirectory (string directory)
		{
			if (! HavePhotoStore)
				return false;

			if (directoryCache == null) {
				directoryCache = new Hashtable ();
				
				SqliteCommand command = new SqliteCommand ();
				command.Connection = PhotoStoreConnection;
				command.CommandText = "SELECT DISTINCT directory_path FROM photos";

				SqliteDataReader reader = command.ExecuteReader ();
				while (reader.Read ()) {
					directoryCache [reader [0]] = true;
				}

				command.Dispose ();
			}

			return directoryCache.Contains (directory);
		}

		static public Photo GetPhoto (string path)
		{
			if (! HavePhotoStore)
				return null;

			path = Path.GetFullPath (path);
			string dir = Path.GetDirectoryName (path);
			string name = Path.GetFileName (path);
			
			if (! IsPossibleDirectory (dir))
				return null;

			SqliteCommand command;
			SqliteDataReader reader;

			command = new SqliteCommand ();
			command.Connection = PhotoStoreConnection;
			command.CommandText = String.Format ("SELECT id, description         " +
							     "FROM photos                    " +
							     "WHERE directory_path = \"{0}\" " +
							     "  AND name = \"{1}\"",
							     dir, name);

			Photo photo = null;
			reader = command.ExecuteReader ();
			if (reader.Read ()) {
				photo = new Photo ();
				photo.Path = path;
				photo.Id = Convert.ToUInt32 (reader [0]);
				photo.Description = (string) reader [1];
			}

			command.Dispose ();

			if (photo != null) {
				command = new SqliteCommand ();
				command.Connection = PhotoStoreConnection;
				command.CommandText = String.Format ("SELECT tag_id       " +
								     "FROM photo_tags     " + 
								     "WHERE photo_id = {0}",
								     photo.Id);

				Hashtable tagHash = new Hashtable ();

				// Mark the photo with both any tags and all parents.
				// Maybe this isn't the right thing to do, but it seems
				// to most closely mirror the tag semantics implied by f-spot.
				reader = command.ExecuteReader ();
				while (reader.Read ()) {
					uint id = Convert.ToUInt32 (reader [0]);
					Tag tag = GetTagById (id);
					while (tag != null) {
						tagHash [tag] = tag;
						tag = tag.Category;
					}
				}
				
				photo.Tags = new Tag [tagHash.Count];
				int i = 0;
				foreach (Tag t in tagHash.Values) {
					photo.Tags [i] = t;
					++i;
				}
			}

			return photo;
		}

	}
}
