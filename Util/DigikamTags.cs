//
// DigikamTags.cs
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2006 Novell, Inc.
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

// Big FIXME: There is a basic problem in retrieving information from Digikam DB.
// If some metadata is added to the DB, the DB will be changed and beagle
// will pick up the changed information _only_ when it indexes the file in
// question. Which means, under normal circumstances, if a user adds some
// information/tags to an image and the image file is not modified on disk,
// beagle will _not_ get the updated information for that file.
// Is there any way to force beagle to re-index the file ? Maybe send an
// IndexableChange message or something like that ... simply doing that
// might cause double indexing of the file.

using System;
using System.Collections;
using System.IO;
using Mono.Data.Sqlite;

namespace Beagle.Util {

	public static class DigikamTags {
		
		public class DigikamData {
			public string caption;
			private ArrayList tags = null;
			// FIXME: Icon ? KdeIcon ?

			public IEnumerable Tags {
				get { return tags; }
			}

			internal void AddTag (string tag) {
				if (tags == null)
					tags = new ArrayList ();
				tags.Add (tag);
			}
		}

		private static string album_path = null;
		private static string db_path = null;
		private static string digikamrc = null;
		private const string tags_sql_string = @"SELECT images.caption, tags.name, tags.pid
FROM albums, images, imagetags, tags 
WHERE images.id=imagetags.imageid AND imagetags.tagid=tags.id AND albums.id=images.dirid AND 
albums.url=@AlbumsUrl AND images.name=@ImagesName";
		private const string tags_parenttags_sql_string = @"SELECT t1.name, t2.name 
FROM tags t1, tags t2 
WHERE t1.pid=t2.id";

		// need to cache a lot of things
		private static DateTime db_last_modified_date;
		private static SqliteConnection connection = null;
		private static Hashtable tag_parent_cache = null;

		// Every value has to be a property since they need to be checked everytime.
		// User may install/start using digikam in the middle of running beagle.
		private static string DBPath {
			get {
				if (db_path != null)
					return db_path;
				
				// FIXME: Also check the last write time of digikamrc.
				string digikamrc = Path.Combine (PathFinder.HomeDir, ".kde");
				digikamrc = Path.Combine (digikamrc, "share");
				digikamrc = Path.Combine (digikamrc, "config");
				digikamrc = Path.Combine (digikamrc, "digikamrc");

				//Console.WriteLine ("Checking digikamrc at :" + digikamrc);
				if ( ! File.Exists (digikamrc))
					return null;

				StreamReader reader = new StreamReader (digikamrc);
				string section = "";
				string line;
				while ((line = reader.ReadLine ()) != null) {
					if (line.StartsWith ("[") && line.EndsWith ("]")) {
						section = line;
					}
					if (section == "[Album Settings]") {
						if (line.StartsWith ("Album Path=") && line.Length > 11) {
							album_path = StringFu.ExpandEnvVariables (line.Substring(11));
						}
					}
				}

				// FIXME: Also support the old-age sqlite-2 based digikam.db ?
				db_path = Path.Combine (album_path, "digikam3.db");

				// remove trailing '/'
				if (album_path.EndsWith ("/"))
					album_path = album_path.Substring (0, album_path.LastIndexOf ("/"));

				//Console.WriteLine ("db should be at:" + db_path);
				return db_path;
			}
		}

		private static SqliteConnection Connection {
			get {
				if (connection != null) {
					/*
					// FIXME: Following lines will enable refreshing cache information
					// if digikam3.db is modified. Not sure how beagled will be able
					// to handle the situation when a user is on a marathon tagging
					// mission. Beware fragile! Take care.
					if (File.GetLastWriteTimeUtc (album_path) <= db_last_modified_date)
						return connection;
					else {
						connection.Close ();
						connection = null;
						tag_parent_cache.Clear ();
						tag_parent_cache = null;
					}
					*/
					return connection;
				}
				
				if (File.Exists (DBPath)) {
					db_last_modified_date = File.GetLastWriteTimeUtc(db_path);
					connection = new SqliteConnection ();
					connection.ConnectionString = "URI=file:" + db_path + ",version=3";
					connection.Open ();
				}

				return connection;
			}
		}

		private static bool IsPresent {
			get { return (Connection != null); }
		}

		public static DigikamData GetDigikamData (string path)
		{
			//Console.WriteLine ("Fetching digikam information about:" + path);
			if (! IsPresent)
				return null;

			if (! path.StartsWith (album_path))
				return null;
			
			string filename = Path.GetFileName (path);
			string dirname = Path.GetDirectoryName (path);
			string relative_path = dirname.Remove (0, album_path.Length);
			//Console.WriteLine ("location=[" + relative_path + "]/[" + filename + "]");

			SqliteCommand command = new SqliteCommand ();
			command.Connection = Connection;
			command.CommandText = tags_sql_string;
			command.Parameters.AddWithValue ("@AlbumsUrl", relative_path);
			command.Parameters.AddWithValue ("@ImagesName", filename);
			//Console.WriteLine (tags_sql_string);
			
			SqliteDataReader reader = command.ExecuteReader ();

			DigikamData imagedata = null;
			ArrayList original_tags = null;
			if (reader.Read ()) {
				imagedata = new DigikamData ();
				original_tags = new ArrayList ();
				imagedata.caption = (string) reader [0];
				original_tags.Add ((string) reader [1]);
				//Console.WriteLine ("Adding tag {0}", (string) reader [1]);
				//Console.WriteLine ("Found caption:" + imagedata.caption);
			}
			while (reader.Read ()) {
				//Console.WriteLine ("Adding more tag {0}", (string) reader [1]);
				original_tags.Add ((string) reader [1]);
			}

			reader.Close ();
			reader = null;
			command.Dispose ();
			command = null;

			if (imagedata == null)
				return null;
			SortedList s_list = new SortedList ();
			foreach (string orig_tag in original_tags) {
				string tag = orig_tag;
				while ( tag != null && tag != String.Empty) {
					if (s_list.ContainsKey (tag)) {
						break;
					}
					//Console.WriteLine ("Found tag: " + tag);
					s_list.Add (tag, null);
					imagedata.AddTag (tag);
					tag = GetParentTag (tag);
				}
			}

			return imagedata;
		}

		private static string GetParentTag (string tag)
		{
			if (! IsPresent)
				return null;

			if (tag_parent_cache != null)
				return (string) tag_parent_cache [tag];

			tag_parent_cache = new Hashtable ();
			SqliteCommand command = new SqliteCommand ();
			command.Connection = Connection;
			command.CommandText = tags_parenttags_sql_string;

			SqliteDataReader reader = command.ExecuteReader ();
			while (reader.Read ()) {
			    string name = (string) reader [0];
			    string parent_name = (string) reader [1];
			    tag_parent_cache [name] = parent_name;
			}

			command.Dispose ();

			return (string) tag_parent_cache [tag];
		}
		
	}
}
