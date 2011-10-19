//
// InternalUriManager.cs
//
// Copyright (C) 2005-2007 Novell, Inc.
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

using Lucene.Net.Documents;
using Lucene.Net.Index;
using LNS = Lucene.Net.Search;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	//
	// This is just a LuceneQueryingDriver with the ability to do the
	// special secondary-index-only queries we need to map internal uris
	// back to filenames.
	//

	public class LuceneNameResolver : LuceneQueryingDriver {

		public class NameInfo {
			public Guid   Id;
			public Guid   ParentId;
			public string Name;
			public bool   IsDirectory;
		}

		public LuceneNameResolver (string index_name, int minor_version, bool read_only)
			: base (index_name, minor_version, read_only)
		{

		}

		////////////////////////////////////////////////////////////////

		// Ouch! Hardcoding prefix is not good
		static FieldSelector fields_nameinfo = new MapFieldSelector (new string[] {"Uri",
											   "prop:k:" + Property.ExactFilenamePropKey,
											   "prop:k:" + Property.ParentDirUriPropKey,
											   "prop:k:" + Property.IsDirectoryPropKey
										});

		private NameInfo DocumentToNameInfo (Document doc)
		{
			NameInfo info;
			info = new NameInfo ();

			string str;
			str = doc.Get ("Uri");
			info.Id = GuidFu.FromUriString (str);

			bool have_name = false;
			bool have_parent_id = false;
			bool have_is_dir = false;

			foreach (Field f in doc.Fields ()) {
				Property prop;
				prop = GetPropertyFromDocument (f, doc, false);
				if (prop == null)
					continue;

				switch (prop.Key) {
					
				case Property.ExactFilenamePropKey:
					info.Name = prop.Value;
					have_name = true;
					break;
					
				case Property.ParentDirUriPropKey:
					info.ParentId = GuidFu.FromUriString (prop.Value);
					have_parent_id = true;
					break;

				case Property.IsDirectoryPropKey:
					info.IsDirectory = (prop.Value == "true");
					have_is_dir = true;
					break;
				}

				if (have_name && have_parent_id && have_is_dir)
					break;
			}

			return info;
		}

		////////////////////////////////////////////////////////////////

		public NameInfo GetNameInfoById (Guid id)
		{
			Uri uri;
			uri = GuidFu.ToUri (id);

			IndexReader reader;
			reader = LuceneCommon.GetReader (SecondaryStore);

			TermDocs term_docs;
			term_docs = reader.TermDocs ();

			Term term = new Term ("Uri", UriFu.UriToEscapedString (uri));
			term_docs.Seek (term);

			int match_id = -1;
			if (term_docs.Next ())
				match_id = term_docs.Doc ();

			term_docs.Close ();

			NameInfo info = null;

			if (match_id != -1) {
				Document doc;
				doc = reader.Document (match_id, fields_nameinfo);
				info = DocumentToNameInfo (doc);
			}

			LuceneCommon.ReleaseReader (reader);
			
			return info;
		}

		////////////////////////////////////////////////////////////////

		public Guid GetIdByNameAndParentId (string name, Guid parent_id)
		{
			string parent_uri_str;
			parent_uri_str = GuidFu.ToUriString (parent_id);

			string key1, key2;

			key1 = PropertyToFieldName (PropertyType.Keyword, Property.ParentDirUriPropKey);
			key2 = PropertyToFieldName (PropertyType.Keyword, Property.ExactFilenamePropKey);

			Term term1, term2;

			term1 = new Term (key1, parent_uri_str);
			term2 = new Term (key2, name.ToLower ());

			// Lets walk the exact file name terms first (term2)
			// since there are probably fewer than parent directory
			// Uri terms.
			List <int> term2_doc_ids = new List <int> ();

			IndexReader reader = LuceneCommon.GetReader (SecondaryStore);
			TermDocs term_docs = reader.TermDocs ();

			term_docs.Seek (term2);
			while (term_docs.Next ())
				term2_doc_ids.Add (term_docs.Doc ());

			term_docs.Seek (term1);
			
			int match_id = -1;

			while (term_docs.Next ()) {
				int doc_id = term_docs.Doc ();

				if (term2_doc_ids.BinarySearch (doc_id) >= 0) {
					match_id = doc_id;
					break;
				}
			}

			term_docs.Close ();

			Guid id;
			if (match_id != -1) {
				Document doc;
				doc = reader.Document (match_id);
				id = GuidFu.FromUriString (doc.Get ("Uri"));
			} else 
				id = Guid.Empty;

			LuceneCommon.ReleaseReader (reader);

			return id;
		}

		////////////////////////////////////////////////////////////////

		// Pull all of the directories out of the index and cache them

		// Not to be confused with LuceneQueryingDriver.BitArrayHitCollector
		private class BitArrayHitCollector : LNS.HitCollector {

			private BetterBitArray matches;
			
			public BitArrayHitCollector (BetterBitArray matches)
			{
				this.matches = matches;
			}

			public override void Collect (int id, float score)
			{
				matches [id] = true;
			}
		}

		// Return all directories with name
		public ICollection GetAllDirectoryNameInfo (string name)
		{
			// First we assemble a query to find all of the directories.
			string field_name;
			field_name = PropertyToFieldName (PropertyType.Keyword,
							  Property.IsDirectoryPropKey);
			LNS.Query isdir_query = new LNS.TermQuery (new Term (field_name, "true"));

			LNS.Query query = null;

			if (name == null) {
				query = isdir_query;
			} else {
				string dirname_field;
				dirname_field = PropertyToFieldName (PropertyType.Text,
								     Property.TextFilenamePropKey);
				LNS.Query dirname_query;
				dirname_query = LuceneCommon.StringToQuery (dirname_field, name, null);
				LNS.BooleanQuery bool_query = new LNS.BooleanQuery ();
				bool_query.Add (isdir_query, LNS.BooleanClause.Occur.MUST);
				bool_query.Add (dirname_query, LNS.BooleanClause.Occur.MUST);

				query = bool_query;
			}

			// Then we actually run the query
			LNS.IndexSearcher searcher;
			//searcher = new LNS.IndexSearcher (SecondaryStore);
			searcher = LuceneCommon.GetSearcher (SecondaryStore);

			BetterBitArray matches;
			matches = new BetterBitArray (searcher.MaxDoc ());

			BitArrayHitCollector collector;
			collector = new BitArrayHitCollector (matches);

			searcher.Search (query, null, collector);
			
			// Finally we pull all of the matching documents,
			// convert them to NameInfo, and store them in a list.

			ArrayList match_list = new ArrayList ();
			int i = 0;
			while (i < matches.Count) {
				
				i = matches.GetNextTrueIndex (i);
				if (i >= matches.Count)
					break;

				Document doc;
				doc = searcher.Doc (i, fields_nameinfo);

				NameInfo info;
				info = DocumentToNameInfo (doc);

				match_list.Add (info);

				++i;
			}

			LuceneCommon.ReleaseSearcher (searcher);

			return match_list;
		}
	}

	// This class knows how to get/set internal uris.
	//
	public class UidManager {
		private FileAttributesStore fa_store;
		// This is just a copy of the LuceneQueryable's QueryingDriver
		// cast into the right type for doing internal->external Uri
		// lookups.
		private LuceneNameResolver name_resolver;

		public UidManager (FileAttributesStore fa_store,
				   LuceneQueryingDriver driver)
		{
			this.fa_store = fa_store;
			this.name_resolver = (LuceneNameResolver) driver;
		}

		////////////////////////////////////////////////////////////////////////

		// cached_uid_by_path contains the <uid,path> mapping for every new file
		// since it is scheduled till PostAddHook (when it is confirmed that the
		// file was added)
		// FIXME: Replace Hashtable by Dictionary<string,Guid>
		private Hashtable cached_uid_by_path = new Hashtable ();

		internal bool HasNewId (string path) {
			return cached_uid_by_path.Contains (path);
		}

		internal Guid GetNewId (string path) {
			if (cached_uid_by_path.Contains (path))
				return (Guid) cached_uid_by_path [path];
			else
				return Guid.Empty;
		}

		internal void RegisterNewId (string name, DirectoryModel dir, Guid id)
		{
			//Log.Debug ("Registering {0}={1}", name, GuidFu.ToShortString (id));
			cached_uid_by_path [Path.Combine (dir.FullName, name)] = id;
		}

		internal void ForgetNewId (string path)
		{
			//Log.Debug ("Forgetting {0}", path);
			cached_uid_by_path.Remove (path);
		}

		internal Guid ReadOrCreateNewId (DirectoryModel dir, string name)
		{
			Guid old_guid = NameAndParentToId (name, dir);

			if (old_guid != Guid.Empty)
				return old_guid;

			return CreateNewId (Path.Combine (dir.FullName, name));
		}

		internal Guid CreateNewId (string path)
		{
			if (cached_uid_by_path.Contains (path))
				return (Guid) cached_uid_by_path [path];

			Guid new_guid = Guid.NewGuid ();
			cached_uid_by_path [path] = new_guid;
			return new_guid;
		}

		//////////////////////////////////////////////////////////////////////////

		internal Guid GetIdByNameAndParentId (string name, Guid parent_id)
		{
			return name_resolver.GetIdByNameAndParentId (name, parent_id);
		}

		// This works for files.  (It probably works for directories
		// too, but you should use one of the more efficient means
		// above if you know it is a directory.)
		// This is mostly used for getting uid for deleted files
		internal Guid NameAndParentToId (string name, DirectoryModel dir)
		{
			string path;
			path = Path.Combine (dir.FullName, name);

			Guid unique_id;
			if (cached_uid_by_path.Contains (path))
				unique_id = (Guid) cached_uid_by_path [path];
			else
				unique_id = name_resolver.GetIdByNameAndParentId (name, dir.UniqueId);

			return unique_id;
		}

		internal LuceneNameResolver.NameInfo GetNameInfoById (Guid id)
		{
			return name_resolver.GetNameInfoById (id);
		}

		///////////////////////////////////////////////////////////////////////////////

		internal ICollection GetAllDirectoryNameInfo (string dir_with_name)
		{
			return name_resolver.GetAllDirectoryNameInfo (dir_with_name);
		}

		internal ICollection GetAllDirectoryNameInfo ()
		{
			return GetAllDirectoryNameInfo (null);
		}

		internal void DebugHook ()
		{
			Log.Debug ("FSQ:InternalUriManager Debughook: {0} cached_uid_by_path", cached_uid_by_path.Count);
		}
	}
}

