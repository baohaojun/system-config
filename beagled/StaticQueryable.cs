//
// StaticQueryable.cs
//
// Copyright (C) 2005 Novell, Inc.
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
using System.IO;
using System.Collections;
using System.Threading;

using System.Xml;
using System.Xml.Serialization;
	
using Beagle.Util;

namespace Beagle.Daemon {

	public class StaticQueryable : LuceneQueryable 	{

		protected TextCache text_cache = null;

		// If not null, then this is a removable index and
		// mount_dir is where the removable media is mounted
		private string mount_dir = null;

		public string RemovableMountDir {
			get { return mount_dir; }
		}

		public StaticQueryable (string index_name, string index_path, bool read_only_mode) : base (index_path, read_only_mode)
		{
			Logger.Log.Debug ("Initializing static queryable: {0}", index_path);

			if (Directory.Exists (Path.Combine (index_path, "TextCache"))) {
				try {
					text_cache = new TextCache (index_path, true);
				} catch (UnauthorizedAccessException) {
					Logger.Log.Warn ("Unable to purge static queryable text cache in {0}.  Will run without it.", index_path);
				}
			}
		}

		// Instantiates and loads a StaticQueryable from an index directory
		internal static Queryable LoadStaticQueryable (DirectoryInfo index_dir, QueryDomain query_domain)
		{
			StaticQueryable static_queryable = null;
			
			if (!index_dir.Exists)
				return null;
			
			try {
				static_queryable = new StaticQueryable (index_dir.Name, index_dir.FullName, true);
			} catch (InvalidOperationException) {
				Logger.Log.Warn ("Unable to create read-only index (likely due to index version mismatch): {0}", index_dir.FullName);
				return null;
			} catch (Exception e) {
				Logger.Log.Error (e, "Caught exception while instantiating static queryable: {0}", index_dir.Name);
				return null;
			}

			if (static_queryable == null)
				return null;

			// Load StaticIndex.xml from index_dir.FullName/config
			string config_file_path = Path.Combine (index_dir.FullName, "StaticIndex.xml");
			Config static_index_config;
			try {
				static_index_config = Conf.LoadFrom (config_file_path);
				if (static_index_config == null) {
					Log.Error ("Unable to read config from {0}", config_file_path);
					return null;
				}
			} catch (Exception e) {
				Log.Error (e, "Caught exception while reading config from {0}", config_file_path);
				return null;
			}

			string source = static_index_config.GetOption ("Source", null);
			if (source == null) {
				Log.Error ("Invalid config file: {0}", config_file_path);
				return null;
			}

			QueryableFlavor flavor = new QueryableFlavor ();
			flavor.Name = source;
			flavor.Domain = query_domain;

			Queryable queryable = new Queryable (flavor, static_queryable);
			return queryable;
		}

		internal static Queryable LoadRemovableQueryable (DirectoryInfo index_dir, string mnt_dir)
		{
			Queryable queryable = LoadStaticQueryable (index_dir, QueryDomain.Local);
			if (queryable == null)
				return null;

			StaticQueryable static_queryable;
			static_queryable = (StaticQueryable) queryable.IQueryable;

			static_queryable.mount_dir = mnt_dir;

			return queryable;
		}

		// FIXME: Move these to LuceneCommon if and when we decide to
		// support adding/removing arbitrary backends at runtime
		internal void Close ()
		{
			Log.Debug ("Removing static queryable {0}", IndexName);
			if (text_cache != null)
				text_cache.Dispose ();

			// Free the cached IndexReaders
			LuceneCommon.CloseReader (LuceneCommon.GetReader (Driver.PrimaryStore));
			LuceneCommon.CloseReader (LuceneCommon.GetReader (Driver.SecondaryStore));

			Driver.PrimaryStore.Close ();
			Driver.SecondaryStore.Close ();
			FileAttributesStore.Dispose ();
		}

#if ENABLE_RDF_ADAPTER
		protected override TextCache TextCache {
			get { return text_cache; }
		}
#endif

		override public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length) 
		{
			if (text_cache == null)
				return null;

			Uri uri;

			if (mount_dir == null || hit ["beagle:RemovableUri"] == null)
				uri = hit.Uri;
			else
				uri = UriFu.EscapedStringToUri (hit ["beagle:RemovableUri"]);

			// Look up the hit in our local text cache.
			// Need to handle self-cached removable:/// uris
			bool self_cache = true;
			TextReader reader = text_cache.GetReader (uri, ref self_cache);

			if (self_cache)
				reader = new StreamReader (hit.Uri.LocalPath);
			else if (reader == null)
				return null;
			
			return SnippetFu.GetSnippet (query_terms, reader, full_text, ctx_length, snp_length);
		}

		private bool HitIsValid (Uri uri)
		{
			// We can't check anything else than file uris
			if (! uri.IsFile)
				return true;
			
			// FIXME: This is a hack, we need to support parent Uri's in some sane way
			try {
				int j = uri.LocalPath.LastIndexOf ('#');
				string actual_path = ((j == -1) ? uri.LocalPath : uri.LocalPath.Substring (0, j));
				return File.Exists (actual_path) || Directory.Exists (actual_path);
			} catch (Exception e) {
				Logger.Log.Warn ("Exception executing HitIsValid on {0}", uri.LocalPath);
				return false;
			}
		}

		// Remap uri based on mount point for removable indexes
		// FIXME: Allow option to search unmounted media ? Return false in that case.
		override protected bool HitFilter (Hit hit)
		{
			if (! HitIsValid (hit.Uri))
				return false;

			return HitIsValidRemovable (hit);
		}

		private bool HitIsValidRemovable (Hit hit)
		{
			if (mount_dir == null || hit.Uri.Scheme != "removable")
				return true;

			hit.AddProperty (Beagle.Property.NewKeyword ("beagle:RemovableUri",
								     hit.EscapedUri));

			string path = hit.Uri.LocalPath;
			path = path.Substring (1); // Remove initial '/'
			path = Path.Combine (mount_dir, path);
			hit.Uri = UriFu.PathToFileUri (path);

			hit.AddProperty (Beagle.Property.NewKeyword ("fixme:mount_dir", mount_dir));

			return true;
		}
	}
}
