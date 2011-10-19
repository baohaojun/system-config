//
// DumpIndex.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.Net;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;
using Hit = Beagle.Hit;

using Lucene.Net.Index;
using Lucene.Net.Search;
using Lucene.Net.Documents;

class DumpIndexTool {

	public class HitByUriComparer : IComparer {

		public int Compare (object a, object b)
		{
			// All of this mapping to and from strings is dreadful.
			return String.Compare (((Hit) a).Uri.ToString (), ((Hit) b).Uri.ToString ());
		}
	}

	static ArrayList RemapUris (LuceneQueryingDriver driver, ArrayList uris)
	{
		// We only need to remap URIs in the file system backend
		if (driver.IndexName != "FileSystemIndex")
			return uris;

		FileAttributesStore fa_store = new FileAttributesStore (new FileAttributesStore_Mixed (Path.Combine (PathFinder.IndexDir, "FileSystemIndex"), driver.Fingerprint));

		for (int i = 0; i < uris.Count; i++) {
			Uri uri = (Uri) uris [i];
			string path = uri.LocalPath;

			Beagle.Daemon.FileAttributes attr = fa_store.Read (path);
			if (attr == null) {
				Console.WriteLine ("No file attribute info for {0}", uri);
				continue;
			}

			Uri internal_uri = new Uri ("uid:" + GuidFu.ToShortString (attr.UniqueId) + uri.Fragment);
			uris [i] = internal_uri;
		}

		return uris;
	}

	static string RemapUriToPath (Hashtable all_hits_by_uri, Hit hit)
	{
		string exact_name;

		if (hit.GetFirstProperty (Property.IsChildPropKey) == "true")
			exact_name = hit.GetFirstProperty ("parent:" + Property.ExactFilenamePropKey);
		else
			exact_name = hit.GetFirstProperty (Property.ExactFilenamePropKey);

		string parent_uri_str = hit.GetFirstProperty (Property.ParentDirUriPropKey);

		if (parent_uri_str == null)
			return exact_name;
		else
			return Path.Combine (RemapUriToPath (all_hits_by_uri, (Hit) all_hits_by_uri [parent_uri_str]),
					     exact_name);
	}

	static int DumpOneIndex_Metadata (string index_name, ArrayList uris, bool show_properties)
	{
		LuceneQueryingDriver driver;
		driver = new LuceneQueryingDriver (index_name, -1, true);

		Hashtable all_hits_by_uri = null;
		ArrayList all_hits = null;

		if (uris.Count == 0 || index_name == "FileSystemIndex") {
			all_hits_by_uri = driver.GetAllHitsByUri ();
			all_hits = new ArrayList (all_hits_by_uri.Values);
		}

		// A hard-wired hack
		if (index_name == "FileSystemIndex") { 
			foreach (Hit hit in all_hits) {
				string internal_uri;

				if (hit [Property.IsChildPropKey] == "true") {
					string path = RemapUriToPath (all_hits_by_uri, hit);
					
					internal_uri = UriFu.UriToEscapedString (hit.ParentUri);
					
					hit.ParentUri = UriFu.PathToFileUri (path);
					hit.Uri = UriFu.AddFragment (UriFu.PathToFileUri (path),
								     hit.Uri.Fragment,
								     true);
				} else {
					internal_uri = UriFu.UriToEscapedString (hit.Uri);

					hit.Uri = UriFu.PathToFileUri (RemapUriToPath (all_hits_by_uri, hit));
					hit.AddProperty (Property.NewUnsearched ("beagle:InternalUri", internal_uri));
				}
			}
		}

		ArrayList matching_hits;

		if (uris.Count == 0)
			matching_hits = all_hits;
		else {
			matching_hits = new ArrayList (driver.GetHitsForUris (RemapUris (driver, uris)));

			if (index_name == "FileSystemIndex") {
				for (int i = 0; i < matching_hits.Count; i++) {
					Hit hit = (Hit) matching_hits [i];
					Hit mapped_hit = (Hit) all_hits_by_uri [hit.Uri];

					matching_hits [i] = mapped_hit;
				}
			}
		}

		matching_hits.Sort (new HitByUriComparer ());

		foreach (Hit hit in matching_hits) {

			if (! show_properties) {
				Console.WriteLine ("{0}: {1}", index_name, hit.Uri);
				continue;
			}

			Console.WriteLine (" Index: {0}", index_name);
			Console.WriteLine ("   Uri: {0}", hit.Uri);
			if (hit.ParentUri != null)
				Console.WriteLine ("Parent: {0}", hit.ParentUri);
			Console.WriteLine (" MimeT: {0}", hit.MimeType);
			Console.WriteLine ("  Type: {0}", hit.Type);
			Console.WriteLine ("Source: {0}", hit.Source);

			ArrayList props;
			props = new ArrayList (hit.Properties);
			props.Sort ();
			foreach (Property prop in props) {
				char [] legend = new char [4];

				legend [0] = prop.IsMutable  ? 'm' : ' ';
				legend [1] = prop.IsSearched ? 's' : ' ';
				legend [2] = prop.IsPersistent ? 'p' : ' ';
				legend [3] = prop.Type == PropertyType.Text ? 't' : ' ';

				Console.WriteLine ("  Prop: [{0}] {1} = '{2}'", new String (legend), prop.Key, prop.Value);
			}
				

			Console.WriteLine ();
		}

		return matching_hits.Count;
	}

	// Dump the term frequencies: we do this via direct Lucene access.
	static void DumpOneIndex_TermFrequencies (string index_name)
	{
		LuceneQueryingDriver driver;
		driver = new LuceneQueryingDriver (index_name, -1, true);
		
		IndexReader reader;
		reader = IndexReader.Open (driver.PrimaryStore);

		TermEnum term_enum;
		term_enum = reader.Terms (new Term ("Text", ""));

		int distinct_term_count = 0;
		int term_count = 0;

		// from LuceneFAQ
		// Terms are sorted first by field, then by text
		// so all terms with a given field are adjacent in enumerations.
		if (term_enum.Term () != null) {
			while (term_enum.Term().Field() == "Text") {
				int freq;
				freq = term_enum.DocFreq ();

				Console.WriteLine ("{0} '{1}' {2}", index_name, term_enum.Term ().Text (), freq);

				// FIXME: spew these as a count
				++distinct_term_count;
				term_count += freq;

				if (!term_enum.Next ())
					break;
			}
		}

		term_enum.Close ();
		reader.Close ();

		Console.WriteLine ();
	}

	// Dump the fields: we do this via direct Lucene access.
	static void DumpOneIndex_Fields (string index_name)
	{
		LuceneQueryingDriver driver;
		driver = new LuceneQueryingDriver (index_name, -1, true);
		
		IndexReader reader;
		reader = IndexReader.Open (driver.PrimaryStore);

		Console.WriteLine ("  -- Primary Index --");
		foreach (DictionaryEntry fi in reader.GetFieldNames (IndexReader.FieldOption.ALL))
			Console.WriteLine ("- [{0}]", fi.Key);
		reader.Close ();

		reader = IndexReader.Open (driver.SecondaryStore);
		if (reader.MaxDoc () != 0) {
			Console.WriteLine ("\n  -- Secondary Index --");
			foreach (DictionaryEntry fi in reader.GetFieldNames (IndexReader.FieldOption.ALL))
				Console.WriteLine ("- [{0}]", fi.Key);
			reader.Close ();
		}

		Console.WriteLine ();
	}

	/////////////////////////////////////////////////////////
		
	public class IndexInfo : IComparable {
		public string Name;
		public int    Count;

		public IndexInfo (string name)
		{
			Name = name;
		}

		public int CompareTo (object obj)
		{
			IndexInfo other = (IndexInfo) obj;
			return String.Compare (this.Name, other.Name);
		}
	}

	static void DumpIndexInformation (ArrayList indexes, ArrayList uris, bool show_properties, bool show_counts)
	{
		LuceneCommon.DumpIndexMode =  true;

		foreach (IndexInfo info in indexes)
			info.Count = DumpOneIndex_Metadata (info.Name, uris, show_properties);

		if (show_properties) {
			Console.WriteLine ("LEGEND:");
			Console.WriteLine ("  m - mutable");
			Console.WriteLine ("  s - searched");
			Console.WriteLine ("  p - persistent");
			Console.WriteLine ("  t - tokenized");
		}

		if (show_counts) {
			Console.WriteLine ();
			Console.WriteLine ("FINAL COUNTS");

			foreach (IndexInfo info in indexes)
				Console.WriteLine ("{0} {1}", info.Count.ToString ().PadLeft (7), info.Name);
		}
	}

	static void DumpIndexTermFreqs (ArrayList indexes)
	{
		foreach (IndexInfo info in indexes)
			DumpOneIndex_TermFrequencies (info.Name);
	}

	static void DumpIndexFields (ArrayList indexes)
	{
		foreach (IndexInfo info in indexes)
			DumpOneIndex_Fields (info.Name);
	}

	/////////////////////////////////////////////////////////

	static void PrintUsage ()
	{
		string usage = @"
beagle-dump-index: Low-level index management
Web page: http://beagle-project.org
Copyright (C) 2004-2007 Novell, Inc.

Usage: beagle-dump-index [options] [[file or URI to match] ...]
			
  --uris                   Dump all Uris (default)
  --properties             Dump all properties
  --term-frequencies       Dump term frequencies
  --fields                 Dump all fields

  --show-counts            Show index count totals (default)
  --hide-counts            Hide index count totals

  --index=<index name>     Limit results to an index by name.  May be used
                           multiple times.
  --indexdir=<directory>   Limit results to an index specified by absolute
                           path.  May be used multiple times.  Example path:
                           /home/user/.beagle/Indexes/FileSystemIndex

  file or URI to match     Get information in index about one or more files
                           or URIs.  Note this doesn't make sense with
                           --term-frequencies and will cause an error.
";
			
		Console.WriteLine (usage);
	}

	enum Mode {
		Uris,
		Properties,
		TermFrequencies,
		Fields
	}

	static void Main (string [] args)
	{
		Mode mode = Mode.Uris;
		bool show_counts = true;
		ArrayList index_dirs = new ArrayList ();
		ArrayList index_names = new ArrayList ();
		ArrayList uris = new ArrayList ();
		
		foreach (string arg in args) {

			switch (arg) {
				
			case "--help":
				PrintUsage ();
				Environment.Exit (0);
				break;
				
			case "--uris":
				mode = Mode.Uris; 
				break;

			case "--properties":
			case "--props":
				mode = Mode.Properties;
				break;

			case "--term-frequencies":
			case "--term-freqs":
				mode = Mode.TermFrequencies;
				break;

			case "--hide-counts":
				show_counts = false;
				break;

			case "--show-counts":
				show_counts = false;
				break;

			case "--fields":
				mode = Mode.Fields;
				break;

			default:
				if (arg.StartsWith ("--indexdir="))
					index_dirs.Add (arg.Remove (0, 11));
				else if (arg.StartsWith ("--index="))
					index_names.Add (arg.Remove (0, 8));
				else {
					Uri uri;
					
					try {
						uri = UriFu.UserUritoEscapedUri (arg);
					} catch (UriFormatException) {
						uri = UriFu.PathToFileUri (arg);
					}

					uris.Add (uri);
				}
				break;
			}
		}

		if (uris.Count > 0 && (mode == Mode.TermFrequencies || mode == Mode.Fields)) {
			Console.WriteLine ("ERROR: --term-frequencies and --fields do not make sense with files or URIs.");
			Environment.Exit (1);
		}

		ArrayList indexes = new ArrayList ();

		// If no --index or --indexdir options, get all the default indexes.
		if (index_dirs.Count == 0 && index_names.Count == 0) {
			foreach (DirectoryInfo subdir in DirectoryWalker.GetDirectoryInfos (PathFinder.IndexDir))
				indexes.Add (new IndexInfo (subdir.Name));
		} else {
			foreach (string name in index_names) {
				DirectoryInfo info = new DirectoryInfo (Path.Combine (PathFinder.IndexDir, name));

				if (!info.Exists) {
					Console.WriteLine ("ERROR: No index named '{0}'", name);
					Environment.Exit (1);
				}

				indexes.Add (new IndexInfo (info.Name));
			}

			foreach (string dir in index_dirs)
				indexes.Add (new IndexInfo (dir));
		}

		indexes.Sort ();

		if (mode == Mode.Uris || mode == Mode.Properties)
			DumpIndexInformation (indexes, uris, mode == Mode.Properties, show_counts);
		else if (mode == Mode.TermFrequencies)
			DumpIndexTermFreqs (indexes);
		else if (mode == Mode.Fields)
			DumpIndexFields (indexes);
	}
}
