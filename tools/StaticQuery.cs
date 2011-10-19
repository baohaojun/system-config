//
// StaticQuery.cs
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Runtime.InteropServices;

using GLib;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;

// Assembly information
[assembly: AssemblyTitle ("beagle-static-query")]
[assembly: AssemblyDescription ("Command-line interface to query the Beagle index")]

public class QueryTool {

	private static int count = 0;
	private static Query query = null;
	private static DateTime queryStartTime;
	private static DateTime lastQueryTime = DateTime.Now;

	// CLI args
	private static bool verbose = false;
	private static bool display_hits = true;
	private static bool display_cached_text = false;

	private static void OnHitsAdded (QueryResult result, ICollection hits, int num_hits)
	{
		lastQueryTime = DateTime.Now;

		if (count == 0 && verbose) {
			Console.WriteLine ("First hit returned in {0:0.000}s",
					   (lastQueryTime - queryStartTime).TotalSeconds);
		}

		if (verbose && num_hits >= 0)
			Console.WriteLine ("Returned latest {0} results out of total {1} matches", hits.Count, num_hits);

		if (! display_hits) {
			count += hits.Count;
			return;
		}

		foreach (Hit hit in hits) {
			if (verbose)
				Console.WriteLine ("  Uri: {0}", hit.Uri);
			else
				Console.WriteLine (hit.Uri);

			if (verbose) {
				SnippetRequest sreq = new SnippetRequest (query, hit);
				if (display_cached_text)
					sreq.FullText = true;

				List<SnippetLine> snippets = GetSnippet (sreq);
				Console.WriteLine ("PaUri: {0}", hit.ParentUri != null ? hit.ParentUri.ToString () : "(null)");
				if (! display_cached_text) {
					Console.Write (" Snip: ");
					if (snippets.Count == 0)
						Console.WriteLine ("(null)");
					else {
						foreach (SnippetLine snippet_line in snippets) {
							Console.Write (snippet_line);
							Console.Write (" ... ");
						}

						Console.WriteLine ();
					}
				}

				Console.WriteLine (" Type: {0}", hit.Type);
				Console.WriteLine ("MimeT: {0}", hit.MimeType == null ? "(null)" : hit.MimeType);
				Console.WriteLine ("  Src: {0}", hit.Source);
				Console.WriteLine ("Score: {0}", hit.Score);
				if (hit.ValidTimestamp)
					Console.WriteLine (" Time: {0}", DateTimeUtil.ToString (hit.Timestamp));
				
				foreach (Property prop in hit.Properties)
					Console.WriteLine ("    {0} = '{1}'",
						prop.Key,
						(prop.Type != PropertyType.Date ? prop.Value : DateTimeUtil.ToString (StringFu.StringToDateTime (prop.Value))));
				

				if (display_cached_text) {
					Console.WriteLine ("-- Cache -------------------------------------");
					if (snippets.Count == 0)
						Console.WriteLine ("(empty)");
					else {
						foreach (SnippetLine snippet_line in snippets) {
							if (snippet_line == null || snippet_line.Fragments == null)
								Console.WriteLine ("(empty)");
							else
								Console.WriteLine (((Fragment)snippet_line.Fragments [0]).Text);
						}
					}
					Console.WriteLine ("----------------------------------------------");
				}
				Console.WriteLine ();
			}

			++count;
		}
	}

	private static void OnFinished (QueryResult result)
	{
		if (verbose) {
			Console.WriteLine ("Elapsed time: {0:0.000}s",
					   (DateTime.Now - queryStartTime).TotalSeconds);
			Console.WriteLine ("Total hits: {0}", count);
		}
	}

	private static List<SnippetLine> GetSnippet (SnippetRequest request)
	{
		Queryable queryable = QueryDriver.GetQueryable (request.Hit.Source);
		ISnippetReader snippet_reader;
		bool full_text = request.FullText;
		int ctx_length = request.ContextLength;
		int snp_length = request.SnippetLength;

		if (queryable == null) {
			Console.WriteLine ("SnippetExecutor: No queryable object matches '{0}'", request.Hit.Source);
			snippet_reader = new SnippetReader (null, null, false, -1, -1);
			full_text = false;
		} else
			snippet_reader = queryable.GetSnippet (request.QueryTerms, request.Hit, full_text, ctx_length, snp_length);

		List<SnippetLine> snippetlines = new List<SnippetLine> ();
		if (snippet_reader == null)
			return snippetlines;

		if (! full_text) {
 			foreach (SnippetLine snippet_line in snippet_reader.GetSnippet ())
				snippetlines.Add (snippet_line);
		} else {
			SnippetLine snippet_line = new SnippetLine ();
			snippet_line.Line = 1;

			Fragment fragment = new Fragment ();
			fragment.QueryTermIndex = -1;
			StringBuilder sb = new StringBuilder ();

			string line;
			// Read data from snippet_reader and write
			while ((line = snippet_reader.ReadLine ()) != null) {
				sb.Append (StringFu.CleanupInvalidXmlCharacters (line));
				sb.Append ("\n");
			}

			fragment.Text = sb.ToString ();
			snippet_line.Fragments = new ArrayList ();
			snippet_line.Fragments.Add (fragment);
			snippetlines.Add (snippet_line);
		}

		snippet_reader.Close ();

		return snippetlines;
	}

	public static void PrintUsageAndExit () 
	{
		VersionFu.PrintHeader ();

		string usage =
			"Usage: beagle-query [OPTIONS] <query string>\n\n" +
			"Options:\n" +
			"  --verbose\t\tPrint detailed information about each hit.\n" +
			"  --cache\t\tShow the entire cached text instead of a snippet\n" +
			"         \t\tshowing the matches, requires --verbose.\n" +
			"         \t\tFor large documents this will produce extremely large output,\n" +
			"         \t\tso use this with uri queries or queries returning only a few results\n" + 
			"         \t\tNot recommended for live-queries or stats-only queries.\n" +
			"  --keywords\t\tLists the keywords allowed in 'query string'.\n" +
			"            \t\tKeyword queries can be specified as keywordname:value e.g. ext:jpg\n" +
			"  --stats-only\t\tOnly display statistics about the query, not\n" +
			"              \t\tthe actual results.\n" +
			"  --max-hits\t\tLimit number of search results per backend\n" +
			"            \t\t(default 100)\n" +
			"\n" +
			"\n" +
			"  --help\t\tPrint this usage message.\n" +
			"  --version\t\tPrint version information.\n" +
			"\n" +
			"Query string supports an advanced query syntax.\n" +
			"For details of the query syntax, please see http://beagle-project.org/Searching_Data\n" +
			"Note: Quotes (\" or \') need to be shell escaped if used.\n";

		Console.WriteLine (usage);

		System.Environment.Exit (0);
	}

	[DllImport("libgobject-2.0.so.0")]
	static extern void g_type_init ();

	public static void Main (string[] args) 
	{
		// Initialize GObject type system
		g_type_init ();

		Beagle.Util.Log.Level = LogLevel.Always; // shhhh... silence

		if (args.Length == 0 || Array.IndexOf (args, "--help") > -1 || Array.IndexOf (args, "--usage") > -1)
			PrintUsageAndExit ();

		if (Array.IndexOf (args, "--version") > -1) {
			VersionFu.PrintVersion ();
			Environment.Exit (0);
		}

		StringBuilder query_str =  new StringBuilder ();

		query = new Query ();

		// Parse args
		int i = 0;
		string next_arg;
		while (i < args.Length) {
			switch (args [i]) {

			case "--verbose":
				verbose = true;
				break;
			case "--cache":
				display_cached_text = true;
				break;
			case "--stats-only":
				verbose = true;
				display_hits = false;
				break;
			case "--max-hits":
				if (++i >= args.Length) PrintUsageAndExit ();
				query.MaxHits = Int32.Parse (args[i]);
				break;

			case "--list-backends":
				Console.WriteLine ("Current available backends:");
				Console.Write (QueryDriver.ListBackends ());
				Environment.Exit (0);
				break;

			case "--backend":
				if (++i >= args.Length) PrintUsageAndExit ();

				next_arg = args [i];
				if (next_arg.StartsWith ("--")) {
					Console.WriteLine ("--backend requires a backend name. Invalid name '{0}'", next_arg);
					Environment.Exit (1);
					break;
				}

				if (next_arg [0] != '+' && next_arg [0] != '-')
					QueryDriver.OnlyAllow (next_arg);
				else {
					if (next_arg [0] == '+')
						QueryDriver.Allow (next_arg.Substring (1));
					else
						QueryDriver.Deny (next_arg.Substring (1));
				}

				break;

			case "--add-static-backend": 
				if (++i >= args.Length) PrintUsageAndExit ();

				next_arg = args [i];
				if (! next_arg.StartsWith ("--"))
					QueryDriver.AddStaticQueryable (next_arg);
				break;

			case "--keywords":
				PropertyKeywordFu.ReadKeywordMappings ();

				Console.WriteLine ("Supported query keywords are:");

				foreach (string key in PropertyKeywordFu.Keys) {
					foreach (QueryKeywordMapping mapping in PropertyKeywordFu.Properties (key)) {
						// Dont print properties without description; they confuse people
						if (string.IsNullOrEmpty (mapping.Description))
							continue;
						Console.WriteLine ("  {0,-20} for {1}", key, mapping.Description);
					}
				}

				System.Environment.Exit (0);
				break;

			default:
				if (args [i].StartsWith ("--"))
					PrintUsageAndExit ();
				if (query_str.Length > 0)
					query_str.Append (' ');
				query_str.Append (args [i]);
				
				break;
			}

			++i;
		}

		if (verbose)
			Beagle.Util.Log.Level = LogLevel.Debug;

		if (query_str.Length > 0)
			query.AddText (query_str.ToString ());

		Stopwatch watch = new Stopwatch ();
		watch.Start ();
		StartQueryDriver ();
		watch.Stop ();
		if (verbose)
			Console.WriteLine ("QueryDriver started in {0}", watch);

		QueryResult result = new QueryResult ();
		result.HitsAddedEvent += OnHitsAdded;
		result.FinishedEvent += OnFinished;

		queryStartTime = DateTime.Now;
		QueryDriver.DoQueryLocal (query, result);

	}

	private static void StartQueryDriver ()
	{
		try {
			string tmp = PathFinder.StorageDir;
			if (! Directory.Exists (tmp))
				throw new IOException ("Beagle directory not found");
		} catch (Exception e) {
			Console.WriteLine ("Unable to start the daemon: {0}", e.Message);
			Environment.Exit (-1);
		}

		QueryDriver.IndexingDelay = -1;

		if (verbose) {
			Console.WriteLine ("Starting Beagle Daemon (version {0})", ExternalStringsHack.Version);
			Console.WriteLine ("Running on {0}", SystemInformation.MonoRuntimeVersion);
		}

		// Check if global configuration files are installed
		if (! Conf.CheckGlobalConfig ()) {
			Console.WriteLine ("Global configuration files not found in '{0}'", PathFinder.ConfigDataDir);
			Environment.Exit (-1);
		}

		QueryDriver.Init ();
		QueryDriver.Start ();
	}
}
