//
// Query.cs
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
using System.IO;
using System.Collections;
using System.Globalization;
using System.Reflection;
using System.Threading;
using System.Text;
using System.Runtime.InteropServices;

using GLib;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;

// Assembly information
[assembly: AssemblyTitle ("beagle-query")]
[assembly: AssemblyDescription ("Command-line interface to the Beagle search system")]

public class QueryTool {

	private static int count = 0;
	private static Query query = null;
	private static DateTime queryStartTime;
	private static DateTime lastQueryTime = DateTime.Now;

	private static MainLoop main_loop = null;

	// CLI args
	private static bool keep_running = false;
	private static bool verbose = false;
	private static bool display_hits = true;
	private static bool flood = false;
	private static bool listener = false;
	private static bool display_cached_text = false;

	private static void OnHitsAdded (HitsAddedResponse response)
	{
		lastQueryTime = DateTime.Now;

		if (count == 0 && verbose) {
			Console.WriteLine ("First hit returned in {0:0.000}s",
					   (lastQueryTime - queryStartTime).TotalSeconds);
		}

		if (verbose) {
			if (response.NumMatches >= 0)
				Console.WriteLine ("Returned latest {0} results out of total {1} matches", response.Hits.Count, response.NumMatches);
			else
				Console.WriteLine ("Returned latest {0} results", response.Hits.Count);
		}

		if (! display_hits) {
			count += response.Hits.Count;
			return;
		}

		foreach (Hit hit in response.Hits) {
			if (verbose)
				Console.WriteLine ("  Uri: {0}", hit.Uri);
			else
				Console.WriteLine (hit.Uri);

			if (verbose) {
				SnippetRequest sreq = new SnippetRequest (query, hit);
				if (display_cached_text)
					sreq.FullText = true;

				SnippetResponse sresp = (SnippetResponse) sreq.Send ();
				Console.WriteLine ("PaUri: {0}", hit.ParentUri != null ? hit.ParentUri.ToString () : "(null)");
				if (! display_cached_text)
					Console.WriteLine (" Snip: {0}", sresp.Snippet != null ? sresp.Snippet : "(null)");
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
					if (sresp.SnippetList.Snippets == null)
						Console.WriteLine ("(empty)");
					else {
						foreach (SnippetLine snippet_line in sresp.SnippetList.Snippets) {
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

	private static void OnHitsSubtracted (HitsSubtractedResponse response)
	{
		lastQueryTime = DateTime.Now;

		if (! display_hits)
			return;

		foreach (Uri uri in response.Uris) {
			Console.WriteLine ("Subtracted Uri '{0}'", uri);
			Console.WriteLine ();

			--count;
		}
	}

	private static void OnFinished (FinishedResponse response)
	{
		if (verbose) {
			Console.WriteLine ("Elapsed time: {0:0.000}s",
					   (DateTime.Now - queryStartTime).TotalSeconds);
			Console.WriteLine ("Total hits: {0}", count);
		}

		if (flood)
			SendQuery ();
		else
			main_loop.Quit ();
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
			"  --live-query\t\tRun continuously, printing notifications if a\n" +
			"              \t\tquery changes.\n" +
			"  --stats-only\t\tOnly display statistics about the query, not\n" +
			"              \t\tthe actual results.\n" +
			"  --max-hits\t\tLimit number of search results per backend\n" +
			"            \t\t(default 100)\n" +
			"\n" +
			"  --domain <local|system|network|global|all> Specify query domain (default local + system)\n" +
			"\n" +
			"  --flood\t\tExecute the query over and over again.  Don't do that.\n" +
			"  --listener\t\tExecute an index listener query.  Don't do that either.\n" +
			"  --help\t\tPrint this usage message.\n" +
			"  --version\t\tPrint version information.\n" +
			"\n" +
			"Query string supports an advanced query syntax.\n" +
			"For details of the query syntax, please see http://beagle-project.org/Searching_Data\n" +
			"Note: Quotes (\" or \') need to be shell escaped if used.\n";

		Console.WriteLine (usage);

		System.Environment.Exit (0);
	}

	private static void OnClosed ()
	{
		if (flood)
			SendQuery ();
		else
			main_loop.Quit ();
	}

	private static int query_counter = 0;
	private static void SendQuery ()
	{
		++query_counter;
		if (flood) {
			if (query_counter > 1)
				Console.WriteLine ();
			Console.WriteLine ("Sending query #{0}", query_counter);
		}

		queryStartTime = DateTime.Now;
		try {
			query.SendAsync ();
		} catch (Exception ex) {
			Console.WriteLine ("Could not connect to the Beagle daemon.  The daemon probably isn't running.");
			Console.WriteLine (ex);
			System.Environment.Exit (-1);
		}
	}
	
	[DllImport("libgobject-2.0.so.0")]
	static extern void g_type_init ();

	public static void Main (string[] args) 
	{
		// Initialize GObject type system
		g_type_init ();

		main_loop = new MainLoop ();

		if (args.Length == 0 || Array.IndexOf (args, "--help") > -1 || Array.IndexOf (args, "--usage") > -1)
			PrintUsageAndExit ();

		if (Array.IndexOf (args, "--version") > -1) {
			VersionFu.PrintVersion ();
			Environment.Exit (0);
		}

		StringBuilder query_str =  new StringBuilder ();

		query = new Query ();

		QueryDomain domain = 0;

		// Parse args
		int i = 0;
		while (i < args.Length) {
			switch (args [i]) {

			case "--live-query":
				keep_running = true;
				break;
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
			case "--flood":
				flood = true;
				break;
			case "--listener":
				listener = true;
				keep_running = true;
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

			case "--domain":
				if (++i >= args.Length) PrintUsageAndExit ();
				switch (args [i].ToLower ()) {
				case "local":
					domain |= QueryDomain.Local;
					break;

				case "system":
					domain |= QueryDomain.System;
					break;

				case "network":
					domain |= QueryDomain.Neighborhood;
					break;

				case "global":
					domain |= QueryDomain.Global;
					break;

				case "all":
					domain |= QueryDomain.All;
					break;

				default:
					PrintUsageAndExit ();
					break;
				}
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

		if (domain != 0)
			query.QueryDomain = domain;

		if (listener) {
			query.IsIndexListener = true;
		} else {
			if (query_str.Length > 0)
				query.AddText (query_str.ToString ());
		}

		query.HitsAddedEvent += OnHitsAdded;
		query.HitsSubtractedEvent += OnHitsSubtracted;

		if (! keep_running)
			query.FinishedEvent += OnFinished;
		else
			query.ClosedEvent += OnClosed;

		SendQuery ();

		main_loop.Run ();
	}
}
