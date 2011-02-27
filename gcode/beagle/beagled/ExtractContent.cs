//
// ExtractContent.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Net;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;

using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;

[assembly: AssemblyTitle ("beagle-extract-content")]
[assembly: AssemblyDescription ("Extracts filtered data from a file")]

class ExtractContentTool {

	static bool tokenize = false;
	static bool analyze = false;
	static bool show_generated = false;
	static string mime_type = null;
	static bool continue_last = false;
	static bool stats_only = false;

	// FIXME: We don't display structural breaks
	static void DisplayContent (string line)
	{
		line = line.Trim ();
		if (line.Length == 0)
			return;

		if (tokenize) {
			
			string [] parts = line.Split (' ');
			for (int i = 0; i < parts.Length; ++i) {
				string part = parts [i].Trim ();
				if (part != "")
					Console.WriteLine ("{0}", part);
			}

		} else {
			Console.WriteLine (line);
		}
	}

	static void DisplayContent (char[] buffer, int length)
	{
		if (tokenize) {
			if (continue_last && buffer [0] == ' ')
				Console.WriteLine ();

			char last_char = buffer [length - 1];
			continue_last = (last_char != '\n' &&
					      last_char != '\t' &&
					      last_char != ' ');

			string line = new string (buffer, 0, length);
			string [] parts = line.Split (' ');
			for (int i = 0; i < parts.Length - 1; ++i) {
				string part = parts [i].Trim ();
				if (part != String.Empty)
					Console.WriteLine ("{0}", part);
			}

			string last = parts [parts.Length - 1];
			last = last.Trim ();
			if (last != String.Empty)
				Console.Write ("{0}{1}", last, (continue_last ? "" : "\n"));

		} else {
			Console.Write (buffer, 0, length);
		}
	}

	static bool first_indexable = true;

	static void Display (Indexable indexable)
	{
		if (!first_indexable) {
			Console.WriteLine ();
			Console.WriteLine ("-----------------------------------------");
			Console.WriteLine ();
		}
		first_indexable = false;

		Console.WriteLine ("Filename: " + indexable.Uri);

		if (indexable.ParentUri != null)
			Console.WriteLine ("Parent: " + indexable.ParentUri);

		Stopwatch watch = new Stopwatch ();

		Filter filter;

		watch.Start ();
		if (! FilterFactory.FilterIndexable (indexable, out filter)) {
			indexable.Cleanup ();
			indexable.NoContent = true;
			filter = null;
		}
		watch.Stop ();

		Console.WriteLine ("Filter: {0} (determined in {1})", filter, watch);
		Console.WriteLine ("MimeType: {0}", indexable.MimeType);
		Console.WriteLine ();

		ArrayList generated_indexables = new ArrayList ();
		Indexable generated_indexable;

		bool first = true;
		if (filter != null && filter.HasGeneratedIndexable) {
			while (filter.GenerateNextIndexable (out generated_indexable)) {
				if (generated_indexable == null)
					continue;

				if (first) {
					Console.WriteLine ("Filter-generated indexables:");
					first = false;
				}
				
				Console.WriteLine ("  {0}", generated_indexable.Uri);

				if (show_generated)
					generated_indexables.Add (generated_indexable);
				else
					generated_indexable.Cleanup ();
			}
		}

		if (! first)
			Console.WriteLine ();

		// Make sure that the properties are sorted.
		ArrayList prop_array = new ArrayList (indexable.Properties);
		prop_array.Sort ();

		Console.WriteLine ("Properties:");

		if (indexable.ValidTimestamp)
			Console.WriteLine ("  Timestamp = {0}", DateTimeUtil.ToString (indexable.Timestamp));

		foreach (Beagle.Property prop in prop_array) {
			if (String.IsNullOrEmpty (prop.Value))
				continue;

			Console.WriteLine ("  {0} = {1}", prop.Key, prop.Value);
		}

		Console.WriteLine ();

		if (indexable.NoContent)
			return;

		watch.Reset ();
		watch.Start ();

		TextReader reader;
		Analyzer indexing_analyzer = new BeagleAnalyzer ();

		char[] buffer = new char [2048];
		reader = indexable.GetTextReader ();
		char separater_char = (tokenize ? '\n' : ' ');
		if (reader != null) {
			first = true;

			if (analyze) {
				if (! stats_only)
					Console.WriteLine ("Content:");

				TokenStream token_stream = indexing_analyzer.TokenStream ("Text", reader);
				Lucene.Net.Analysis.Token token = token_stream.Next ();
				first = (token == null);

				if (! stats_only)
					for (; token != null; token = token_stream.Next ())
						Console.Write ("{0}{1}", token.TermText (), separater_char);

				token_stream.Close ();
			} else {
#if false
				while (true) {
					int l = reader.Read (buffer, 0, 2048);
					if (l <= 0)
						break;
					if (first)
						first = false;
					if (! stats_only)
						DisplayContent (buffer, l);
				}
#else
				string line;
				first = true;
				while ((line = reader.ReadLine ()) != null) {
					if (first) {
						Console.WriteLine ("Content:");
						first = false;
					}
					if (! stats_only)
						DisplayContent (line);
				}
#endif
			}

			reader.Close ();

			if (first)
				Console.WriteLine ("(no content)");
			else
				Console.WriteLine ('\n');
		}
			
		/*
		reader = indexable.GetHotTextReader ();
		first = true;
		if (reader != null) {
			Console.WriteLine ("HotContent:");

			if (analyze) {
				TokenStream token_stream = indexing_analyzer.TokenStream ("HotText", reader);
				Lucene.Net.Analysis.Token token = token_stream.Next ();
				first = (token == null);

				for (; token != null; token = token_stream.Next ())
					Console.Write ("{0}{1}", token.TermText (), separater_char);

				token_stream.Close ();
			} else {
				while (true) {
					int l = reader.Read (buffer, 0, 2048);
					if (l <= 0)
						break;
					if (first)
						first = false;
					DisplayContent (buffer, l);
				}
			}

			reader.Close ();

			if (first)
				Console.WriteLine ("(no hot content)");
			else
				Console.WriteLine ('\n');
		}
		*/

		watch.Stop ();

		Console.WriteLine ();
		Console.WriteLine ("Text extracted in {0}", watch);


		foreach (Indexable gi in generated_indexables)
			Display (gi);

		Stream stream = indexable.GetBinaryStream ();
		if (stream != null)
			stream.Close ();

		// Clean up any temporary files associated with filtering this indexable.
		indexable.Cleanup ();
	}

	static void PrintUsage ()
	{
		VersionFu.PrintHeader ();

		Console.WriteLine ("Usage: beagle-extract-content [OPTIONS] file [file ...]");
		Console.WriteLine ();
		Console.WriteLine ("Options:");
		Console.WriteLine ("  --debug\t\t\tPrint debug info to the console");
		Console.WriteLine ("  --tokenize\t\t\tTokenize the text before printing");
		Console.WriteLine ("  --analyze\t\t\tAnalyze the text before printing.\n\t\t\t\tThis will output exactly the words, separated by whitespace, that go into beagle index.");
		Console.WriteLine ("  --show-generated\t\tShow filtering information for items created by filters");
		Console.WriteLine ("  --mimetype=<mime_type>\tUse filter for mime_type");
		Console.WriteLine ("  --outfile=<filename>\t\tOutput file name");
		Console.WriteLine ("  --help\t\t\tShow this message");
		Console.WriteLine ("  --version\t\t\tPrint version information");
		Console.WriteLine ();
	}

	static int Main (string[] args)
	{
		SystemInformation.SetProcessName ("beagle-extract-content");

		if (args.Length < 1 || Array.IndexOf (args, "--help") != -1) {
			PrintUsage ();
			return 0;
		}

		if (Array.IndexOf (args, "--debug") == -1)
			Log.Disable ();

		if (Array.IndexOf (args, "--version") != -1) {
			VersionFu.PrintVersion ();
			return 0;
		}

		if (Array.IndexOf (args, "--tokenize") != -1)
			tokenize = true;
		
		if (Array.IndexOf (args, "--analyze") != -1)
			analyze = true;
		
		if (Array.IndexOf (args, "--show-generated") != -1 || Array.IndexOf (args, "--show-children") != -1)
			show_generated = true;

		StreamWriter writer = null;
		string outfile = null;
		foreach (string arg in args) {

			// mime-type option
			if (arg.StartsWith ("--mimetype=")) {
				mime_type = arg.Substring (11);    
				continue;
			// output file option
			// we need this in case the output contains different encoding
			// printing to Console might not always display properly
			} else if (arg.StartsWith ("--outfile=")) {
				outfile = arg.Substring (10);    
				Console.WriteLine ("Redirecting output to " + outfile);
				FileStream f = new FileStream (outfile, FileMode.Create);
				writer = new StreamWriter (f, System.Text.Encoding.UTF8);
				continue;
			} else if (arg.StartsWith ("--")) // option, skip it 
				continue;
			
			Uri uri = UriFu.PathToFileUri (arg);
			Indexable indexable = new Indexable (uri);
			if (mime_type != null)
				indexable.MimeType = mime_type;

			try {
				if (writer != null) {
					Console.SetOut (writer);
				}

				Display (indexable);
				if (writer != null) {
					writer.Flush ();
				}
				
				if (outfile != null) {
					StreamWriter standardOutput = new StreamWriter(Console.OpenStandardOutput());
					standardOutput.AutoFlush = true;
					Console.SetOut(standardOutput);
				}
				
			} catch (Exception e) {
				Console.WriteLine ("Unable to filter {0}: {1}", uri, e.Message);
				return -1;
			}
			
			// Super Lame Hack: gtk-sharp up to 2.10 requires a main loop
			// to dispose of any managed wrappers around GObjects.  Since
			// we don't have one, we'll process all the pending items in
			// a loop here.  This is particularly an issue with maildirs,
			// because we need the loop to clean up after GMime.  Without
			// it, GMime's streams are never completely unref'd, the
			// file descriptors aren't closed, and we run out and crash.
			while (GLib.MainContext.Pending ())
				GLib.MainContext.Iteration ();
		}
		if (writer != null)
			writer.Close ();

		return 0;
	}

	// A stripped version of LuceneCommon.BeagleAnalyzer
	internal class BeagleAnalyzer : StandardAnalyzer {

		public BeagleAnalyzer ()
		{
		}

		public override TokenStream TokenStream (string fieldName, TextReader reader)
		{
			TokenStream outstream;
			outstream = base.TokenStream (fieldName, reader);

			return outstream;
		}
	}

}
