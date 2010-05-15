//
// SnippetFu.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Collections;
using System.IO;

using Beagle.Util;
using Beagle;

// FIXME: Use Lucence.Net highlighting.

namespace Beagle.Daemon {
	
	public class SnippetFu {

		static public SnippetReader GetSnippet (string[] query_terms, TextReader line_reader, bool full_text, int ctx_length, int snp_length)
		{
			// FIXME: If the query doesn't have search text (or is null), we should
			// generate a 'summary snippet'.

			if (line_reader == null)
				return null;

			SnippetReader snippet_reader = new SnippetReader (line_reader, query_terms, full_text, ctx_length, snp_length);
			return snippet_reader;
		}
		
		static public SnippetReader GetSnippetFromFile (string[] query_terms, string filename, bool full_text, int ctx_length, int snp_length)
		{
			FileStream stream = new FileStream (filename, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);

			return GetSnippet (query_terms, new StreamReader (stream), full_text, ctx_length, snp_length);
		}

		static public SnippetReader GetSnippetFromTextCache (string[] query_terms, string filename, bool full_text, int ctx_length, int snp_length)
		{
			TextReader reader = TextCache.UserCache.GetReader (new Uri (filename));
			if (reader == null)
				return null;
			try {
				return GetSnippet (query_terms, reader, full_text, ctx_length, snp_length);
			} catch (ICSharpCode.SharpZipLib.SharpZipBaseException ex) {
				Log.Debug ("Unexpected exception '{0}' while extracting snippet for {1}", ex.Message, filename);
				return null;
			}
		}
	}

	public class SnippetReader : ISnippetReader, IDisposable {
		private TextReader line_reader;
		private ArrayList query_terms_list = null;
		private int found_snippet_length;
		private bool full_text;

		// A sliding window of size 'size' basically a circular linked list
		private class SlidingWindow {
			private int[] window;
			private int next; // next to next-1 is the current list
			private int size;
			private int count;

			public SlidingWindow (int size)
			{
				this.window = new int [size];
				this.size = size;
				Reset ();
			}

			public int Count {
				get { return count; }
			}

			// For reuse
			public void Reset ()
			{
				next = 0;
				count = 0;
				for (int i = 0; i < size; ++i)
					window [i] = -1;
			}

			public void Add (int pos)
			{
				window [next] = pos;
				//Console.WriteLine ("windows [{0}]={1}", next, pos);
				next = ((next + 1) % size);
				count = (count < size ? count + 1 : count);
			}

			// Scan from next to next-1 for the first non -1 entry
			// That will be the valid beginning of the sliding window
			public int StartValue {
				get {
					if (next == count)
						return window [0];
					else /* next < count */
						return window [(next + 1) % size];
				}
			}
		}

		// Keep a sliding window of the starting positions of words
		SlidingWindow sliding_window;

		private const int context_length_default = 6;
		private const int snippet_length_default = 200;

		private int context_length;
		private int snippet_length;

		public SnippetReader (TextReader line_reader, string[] query_terms, bool full_text, int context_length, int snippet_length)
		{
			this.line_reader = line_reader;
			this.found_snippet_length = 0;
			this.full_text = full_text;
			this.context_length = (context_length > 0 ? context_length : context_length_default);
			this.snippet_length = (snippet_length > 0 ? snippet_length : snippet_length_default);

			if (query_terms == null)
				return;

			this.sliding_window = new SlidingWindow (this.context_length);

			// remove stop words from query_terms
			query_terms_list = new ArrayList (query_terms.Length);
			foreach (string term in query_terms) {
				if (LuceneCommon.IsStopWord (term))
					continue;
				query_terms_list.Add (term);
			}
			//Console.WriteLine ("Creating snippet reader");
		}

		public string ReadLine ()
		{
			if (line_reader == null)
				return null;
			return line_reader.ReadLine ();
		}

		public void Close ()
		{
			if (line_reader == null)
				return;
			line_reader.Close ();
		}

		public void Dispose ()
		{
			Close ();
		}

		public IEnumerable GetSnippet ()
		{
			// No point in getting a snippet if there was no query term to mark
			if (line_reader == null || query_terms_list == null)
				yield break;

			string str = null;
			bool continue_line = false;
			int pos = 0; // where to start highlighting in the line
			SnippetLine snippet_line;
			ulong line = 0;

			while (found_snippet_length < snippet_length) {
				//Console.WriteLine ("Continue with last line ? {0}", continue_line);
				if (! continue_line) {
					try {
						if ((str = line_reader.ReadLine ()) == null)
							break;
					} catch (Exception e) {
						Log.Error ("Caught exception while fetching snippet: {0}", e.Message);
						str = null;
						break;
					}
					//Console.WriteLine ("Read line: [{0}]", str);
					continue_line = false;
					pos = 0;
					line ++;
				}

				if (str.Length <= pos)
					continue;

				snippet_line = MarkTerms (query_terms_list, str, ref pos);
				if (pos == str.Length)
					continue_line = false;

				if (snippet_line == null)
					continue;

				snippet_line.Line = line;
				found_snippet_length += snippet_line.Length;

				yield return  snippet_line;
			}

			yield break;
		}

		static private bool IsTokenSeparator (char c)
		{
			return Char.IsWhiteSpace (c)
				|| Char.IsSeparator (c)
				|| Char.IsPunctuation (c);
		}

		// Starts scanning at character pos of string text for occurrence of any word
		// in stemmed_terms. Returns a list of (words)*[(matched word)(words)*]+
		private SnippetLine MarkTerms (ArrayList stemmed_terms, string text, ref int pos)
		{
			SnippetLine snippet_line = null;
			int prev_match_end_pos = pos; // misnomer; means 1 + end_pos of previous word

			// 1. get next word
			// 2. if no next word, return arraylist
			// 3. if word is not a match, following_words ++
			// 4. else {
			// 4a. add list to the arraylist
			// 4b. add word to the arraylist
			// 4c. clear list
			// 4d. following_words=0
			// }
			// 5. if (following_words >= max_following_words) {
			// 5a. add list to the arraylist
			// 5b. clear list
			// 5c. return list
			// }

			while (pos < text.Length) {
				// Find the beginning of the next token
				if (IsTokenSeparator (text [pos])) {
					++ pos;
					continue;
				}

				// Find the end of the next token
				int end_pos = pos + 1;
				while (end_pos < text.Length && ! IsTokenSeparator (text [end_pos]))
					    ++ end_pos;

				string token = text.Substring (pos, end_pos - pos);
				string stemmed_token = null;
				bool found_match = false;

				// Iterate through the stemmed terms and match the token
				for (int i = 0; i < stemmed_terms.Count; i++) {
					
					// If this term is longer than the token in question, give up.
					if (end_pos - pos < ((string)stemmed_terms [i]).Length)
						continue;
					
					// We cache the token, so as to avoid stemming it more than once
					// when considering multiple terms.
					if (stemmed_token == null) {
						stemmed_token = LuceneCommon.Stem (token.ToLower ());
					}

					if (String.Compare ((string) stemmed_terms [i], stemmed_token, true) != 0)
						continue;
					
					// We have a match!
					found_match = true;
					//Console.WriteLine ("Found match");

					if (snippet_line == null)
						snippet_line = new SnippetLine ();

					// Find the fragment before the match
					int start_pos = sliding_window.StartValue;
					if (start_pos == -1) // If no non-match words seen after last match
						start_pos = prev_match_end_pos; // Use wherever previous word ended
					sliding_window.Reset ();

					string before_match = text.Substring (start_pos, pos - start_pos);
					snippet_line.AddNonMatchFragment (before_match);
					//Console.WriteLine ("Adding [{0}, {1}]:[{2}]", start_pos, pos - 1, before_match);

					snippet_line.AddMatchFragment (i, token);
					//Console.WriteLine ("Adding word [{0}, {1}]:[{2}]", pos, end_pos - 1, token);
					prev_match_end_pos = end_pos;

					break;
				}

				if (! found_match) {
					// Add the start pos of the token to the window
					sliding_window.Add (pos);
					// If we found a match previously and saw enough following words, stop
					if (snippet_line != null && snippet_line.Count > 0 && sliding_window.Count == context_length) {
						sliding_window.Reset ();
						string after_match = text.Substring (prev_match_end_pos, end_pos - prev_match_end_pos);
						snippet_line.AddNonMatchFragment (after_match);
						//Console.WriteLine ("Adding [{0}, {1}]:[{2}]", prev_match_end_pos, end_pos - 1, after_match);
						return snippet_line;
					}
				}

				pos = end_pos;
			}

			// If less than 6 words came after the last match, add the rest here
			if (snippet_line != null && snippet_line.Count > 0) {
				sliding_window.Reset ();
				string after_match = text.Substring (prev_match_end_pos, pos - prev_match_end_pos);
				snippet_line.AddNonMatchFragment (after_match);
				//Console.WriteLine ("Adding [{0}, {1}]:[{2}]", prev_match_end_pos, pos - 1, after_match);

				//Console.WriteLine ("Sending snippet: {0}", snippet_line.ToString ());
				return snippet_line;
			}

			sliding_window.Reset ();
			return null;
		}
	}
}
