//
// StringMatcher.cs
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

//
// This code implements the Knuth-Morris-Pratt algorithm, and is based
// on information and code I found at 
// http://www.ics.uci.edu/~eppstein/161/960227.html
//

using System;
using System.Collections;
using System.IO;

namespace Beagle.Util {

	public class StringMatcher {

		private class Pattern {
			public string Term;
			public bool   RequireLeadingWordBoundary;
			public bool   RequireTrailingWordBoundary;
		}

		public class Match {
			protected string term;
			protected int position;

			public Match (string term, int position)
			{
				this.term = term;
				this.position = position;
			}

			public string Term {
				get { return term; }
			}

			public int Position {
				get { return position; }
			}
		}

		ArrayList pattern_array = new ArrayList ();
		int [][]  overlap_vector;
		int []    state_vector;
		
		TextReader reader;
		char [] buffer = new char [8192];
		int buffer_offset;
		int buffer_len;
		int buffer_pos;
		bool finished;

		public void Add (string term,
				 bool   require_leading,
				 bool   require_trailing)
		{
			Pattern pattern = new Pattern ();
			pattern.Term = term;
			pattern.RequireLeadingWordBoundary = require_leading;
			pattern.RequireTrailingWordBoundary = require_trailing;

			pattern_array.Add (pattern);
		}

		public void Study ()
		{


		}

		public void Start (TextReader _reader)
		{
			int N = pattern_array.Count;

			// Build the overlap vector
			overlap_vector = new int [N] [];
			for (int i = 0; i < N; ++i) {
				Pattern pattern = pattern_array [i] as Pattern;
				string term = pattern.Term;
				
				int [] overlap = new int [term.Length];
				overlap [0] = -1;
				for (int j = 0; i < term.Length-1; ++j) {
					overlap [j + 1] = overlap [j] + 1;
					while (overlap [j + 1] > 0
					       && term [j] != term [overlap [j + 1] - 1])
						overlap [j + 1] = overlap [overlap [j + 1] - 1] + 1;
				}
				
				overlap_vector [i] = overlap;
			}

			state_vector = new int [N];
			
			reader = _reader;

			buffer_offset = 0;
			buffer_len = -1;
			buffer_pos = 0;

			finished = false;
		}

		public Match Find ()
		{
			if (finished)
				return null;

			int N = pattern_array.Count;

			while (true) {

				// If necessary, read another block of characters
				// into our buffer.
				if (buffer_pos >= buffer_len) {
					if (buffer_len > 0)
						buffer_offset += buffer_len;
					buffer_len = reader.Read (buffer, 0, buffer.Length);
					if (buffer_len <= 0) {
						finished = true;
						return null;
					}
				}

				while (buffer_pos < buffer_len) {
					char buffer_c = buffer [buffer_pos];
					++buffer_pos;

					// Try each pattern in sequence
					for (int i = 0; i < N; ++i) {
						Pattern pattern = pattern_array [i] as Pattern;
						string term = pattern.Term;

						while (true) {

							int state = state_vector [i];
							if (buffer_c == term [state]) {

								// If we match, move to the next state.
								++state_vector [i];

								// if we reach the last state, return
								// a Match object.
								if (state_vector [i] == term.Length) {
									state_vector [i] = 0;
									return new Match (term, buffer_offset + buffer_pos - term.Length);
								}

								// Otherwise break out of the loop so that we can
								// proceed to the next needle.
								break;

							} else if (state == 0) {
								// If we don't get a match in the first
								// state, there is no hope...
								break;
							} else {
								// Try a shorter partial match
								state_vector [i] = overlap_vector [i] [state];
							}
						}
					}
				}
			}
		}

#if false		
		public int Find (TextReader reader)
		{
			if (needle_vector == null || overlap_vector == null)
				Study ();

			int N = needle_vector.Length;

			
			int [] offset_vector = new int [N];

			while (true) {

				if (buffer_len > 0)
					buffer_offset += buffer_len;

				buffer_len = reader.Read (buffer, 0, buffer.Length);
				if (buffer_len <= 0) 
					break; // Oops... no matches found.

				// Walk across the buffer
				for (int i = 0; i < buffer_len; ++i) {

					char buffer_c = buffer [i];

					// Try each needle in sequences
					for (int j = 0; j < N; ++j) {
						string needle = needle_vector [j];
						int k = offset_vector [j];

						while (true) {
							if (buffer_c == needle [k]) {
								// If we match, move to the next state.
								++k;
								// If we reach the last state, return the
								// offset of the match.
								if (k == needle.Length)
									return buffer_offset + i - k + 1;
								// Otherwise remember the state and
								// break out of the loop so that we can
								// proceed to the next needle.
								offset_vector [j] = k;
								break;
							} else if (k == 0) {
								// If we don't get a match in the first
								// state, there is no hope...
								offset_vector [j] = 0;
								break;
							} else {
								// Try a shorter partial match
								k = overlap_vector [j] [k];
							}
						}
					}
				}
			}

			return null;
		}
#endif

#if false
		static void Main ()
		{
			StringMatcher matcher = new StringMatcher ();
			matcher.Add ("proceed");
			matcher.Add ("remember");

			TextReader reader = new StreamReader ("StringMatcher.cs");
			int offset = matcher.Find (reader);
			reader.Close ();
			if (offset < 0) {
				Console.WriteLine ("No matches found.");
			} else {
				FileStream stream = new FileStream ("StringMatcher.cs", FileMode.Open, FileAccess.Read);
				stream.Seek (offset, SeekOrigin.Begin);
				StreamReader foo = new StreamReader (stream);
				string line = foo.ReadLine ();

				Console.WriteLine ("Found match at offset {0}", offset);
				Console.WriteLine ("[{0}]", line);
			}
		}
#endif
	}
}
	

