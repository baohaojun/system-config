//
// NoiseFilter.cs
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2004-2005 Novell, Inc.
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

using Lucene.Net.Analysis;
using LNSA = Lucene.Net.Analysis.Standard;

namespace Beagle.Daemon {

	// TokenFilter which does several fancy things
	// 1. Removes words which are potential noise like dhyhy8ju7q9
	// 2. Splits email addresses into meaningful tokens
	// 3. Splits hostnames into subparts
	public class NoiseEmailHostFilter : TokenFilter {
			
		public delegate void LinkCallback (string s, bool email);
		private LinkCallback link_call_back;

		private bool tokenize_email_hostname;

		TokenStream token_stream;

		public NoiseEmailHostFilter (TokenStream input, bool tokenize_email_hostname)
			: this (input, tokenize_email_hostname, null)
		{
		}

		public NoiseEmailHostFilter (TokenStream input, bool tokenize_email_hostname, LinkCallback link_call_back)
			: base (input)
		{
			this.token_stream = input;
			this.tokenize_email_hostname = tokenize_email_hostname;
			this.link_call_back = link_call_back;
		}

		// FIXME: we should add some heuristics that are stricter
		// but explicitly try to avoid filtering out dates,
		// phone numbers, etc.
		private static bool IsNoise (string text)
		{
			// Anything really long is almost certainly noise.
			if (text.Length > 30) 
				return true;

			// Look at how often we switch between numbers and letters.
			// Scoring:
			// <letter> <digit>   1
			// <digit> <letter>   1
			// <x> <punct>+ <x>   1
			// <x> <punct>+ <y>   2
			const int transitions_cutoff = 4;
			int last_type = -1, last_non_punct_type = -1, first_type = -1;
			bool has_letter = false, has_digit = false, has_punctuation = false;
			int transitions = 0;
			for (int i = 0; i < text.Length && transitions < transitions_cutoff; ++i) {
				char c = text [i];
				int type = -1;
				if (Char.IsLetter (c)) {
					type = 1;
					has_letter = true;
				} else if (Char.IsDigit (c)) {
					type = 2;
					has_digit = true;
				} else if (Char.IsPunctuation (c)) {
					type = 3;
					has_punctuation = true;
				}
					
				if (type != -1) {
						
					if (type != last_type) {
						if (last_type == 3) {
							if (type != last_non_punct_type)
								++transitions;
						} else {
							++transitions;
						}
					}

					if (first_type == -1)
						first_type = type;

					last_type = type;
					if (type != 3)
						last_non_punct_type = type;
				}
			}

			// If we make too many transitions, it must be noise.
			if (transitions >= transitions_cutoff) 
				return true;

			// If we consist of nothing but digits and punctuation, treat it
			// as noise if it is too long.
			if (transitions == 1 && first_type != 1 && text.Length > 10)
				return true;

			// We are very suspicious of long things that make lots of
			// transitions
			if (transitions > 3 && text.Length > 10) 
				return true;

			// Beware of anything long that contains a little of everything.
			if (has_letter && has_digit && has_punctuation && text.Length > 10)
				return true;

			//Logger.Log.Debug ("BeagleNoiseFilter accepted '{0}'", text);
			return false;
				
		}

		// Dont scan these tokens for additional noise
		// Someone might like to search for emails, hostnames and
		// phone numbers (which fall under type NUM)
		private static readonly string tokentype_email
			= LNSA.StandardTokenizerImpl.TOKEN_TYPES [LNSA.StandardTokenizerImpl.EMAIL];
		private static readonly string tokentype_host 
			= LNSA.StandardTokenizerImpl.TOKEN_TYPES [LNSA.StandardTokenizerImpl.HOST];
		private static readonly string tokentype_number 
			= LNSA.StandardTokenizerImpl.TOKEN_TYPES [LNSA.StandardTokenizerImpl.NUM];
		private static readonly string tokentype_alphanum
			= LNSA.StandardTokenizerImpl.TOKEN_TYPES [LNSA.StandardTokenizerImpl.ALPHANUM];

		private bool ProcessToken (ref Lucene.Net.Analysis.Token token)
		{
			string type = token.Type ();

			if (type == tokentype_number) {
				// nobody will remember more than 20 digits
				return (token.TermText ().Length <= 20);
			} else if (type == tokentype_alphanum) {
				string text = token.TermText ();
				int begin = 0;
				bool found = false;
				// Check if number, in that case strip 0's from beginning
				foreach (char c in text) {
					if (! Char.IsDigit (c)) {
						begin = 0;
						break;
					} else if (! found) {
						if (c == '0')
							begin ++;
						else
							found = true;
					}
				}

				if (begin == 0)
					return ! IsNoise (text);
				token = new Lucene.Net.Analysis.Token (
					text.Remove (0, begin),
					begin,
					token.EndOffset (),
					type);
				return true;
			} else if (type == tokentype_email) {
				if (tokenize_email_hostname)
					ProcessEmailToken (token);
				return true;
			} else if (type == tokentype_host) {
				if (tokenize_email_hostname)
					ProcessURLToken (token);
				return true;
			} else
				// FIXME: Noise should be only tested on token type alphanum
				return ! IsNoise (token.TermText ());
		}

		// State for creating smaller tokens from larger email/hostname tokens
		private string[] parts = null;
		private int parts_index = -1;
		private int last_end_offset = -1;
		private string token_type = null;

		public override Lucene.Net.Analysis.Token Next ()
		{
			if (parts != null) {
				if (++parts_index < parts.Length) {
					string part = parts [parts_index];
					Lucene.Net.Analysis.Token part_token;
					// FIXME: Searching for google.com will not match www.google.com.
					// If we decide to allow google-style "abcd.1234" which means
					// "abcd 1234" as a consequtive phrase, then adjusting
					// the startOffset and endOffset would enable matching
					// google.com to www.google.com
					int start_offset = (parts_index == 0 && token_type == tokentype_email ?
						0 :
						last_end_offset + 1); // assuming only one separator
					int end_offset = start_offset + part.Length;
					part_token = new Lucene.Net.Analysis.Token (part,
									       start_offset,
									       end_offset,
									       token_type);
					part_token.SetPositionIncrement (0);
					last_end_offset = (parts_index == 0 && token_type == tokentype_email ?
						-1 :
						end_offset); // assuming only one separator
					return part_token;
				} else {
					// clear the array
					parts = null;
					parts_index = -1;
					last_end_offset = -1;
					token_type = null;
				}
			}

			Token token;
			while ( (token = token_stream.Next ()) != null) {
                          //Console.WriteLine ("Found token: [{0}]", token.TermText ());
                          return token;
			}
			return null;
		}

		private static readonly char[] replace_array = { '@', '.', '-', '_', '+' };

		private void ProcessEmailToken (Lucene.Net.Analysis.Token token)
		{
			token_type = tokentype_email;

			string email = token.TermText ();
			parts = email.Split (replace_array);
			if (parts.Length == 1) // safety check
				return;

			int index_at = email.IndexOf ('@');
			// store username part as a large token
			// and also remove the final tld part
			Array.Copy (parts, 0, parts, 1, parts.Length - 1);
			parts [0] = email.Substring (0, index_at);
#if ENABLE_RDF_ADAPTER
			if (link_call_back != null)
				link_call_back ("mailto://" + email, true);
#endif
		}

		private void ProcessURLToken (Lucene.Net.Analysis.Token token)
		{
			token_type = tokentype_host;

			string hostname = token.TermText ();
			parts = hostname.Split ('.');

			if (parts [0] != "www")
				return;

			// remove initial www
			Array.Copy (parts, 1, parts, 0, parts.Length - 1);
			Array.Resize (ref parts, parts.Length - 1);
			// FIXME: Remove final tld
			// Any string of form "<alnum> '.')+<alnum>" has type HOST
			// Removing last token might remove important words from non-host
			// string of that form. To fix that, we need to match against the
			// huge list of TLDs.
		}
	}

#if Noisefilter
	public class AnalyzerTest {
		public static void Main ()
		{
			Analyze (Console.In);
		}

		public static void Analyze (TextReader reader)
		{
			Lucene.Net.Analysis.Token lastToken = null;
			Analyzer indexing_analyzer = new LuceneCommon.BeagleAnalyzer (true);
			TokenStream stream = indexing_analyzer.TokenStream ("Text", reader);

			int position = 1;
			for (Lucene.Net.Analysis.Token t = stream.Next(); t != null; t = stream.Next())
			{
				position += (t.GetPositionIncrement() - 1);
				Console.WriteLine (t);
			}
		}
	}
#endif
}
