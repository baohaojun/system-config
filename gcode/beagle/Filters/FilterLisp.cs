//
// FilterLisp.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2005 Novell, Inc.
//
// Author: Wojciech Polak <wojciechpolak at gmail.com>

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
using System.Collections.Generic;
using System.IO;
using System.Text;

using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterScheme : FilterSource {

		static Dictionary<string, bool> key_words_hash = null;
		protected override Dictionary<string,bool> KeyWordsHash {
			get {
				if (key_words_hash == null)
					Init ();
				return key_words_hash;
			}
		}

		static void Init ()
		{
			int NumKeyWords = 64;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			// --- keywords --
			/* 1 - 10 */
			key_words_hash ["and"] = true;
			key_words_hash ["begin"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["cond"] = true;
			key_words_hash ["define"] = true;
			key_words_hash ["delay"] = true;
			key_words_hash ["do"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["lambda"] = true;

			/* 11 - 20 */
			key_words_hash ["let"] = true;
			key_words_hash ["let*"] = true;
			key_words_hash ["letrec"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["quasiquote"] = true;
			key_words_hash ["quote"] = true;
			key_words_hash ["set!"] = true;
			key_words_hash ["unquote"] = true;
			key_words_hash ["unquote-splicing"] = true;
			// -- common procedures --
			key_words_hash ["abs"] = true;

			/* 21 - 30 */
			key_words_hash ["append"] = true;
			key_words_hash ["apply"] = true;
			key_words_hash ["assoc"] = true;
			key_words_hash ["assq"] = true;
			key_words_hash ["assv"] = true;
			key_words_hash ["caar"] = true;
			key_words_hash ["cadr"] = true;
			key_words_hash ["car"] = true;
			key_words_hash ["cdr"] = true;
			key_words_hash ["ceiling"] = true;

			/* 31 - 40 */
			key_words_hash ["cons"] = true;
			key_words_hash ["denominator"] = true;
			key_words_hash ["display"] = true;
			key_words_hash ["eval"] = true;
			key_words_hash ["exp"] = true;
			key_words_hash ["expt"] = true;
			key_words_hash ["floor"] = true;
			key_words_hash [ "gcd"] = true;
			key_words_hash ["lcm"] = true;
			key_words_hash ["length"] = true;

			/* 41 - 50 */
			key_words_hash ["list-ref"] = true;
			key_words_hash ["list-tail"] = true;
			key_words_hash ["log"] = true;
			key_words_hash ["map"] = true;
			key_words_hash ["max"] = true;
			key_words_hash ["member"] = true;
			key_words_hash ["memq"] = true;
			key_words_hash ["memv"] = true;
			key_words_hash ["min"] = true;
			key_words_hash ["modulo"] = true;

			/* 51 - 60 */
			key_words_hash ["newline"] = true;
			key_words_hash ["not"] = true;
			key_words_hash ["numerator"] = true;
			key_words_hash ["quotient"] = true;
			key_words_hash ["rationalize"] = true;
			key_words_hash ["read"] = true;
			key_words_hash ["remainder"] = true;
			key_words_hash ["reverse"] = true;
			key_words_hash ["round"] = true;
			key_words_hash ["sqrt"] = true;

			/* 61 - 64 */
			key_words_hash ["string"] = true;
			key_words_hash ["truncate"] = true;
			key_words_hash ["vector"] = true;
			key_words_hash ["write"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterScheme ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-scheme"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.Lisp_Style;
		}
	}

	// TO DO: Add Emacs Lisp (FilterEmacsLisp), Common Lisp (FilterCommonLisp).
}
