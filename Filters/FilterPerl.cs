//
// FilterPerl.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterPerl : FilterSource {

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
			int NumKeyWords = 55;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["and"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["chop"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["close"] = true;
			key_words_hash ["closedir"] = true;
			key_words_hash ["continue"] = true;
			key_words_hash ["defined"] = true;
			key_words_hash ["die"] = true;
			key_words_hash ["do"] = true;

			/* 11 - 20 */
			key_words_hash ["eval"] = true;
			key_words_hash ["each"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["elsif"] = true;
			key_words_hash ["eof"] = true;
			key_words_hash ["eq"] = true;
			key_words_hash ["exec"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["foreach"] = true;
			key_words_hash ["ge"] = true;

			/* 21 - 30 */
			key_words_hash ["getc"] = true;
			key_words_hash ["glob"] = true;
			key_words_hash ["goto"] = true;
			key_words_hash ["gt"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["index"] = true;
			key_words_hash ["keys"] = true;
			key_words_hash ["last"] = true;
			key_words_hash ["le"] = true;
			key_words_hash ["length"] = true;

			/* 31 - 40 */
			key_words_hash ["lt"] = true;
			key_words_hash ["ne"] = true;
			key_words_hash ["next"] = true;
			key_words_hash ["not"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["pick"] = true;
			key_words_hash ["print"] = true;
			key_words_hash ["quit"] = true;
			key_words_hash ["redo"] = true;
			key_words_hash ["rename"] = true;

			/* 41 - 50 */
			key_words_hash ["reply"] = true;
			key_words_hash ["require"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["scalar"] = true;
			key_words_hash ["sub"] = true;
			key_words_hash ["tr"] = true;
			key_words_hash ["unless"] = true;
			key_words_hash ["until"] = true;
			key_words_hash ["use"] = true;
			key_words_hash ["undef"] = true;

			/* 51 - 55 */
			key_words_hash ["values"] = true;
			key_words_hash ["wantarray"] = true;
			key_words_hash ["warn"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["xor"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterPerl ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{

		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.Python_Style;
		}

	}
}
