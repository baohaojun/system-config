//
// FilterPython.cs
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

	public class FilterPython : FilterSource {

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
			int NumKeyWords = 29;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["and"] = true;
			key_words_hash ["assert"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["continue"] = true;
			key_words_hash ["def"] = true;
			key_words_hash ["del"] = true;
			key_words_hash ["elif"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["except"] = true;

			/* 11 - 20 */
			key_words_hash ["exec"] = true;
			key_words_hash ["finally"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["from"] = true;
			key_words_hash ["global"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["import"] = true;
			key_words_hash ["in"] = true;
			key_words_hash ["is"] = true;
			key_words_hash ["lambda"] = true;

			/* 21 - 29 */
			key_words_hash ["not"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["pass"] = true;
			key_words_hash ["print"] = true;
			key_words_hash ["raise"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["try"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["yield"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterPython ()
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
