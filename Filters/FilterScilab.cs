//
// FilterScilab.cs
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

	public class FilterScilab : FilterSource {

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
			int NumKeyWords = 36;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["abort"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["clear"] = true;
			key_words_hash ["clearglobal"] = true;
			key_words_hash ["continue"] = true;
			key_words_hash ["debug"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["elsif"] = true;
			key_words_hash ["end"] = true;

			/* 11 - 20 */
			key_words_hash ["endfunction"] = true;
			key_words_hash ["exit"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["function"] = true;
			key_words_hash ["funptr"] = true;
			key_words_hash ["global"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["iserror"] = true;
			key_words_hash ["isglobal"] = true;
			key_words_hash ["mode"] = true;

			/* 21 - 30 */
			key_words_hash ["null"] = true;
			key_words_hash ["pause"] = true;
			key_words_hash ["predef"] = true;
			key_words_hash ["quit"] = true;
			key_words_hash ["resume"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["select"] = true;
			key_words_hash ["then"] = true;
			key_words_hash ["typename"] = true;
			key_words_hash ["what"] = true;

			/* 31 - 36 */
			key_words_hash ["where"] = true;
			key_words_hash ["whereami"] = true;
			key_words_hash ["whereis"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["who"] = true;
			key_words_hash ["whos"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterScilab ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromExtension (".sci"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.C_Style;
		}
	}
}
