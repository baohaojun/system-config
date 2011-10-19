//
// FilterRuby.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Author: Uwe Hermann <uwe@hermann-uwe.de>
//
// Copyright (C) 2005 Uwe Hermann
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

	public class FilterRuby : FilterSource {

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
			int NumKeyWords = 38;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["BEGIN"] = true;
			key_words_hash ["END"] = true;
			key_words_hash ["alias"] = true;
			key_words_hash ["and"] = true;
			key_words_hash ["begin"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["def"] = true;
			key_words_hash ["defined"] = true;

			/* 11 - 20 */
			key_words_hash ["do"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["elsif"] = true;
			key_words_hash ["end"] = true;
			key_words_hash ["ensure"] = true;
			key_words_hash ["false"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["in"] = true;
			key_words_hash ["module"] = true;

			/* 21 - 30 */
			key_words_hash ["next"] = true;
			key_words_hash ["nil"] = true;
			key_words_hash ["not"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["redo"] = true;
			key_words_hash ["rescue"] = true;
			key_words_hash ["retry"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["self"] = true;
			key_words_hash ["super"] = true;

			/* 31 - 38 */
			key_words_hash ["then"] = true;
			key_words_hash ["true"] = true;
			key_words_hash ["undef"] = true;
			key_words_hash ["unless"] = true;
			key_words_hash ["until"] = true;
			key_words_hash ["when"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["yield"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterRuby ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{

		}

		override protected void DoPullSetup ()
		{
                        // Ruby also supports "#" as comment, so,
			// adding Python_Style will process that as well.
			SrcLangType = LangType.Python_Style;
		}
	}
}
