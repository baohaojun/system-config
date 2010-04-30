//
// FilterJava.cs
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

	public class FilterJava : FilterSource {

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
			int NumKeyWords = 48;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["abstract"] = true;
			key_words_hash ["boolean"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["byte"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["catch"] = true;
			key_words_hash ["char"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["const"] = true;
			key_words_hash ["continue"] = true;

			/* 11 - 20 */
			key_words_hash ["default"] = true;
			key_words_hash ["do"] = true;
			key_words_hash ["double"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["extends"] = true;
			key_words_hash ["final"] = true;
			key_words_hash ["finally"] = true;
			key_words_hash ["float"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["goto"] = true;

			/* 21 - 30 */
			key_words_hash ["if"] = true;
			key_words_hash ["implements"] = true;
			key_words_hash ["import"] = true;
			key_words_hash ["instanceof"] = true;
			key_words_hash ["int"] = true;
			key_words_hash ["interface"] = true;
			key_words_hash ["long"] = true;
			key_words_hash ["native"] = true;
			key_words_hash ["new"] = true;
			key_words_hash ["package"] = true;

			/* 31 - 40 */
			key_words_hash ["private"] = true;
			key_words_hash ["protected"] = true;
			key_words_hash ["public"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["short"] = true;
			key_words_hash ["static"] = true;
			key_words_hash ["strictfp"] = true;
			key_words_hash ["super"] = true;
			key_words_hash ["switch"] = true;
			key_words_hash ["synchronized"] = true;

			/* 41 - 48 */
			key_words_hash ["this"] = true;
			key_words_hash ["throw"] = true;
			key_words_hash ["throws"] = true;
			key_words_hash ["transient"] = true;
			key_words_hash ["try"] = true;
			key_words_hash ["void"] = true;
			key_words_hash ["volatile"] = true;
			key_words_hash ["while"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterJava ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
                  //AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-java"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.C_Style;
		}

	}
}
