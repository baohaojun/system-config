//
// FilterCpp.cs
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

	public class FilterCpp : FilterSource {

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
			int NumKeyWords = 60;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["asm"] = true;
			key_words_hash ["auto"] = true;
			key_words_hash ["bool"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["catch"] = true;
			key_words_hash ["char"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["const"] = true;
			key_words_hash ["const_cast"] = true;

			/* 11 - 20 */
			key_words_hash ["do"] = true;
			key_words_hash ["double"] = true;
			key_words_hash ["dynamic_cast"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["enum"] = true;
			key_words_hash ["explicit"] = true;
			key_words_hash ["export"] = true;
			key_words_hash ["extern"] = true;
			key_words_hash ["false"] = true;
			key_words_hash ["float"] = true;

			/* 21 - 30 */
			key_words_hash ["for"] = true;
			key_words_hash ["friend"] = true;
			key_words_hash ["goto"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["inline"] = true;
			key_words_hash ["int"] = true;
			key_words_hash ["long"] = true;
			key_words_hash ["mutable"] = true;
			key_words_hash ["namespace"] = true;
			key_words_hash ["new"] = true;

			/* 31 - 40 */
			key_words_hash ["operator"] = true;
			key_words_hash ["private"] = true;
			key_words_hash ["public"] = true;
			key_words_hash ["protected"] = true;
			key_words_hash ["register"] = true;
			key_words_hash ["reinterpret_cast"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["short"] = true;
			key_words_hash ["signed"] = true;
			key_words_hash ["sizeof"] = true;

			/* 41 - 50 */
			key_words_hash ["static"] = true;
			key_words_hash ["static_cast"] = true;
			key_words_hash ["struct"] = true;
			key_words_hash ["switch"] = true;
			key_words_hash ["template"] = true;
			key_words_hash ["this"] = true;
			key_words_hash ["throw"] = true;
			key_words_hash ["true"] = true;
			key_words_hash ["try"] = true;
			key_words_hash ["typedef"] = true;

			/* 51 - 60 */
			key_words_hash ["typeid"] = true;
			key_words_hash ["typename"] = true;
			key_words_hash ["union"] = true;
			key_words_hash ["unsigned"] = true;
			key_words_hash ["using"] = true;
			key_words_hash ["virtual"] = true;
			key_words_hash ["void"] = true;
			key_words_hash ["volatile"] = true;
			key_words_hash ["wchar_t"] = true;
			key_words_hash ["while"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterCpp ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.C_Style;
		}

	}
}
