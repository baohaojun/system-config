//
// FilterBoo.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Paul Betts (Paul.Betts@Gmail.com)
// Copyright (C) 2006 Novell, Inc.
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

	public class FilterBoo : FilterSource {

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

			/* 1 - 10 */
			key_words_hash ["abstract"] = true;
			key_words_hash ["and"] = true;
			key_words_hash ["as"] = true;
			key_words_hash ["ast"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["continue"] = true;
			key_words_hash ["callable"] = true;
			key_words_hash ["cast"] = true;
			key_words_hash ["char"] = true;
			key_words_hash ["class"] = true;

			/* 11 - 20 */
			key_words_hash ["constructor"] = true;
			key_words_hash ["def"] = true;
			key_words_hash ["destructor"] = true;
			key_words_hash ["do"] = true;
			key_words_hash ["elif"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["ensure"] = true;
			key_words_hash ["enum"] = true;
			key_words_hash ["event"] = true;
			key_words_hash ["except"] = true;

			/* 21 - 30 */
			key_words_hash ["failure"] = true;
			key_words_hash ["final"] = true;
			key_words_hash ["from"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["false"] = true;
			key_words_hash ["get"] = true;
			key_words_hash ["given"] = true;
			key_words_hash ["goto"] = true;
			key_words_hash ["import"] = true;
			key_words_hash ["interface"] = true;

			/* 31 - 40 */
			key_words_hash ["internal"] = true;
			key_words_hash ["is"] = true;
			key_words_hash ["isa"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["in"] = true;
			key_words_hash ["not"] = true;
			key_words_hash ["null"] = true;
			key_words_hash ["of"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["otherwise"] = true;

			/* 41 - 50 */
			key_words_hash ["override"] = true;
			key_words_hash ["pass"] = true;
			key_words_hash ["namespace"] = true;
			key_words_hash ["partial"] = true;
			key_words_hash ["public"] = true;
			key_words_hash ["protected"] = true;
			key_words_hash ["private"] = true;
			key_words_hash ["raise"] = true;
			key_words_hash ["ref"] = true;
			key_words_hash ["return"] = true;

			/* 51 - 60 */
			key_words_hash ["retry"] = true;
			key_words_hash ["set"] = true;
			key_words_hash ["self"] = true;
			key_words_hash ["static"] = true;
			key_words_hash ["struct"] = true;
			key_words_hash ["try"] = true;
			key_words_hash ["transient"] = true;
			key_words_hash ["true"] = true;
			key_words_hash ["typeof"] = true;
			key_words_hash ["unless"] = true;

			/* 61 - 64 */
			key_words_hash ["virtual"] = true;
			key_words_hash ["when"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["yield"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterBoo ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
//			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-boo"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.Python_Style;
		}

	}
}
