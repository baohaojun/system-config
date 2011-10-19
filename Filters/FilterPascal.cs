//
// FilterPascal.cs
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

	public class FilterPascal : FilterSource {

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
			int NumKeyWords = 52;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["and"] = true;
			key_words_hash ["end"] = true;
			key_words_hash ["library"] = true;
			key_words_hash ["shl"] = true;
			key_words_hash ["array"] = true;
			key_words_hash ["exports"] = true;
			key_words_hash ["mod"] = true;
			key_words_hash ["shr"] = true;
			key_words_hash ["asm"] = true;
			key_words_hash ["file"] = true;

			/* 11 - 20 */
			key_words_hash ["nil"] = true;
			key_words_hash ["string"] = true;
			key_words_hash ["begin"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["not"] = true;
			key_words_hash ["then"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["function"] = true;
			key_words_hash ["object"] = true;
			key_words_hash ["to"] = true;

			/* 21 - 30 */
			key_words_hash ["const"] = true;
			key_words_hash ["goto"] = true;
			key_words_hash ["of"] = true;
			key_words_hash ["type"] = true;
			key_words_hash ["constructor"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["unit"] = true;
			key_words_hash ["declare"] = true;
			key_words_hash ["implementation"] = true;

			/* 31 - 40 */
			key_words_hash ["packed"] = true;
			key_words_hash ["until"] = true;
			key_words_hash ["destructor"] = true;
			key_words_hash ["in"] = true;
			key_words_hash ["procedure"] = true;
			key_words_hash ["uses"] = true;
			key_words_hash ["div"] = true;
			key_words_hash ["inherited"] = true;
			key_words_hash ["program"] = true;
			key_words_hash ["var"] = true;

			/* 41 - 50 */
			key_words_hash ["do"] = true;
			key_words_hash ["inline"] = true;
			key_words_hash ["record"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["downto"] = true;
			key_words_hash ["interface"] = true;
			key_words_hash ["repeat"] = true;
			key_words_hash ["with"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["label"] = true;

			/* 51 - 52 */
			key_words_hash ["set"] = true;
			key_words_hash ["xor"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterPascal ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
//			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-pascal"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.Pascal_Style;
		}
	}
}
