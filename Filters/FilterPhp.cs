//
// FilterPhp.cs
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

	public class FilterPhp : FilterSource {

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
			int NumKeyWords = 59;
			key_words_hash = new Dictionary<string, bool> (NumKeyWords);

			/* 1 - 10 */
			key_words_hash ["and"] = true;
			key_words_hash ["or"] = true;
			key_words_hash ["xor"] = true;
			key_words_hash ["exception"] = true;
			key_words_hash ["array"] = true;
			key_words_hash ["as"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["class"] = true;
			key_words_hash ["const"] = true;

			/* 11 - 20 */
			key_words_hash ["continue"] = true;
			key_words_hash ["declare"] = true;
			key_words_hash ["default"] = true;
			key_words_hash ["die"] = true;
			key_words_hash ["do"] = true;
			key_words_hash ["echo"] = true;
			key_words_hash ["else"] = true;
			key_words_hash ["elseif"] = true;
			key_words_hash ["empty"] = true;
			key_words_hash ["enddeclare"] = true;

			/* 21 - 30 */
			key_words_hash ["endfor"] = true;
			key_words_hash ["endforeach"] = true;
			key_words_hash ["endif"] = true;
			key_words_hash ["extends"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["foreach"] = true;
			key_words_hash ["function"] = true;
			key_words_hash ["global"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["include"] = true;

			/* 31 - 40 */
			key_words_hash ["includeonce"] = true;
			key_words_hash ["isset"] = true;
			key_words_hash ["list"] = true;
			key_words_hash ["new"] = true;
			key_words_hash ["print"] = true;
			key_words_hash ["require"] = true;
			key_words_hash ["require_once"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["static"] = true;
			key_words_hash ["switch"] = true;

			/* 41 - 50 */
			key_words_hash ["unset"] = true;
			key_words_hash [ "use"] = true;
			key_words_hash ["var"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["final"] = true;
			key_words_hash ["php_user_filter"] = true;
			key_words_hash ["interface"] = true;
			key_words_hash ["implements"] = true;
			key_words_hash ["extends"] = true;
			key_words_hash ["public"] = true;

			/* 51 - 59 */
			key_words_hash ["private"] = true;
			key_words_hash ["protected"] = true;
			key_words_hash ["abstract"] = true;
			key_words_hash ["clone"] = true;
			key_words_hash ["try"] = true;
			key_words_hash ["catch"] = true;
			key_words_hash ["throw"] = true;
			key_words_hash ["cfunction"] = true;
			key_words_hash ["old_function"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		public FilterPhp ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
//			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-php"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-php"));
		}

		override protected void DoPullSetup ()
		{
			// By default, "C" type comments are processed.
			// Php also supports "#" as comment, so,
			// adding Python_Style will process that as well.
			SrcLangType = LangType.Python_Style;
		}

	}
}
