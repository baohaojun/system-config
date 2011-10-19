//
// FilterShellscript.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2004-2006 Novell, Inc.
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
using Beagle.Util;

namespace Beagle.Filters {

	public class FilterShellscript : FilterSource {

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
			key_words_hash ["bash"] = true;
			key_words_hash ["mv"] = true;
			key_words_hash ["cp"] = true;
			key_words_hash ["ls"] = true;
			key_words_hash ["ps"] = true;
			key_words_hash ["exit"] = true;
			key_words_hash ["export"] = true;
			key_words_hash ["echo"] = true;
			key_words_hash ["if"] = true;
			key_words_hash ["else"] = true;

			/* 11 - 20 */
			key_words_hash ["elif"] = true;
			key_words_hash ["then"] = true;
			key_words_hash ["fi"] = true;
			key_words_hash ["while"] = true;
			key_words_hash ["do"] = true;
			key_words_hash ["done"] = true;
			key_words_hash ["until"] = true;
			key_words_hash ["case"] = true;
			key_words_hash ["in"] = true;
			key_words_hash ["esac"] = true;

			/* 21 - 30 */
			key_words_hash ["select"] = true;
			key_words_hash ["for"] = true;
			key_words_hash ["function"] = true;
			key_words_hash ["time"] = true;
			key_words_hash ["break"] = true;
			key_words_hash ["cd"] = true;
			key_words_hash ["continue"] = true;
			key_words_hash ["declare"] = true;
			key_words_hash ["fg"] = true;
			key_words_hash ["kill"] = true;

			/* 31 - 38 */
			key_words_hash ["pwd"] = true;
			key_words_hash ["read"] = true;
			key_words_hash ["return"] = true;
			key_words_hash ["set"] = true;
			key_words_hash ["test"] = true;
			key_words_hash ["unset"] = true;
			key_words_hash ["wait"] = true;
			key_words_hash ["touch"] = true;

			// Increase NumKeyWords if more keywords are added
		}

		private int count;
			
		public FilterShellscript ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-shellscript"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-sh"));
		}

		override protected void DoPullSetup ()
		{
			SrcLangType = LangType.Shell_Style;
			this.count = 0;
		}

		override protected void DoPull ()
		{
			string str = TextReader.ReadLine ();
			if (str == null)
				Finished ();
			else {
				// Shell scripts usually aren't very big, so
				// never index more than 20k.  This prevents us
				// from going insane when we *do* index large
				// ones that embed binaries in them.  We're
				// really only counting characters here, but we
				// don't need to be very precise.
				this.count += str.Length;

				ExtractTokens (str);

				if (this.count > 20 * 1024) {
					Log.Debug ("Truncating shell script to 20k");
					Finished ();
				}
			}
		}
	}
}
