//
// Beagle
//
// FilterTexi.cs : Trivial implementation of a Texi filter.
// Author :
//      Nagappan A <anagappan@novell.com>
//
// Copyright (C) 2004 Novell Inc
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
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterTexi : Beagle.Daemon.Filter {

		static string [] texiKeywords = {"@c ", "\\input ", "@setfilename", "@settitle",
						 "@setchapternewpage", "@ifinfo", "@end", "@titlepage",
						 "@sp", "@comment", "@center", "@page", "@vskip", "@node",
						 "@chapter", "@cindex", "@enumerate", "@item", "@code",
						 "@printindex", "@contents", "@bye"};
		
		public FilterTexi ()
		{
			// FIXME: Should this be documentation ?
			SetFileType ("documentation");
		}

		protected override void RegisterSupportedTypes ()
		{
			// Make this a general texi filter.
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-texinfo"));
		}

		/*
		FIXME:
		Other texi keywords and formatting tags needs to be handled.
		*/
		override protected void DoPull ()
		{
			string line;
						
			line = TextReader.ReadLine ();
			foreach (string keyword in texiKeywords)
				line = line.Replace (keyword, String.Empty);

			AppendLine (line);
		}
	}
}
