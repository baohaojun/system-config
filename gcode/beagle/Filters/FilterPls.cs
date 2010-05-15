//
// FilterPls.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;

using Beagle.Daemon;

namespace Beagle.Filters {
	public class FilterPls : Beagle.Daemon.Filter {
		public FilterPls ()
		{
			SnippetMode = false;
			OriginalIsText = true;
			SetFileType ("audio");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("audio/x-scpls"));
		}

		protected override void DoOpen (FileInfo file)
		{
			string line = TextReader.ReadLine ();
			if (line != "[Playlist]")
				Error ();
		}

		override protected void DoPull ()
		{
			bool pull = false;
			do {
				string line = TextReader.ReadLine ();
				if (line == null) {
					Finished ();
					return;
				}

				// Format
				//	FileX=<path>
				//	TitleX=<title>
				//	other lines
				if (line.StartsWith ("File") ||
				    line.StartsWith ("Title")) {
					int index = line.IndexOf ('=');
					if (index != -1 && index < (line.Length - 1))
						pull = AppendLine (line.Substring (index + 1));
				}
			} while (pull);
		}
	}
}
