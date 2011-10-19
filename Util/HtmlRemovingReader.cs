//
// HtmlRemovingReader.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.Text;

namespace Beagle.Util {

	// Light weight textreader to remove HTML tags from the stream
	// For heavy duty real html-to-text reader, use Filters/FilterHtml
	public class HtmlRemovingReader : TextReader {

		private TextReader reader;
		private StringBuilder sb;
	    
		public HtmlRemovingReader (TextReader reader)
		{
			this.reader = reader;
			this.sb = new StringBuilder ();
		}

		public override string ReadLine ()
		{
			string line = reader.ReadLine ();

			if (line == null)
				return null;

			sb.Length = 0;
			line = StringFu.StripTags (line, sb);
			line = StringFu.ConvertSpecialEntities (line);

			return line;
		}

		public override void Close ()
		{
			reader.Close ();
		}
	}
}
