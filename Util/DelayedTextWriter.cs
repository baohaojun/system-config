//
// DelayedTextWriter.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.IO;
using System.Text;

namespace Beagle.Util {

	// This allows you to delay instantiating a TextWriter
	// (and thus e.g. opening a file) until the first
	// write.
	public class DelayedTextWriter : TextWriter {

		public delegate TextWriter Builder ();

		Builder builder;
		TextWriter writer;

		public DelayedTextWriter (Builder builder)
		{
			this.builder = builder;
		}

		public override Encoding Encoding {
			get {
				lock (builder) {
					if (writer == null)
						writer = builder ();
				}

				return writer.Encoding;
			}
		}

		public override void Close ()
		{
			if (writer != null)
				writer.Close ();
		}

		public override void Flush ()
		{
			if (writer != null)
				writer.Flush ();
		}

		public override void Write (char value)
		{
			lock (builder) {
				if (writer == null)
					writer = builder ();
			}
			
			writer.Write (value);
		}
	}
}
