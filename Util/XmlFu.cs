//
// XmlFu.cs
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
using System.Xml;
using System.Xml.Serialization;

namespace Beagle.Util {

	public class XmlFu {

		public static bool Debug = Beagle.Util.Debug.Enabled ("XmlFu");

		public static void SerializeUtf8 (XmlSerializer serializer, Stream stream, object o)
		{
			if (serializer == null)
				throw new ArgumentNullException ("serializer");

			if (stream == null)
				throw new ArgumentNullException ("stream");

			if (o == null)
				throw new ArgumentNullException ("o");

			UnclosableStream unclosable_stream = new UnclosableStream (stream);
			BufferedStream buffered_stream = new BufferedStream (unclosable_stream, 8192);

			Stopwatch w = new Stopwatch ();
			w.Start ();
			XmlTextWriter xml_writer = new XmlTextWriter (buffered_stream, Encoding.UTF8);
			xml_writer.Formatting = Formatting.Indented;
			serializer.Serialize (xml_writer, o);

			// This will flush the stream and release the buffer.
			// Normally it also closes the underlying stream,
			// which is why it wraps an UnclosableStream.
			buffered_stream.Dispose ();
			w.Stop ();

			if (Debug)
				Log.Debug (">>> Serialization of {0}: {1}", o.GetType (), w);
		}
	}
}
