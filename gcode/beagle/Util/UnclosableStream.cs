//
// UnclosableStream.cs
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

namespace Beagle.Util {

	public class UnclosableStream : Stream {

		Stream base_stream;
		bool   close_was_called = false;

		public UnclosableStream (Stream base_stream) : base ()
		{
			this.base_stream = base_stream;
		}
		
		public Stream BaseStream {
			get { return base_stream; }
		}

		public bool CloseWasCalled {
			get { return close_was_called; }
		}

		public override bool CanRead {
			get { return base_stream.CanRead; }
		}

		public override bool CanWrite {
			get { return base_stream.CanWrite; }
		}

		public override bool CanSeek {
			get { return base_stream.CanSeek; }
		}

		public override long Length {
			get { return base_stream.Length; }
		}

		public override long Position {
			get { return base_stream.Position; }
			set { base_stream.Position = value; }
		}

		public override void Close ()
		{
			base_stream.Flush ();
			close_was_called = true;
		}

		public override void Flush ()
		{
			base_stream.Flush ();
		}

		public override int Read (byte [] buffer, int offset, int count)
		{
			return base_stream.Read (buffer, offset, count);
		}

		public override int ReadByte ()
		{
			return base_stream.ReadByte ();
		}

		public override long Seek (long offset, SeekOrigin origin)
		{
			return base_stream.Seek (offset, origin);
		}

		public override void SetLength (long value)
		{
			base_stream.SetLength (value);
		}

		public override void Write (byte [] buffer, int offset, int count)
		{
			base_stream.Write (buffer, offset, count);
		}

		public override void WriteByte (byte value)
		{
			base_stream.WriteByte (value);
		}

		public override IAsyncResult
                BeginRead (byte [] buffer, int offset, int count, AsyncCallback cback, object state)
		{
			return base_stream.BeginRead (buffer, offset, count, cback, state);
		}

                public override IAsyncResult
                BeginWrite (byte [] buffer, int offset, int count, AsyncCallback cback, object state)
		{
			return base_stream.BeginWrite (buffer, offset, count, cback, state);
		}

		public override int EndRead (IAsyncResult async_result)
		{
			return base_stream.EndRead (async_result);
		}

		public override void EndWrite (IAsyncResult async_result)
		{
			base_stream.EndWrite (async_result);
		}
	}
}
