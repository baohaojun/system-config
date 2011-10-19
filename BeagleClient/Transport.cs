//
// Transport.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2007 Lukas Lipka <lukaslipka@gmail.com>.
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
using System.Net;
using System.Net.Sockets;
using System.Xml.Serialization;
using System.Threading;
using Mono.Unix;

using GLib;

using Beagle.Util;

namespace Beagle {

	public abstract class Transport {

		protected static XmlSerializer req_serializer = new XmlSerializer (typeof (RequestWrapper), RequestMessage.Types);
		protected static XmlSerializer resp_serializer = new XmlSerializer (typeof (ResponseWrapper), ResponseMessage.Types);

		private bool local = false;

		private MemoryStream buffer_stream = new MemoryStream ();
		private bool closed = false;

		public delegate void AsyncResponseDelegate (ResponseMessage response);
		public delegate void ClosedDelegate ();

		public event AsyncResponseDelegate AsyncResponse;
		public event ClosedDelegate Closed;

		public Transport (bool local)
		{
			this.local = local;
		}

		public Transport ()
			: this (false)
		{
		}

		~Transport ()
		{
			Close ();
		}

		public abstract void Close ();
		public abstract ResponseMessage Send (RequestMessage request);
		public abstract void SendAsyncBlocking (RequestMessage request);
		protected abstract void SendRequest (RequestMessage request);
		protected abstract void ReadCallback (IAsyncResult ar);
		protected abstract void BeginRead ();
		
		protected void SendRequest (RequestMessage request, Stream stream)
		{
			// The socket may be shut down at some point here.  It
			// is the caller's responsibility to handle the error
			// correctly.
#if ENABLE_XML_DUMP
			MemoryStream mem_stream = new MemoryStream ();
			XmlFu.SerializeUtf8 (req_serializer, mem_stream, new RequestWrapper (request));
			mem_stream.Seek (0, SeekOrigin.Begin);
			StreamReader r = new StreamReader (mem_stream);
			Logger.Log.Debug ("Sending request:\n{0}\n", r.ReadToEnd ());
			mem_stream.Seek (0, SeekOrigin.Begin);
			mem_stream.WriteTo (stream);
			mem_stream.Close ();
#else
			XmlFu.SerializeUtf8 (req_serializer, stream, new RequestWrapper (request));
#endif
			// Send end of message marker
			stream.WriteByte (0xff);
			stream.Flush ();
		}

		protected void HandleResponse (Stream deserialize_stream)
		{
#if ENABLE_XML_DUMP
			StreamReader r = new StreamReader (deserialize_stream);
			Logger.Log.Debug ("Received response:\n{0}\n", r.ReadToEnd ());
			deserialize_stream.Seek (0, SeekOrigin.Begin);
#endif
			ResponseWrapper wrapper = (ResponseWrapper)resp_serializer.Deserialize (deserialize_stream);
			ResponseMessage response = wrapper.Message;
			deserialize_stream.Close ();

			// Run the handler in an idle handler so that events are thrown
			// in the main thread instead of this inferior helper thread.
			EventThrowingClosure closure = new EventThrowingClosure (this, response);
			GLib.Idle.Add (new IdleHandler (closure.ThrowEvent));
		}

		public void SendAsync (RequestMessage request)
		{
			Exception exception = null;

			try {
				SendRequest (request);
			} catch (IOException e) {
				exception = e;
			} catch (SocketException e) {
				exception = e;
			}

			if (exception == null) {
				BeginRead ();
				return;
			}

			ResponseMessage response = new ErrorResponse (exception);
			
			if (AsyncResponse != null)
				AsyncResponse (response);
		}

		protected void InvokeClosedEvent ()
		{
			if (Closed != null)
				Closed ();
		}
		
		protected void InvokeAsyncResponseEvent (ResponseMessage response)
		{
			if (AsyncResponse != null)
				AsyncResponse (response);
		}

		private class EventThrowingClosure {
		
			private Transport transport = null;
			private ResponseMessage response = null;

			public EventThrowingClosure (Transport transport, ResponseMessage response)
			{
				this.transport = transport;
				this.response = response;
			}

			public bool ThrowEvent ()
			{
				if (transport.AsyncResponse != null)
					transport.AsyncResponse (this.response);

				return false;
			}
		}

		public bool IsLocal {
			get { return local; }
		}

		protected MemoryStream BufferStream {
			get { return buffer_stream; }
			set { buffer_stream = value; }
		}

		protected bool IsClosed {
			get { return closed; }
			set { closed = value; }
		}
	}	
}
