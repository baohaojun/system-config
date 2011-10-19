//
// UnixTransport.cs
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

	public class UnixTransport : Transport {
		
		private string socket_name = null;
		private UnixClient client = null;
		private byte[] network_data = new byte [4096];

		public UnixTransport ()
			: this (null)
		{			
		}

		public UnixTransport (string client_name)
			: base (true)
		{
			// Use the default socket name when passed null
			if (String.IsNullOrEmpty (client_name))
				client_name = "socket";

			string storage_dir = PathFinder.GetRemoteStorageDir (false);

			if (storage_dir == null)
				throw new System.Net.Sockets.SocketException ();

			this.socket_name = Path.Combine (storage_dir, client_name);
		}

		public override void Close ()
		{
			bool previously_closed = this.IsClosed;

			// Important to set this before we close the
			// UnixClient, since that will trigger the
			// ReadCallback() method, reading 0 bytes off the
			// wire, and we check this.closed in there.
			this.IsClosed = true;

			if (client != null) {
				client.Close ();
				client = null;
			}

			if (!previously_closed)
				InvokeClosedEvent ();
		}

		protected override void SendRequest (RequestMessage request)
		{
			client = new UnixClient (this.socket_name);
			client.SendBufferSize = 4096;
			client.ReceiveBufferSize = 4096;
			NetworkStream stream = client.GetStream ();
			
			base.SendRequest (request, stream);
		}
		
		// This function will be called from its own thread
		protected override void ReadCallback (IAsyncResult ar)
		{
			if (this.IsClosed)
				return;

			try {
				NetworkStream stream = this.client.GetStream ();
				int bytes_read = 0;

				try { 
					bytes_read = stream.EndRead (ar);
				} catch (SocketException) {
					Logger.Log.Debug ("Caught SocketException in ReadCallback");
				} catch (IOException) {
					Logger.Log.Debug ("Caught IOException in ReadCallback");
				}

				// Connection hung up, we're through
				if (bytes_read == 0) {
					this.Close ();
					return;
				}

				int end_index = -1;
				int prev_index = 0;

				do {
					// 0xff signifies end of message
					end_index = Array.IndexOf<byte> (this.network_data, (byte) 0xff, prev_index);

					int bytes_count = (end_index == -1 ? bytes_read : end_index) - prev_index;
					this.BufferStream.Write (this.network_data, prev_index, bytes_count);

					if (end_index != -1) {
						MemoryStream deserialize_stream = this.BufferStream;
						this.BufferStream = new MemoryStream ();
						
						deserialize_stream.Seek (0, SeekOrigin.Begin);
						HandleResponse (deserialize_stream);
						
						// Move past the end-of-message marker
						prev_index = end_index + 1;
					}
				} while (end_index != -1);

				// Check to see if we're still connected, and keep
				// looking for new data if so.
				if (!this.IsClosed)
					BeginRead ();
			} catch (Exception e) {
				Logger.Log.Error (e, "Got an exception while trying to read data:");
			}
		}

		protected override void BeginRead ()
		{
			NetworkStream stream = this.client.GetStream ();

			Array.Clear (this.network_data, 0, this.network_data.Length);
			stream.BeginRead (this.network_data, 0, this.network_data.Length, new AsyncCallback (ReadCallback), null);
		}

		public override void SendAsyncBlocking (RequestMessage request)
		{
			Exception ex = null;

			try {
				SendRequest (request);
			} catch (IOException e) {
				ex = e;
			} catch (SocketException e) {
				ex = e;
			}

			if (ex != null) {
				ResponseMessage resp = new ErrorResponse (ex);				
				InvokeAsyncResponseEvent (resp);
				return;
			}
			
			NetworkStream stream = this.client.GetStream ();
			MemoryStream deserialize_stream = new MemoryStream ();

			// This buffer is annoyingly small on purpose, to avoid
			// having to deal with the case of multiple messages
			// in a single block.
			byte [] buffer = new byte [32];

			while (!this.IsClosed) {

				Array.Clear (buffer, 0, buffer.Length);

				int bytes_read;
				bytes_read = stream.Read (buffer, 0, buffer.Length);
				if (bytes_read == 0)
					break;

				int end_index;
				end_index = Array.IndexOf<byte> (buffer, (byte) 0xff);

				if (end_index == -1) {
					deserialize_stream.Write (buffer, 0, bytes_read);
				} else {
					deserialize_stream.Write (buffer, 0, end_index);
					deserialize_stream.Seek (0, SeekOrigin.Begin);

#if ENABLE_XML_DUMP
					StreamReader r = new StreamReader (deserialize_stream);
					Logger.Log.Debug ("Received response:\n{0}\n", r.ReadToEnd ());
					deserialize_stream.Seek (0, SeekOrigin.Begin);
#endif

					ResponseMessage resp;
					try {
						ResponseWrapper wrapper;
						wrapper = (ResponseWrapper) resp_serializer.Deserialize (deserialize_stream);
						
						resp = wrapper.Message;
					} catch (Exception e) {
						resp = new ErrorResponse (e);
					}

					InvokeAsyncResponseEvent (resp);

					deserialize_stream.Close ();
					deserialize_stream = new MemoryStream ();
					if (bytes_read - end_index - 1 > 0)
						deserialize_stream.Write (buffer, end_index + 1, bytes_read - end_index - 1);
				}
			}
		}


		public override ResponseMessage Send (RequestMessage request)
		{
			if (request.Keepalive)
				throw new Exception ("A blocking connection on a keepalive request is not allowed");

			Exception throw_me = null;

			try {
				SendRequest (request);
			} catch (IOException e) {
				throw_me = e;
			} catch (SocketException e) {
				throw_me = e;
			}

			if (throw_me != null)
				throw new ResponseMessageException (throw_me);

			NetworkStream stream = this.client.GetStream ();
			int bytes_read, end_index = -1;

			do {
				bytes_read = stream.Read (this.network_data, 0, 4096);

				//Logger.Log.Debug ("Read {0} bytes", bytes_read);

				if (bytes_read > 0) {
					// 0xff signifies end of message
					end_index = Array.IndexOf<byte> (this.network_data, (byte) 0xff);
					
					this.BufferStream.Write (this.network_data, 0,
								  end_index == -1 ? bytes_read : end_index);
				}
			} while (bytes_read > 0 && end_index == -1);

			// It's possible that the server side shut down the
			// connection before we had a chance to read any data.
			// If this is the case, throw a rather descriptive
			// exception.
			if (this.BufferStream.Length == 0) {
				this.BufferStream.Close ();
				throw new ResponseMessageException ("Socket was closed before any data could be read");
			}

			this.BufferStream.Seek (0, SeekOrigin.Begin);

#if ENABLE_XML_DUMP
			StreamReader dump_reader = new StreamReader (this.BufferStream);
			Logger.Log.Debug ("Received response:\n{0}\n", dump_reader.ReadToEnd ());
			this.BufferStream.Seek (0, SeekOrigin.Begin);
#endif
			
			ResponseMessage resp = null;

			try {
				ResponseWrapper wrapper = (ResponseWrapper)resp_serializer.Deserialize (this.BufferStream);
				resp = wrapper.Message;
			} catch (Exception e) {
				this.BufferStream.Seek (0, SeekOrigin.Begin);
				StreamReader r = new StreamReader (this.BufferStream);
				throw_me = new ResponseMessageException (e, "Exception while deserializing response", String.Format ("Message contents: '{0}'", r.ReadToEnd ()));
				this.BufferStream.Seek (0, SeekOrigin.Begin);
			}

			this.BufferStream.Close ();

			if (throw_me != null)
				throw throw_me;
			
			return resp;
		}
	}
}
