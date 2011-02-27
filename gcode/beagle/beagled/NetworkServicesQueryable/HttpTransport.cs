//
// HttpTransport.cs
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

using Beagle;
using Beagle.Util;

namespace Beagle.Daemon.NetworkServicesQueryable {

	internal class HttpTransport : Transport {
		
		private string url = null;
		private System.Net.HttpWebRequest http_request = null;
		private byte[] network_data = new byte [14096];
	
		public HttpTransport (string url)
		{
			Logger.Log.Debug ("HttpClient: Created client for {0}", url);
			this.url = url; 
		}
		
		protected override void SendRequest (RequestMessage request)
		{
			Logger.Log.Debug ("Sending request to {0}", url);

			http_request = (HttpWebRequest) System.Net.WebRequest.Create (url);
			http_request.Method = "POST";
			http_request.KeepAlive = true;
			http_request.AllowWriteStreamBuffering = false;
			http_request.SendChunked = true;
			http_request.Timeout = 5*1000; // 5 sec
			http_request.ReadWriteTimeout = 5*1000; // 5 sec
			
			Stream stream = http_request.GetRequestStream ();			
			base.SendRequest (request, stream);

			// FIXME: Should we close the stream ?
			stream.Close ();
				
			Logger.Log.Debug ("HttpClient: Sent request");
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

			WebResponse response = http_request.GetResponse ();
			Stream stream = response.GetResponseStream ();
			int bytes_read, end_index = -1;

			do {
				bytes_read = stream.Read (network_data, 0, 4096);

				if (bytes_read > 0) {
					// 0xff signifies end of message
					end_index = Array.IndexOf<byte> (network_data, (byte) 0xff);
					this.BufferStream.Write (network_data, 0, end_index == -1 ? bytes_read : end_index);
				}
			} while (bytes_read > 0 && end_index == -1);

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
				throw_me = new ResponseMessageException (e);
			}

			this.BufferStream.Close ();

			if (throw_me != null)
				throw throw_me;
			
			return resp;
		}
		
		public override void Close ()
		{
			bool previously_closed = this.IsClosed;
			
			if (http_request != null) {
				http_request.Abort ();
				http_request = null;
			}
				
			this.IsClosed = true;
			
			if (!previously_closed)
				InvokeClosedEvent ();
		}
		
		protected override void BeginRead ()
		{
			WebResponse response = http_request.GetResponse ();
			Stream stream = response.GetResponseStream ();
			
			Array.Clear (network_data, 0, network_data.Length);
			
			try {
				stream.BeginRead (network_data, 0, network_data.Length,
						  new AsyncCallback (ReadCallback), stream);
			} catch (IOException) {
				Logger.Log.Debug ("Caught IOException in BeginRead");
				Close ();
			}
			
		}
		
		protected override void ReadCallback (IAsyncResult result)
		{			
			if (this.IsClosed)
				return;

			try {
				Stream stream = (Stream) result.AsyncState;
				int bytes_read = 0;
  				
				try { 
					bytes_read = stream.EndRead (result);
				} catch (SocketException) {
					Logger.Log.Debug ("Caught SocketException in ReadCallback");
					Close ();
				} catch (IOException) {
					Logger.Log.Debug ("Caught IOException in ReadCallback");
					Close ();
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
					end_index = Array.IndexOf<byte> (network_data, (byte) 0xff, prev_index);
					
					if (end_index > bytes_read) {
						// I'm not sure how this ever comes to be true, but it does,
						// even though the array is cleared
						end_index = -1;
					}
					
					int bytes_count = ((end_index == -1) ? bytes_read : end_index) - prev_index;
					this.BufferStream.Write (network_data, prev_index, bytes_count);
					
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
				Logger.Log.Error ("Got an exception while trying to read data:");
				Logger.Log.Error (e);
				
				ResponseMessage resp = new ErrorResponse (e);
				InvokeAsyncResponseEvent (resp);

				return;
			}
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
				this.InvokeAsyncResponseEvent (resp);

				return;
			}
			

			Stream stream = this.http_request.GetResponse ().GetResponseStream ();
			MemoryStream deserialize_stream = new MemoryStream ();

			// This buffer is annoyingly small on purpose, to avoid
			// having to deal with the case of multiple messages
			// in a single block.
			byte [] buffer = new byte [32];

			while (! this.IsClosed) {

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
					
					this.InvokeAsyncResponseEvent (resp);

					deserialize_stream.Close ();
					deserialize_stream = new MemoryStream ();
					if (bytes_read - end_index - 1 > 0)
						deserialize_stream.Write (buffer, end_index+1, bytes_read-end_index-1);
				}
			}
		}
	}
}
