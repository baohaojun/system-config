//
// Message.cs
//
// Copyright (C) 2005 Novell, Inc.
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
using System.Text;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

	public abstract class Message {

		protected static Type[] GetTypes (Type parent_type, Type attr_type)
		{
			ArrayList types = new ArrayList ();

			foreach (Assembly a in AppDomain.CurrentDomain.GetAssemblies ())
				types.AddRange (ReflectionFu.GetTypesFromAssemblyAttribute (a, attr_type));

			return (Type[]) types.ToArray (typeof (Type));
		}
	}

	public class RequestWrapper {

		public RequestMessage Message;

		// Needed by the XmlSerializer for deserialization
		public RequestWrapper () { }

		public RequestWrapper (RequestMessage request)
		{
			this.Message = request;
		}
	}
	
	public abstract class RequestMessage : Message {

		private static Type[] request_types = null;
		private static object type_lock = new object ();

		private List<Transport> transports = new List<Transport> ();
		private Hashtable handlers = new Hashtable ();
		private bool keepalive = false;

		public delegate void AsyncResponseHandler (ResponseMessage response);
		public delegate void Closed ();

		public event Closed ClosedEvent;

		public RequestMessage (Transport transport)
		{
			this.RegisterTransport (transport);
		}

		public RequestMessage (bool keepalive)
		{
			this.keepalive = keepalive;

			// Register the Unix socket transport by default
			this.RegisterTransport (new UnixTransport ());
		}

		public RequestMessage ()
			: this (false)
		{
		}
		
		~RequestMessage ()
		{
			this.Close ();
		}

		public static Type[] Types {
			get {
				lock (type_lock) {
					if (request_types == null)
						request_types = GetTypes (typeof (RequestMessage), typeof (RequestMessageTypesAttribute));
				}

				return request_types;
			}
		}

		public void Close ()
		{
			foreach (Transport transport in transports)
				transport.Close ();
		}

		public void RegisterTransport (Transport transport)
		{
			transports.Add (transport);
		}

		public void UnregisterTransport (Transport transport)
		{
			transports.Remove (transport);
		}

		public void RegisterAsyncResponseHandler (Type t, AsyncResponseHandler handler)
		{
			if (!t.IsSubclassOf (typeof (ResponseMessage)))
				throw new ArgumentException ("Type must be a subclass of ResponsePayload");

			this.handlers [t] = handler;
		}

		public void UnregisterAsyncResponseHandler (Type t)
		{
			if (!t.IsSubclassOf (typeof (ResponseMessage)))
				throw new ArgumentException ("Type must be a subclass of ResponsePayload");

			this.handlers.Remove (t);
		}

		private void OnClosed ()
		{
			if (this.ClosedEvent != null)
				this.ClosedEvent ();
		}

		private void OnAsyncResponse (ResponseMessage response)
		{
			AsyncResponseHandler async_response = (AsyncResponseHandler) this.handlers [response.GetType ()];

			if (async_response != null) {
				async_response (response);
			}
		}

		public void SendAsync ()
		{
			foreach (Transport transport in transports) {
				transport.AsyncResponse += OnAsyncResponse;
				transport.Closed += OnClosed;
				transport.SendAsync (this);
			}
		}

		public void SendAsyncBlocking ()
		{
			foreach (Transport transport in transports) {
				transport.AsyncResponse += OnAsyncResponse;
				transport.Closed += OnClosed;
				transport.SendAsyncBlocking (this);
			}
		}

		public ResponseMessage Send ()
		{
			if (transports.Count != 1)
				throw new ResponseMessageException (String.Format ("Invalid number of transports on '{0}'", this.GetType()));

			if (! transports [0].IsLocal)
				throw new ResponseMessageException (String.Format ("Non-local transports not allowed on non-async message '{0}'", this.GetType()));

			// FIXME FIXME: If ErrorResponse is received, then Daemon will try
			// to send the message. Since transport is closed ... this will cause
			// all kinds of trouble like ObjectDisposedException. The same
			// message cannot be resend in this model.
			ResponseMessage response = transports [0].Send (this);
			transports [0].Close ();
				
			// Add some nice syntactic sugar by throwing an
			// exception if the response is an error.
			ErrorResponse error = response as ErrorResponse;		

			if (error != null)
				throw new ResponseMessageException (error);
			
			if (response == null)
				throw new ResponseMessageException ("Response is null");

			return response;
		}

		[XmlIgnore]
		public bool Keepalive {
			get { return keepalive; }
			set { keepalive = value; }
		}

		[XmlIgnore]
		public ICollection<Transport> Transports
		{
			get { return transports; }
		}
	}

	public abstract class RequestMessageExecutor {
		
		public delegate void AsyncResponse (ResponseMessage response);
		public event AsyncResponse AsyncResponseEvent;

		public abstract ResponseMessage Execute (RequestMessage req);

		protected void SendAsyncResponse (ResponseMessage response)
		{
			if (this.AsyncResponseEvent != null)
				this.AsyncResponseEvent (response);
		}

		// Really only worth overriding if the request is a keepalive
		public virtual void Cleanup () { }
	}

	[AttributeUsage (AttributeTargets.Class)]
	public class RequestMessageAttribute : Attribute {

		private Type message_type;

		public RequestMessageAttribute (Type message_type)
		{
			this.message_type = message_type;
		}

		public Type MessageType {
			get { return this.message_type; }
		}
	}

	public class ResponseWrapper {

		public ResponseMessage Message;

		// Needed by the XmlSerializer for deserialization
		public ResponseWrapper () { }

		public ResponseWrapper (ResponseMessage response)
		{
			this.Message = response;
		}
	}

	public abstract class ResponseMessage : Message {

		private static Type[] response_types = null;
		private static object type_lock = new object ();

		public static Type[] Types {
			get {
				lock (type_lock) {
					if (response_types == null)
						response_types = GetTypes (typeof (ResponseMessage), typeof (ResponseMessageTypesAttribute));
				}

				return response_types;
			}
		}
	}

	public class EmptyResponse : ResponseMessage {
	}

	public class ErrorResponse : ResponseMessage {

		public string ErrorMessage;
		public string Details;

		// Needed by the XmlSerializer for deserialization
		public ErrorResponse () { }

		public ErrorResponse (Exception e)
		{
			this.ErrorMessage = e.Message;
			this.Details = e.ToString ();
		}

		public ErrorResponse (string message)
		{
			this.ErrorMessage = message;
		}
	}

	public class ResponseMessageException : Exception {

		private string details = null;

		public ResponseMessageException (ErrorResponse response)
			: base (response.ErrorMessage)
		{ 
			this.details = response.Details;
		}

		public ResponseMessageException (Exception e)
			: base (e.Message, e)
		{
		}

		public ResponseMessageException (Exception e, string message)
			: base (message, e) 
		{
		}

		public ResponseMessageException (Exception e, string message, string details)
			: base (message, e)
		{
			this.details = details;
		}

		public ResponseMessageException (string message) 
			: base (message)
		{
		}

		public override string ToString ()
		{
			StringBuilder sb = new StringBuilder ();

			sb.AppendFormat ("{0}: {1}", this.GetType (), this.Message);

			if (this.details != null)
				sb.AppendFormat ("\n  Details: {0}", this.details);

			if (this.InnerException != null) {
				sb.Append ("\n  Inner exception: ");
				sb.Append (this.InnerException.ToString ());
			}

			return sb.ToString ();
		}
	}

	[AttributeUsage (AttributeTargets.Assembly)]
	public class RequestMessageTypesAttribute : TypeCacheAttribute {
		public RequestMessageTypesAttribute (params Type[] message_types) : base (message_types) { }
	}

	[AttributeUsage (AttributeTargets.Assembly)]
	public class ResponseMessageTypesAttribute : TypeCacheAttribute {
		public ResponseMessageTypesAttribute (params Type[] message_types) : base (message_types) { }
	}

	[AttributeUsage (AttributeTargets.Assembly)]
	public class RequestMessageExecutorTypesAttribute : TypeCacheAttribute {
		public RequestMessageExecutorTypesAttribute (params Type[] executor_types) : base (executor_types) { }
	}
}
