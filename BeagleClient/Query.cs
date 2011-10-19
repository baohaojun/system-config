//
// Query.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.Collections;
using System.Collections.Generic;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

	[Flags]
	public enum QueryDomain {
		Local        = 1,
		System       = 2,
		Neighborhood = 4,
		Global       = 8,
		All          = 15
	}

	public class Query : RequestMessage {

		private ArrayList parts = new ArrayList ();

		private ArrayList exact_text = null;
		private ArrayList stemmed_text = null;

		// FIXME: This is a good default when on an airplane.
		private QueryDomain domain_flags = QueryDomain.Local | QueryDomain.System;

		public delegate void HitsAdded (HitsAddedResponse response);
		public event HitsAdded HitsAddedEvent;

		public delegate void HitsSubtracted (HitsSubtractedResponse response);
		public event HitsSubtracted HitsSubtractedEvent;

		public delegate void Finished (FinishedResponse response);
		public event Finished FinishedEvent;

		public Query () : base (true)
		{
			this.RegisterAsyncResponseHandler (typeof (HitsAddedResponse), OnHitsAdded);
			this.RegisterAsyncResponseHandler (typeof (HitsSubtractedResponse), OnHitsSubtracted);
			this.RegisterAsyncResponseHandler (typeof (FinishedResponse), OnFinished);
			this.RegisterAsyncResponseHandler (typeof (ErrorResponse), OnError);
			this.RegisterAsyncResponseHandler (typeof (SearchTermResponse), OnSearchTerms);
		}

		public Query (string str) : this ()
		{
			AddText (str);
		}

		///////////////////////////////////////////////////////////////

		private void OnHitsAdded (ResponseMessage r)
		{
			HitsAddedResponse response = (HitsAddedResponse) r;

			if (this.HitsAddedEvent != null)
				this.HitsAddedEvent (response);
		}

		private void OnHitsSubtracted (ResponseMessage r)
		{
			HitsSubtractedResponse response = (HitsSubtractedResponse) r;

			if (this.HitsSubtractedEvent != null)
				this.HitsSubtractedEvent (response);
		}

		private void OnFinished (ResponseMessage r)
		{
			FinishedResponse response = (FinishedResponse) r;
	
			if (this.FinishedEvent != null)
				this.FinishedEvent (response);
		}

		private void OnError (ResponseMessage r)
		{
			ErrorResponse response = (ErrorResponse) r;
			throw new ResponseMessageException (response);
		}

		private void OnSearchTerms (ResponseMessage r)
		{
			SearchTermResponse response = (SearchTermResponse) r;
			ProcessSearchTermResponse (response);
		}

		///////////////////////////////////////////////////////////////

		// This is exposed for the benefit of QueryDriver.DoQueryLocal
		public void ProcessSearchTermResponse (SearchTermResponse response)
		{
			exact_text = response.ExactText;
			stemmed_text = response.StemmedText;
		}

		///////////////////////////////////////////////////////////////

		// Warning: For the moment, the daemon is allowed to IGNORE
		// index listener queries at its discretion... so don't assume
		// that they will work for you!  Listener queries should only be
		// used for debugging and testing.

		private bool is_index_listener = false;
		public bool IsIndexListener {
			set { is_index_listener = value; }
			get { return is_index_listener; }
		}

		///////////////////////////////////////////////////////////////

		public void ClearParts ()
		{
			if (parts != null)
				parts.Clear ();
		}

		public void AddPart (QueryPart part)
		{
			if (part != null)
				parts.Add (part);
		}

		/// <summary>
		/// This is a human-entered query string that will be parsed in
		/// the daemon.
		/// </summary>
		/// <param name="str">
		/// A <see cref="System.String"/>
		/// </param>
		public void AddText (string str)
		{
			QueryPart_Human part = new QueryPart_Human ();
			part.QueryString = str;
			AddPart (part);
		}

		[XmlArrayItem (ElementName="Part", Type=typeof (QueryPart))]
		[XmlArray (ElementName="Parts")]
		public ArrayList Parts {
			get { return parts; }
		}

		[XmlIgnore]
		public ICollection Text {
			get { return exact_text; }
		}

		[XmlIgnore]
		public string QuotedText {
			get {
				StringBuilder builder = new StringBuilder ();
				foreach (string text in Text) {
					string text_cooked = text;
					if (builder.Length > 0)
						builder.Append (' ');
					bool contains_space = (text.IndexOf (' ') != -1);
					if (contains_space) {
						text_cooked = text.Replace ("\"", "\\\"");
						builder.Append ('"');
					}
					builder.Append (text_cooked);
					if (contains_space)
						builder.Append ('"');
				}
				return builder.ToString ();
			}
		}

		[XmlIgnore]
		public ICollection StemmedText {
			get { return stemmed_text; }
		}
						
		///////////////////////////////////////////////////////////////

		public void AddDomain (QueryDomain domain)
		{
			domain_flags |= domain;
		}

		public void RemoveDomain (QueryDomain domain)
		{
			domain_flags &= ~domain;
		}

		public bool AllowsDomain (QueryDomain domain)
		{
			return (domain_flags & domain) != 0;
		}

		public QueryDomain QueryDomain {
			get { return domain_flags; }
			set { domain_flags = value; }
		}

		///////////////////////////////////////////////////////////////

		private int max_hits = 100;
		public int MaxHits {
			get { return max_hits; }
			set { max_hits = value; }
		}

		///////////////////////////////////////////////////////////////

		[XmlIgnore]
		public bool IsEmpty {
			get { return parts.Count == 0; }
		}

		public override string ToString ()
		{
			StringBuilder sb = new StringBuilder ();

			foreach (QueryPart p in parts)
				sb.Append (p.ToString () + "\n");

			return sb.ToString ();
		}
	}


	// Synchronous query to return the number of matches
	public class CountMatchQuery : Query {

		public CountMatchQuery (string str) : this ()
		{
			AddText (str);
		}

		public CountMatchQuery ()
		{
			// RDFQuery is a sync message
			this.UnregisterAsyncResponseHandler (typeof (HitsAddedResponse));
			this.UnregisterAsyncResponseHandler (typeof (HitsSubtractedResponse));
			this.UnregisterAsyncResponseHandler (typeof (FinishedResponse));
			this.UnregisterAsyncResponseHandler (typeof (ErrorResponse));
			this.UnregisterAsyncResponseHandler (typeof (SearchTermResponse));

			Keepalive = false;
		}
	}
}
