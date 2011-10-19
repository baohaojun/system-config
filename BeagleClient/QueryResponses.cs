//
// QueryResponses.cs
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
using System.Collections;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

        // The various async responses from a Query request

	public class HitsAddedResponse : ResponseMessage {

		[XmlArray (ElementName="Hits")]
		[XmlArrayItem (ElementName="Hit", Type=typeof (Hit))]
		public ArrayList Hits;

		[XmlElement ("NumMatches")]
		public int NumMatches;

		public HitsAddedResponse () { }

		public HitsAddedResponse (ICollection hits, int total)
		{
			this.Hits = new ArrayList (hits);
			this.NumMatches = total;
		}
	}

	public class HitsSubtractedResponse : ResponseMessage {

		private ICollection uris;

		public HitsSubtractedResponse () { }

		public HitsSubtractedResponse (ICollection uris)
		{
			this.uris = uris;
		}

		[XmlIgnore]
		public ICollection Uris {
			get { return this.uris; }
		}

		[XmlArray (ElementName="Uris")]
		[XmlArrayItem (ElementName="Uri")]
		public string[] UrisAsStrings {
			get {
				string[] uris = new string [this.uris.Count];
				
				int i = 0;
				foreach (Uri uri in this.uris) {
					uris [i] = UriFu.UriToEscapedString (uri);
					i++;
				}

				return uris;
			}

			set {
				int N = value.Length;
				Uri[] uris = new Uri [N];

				for (int i = 0; i < N; i++)
					uris [i] = UriFu.EscapedStringToUri (value [i]);

				this.uris = uris;
			}
		}
	}

	public class FinishedResponse : ResponseMessage {
	}

	public class SearchTermResponse : ResponseMessage {
		
		[XmlArray (ElementName="Exact")]
		[XmlArrayItem (ElementName="Text", Type=typeof (string))]
		public ArrayList ExactText;

		[XmlArray (ElementName="Stemmed")]
		[XmlArrayItem (ElementName="Text", Type=typeof (string))]
		public ArrayList StemmedText;

		public SearchTermResponse ()
		{
			ExactText = new ArrayList ();
			StemmedText = new ArrayList ();
		}
	}

	public class CountMatchQueryResponse : ResponseMessage {
		public int NumMatches = 0;
	}
}
