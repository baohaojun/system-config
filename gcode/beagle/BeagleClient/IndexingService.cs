//
// IndexingService.cs
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
	
	public class IndexingServiceRequest : RequestMessage {

		private string source = null;
		private ArrayList to_remove = new ArrayList ();
		private ArrayList to_add = new ArrayList ();

		public IndexingServiceRequest ()
		{
		}

		public string Source {
			get { return source; }
			set { source = value; }
		}

		[XmlArray (ElementName="ToAdd")]
		[XmlArrayItem (ElementName="Indexable", Type=typeof (Indexable))]
		public ArrayList ToAdd {
			get { return to_add; }
		}

		[XmlArray (ElementName="ToRemove")]
		[XmlArrayItem (ElementName="Uri")]
		public string [] ToRemoveAsStrings {
			get {
				string[] uris = new string [to_remove.Count];
				
				int i = 0;
				foreach (Uri uri in to_remove) {
					uris [i] = UriFu.UriToEscapedString (uri);
					i++;
				}

				return uris;
			}

			set {
				int N = value.Length;

				to_remove.Clear ();
				for (int i = 0; i < N; i++) 
					to_remove.Add (UriFu.EscapedStringToUri (value [i]));
			}
		}

		[XmlIgnore]
		public ICollection ToRemove {
			get { return to_remove; }
		}

		public void Add (Indexable indexable)
		{
			indexable.StoreStream ();
			to_add.Add (indexable);
		}

		public void Remove (Uri uri)
		{
			to_remove.Add (uri);
		}

	}
}
