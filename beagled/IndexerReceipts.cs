//
// IndexerReceipts.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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

namespace Beagle.Daemon {

	[XmlInclude (typeof (IndexerAddedReceipt)),
	 XmlInclude (typeof (IndexerRemovedReceipt)),
	 XmlInclude (typeof (IndexerIndexablesReceipt))]
	public abstract class IndexerReceipt {
		
		public IndexerReceipt () { }

		public IndexerReceipt (int id)
		{
			this.Id = id;
		}

		// Some abstract id copied from the indexable which caused this receipt
		[XmlAttribute ("Id")]
		public int Id = -1;
	}

	public class IndexerAddedReceipt : IndexerReceipt {
		
		public IndexerAddedReceipt () { }

		public IndexerAddedReceipt (int id) : base (id) { }

		public IndexerAddedReceipt (int id, string filter_name, int filter_version)
			: base (id)
		{
			this.FilterName = filter_name;
			this.FilterVersion = filter_version;
		}
		
		public bool PropertyChangesOnly = false;
		
		public string FilterName = null;
		
		public int FilterVersion = -1;
		
		public object Clone ()
		{
			return this.MemberwiseClone ();
		}

	}

	public class IndexerRemovedReceipt : IndexerReceipt {
		
		public IndexerRemovedReceipt () { }

		public IndexerRemovedReceipt (int id) : base (id) { }

		public int NumRemoved = -1;
	}

	// This fake receipt is sent to the daemon to basically schedule indexing of filter generated indexables
	public class IndexerIndexablesReceipt : IndexerReceipt {

		public IndexerIndexablesReceipt () { }
		// FIXME: We could send id here to get "permission" from the backend whether to continue
		// indexing the generated indexables from some backend scheduled indexable; currently I see
		// no need.
		// Another use for the id(s) could to tell the backend what parent indexable(s) are being currently indexed.
		// This could be useful in displaying correct status information.
	}
}
