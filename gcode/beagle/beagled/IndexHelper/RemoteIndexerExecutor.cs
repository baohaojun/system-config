//
// RemoteIndexerExecutor.cs
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

using Beagle;
using Beagle.Util;
using Beagle.Daemon;

// Register the executor class
[assembly: RequestMessageExecutorTypes (typeof (Beagle.IndexHelper.RemoteIndexerExecutor))]

namespace Beagle.IndexHelper {

	[RequestMessage (typeof (RemoteIndexerRequest))]
	public class RemoteIndexerExecutor : RequestMessageExecutor {

		static public int Count = 0;
		static Hashtable indexer_table = new Hashtable ();

		public override ResponseMessage Execute (RequestMessage raw_request)
		{
			RemoteIndexerRequest remote_request = (RemoteIndexerRequest) raw_request;

			IndexHelperTool.ReportActivity ();

			// Find the appropriate driver for this request.
			LuceneIndexingDriver indexer;
			lock (indexer_table) {
				indexer = (LuceneIndexingDriver) indexer_table [remote_request.RemoteIndexName];

				if (indexer == null) {
					indexer = new LuceneIndexingDriver (remote_request.RemoteIndexName,
									    remote_request.RemoteIndexMinorVersion);
					indexer.DisableTextCache = IndexHelperTool.DisableTextCache;

					indexer_table [remote_request.RemoteIndexName] = indexer;

					indexer.FileFilterNotifier += delegate (Uri display_uri, Uri content_uri, Filter filter) {
						IndexHelperTool.ReportActivity ();
						IndexHelperTool.CurrentDisplayUri = display_uri;
						IndexHelperTool.CurrentContentUri = content_uri;
						IndexHelperTool.CurrentFilter = filter;
					};
				}
			}

			IndexerReceipt [] receipts = null;
			int item_count = 0;

			try {
				if (remote_request.Request != null) // If we just want the item count, this will be null
					receipts = indexer.Flush (remote_request.Request);
				item_count = indexer.GetItemCount ();
			} catch (Exception e) {
				// Send error response
				++Count;
				return new ErrorResponse (e);
			}

			// Construct a response containing the item count and
			// the receipts produced by the actual indexing.
			RemoteIndexerResponse response = new RemoteIndexerResponse ();
			response.ItemCount = item_count;
			response.Receipts = receipts;

			++Count;

			IndexHelperTool.ReportActivity ();

			return response;
		}
	}
}
