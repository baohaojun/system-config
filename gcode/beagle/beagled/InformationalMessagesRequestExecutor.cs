//
// InformationalMessagesRequestExecutor.cs
//
// Copyright (C) 2006 Novell, Inc.
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

using Beagle.Util;

namespace Beagle.Daemon {

	[RequestMessage (typeof (Beagle.InformationalMessagesRequest))]
	public class InformationalMessagesRequestExecutor : RequestMessageExecutor {

		private InformationalMessagesRequest info_request;
		private IndexingStatus crawl_status = IndexingStatus.NotRunning;

		public override ResponseMessage Execute (RequestMessage req)
		{
			this.info_request = (InformationalMessagesRequest) req;

			if (QueryDriver.IsIndexing)
				SendIndexingStatusResponse (IndexingStatus.Running);

			QueryDriver.ChangedEvent += OnQueryDriverChanged;

			// Don't send a response; we'll be sending them async.
			return null;
		}

		public override void Cleanup ()
		{
			QueryDriver.ChangedEvent -= OnQueryDriverChanged;
		}

		private void OnQueryDriverChanged (Queryable queryable, IQueryableChangeData change_data)
		{
			bool is_indexing = QueryDriver.IsIndexing;

			if (is_indexing && crawl_status == IndexingStatus.NotRunning)
				SendIndexingStatusResponse (IndexingStatus.Running);
			else if (! is_indexing && crawl_status == IndexingStatus.Running)
				SendIndexingStatusResponse (IndexingStatus.NotRunning);
		}

		private void SendIndexingStatusResponse (IndexingStatus status)
		{
			Log.Debug ("Sending indexing status change from {0} to {1}", crawl_status, status);
			this.crawl_status = status;
			this.SendAsyncResponse (new IndexingStatusResponse (status));
		}
	}
}
