//
// ExternalMetadataQueryable.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.IO;

using Beagle.Daemon;
using Beagle.Util;
using Beagle;

namespace Beagle.Daemon {

	// An abstract class which doesn't have any storage backing it.
	// These backends exist solely to add property change indexables to
	// existing backends.
	public abstract class ExternalMetadataQueryable : IQueryable {

		private FileAttributesStore fa_store;

		public ExternalMetadataQueryable ()
		{
		}

		public Scheduler ThisScheduler {
			get { return Scheduler.Global; }
		}

		public virtual void Start ()
		{
		}

		public bool AcceptQuery (Query query)
		{
			// Always return false; there is nothing backing this
			// backend.
			return false;
		}

		public void DoQuery (Query query, IQueryResult result, IQueryableChangeData data)
		{
		}


		public int DoCountMatchQuery (Query query)
		{
			return 0;
		}

		public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			return null;
		}

		public QueryableStatus GetQueryableStatus ()
		{
			QueryableStatus status = new QueryableStatus ();

			// FIXME
			status.ItemCount = -1;
			status.ProgressPercent = -1;
			status.IsIndexing = false;

			return status;
		}

		protected FileAttributesStore FileAttributesStore {
			get { return fa_store; }
		}				
				
		protected void InitFileAttributesStore (string name, string external_fingerprint)
		{
			string storage_path = Path.Combine (PathFinder.IndexDir, name);
			string fingerprint_file = Path.Combine (storage_path, "fingerprint");
			string internal_fingerprint;

			if (! Directory.Exists (storage_path)) {
				Directory.CreateDirectory (storage_path);
				internal_fingerprint = GuidFu.ToShortString (Guid.NewGuid ());
				StreamWriter writer = new StreamWriter (fingerprint_file);
				writer.WriteLine (internal_fingerprint);
				writer.Close ();
			} else {
				StreamReader reader = new StreamReader (fingerprint_file);
				internal_fingerprint = reader.ReadLine ();
				reader.Close ();
			}

			string fingerprint;
			if (external_fingerprint != null)
				fingerprint = internal_fingerprint + "-" + external_fingerprint;
			else
				fingerprint = internal_fingerprint;

			IFileAttributesStore ifa_store;

			if (ExtendedAttribute.Supported)
				ifa_store = new FileAttributesStore_ExtendedAttribute (fingerprint);
			else
				ifa_store = new FileAttributesStore_Sqlite (storage_path, fingerprint);

			fa_store = new FileAttributesStore (ifa_store);
		}
	}
}
