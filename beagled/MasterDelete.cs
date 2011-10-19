//
// MasterDelete.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//


using System;
using System.Collections;
using System.IO;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;

class MasterDeleteTool {

	// Remapping hack from DumpIndex
	static Uri RemapUri (LuceneQueryingDriver driver, Uri uri)
	{
		// We only need to remap URIs in the file system backend
		if (driver.IndexName != "FileSystemIndex")
			return uri;

		FileAttributesStore fa_store = new FileAttributesStore (new FileAttributesStore_Mixed (Path.Combine (PathFinder.IndexDir, "FileSystemIndex"), driver.Fingerprint));

		string path = uri.LocalPath;

		Beagle.Daemon.FileAttributes attr = fa_store.Read (path);
		if (attr == null) {
			Console.WriteLine ("No file attribute info for {0}", uri);
			return uri;
		}

		return new Uri ("uid:" + GuidFu.ToShortString (attr.UniqueId) + uri.Fragment);
	}

	static void Main (string[] args)
	{
		if (args.Length != 2) {
			Console.WriteLine ("Usage: beagle-master-delete-button index-name uri-to-delete");
			return;
		}

		string index_name = args [0];

		LuceneQueryingDriver driver = new LuceneQueryingDriver (index_name, -1, true);

		Uri uri = new Uri (args [1], false);
		Uri uri_to_delete = RemapUri (driver, uri);

		LuceneIndexingDriver indexer = new LuceneIndexingDriver (index_name, false);

		Indexable indexable = new Indexable (uri_to_delete);
		indexable.Type = IndexableType.Remove;

		IndexerRequest request = new IndexerRequest ();
		request.Add (indexable);

		IndexerReceipt [] receipts = indexer.Flush (request);
		if (receipts == null || receipts.Length == 0) {
			Console.WriteLine ("Uri {0} not found in {1}",
					   uri, index_name);
			return;
		}

		IndexerRemovedReceipt r = receipts [0] as IndexerRemovedReceipt;
		if (r == null || r.NumRemoved == 0) {
			Console.WriteLine ("Uri {0} not found in {1}",
					   uri, index_name);
			return;
		}

		Console.WriteLine ("Uri {0} deleted", uri);
	}
}
