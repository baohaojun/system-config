//
// LuceneBitArray.cs
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

using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;
using Lucene.Net.Documents;
using Lucene.Net.Index;
using Lucene.Net.QueryParsers;
using LNS = Lucene.Net.Search;

using Beagle.Util;

namespace Beagle.Daemon {

	public class LuceneBitArray : BetterBitArray {

		private class BitArrayHitCollector : LNS.HitCollector {

			public BetterBitArray Array;

			public override void Collect (int id, float score)
			{
				this.Array.Set (id, true);
			}
		}

		private static bool Debug = Beagle.Util.Debug.Enabled ("LuceneBitArray");

		private LNS.IndexSearcher searcher;
		private BitArrayHitCollector collector;
		private BetterBitArray scratch;
		private ArrayList pending_uris = null;

		// To keep pending_uris a reasonable size
		const int MAX_URI_COUNT = 1000;

		public LuceneBitArray (LNS.IndexSearcher searcher) : base (searcher.MaxDoc ())
		{
			this.searcher = searcher;
			this.collector = new BitArrayHitCollector ();
			this.scratch = null;
		}

		public LuceneBitArray (LNS.IndexSearcher searcher,
				       LNS.Query query) : this (searcher)
		{
			this.Or (query);
		}

		private void UseScratch ()
		{
			if (scratch == null)
				scratch = new BetterBitArray (searcher.MaxDoc ());
			else
				scratch.SetAll (false);
			collector.Array = scratch;
		}

		public LuceneBitArray Search (LNS.Query query)
		{
			this.SetAll (false);
			this.Or (query);
			return this;
		}

		public LuceneBitArray And (LNS.Query query)
		{
			UseScratch ();
			searcher.Search (query, null, collector);
			if (Debug)
				Explain (query);
			this.And (scratch);
			return this;
		}

		public LuceneBitArray AndNot (LNS.Query query)
		{
			UseScratch ();
			searcher.Search (query, null, collector);
			if (Debug)
				Explain (query);
			this.AndNot (scratch);
			return this;
		}

		public LuceneBitArray Or (LNS.Query query)
		{
			collector.Array = this;
			searcher.Search (query, null, collector);
			if (Debug)
				Explain (query);
			return this;
		}
		
		public LuceneBitArray Xor (LNS.Query query)
		{
			UseScratch ();
			searcher.Search (query, null, collector);
			if (Debug)
				Explain (query);
			this.Xor (scratch);
			return this;
		}

		private void Explain (LNS.Query query)
		{
			int j = 0;
			while (j < collector.Array.Count) {
				int i;
				i = collector.Array.GetNextTrueIndex (j);
				if (i >= collector.Array.Count)
					break;
				j = i + 1;

				Document doc = searcher.Doc (i);
				LNS.Explanation exp = searcher.Explain (query, i);

				Log.Debug ("Query: [{0}]", query);
				Log.Debug ("Matching URI: {0}", doc.Get ("Uri"));
				Log.Debug ("Explanation: {0}", exp);
			}
		}

		////////////////////////////////////////////////////////////

		public void AddUri (Uri uri)
		{
			AddUri (UriFu.UriToEscapedString (uri));
		}

		public void AddUri (string str)
		{
			if (pending_uris == null)
				pending_uris = new ArrayList ();

			int pos;

			// OrdinalComparer gives us the same order that the
			// URIs are stored as terms in the index, so that our
			// walk along them is linear.
			pos = pending_uris.BinarySearch (str, StringFu.OrdinalComparer.Instance);

			// Value is already present
			if (pos >= 0)
				return;

			pending_uris.Insert (~pos, str);

			if (pending_uris.Count == MAX_URI_COUNT)
				FlushUris ();
		}

		public void FlushUris ()
		{
			if (pending_uris == null)
				return;

			TermDocs term_docs = this.searcher.Reader.TermDocs ();

			for (int i = 0; i < pending_uris.Count; i++) {
				Term term = new Term ("Uri", (string) pending_uris [i]);
				term_docs.Seek (term);

				if (term_docs.Next ())
					this.Set (term_docs.Doc (), true);
			}

			term_docs.Close ();

			pending_uris = null;
		}

		////////////////////////////////////////////////////////////

		static FieldSelector fields_uri = new MapFieldSelector (new string[] {"Uri"});

		public void ProjectOnto (LuceneBitArray other)
		{
			int j = 0;
			while (j < this.Count) {
				int i;
				i = this.GetNextTrueIndex (j);
				if (i >= this.Count)
					break;
				j = i+1;

				Document doc;
				doc = searcher.Doc (i, fields_uri);

				other.AddUri (doc.Get ("Uri"));
			}

			other.FlushUris ();
		}

		public void Join (LuceneBitArray other)
		{
			LuceneBitArray image;
			image = new LuceneBitArray (other.searcher);
			this.ProjectOnto (image);

			other.Or (image);

			// We only need to project back items in the other
			// bit array that are not in the image of the
			// first projection.
			image.Not ();
			image.And (other);
			image.ProjectOnto (this);
		}
	}
}
