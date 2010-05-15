//
// IndexerRequest.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
// Copyright (C) 2005-2006 Novell, Inc.
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
using System.Collections.Generic;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle.Daemon {

	public class IndexerRequest {

		public bool OptimizeIndex = false;

		// Set to true to signal the indexer to continue indexing if it was paused before
		public bool ContinueIndexing = false;

		// Keep a list of submitted indexables, listed by the globally unique id
		private Dictionary<int, Indexable> indexables = null;

		// The hashtable is needed to remove/merge multiple indexables for the same uri
		private Hashtable indexables_by_uri = null;

		// Used to uniquely determine any indexable
		private static int indexable_id = 0;
		private static object indexable_id_lock = new object ();

		public IndexerRequest ()
		{
			// Just a good initial size
			indexables = new Dictionary<int, Indexable> (LuceneCommon.RequestFlushThreshold);
		}

		public void Add (Indexable indexable)
		{
			if (indexable == null)
				return;

			if (indexables_by_uri == null)
				indexables_by_uri = UriFu.NewHashtable ();

			Indexable prior;
			prior = indexables_by_uri [indexable.Uri] as Indexable;

			if (prior != null) {
				
				switch (indexable.Type) {

				case IndexableType.Add:
				case IndexableType.Remove:
					// Clobber the prior indexable.
					indexable.Id = prior.Id;
					indexables [prior.Id] = indexable;
					indexables_by_uri [indexable.Uri] = indexable;
					break;

				case IndexableType.PropertyChange:
					if (prior.Type != IndexableType.Remove) {
						// Merge with the prior indexable.
						prior.Merge (indexable);
					}
					break;
				}
			} else {
				lock (indexable_id_lock) {
					indexable.Id = indexable_id;
					indexable_id ++;
				}
				indexables [indexable.Id] = indexable;
				indexables_by_uri [indexable.Uri] = indexable;
			}
		}

		// Returns the indexable generating this receipt and also remove it from the cache
		public Indexable RetrieveRequestIndexable (IndexerReceipt r)
		{
			Indexable requested_indexable = null;
			if (indexables.TryGetValue (r.Id, out requested_indexable))
				indexables.Remove (r.Id);
			else if (deferred_indexables.TryGetValue (r.Id, out requested_indexable))
				deferred_indexables.Remove (r.Id);
			else
				Log.Warn ("Unable to match receipt #{0} to any previous request!", r.Id);

			return requested_indexable;

		}

		// The list of deferred indexables should be per queryable. Ideally, this does not belong
		// here, but to LuceneQueryable. However, current implementation of LuceneQueryable
		// uses only one active request at a time, so this is still ok for now.
		private Dictionary<int, Indexable> deferred_indexables = null;

		[XmlIgnore]
		public Dictionary<int, Indexable> DeferredIndexables {
			get {
				if (deferred_indexables == null)
					deferred_indexables = new Dictionary<int, Indexable> (indexables.Count);

				// Return the previous set of deferred_indexables + indexables not retrieved after Flush() - they must have been deferred
				foreach (KeyValuePair<int, Indexable> kvp in indexables)
					// should not throw argumentexception
					deferred_indexables.Add (kvp.Key, kvp.Value);

				indexables = null;
				return deferred_indexables;
			}
			// Store the deferred indexables so that when receipts are received later, they can be processed
			set {
				if (deferred_indexables == null)
					deferred_indexables = value;
				else {
					foreach (KeyValuePair<int, Indexable> kvp in value)
						deferred_indexables.Add (kvp.Key, kvp.Value);
				}
			}
		}

		public bool DeferIndexable (int id)
		{
			try {
				Indexable requested_indexable = indexables [id];
				indexables.Remove (id);

				if (deferred_indexables == null)
					deferred_indexables = new Dictionary<int, Indexable> ();
				deferred_indexables.Add (id, requested_indexable);
			// None of the exceptions should happen!
			} catch (KeyNotFoundException) {
				Log.Warn ("Indexable #{0} was not requested!", id);
				return false;
			} catch (ArgumentException) {
				Log.Warn ("Indexable #{0} is already deferred!", id);
				return false;
			}

			return true;
		}

		/* Fake IEnumerable class to serialize a Dictionary. */
		public class IndexablesList : IEnumerable {
			public Dictionary<int, Indexable> indexables;
			
			public IndexablesList (Dictionary<int, Indexable> dict)
			{
			        indexables = dict;
			}
			
			public IEnumerator GetEnumerator ()
			{
			        return indexables.Values.GetEnumerator ();
			}
			
			public void Add (object o)
			{
				Indexable i = (Indexable) o;
				indexables [i.Id] = i;
			}
		}
	
		[XmlArray (ElementName="Indexables")]
		[XmlArrayItem (ElementName="Indexable", Type=typeof (Indexable))]
		public IndexablesList Indexables {
			get {
				indexables_by_uri = null;
				return new IndexablesList (indexables);
			}
			set { indexables = value.indexables; }
		}

		[XmlIgnore]
		public int Count {
			get { return indexables.Count; }
		}
		
		[XmlIgnore]
		public bool IsEmpty {
			get { return indexables.Count == 0 && ! OptimizeIndex && ! ContinueIndexing; }
		}

		public void Cleanup ()
		{
			foreach (Indexable i in indexables.Values)
				i.Cleanup ();

			// Clear the deferred indexables
			if (deferred_indexables != null)
				foreach (Indexable indexable in deferred_indexables.Values)
					indexable.Cleanup ();
		}
	}
}
