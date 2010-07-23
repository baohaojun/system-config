//
// LuceneIndexingDriver.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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

using Lucene.Net.Documents;
using Lucene.Net.Index;

using Beagle.Util;
using Stopwatch = Beagle.Util.Stopwatch;

namespace Beagle.Daemon {

	public class LuceneIndexingDriver : LuceneCommon, IIndexer {

		object flush_lock = new object ();

		public LuceneIndexingDriver (string index_name, int minor_version, bool build_usercache) 
			: base (index_name, minor_version)
		{
			if (Exists ())
				Open ();
			else
				Create ();

			if (build_usercache)
				text_cache = TextCache.UserCache;

			Shutdown.ShutdownEvent += OnShutdown;
		}

		public LuceneIndexingDriver (string index_name, int minor_version)
			: this (index_name, minor_version, true) { }
		
		public LuceneIndexingDriver (string index_name, bool build_usercache)
			: this (index_name, -1, build_usercache) { }

		public LuceneIndexingDriver (string index_name) 
			: this (index_name, -1, true) { }

		////////////////////////////////////////////////////////////////
		// DisableTextCache will disable text cache (surprise!) for the documents
		// indexed in this session of beagle-index-helper.
		// To consistently handle correct snippets where,
		// some sessions were called with disable-textcache and some without
		// when DisableTextCache is set, not only is no textcache is stored,
		// but any previous textcache entry is removed.
		// This is important, because otherwise snippets might be returned
		// for old versions of documents.
		// Note that, disable_textcache merely disables text-cache for documents indexed in the current session,
		// so you would still get snippets on documents that do not use
		// text cache for storing snippets, e.g. IM logs, KMail emails
		// and documents that were indexed in previous sessions.
		// To get rid of text cache completely, delete TextCache directory
		// and always run beagled with --disable-textcache.
		// Of course, if you ask to disable text-cache and then request snippets,
		// you will look dumb. But that's your choice. We love choice.
		private bool disable_textcache = false;
		public bool DisableTextCache {
			get { return disable_textcache; }
			set { disable_textcache = value; }
		}
	
		////////////////////////////////////////////////////////////////

		// We use this in the index helper so that we can report what's
		// going on if the helper spins the CPU.  The method will be
		// called with null parameters after filtering has finished.

		public delegate void FileFilterDelegate (Uri display_uri, Uri content_uri, Filter filter);
		public FileFilterDelegate FileFilterNotifier = null;

		////////////////////////////////////////////////////////////////

		private class DeferredInfo {
			public Indexable Indexable;
			public Filter Filter; // Need to store the filter to ask for GenerateNextIndexable
			// FIXME: Later, the filter could be replaced by a closure which returns the list of generated indexables
			public Hashtable PersistentPropDocs; // Contains the persistent properties for itself and children

			public DeferredInfo (Indexable indexable,
						      Filter filter,
						      Hashtable persistent_property_docs)
			{
				this.Indexable = indexable;
				this.Filter = filter;
				this.PersistentPropDocs = persistent_property_docs;
			}

			public void Cleanup ()
			{
				Filter.Cleanup ();
				Filter.CleanGeneratedIndexables ();
				Indexable.Cleanup ();
				Filter = null;
				Indexable = null;
				if (PersistentPropDocs != null)
					PersistentPropDocs.Clear ();
			}
		}

		private ArrayList deferred_indexables = new ArrayList ();

		////////////////////////////////////////////////////////////////

		//
		// Implementation of the IIndexer interface
		//

		public IndexerReceipt [] Flush (IndexerRequest request)
		{
			// This is just to keep a big block of code from being
			// indented an extra eight spaces.
			lock (flush_lock) {
				if (request.ContinueIndexing)
					return FlushGeneratedIndexables_Unlocked (request);
				else
					return Flush_Unlocked (request);
			}
		}

		private IndexerReceipt [] Flush_Unlocked (IndexerRequest request)
		{
			ArrayList receipt_queue;
			receipt_queue = new ArrayList ();

			IndexReader primary_reader, secondary_reader;
			primary_reader = IndexReader.Open (PrimaryStore);
			secondary_reader = IndexReader.Open (SecondaryStore);

			// Step #1: Make our first pass over the list of
			// indexables that make up our request.  For each add
			// or property change in the request, get the Lucene
			// documents so we can move forward any persistent
			// properties (for adds) or all old properties (for
			// property changes).
			//
			// Then, for each add or remove in the request,
			// delete the associated documents from the index.
			// Note that we previously cached added documents so
			// that we can move persistent properties forward.

			// parent_child_old_props is double-nested hashtable (depth-2 tree)
			// indexed by the parent uri, it stores another hashtable indexed by the (parent+child documents)
			// FIXME: 2-level hashtable is a waste for any non-child document.
			// Replace this by a better data structure.
			Hashtable parent_child_old_props = UriFu.NewHashtable ();
			TermDocs term_docs = secondary_reader.TermDocs ();
			int delete_count = 0;

			IEnumerable request_indexables = request.Indexables;

			foreach (Indexable indexable in request_indexables) {

				string uri_str = UriFu.UriToEscapedString (indexable.Uri);
				Term term;

				// Store the necessary properties from old documents for re-addition
				if (indexable.Type == IndexableType.Add ||
				    indexable.Type == IndexableType.PropertyChange) {

					term = new Term ("Uri", uri_str);
					term_docs.Seek (term);

					Hashtable this_parent_child_props = null;

					if (term_docs.Next ()) {
						this_parent_child_props = UriFu.NewHashtable ();
						this_parent_child_props [indexable.Uri] = secondary_reader.Document (term_docs.Doc ());
						parent_child_old_props [indexable.Uri] = this_parent_child_props;
					}

					term = new Term ("ParentUri", uri_str);
					term_docs.Seek (term);

					while (term_docs.Next ()) {
						Document doc = secondary_reader.Document (term_docs.Doc ());

						string child_uri_str = doc.Get ("Uri");
						Uri child_uri = UriFu.EscapedStringToUri (child_uri_str);
						// Any valid lucene document *should* have a Uri, so no need to check for null
						// Store the child documents too, to save persistent-properties
						// of child documents
						this_parent_child_props [child_uri] = doc;
					}
				}

				// Now remove (non-remove indexables will be re-added in next block)
				Logger.Log.Debug ("-{0}", indexable.DisplayUri);
				
				int num_delete = 0;

				term = new Term ("Uri", uri_str);
				// For property changes, only secondary index is modified
				secondary_reader.DeleteDocuments (term);

				// Now remove from everywhere else (if asked to remove or if asked to add, in which case
				// we first remove and then add)
				// So we also need to remove child documents
				if (indexable.Type != IndexableType.PropertyChange) {
					num_delete = primary_reader.DeleteDocuments (term);

					// When we delete an indexable, also delete any children.
					// FIXME: Shouldn't we also delete any children of children, etc.?
					term = new Term ("ParentUri", uri_str);
					num_delete += primary_reader.DeleteDocuments (term);
					secondary_reader.DeleteDocuments (term);
				}

				// If this is a strict removal (and not a deletion that
				// we are doing in anticipation of adding something back),
				// queue up a removed receipt.
				if (indexable.Type == IndexableType.Remove) {
					IndexerRemovedReceipt r;
					r = new IndexerRemovedReceipt (indexable.Id);
					r.NumRemoved = num_delete;
					receipt_queue.Add (r);
				}

				delete_count += num_delete;
			}

			term_docs.Close ();

			if (HaveItemCount)
				AdjustItemCount (-delete_count);
			else
				SetItemCount (primary_reader);
			
			// We are now done with the readers, so we close them.
			// And also free them. Somehow not freeing them is preventing them from
			// GCed at all.
			primary_reader.Close ();
			primary_reader = null;
			secondary_reader.Close ();
			secondary_reader = null;

			// FIXME: If we crash at exactly this point, we are in
			// trouble.  Items will have been dropped from the index
			// without the proper replacements being added.  We can
			// hopefully fix this when we move to Lucene 2.1.

			// Step #2: Make another pass across our list of indexables
			// and write out any new documents.

			if (text_cache != null)
				text_cache.BeginTransaction ();
				
			IndexWriter primary_writer, secondary_writer;
			// FIXME: Lock obtain time-out can happen here; if that happens,
			// an exception will be thrown and this method will break in the middle
			// leaving IndexWriters unclosed! Same for any Lucene.Net-index modification
			// methods.
			primary_writer = new IndexWriter (PrimaryStore, IndexingAnalyzer, false);
			secondary_writer = null;

			foreach (Indexable indexable in request_indexables) {
				// If shutdown has been started, break here
				// FIXME: Some more processing will continue, a lot of them
				// concerning receipts, but the daemon will anyway ignore receipts
				// now, what is the fastest way to stop from here ?
				if (Shutdown.ShutdownRequested) {
					Log.Debug ("Shutdown initiated. Breaking while flushing indexables.");
					break;
				}

				// Receipts for removes were generated in the
				// previous block.  Now we just have to remove
				// items from the text cache.
				if (indexable.Type == IndexableType.Remove) {
					if (text_cache != null)
						text_cache.Delete (indexable.Uri);

					continue;
				}

				IndexerAddedReceipt r;
				Hashtable prop_change_docs = (Hashtable) parent_child_old_props [indexable.Uri];

				if (indexable.Type == IndexableType.PropertyChange) {

					Logger.Log.Debug ("+{0} (props only)", indexable.DisplayUri);

					r = new IndexerAddedReceipt (indexable.Id);
					r.PropertyChangesOnly = true;
					receipt_queue.Add (r);

					Document doc;
					if (prop_change_docs == null)
						doc = null;
					else
						doc = (Document) prop_change_docs [indexable.Uri];

					Document new_doc;
					new_doc = RewriteDocument (doc, indexable);

					// Write out the new document...
					if (secondary_writer == null)
						secondary_writer = new IndexWriter (SecondaryStore, IndexingAnalyzer, false);
					secondary_writer.AddDocument (new_doc);

					// Get child property change indexables...
					ArrayList prop_change_indexables;
					prop_change_indexables = GetChildPropertyChange (prop_change_docs, indexable);
					// and store them; no need to delete them first, since they were already removed from the index
					if (prop_change_indexables == null)
						continue;

					foreach (Indexable prop_change_indexable in prop_change_indexables) {
						Log.Debug ("+{0} (props only, generated indexable)", prop_change_indexable.Uri);
						doc = (Document) prop_change_docs [prop_change_indexable.Uri];
						new_doc = RewriteDocument (doc, prop_change_indexable);
						secondary_writer.AddDocument (new_doc);
					}

					continue; // ...and proceed to the next Indexable
				}

				// If we reach this point we know we are dealing with an IndexableType.Add

				if (indexable.Type != IndexableType.Add)
					throw new Exception ("When I said it was an IndexableType.Add, I meant it!");

				r = AddIndexableToIndex (indexable, primary_writer, ref secondary_writer, prop_change_docs);
				if (r != null)
					receipt_queue.Add (r);
			}

			if (text_cache != null)
				text_cache.CommitTransaction ();

			if (Shutdown.ShutdownRequested) {
				foreach (DeferredInfo di in deferred_indexables)
					di.Cleanup ();
				deferred_indexables.Clear ();

				foreach (Indexable indexable in request_indexables)
					indexable.Cleanup ();

				primary_writer.Close ();
				if (secondary_writer != null)
					secondary_writer.Close ();

				return null;
			}

			if (request.OptimizeIndex) {
				Stopwatch watch = new Stopwatch ();
				Logger.Log.Debug ("Optimizing {0}", IndexName);
				watch.Start ();
				primary_writer.Optimize ();
				if (secondary_writer == null)
					secondary_writer = new IndexWriter (SecondaryStore, IndexingAnalyzer, false);
				secondary_writer.Optimize ();
				watch.Stop ();
				Logger.Log.Debug ("{0} optimized in {1}", IndexName, watch);
			}

			// Step #4. Close our writers and return the events to
			// indicate what has happened.

			primary_writer.Close ();
			if (secondary_writer != null)
				secondary_writer.Close ();

			// Send a single IndexerIndexablesReceipt if there were deferred indexables
			if (deferred_indexables.Count > 0) {
				Log.Debug ("{0} indexables generated more indexables; asking daemon to schedule their indexing.", deferred_indexables.Count);
				IndexerIndexablesReceipt r = new IndexerIndexablesReceipt ();
				receipt_queue.Add (r);
			}

			IndexerReceipt [] receipt_array;
			receipt_array = new IndexerReceipt [receipt_queue.Count];
			for (int i = 0; i < receipt_queue.Count; ++i)
				receipt_array [i] = (IndexerReceipt) receipt_queue [i];

			return receipt_array;
		}

		private IndexerReceipt [] FlushGeneratedIndexables_Unlocked (IndexerRequest request)
		{
			int num_indexed = 0;
			ArrayList receipt_queue;
			receipt_queue = new ArrayList ();

			if (text_cache != null)
				text_cache.BeginTransaction ();
				
			IndexWriter primary_writer, secondary_writer;
			primary_writer = new IndexWriter (PrimaryStore, IndexingAnalyzer, false);
			secondary_writer = null;
			IndexerAddedReceipt r;

			Log.Debug ("Continuing indexing generated indexables from {0} indexables", deferred_indexables.Count);

			// Access using index so that we can add more deferred_indexable at the front
			// deferred_indexables are added at the front and fetched from the front like a stack
			while (deferred_indexables.Count > 0) {
				DeferredInfo di = (DeferredInfo) deferred_indexables [0];

				if (di.Indexable.LocalState ["HasNextIndexable"] != null) {
					// Finally, good to index
					// Should we do a sanity check ? deferred_indexables [0] =?= di
					deferred_indexables.RemoveAt (0);

					Document persistent_prop_doc = null;
					if (di.PersistentPropDocs != null)
						persistent_prop_doc = (Document) di.PersistentPropDocs [di.Indexable.Uri];
					
					if (di.Indexable.DisplayUri != di.Indexable.ContentUri)
						Log.Debug ("+{0} ({1}) [deferred]", di.Indexable.DisplayUri, di.Indexable.ContentUri);
					else
						Log.Debug ("+{0} [deferred]", di.Indexable.DisplayUri);

					AddDocumentToIndex (di.Indexable, persistent_prop_doc, primary_writer, ref secondary_writer);

					// Add the receipt if the indexable was submitted and not generated
					if (di.Indexable.LocalState ["GeneratedIndexable"] == null) {
						r = new IndexerAddedReceipt (di.Indexable.Id);
						r.FilterName = di.Filter.GetType ().ToString ();
						r.FilterVersion = di.Filter.Version;
						receipt_queue.Add (r);
					}

					// Cleanup, and text cache maintenance.
					di.Cleanup ();

					if (disable_textcache && text_cache != null)
						text_cache.Delete (di.Indexable.Uri);

					num_indexed ++;
					continue;
				}

				Log.Debug ("Processing deferred indexable from {0}", di.Indexable.DisplayUri);
				bool next = false;
				while (! next && ! Shutdown.ShutdownRequested && num_indexed <= RequestFlushThreshold) {
					Indexable generated_indexable = null;

					bool next_indexable = false;
					try {
						next_indexable = di.Filter.GenerateNextIndexable (out generated_indexable);
					} catch (Exception e) {
						Log.Error (e, "Error while generating next indexable from {0}", di.Indexable.DisplayUri);
					}

					if (! next_indexable) {
						// Mark it for indexing and leave it in the stack
						di.Indexable.LocalState ["HasNextIndexable"] = false;
						next = true;
						break;
					}

					if (generated_indexable == null)
						continue;

					Log.Debug ("Adding generated indexable {0}", generated_indexable.DisplayUri);

					// Mark this indexable
					generated_indexable.LocalState ["GeneratedIndexable"] = true;

					// IndexerGenerated indexables have a common parenturi, which has been used before
					// to remove all docs from the lucene index with that parenturi. So, now we can safely
					// go ahead and just add the new information.
					r = AddIndexableToIndex (generated_indexable, primary_writer, ref secondary_writer, di.PersistentPropDocs);
					// But do not add r to the receipt queue, since this was generated
					if (r != null) // null receipt is returned if generated_indexable is deferred
						num_indexed ++;
				}

				if (Shutdown.ShutdownRequested || num_indexed > RequestFlushThreshold)
					break;
			}

			if (text_cache != null)
				text_cache.CommitTransaction ();

			if (Shutdown.ShutdownRequested) {
				foreach (DeferredInfo di in deferred_indexables)
					di.Cleanup ();
				deferred_indexables.Clear ();

				primary_writer.Close ();
				if (secondary_writer != null)
					secondary_writer.Close ();
			
				return null;
			}

			primary_writer.Close ();
			if (secondary_writer != null)
				secondary_writer.Close ();
			
			// Send a single IndexerIndexablesReceipt if there were deferred indexables
			if (deferred_indexables.Count > 0) {
				Log.Debug ("{0} more indexable-generating indexable remainding to index; asking daemon to schedule their indexing.", deferred_indexables.Count);
				IndexerIndexablesReceipt paused_receipt = new IndexerIndexablesReceipt ();
				receipt_queue.Add (paused_receipt);
			}

			IndexerReceipt [] receipt_array;
			receipt_array = new IndexerReceipt [receipt_queue.Count];
			for (int i = 0; i < receipt_queue.Count; ++i)
				receipt_array [i] = (IndexerReceipt) receipt_queue [i];
			
			return receipt_array;
		}

		private IndexerAddedReceipt AddIndexableToIndex (Indexable indexable,
								 IndexWriter primary_writer,
					          		 ref IndexWriter secondary_writer,
						  		 Hashtable prop_change_docs)
		{
			Filter filter = null;
			if (FileFilterNotifier != null)
				FileFilterNotifier (indexable.DisplayUri, indexable.ContentUri, null); // We don't know what filter yet.

			// If we have content, try to find a filter
			// we we can use to process the indexable
			bool filter_content = false;
			try {
				filter_content = FilterFactory.FilterIndexable (indexable, (disable_textcache ? null : text_cache), out filter);
			} catch { }

			if (! filter_content) {
				indexable.NoContent = true;
				filter = null;
			}

			if (FileFilterNotifier != null)
				FileFilterNotifier (indexable.DisplayUri, indexable.ContentUri, filter); // Update with our filter

			IndexerAddedReceipt r = new IndexerAddedReceipt (indexable.Id);

			if (filter != null) {
				if (filter.HasGeneratedIndexable) {
					Log.Debug ("{0} might generate indexables from {1}; deferring until later",
						   indexable.DisplayUri,
						   filter.GetType ().ToString ());

					// This indexable can potentially generate indexables,
					// so defer its indexing

					DeferredInfo di;
					di = new DeferredInfo (indexable, filter, prop_change_docs);
					deferred_indexables.Insert (0, di);

					// Since we are deferred, continue. Do not cleanup indexable or remove text-cache yet.
					// FIXME: Make sure all indexable.Cleanup is called for all indexables if
					// shutdown is signalled.
					if (FileFilterNotifier != null)
						FileFilterNotifier (null, null, null); // reset

					// Return null to signal the indexable was deferred
					return null;
				}

				// Force the clean-up of temporary files, just in case.
				// FIXME: I am not sure if the cleanup should happen now.
				// What is the difference between filter.Cleanup and Indexable.Cleanup ?
				filter.Cleanup ();

				r.FilterName = filter.GetType ().ToString ();
				r.FilterVersion = filter.Version;
			}

			// If this indexables is not deferred, add it to the index.
			if (indexable.DisplayUri != indexable.ContentUri)
				Log.Debug ("+{0} ({1})", indexable.DisplayUri, indexable.ContentUri);
			else
				Log.Debug ("+{0}", indexable.DisplayUri);

			Document persistent_prop_doc = null;
			if (prop_change_docs != null)
				persistent_prop_doc = (Document) prop_change_docs [indexable.Uri];
			AddDocumentToIndex (indexable, persistent_prop_doc, primary_writer, ref secondary_writer);

			if (FileFilterNotifier != null)
				FileFilterNotifier (null, null, null); // reset

			// Clean up any temporary files associated with filtering this indexable.
			indexable.Cleanup ();

			// Remove any existing text cache for this item
			if (disable_textcache && text_cache != null)
				text_cache.Delete (indexable.Uri);

			return r;
		}

		private void AddDocumentToIndex (Indexable indexable,
						 Document persistent_prop_doc,
						 IndexWriter primary_writer,
						 ref IndexWriter secondary_writer)
		{
			Document primary_doc = null, secondary_doc = null;

			try {
				BuildDocuments (indexable, out primary_doc, out secondary_doc);
				primary_writer.AddDocument (primary_doc);
			} catch (Exception ex) {
					
				// If an exception was thrown, something bad probably happened
				// while we were filtering the content.  Set NoContent to true
				// and try again -- that way it will at least end up in the index,
				// even if we don't manage to extract the fulltext.

				Logger.Log.Debug (ex, "First attempt to index {0} failed", indexable.DisplayUri);
					
				indexable.NoContent = true;
						
				try {
					BuildDocuments (indexable, out primary_doc, out secondary_doc);
					primary_writer.AddDocument (primary_doc);
				} catch (Exception ex2) {
					Logger.Log.Debug (ex2, "Second attempt to index {0} failed, giving up...", indexable.DisplayUri);
				}
			}

			secondary_doc = MergeDocuments (secondary_doc, persistent_prop_doc);

			if (secondary_doc != null) {
				if (secondary_writer == null)
					secondary_writer = new IndexWriter (SecondaryStore, IndexingAnalyzer, false);

				secondary_writer.AddDocument (secondary_doc);
			}

#if ENABLE_RDF_ADAPTER
			// Store the extracted links in the textcache
			if (! disable_textcache && text_cache != null)
				text_cache.AddLinks (indexable.Uri, indexable.Links);
#endif

			AdjustItemCount (1);
		}

		// Since some parent properties maybe stored in child properties
		// as parent: property, any property change should be propagated
		// to all its children as well.
		private ArrayList GetChildPropertyChange (Hashtable children_docs,
							  Indexable parent)
		{
			// FIXME FIXME FIXME: Post-Child-Indexable-Fix
			if (children_docs == null)
				return null;

			Uri parent_uri = parent.Uri;
			ArrayList child_indexable_list = new ArrayList ();

			foreach (Uri uri in children_docs.Keys) {
				// FIXME: Currently, children_docs has both the parent and children docs
				if (UriFu.Equals (uri, parent_uri))
					continue;

				Indexable child_indexable;
				child_indexable = new Indexable (IndexableType.PropertyChange, uri);
				Log.Debug ("Creating property change child indexable for {1} (parent {0})", parent.Uri, uri);

				// This is where the child_indexables will have new properties from parent
				child_indexable.SetChildOf (parent);
				child_indexable_list.Add (child_indexable);
			}

			return child_indexable_list;
		}
		
		////////////////////////////////////////////////////////////////

		public void OptimizeNow ()
		{
			IndexWriter writer;

			writer = new IndexWriter (PrimaryStore, null, false);
			writer.Optimize ();
			writer.Close ();

			if (SecondaryStore != null) {
				writer = new IndexWriter (SecondaryStore, null, false);
				writer.Optimize ();
				writer.Close ();
			}
		}

		
		public void Merge (LuceneCommon index_to_merge)
		{
                        // FIXME: Error recovery

			// Merge the primary index
			IndexWriter primary_writer;
			Lucene.Net.Store.Directory[] primary_store = {index_to_merge.PrimaryStore};
			primary_writer = new IndexWriter (PrimaryStore, null, false);

			primary_writer.AddIndexes (primary_store);
			primary_writer.Close ();

			// Merge the secondary index
			IndexWriter secondary_writer;
			Lucene.Net.Store.Directory[] secondary_store = {index_to_merge.SecondaryStore};
			secondary_writer = new IndexWriter (SecondaryStore, null, false);

			secondary_writer.AddIndexes (secondary_store);
			secondary_writer.Close ();
		}

		//////////////////////////////////////////////////////

		public void OnShutdown ()
		{
			lock (flush_lock) {
				foreach (DeferredInfo di in deferred_indexables)
					di.Cleanup ();
				deferred_indexables.Clear ();
			}
		}
	}
}
