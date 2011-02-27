//
// LuceneQueryingDriver.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;

using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;
using Lucene.Net.Documents;
using Lucene.Net.Index;
using Lucene.Net.QueryParsers;
using LNS = Lucene.Net.Search;

using Beagle.Util;
using Stopwatch = Beagle.Util.Stopwatch;

namespace Beagle.Daemon {

	public class LuceneQueryingDriver : LuceneCommon {

		static private bool Debug = Beagle.Util.Debug.Enabled ("LuceneQueryingDriver");

		public delegate double RelevancyMultiplier (Hit hit);

		public LuceneQueryingDriver (string index_name, bool read_only)
			: this (index_name, -1, read_only) { }

		public LuceneQueryingDriver (string index_name, int minor_version, bool read_only) 
			: base (index_name, minor_version)
		{
			// FIXME: Maybe the LuceneQueryingDriver should never try to create the index?
			if (Exists ())
				Open (read_only);
			else if (!read_only)
				Create ();
			else {
				// We're in read-only mode, but we can't create an index.
				// Maybe a different exception would be better?  This one is caught
				// in QueryDriver.LoadStaticQueryable ()
				throw new InvalidOperationException (String.Format ("Index {0} does not exist.", index_name));
			}

			// Initialize the user text cache only if we're not in
			// read-only mode.  StaticQueryables instantiate their
			// own text caches that are stored in a separate
			// location.
			if (!read_only)
				text_cache = TextCache.UserCache;
		}

		////////////////////////////////////////////////////////////////

		public Uri[] PropertyQuery (Property prop)
		{
			// FIXME: Should we support scanning the secondary
			// index as well?

			IndexReader primary_reader;
			primary_reader = LuceneCommon.GetReader (PrimaryStore);

			Term term;
			term = new Term (PropertyToFieldName (prop.Type, prop.Key), prop.Value.ToLower ());

			TermDocs term_docs;
			term_docs = primary_reader.TermDocs ();
			term_docs.Seek (term);

			ArrayList uri_list = new ArrayList ();
			while (term_docs.Next ()) {
				Document doc = primary_reader.Document (term_docs.Doc ());
				uri_list.Add (GetUriFromDocument (doc));
			}

			term_docs.Close ();
			LuceneCommon.ReleaseReader (primary_reader);

			return (Uri[]) uri_list.ToArray (typeof (Uri));
		}

		////////////////////////////////////////////////////////////////

		public bool HasUri (Uri uri)
		{
			IndexReader primary_reader;
			primary_reader = LuceneCommon.GetReader (PrimaryStore);

			Term term;
			term = new Term ("Uri", UriFu.UriToEscapedString (uri));

			TermDocs term_docs;
			term_docs = primary_reader.TermDocs ();
			term_docs.Seek (term);

			bool has_uri = false;

			if (term_docs.Next ())
				has_uri = true;

			term_docs.Close ();
			LuceneCommon.ReleaseReader (primary_reader);

			return has_uri;
		}

		////////////////////////////////////////////////////////////////

		// Returns the lists of terms in the query
		private ArrayList AssembleQuery (Query			query,
						 QueryPartHook		query_part_hook,
					    	 HitFilter		hit_filter,
					    	 out ArrayList		primary_required_part_queries,
					    	 out ArrayList		secondary_required_part_queries,
					    	 out LNS.BooleanQuery	primary_prohibited_part_query,
					    	 out LNS.BooleanQuery	secondary_prohibited_part_query,
						 out AndHitFilter	all_hit_filters)
		{
			primary_required_part_queries = null;
			secondary_required_part_queries = null;
			primary_prohibited_part_query = null;
			secondary_prohibited_part_query = null;

			all_hit_filters = new AndHitFilter ();
			if (hit_filter != null)
				all_hit_filters.Add (hit_filter);

			ArrayList term_list = new ArrayList ();

			foreach (QueryPart part in query.Parts) {
				LNS.Query primary_part_query;
				LNS.Query secondary_part_query;
				HitFilter part_hit_filter;
				QueryPartToQuery (part,
						  false, // we want both primary and secondary queries
						  part.Logic == QueryPartLogic.Required ? term_list : null,
						  query_part_hook,
						  out primary_part_query,
						  out secondary_part_query,
						  out part_hit_filter);

				if (primary_part_query == null)
					continue;

				switch (part.Logic) {
					
				case QueryPartLogic.Required:
					if (primary_required_part_queries == null) {
						primary_required_part_queries = new ArrayList ();
						secondary_required_part_queries = new ArrayList ();
					}
					primary_required_part_queries.Add (primary_part_query);
					secondary_required_part_queries.Add (secondary_part_query);
					
					if (part_hit_filter != null)
						all_hit_filters.Add (part_hit_filter);
					
					break;

				case QueryPartLogic.Prohibited:
					if (primary_prohibited_part_query == null)
						primary_prohibited_part_query = new LNS.BooleanQuery ();
					primary_prohibited_part_query.Add (primary_part_query, LNS.BooleanClause.Occur.SHOULD);

					if (secondary_part_query != null) {
						if (secondary_prohibited_part_query == null)
							secondary_prohibited_part_query = new LNS.BooleanQuery ();
						secondary_prohibited_part_query.Add (secondary_part_query, LNS.BooleanClause.Occur.SHOULD);
					}

					if (part_hit_filter != null) {
						NotHitFilter nhf;
						nhf = new NotHitFilter (part_hit_filter);
						all_hit_filters.Add (new HitFilter (nhf.HitFilter));
					}

					break;
				}
			}

			return term_list;
		}

		// Returns true if there are docs to search and creates the readers and searchers
		// in that case. Otherwise, returns false.
		private bool BuildSearchers (out IndexReader primary_reader,
					    out LNS.IndexSearcher primary_searcher,
					    out IndexReader secondary_reader,
					    out LNS.IndexSearcher secondary_searcher)
		{
			primary_searcher = null;
			secondary_reader = null;
			secondary_searcher = null;

			primary_reader = LuceneCommon.GetReader (PrimaryStore);
			if (primary_reader.NumDocs() == 0) {
				ReleaseReader (primary_reader);
				primary_reader = null;
				return false;
			}

			primary_searcher = new LNS.IndexSearcher (primary_reader);

			if (SecondaryStore != null) {
				secondary_reader = LuceneCommon.GetReader (SecondaryStore);
				if (secondary_reader.NumDocs () == 0) {
					ReleaseReader (secondary_reader);
					secondary_reader = null;
				}
			}

			if (secondary_reader != null)
				secondary_searcher = new LNS.IndexSearcher (secondary_reader);

			return true;
		}

		private void CloseSearchers (IndexReader primary_reader,
					    LNS.IndexSearcher primary_searcher,
					    IndexReader secondary_reader,
					    LNS.IndexSearcher secondary_searcher)
		{
			primary_searcher.Close ();
			if (secondary_searcher != null)
				secondary_searcher.Close ();
			ReleaseReader (primary_reader);
			if (secondary_reader != null)
				ReleaseReader (secondary_reader);
		}

		private void CreateQueryWhitelists (ICollection		search_subset_uris,
						    LNS.IndexSearcher	primary_searcher,
						    LNS.IndexSearcher	secondary_searcher,
						    LNS.BooleanQuery	primary_prohibited_part_query,
						    LNS.BooleanQuery	secondary_prohibited_part_query,
						    out LuceneBitArray	primary_whitelist,
						    out LuceneBitArray	secondary_whitelist)
		{
			primary_whitelist = null;
			secondary_whitelist = null;
			
			if (search_subset_uris != null && search_subset_uris.Count > 0) {
				primary_whitelist = new LuceneBitArray (primary_searcher);
				if (secondary_searcher != null)
					secondary_whitelist = new LuceneBitArray (secondary_searcher);

				foreach (Uri uri in search_subset_uris) {
					primary_whitelist.AddUri (uri);
					if (secondary_whitelist != null)
						secondary_whitelist.AddUri (uri);
				}
				primary_whitelist.FlushUris ();
				if (secondary_whitelist != null)
					secondary_whitelist.FlushUris ();
			}


			// Build blacklists from our prohibited parts.
			
			LuceneBitArray primary_blacklist = null;
			LuceneBitArray secondary_blacklist = null;

			if (primary_prohibited_part_query != null) {
				primary_blacklist = new LuceneBitArray (primary_searcher,
									primary_prohibited_part_query);
				
				if (secondary_searcher != null) {
					secondary_blacklist = new LuceneBitArray (secondary_searcher);
					if (secondary_prohibited_part_query != null)
						secondary_blacklist.Or (secondary_prohibited_part_query);
					primary_blacklist.Join (secondary_blacklist);
				}
			}

			
			// Combine our whitelist and blacklist into just a whitelist.
			
			if (primary_blacklist != null) {
				if (primary_whitelist == null) {
					primary_blacklist.Not ();
					primary_whitelist = primary_blacklist;
				} else {
					primary_whitelist.AndNot (primary_blacklist);
				}
			}

			if (secondary_blacklist != null) {
				if (secondary_whitelist == null) {
					secondary_blacklist.Not ();
					secondary_whitelist = secondary_blacklist;
				} else {
					secondary_whitelist.AndNot (secondary_blacklist);
				}
			}
		}

#if ENABLE_RDF_ADAPTER
		///////// RDF fu ///////////////////////////////////////////////

		// Returns a collection of Uris
		// HitFilter and UriFilter are ignored for now
		// They will come into play in the final FetchDocument part
		// FIXME: Should RDFQuery do any query mapping using backend_query_part_hook ?
		// I think it should not. QueryPart hooks are for human beings, RDF is for softwares.
		public ICollection DoRDFQuery (Query _query, TextCache text_cache)
		{
			RDFQuery query = (RDFQuery) _query;

			string subject, predicate, _object;
			PropertyType pred_type;

			subject = query.SubjectString;
			predicate = query.Predicate;
			pred_type = query.PredicateType;
			_object = query.Object;

			if (Debug)
				Logger.Log.Debug ("###### {0}: Starting low-level queries '{1}' : '{4}:{2}' = '{3}'", IndexName, subject, predicate, _object, pred_type);

			// ******** 8 cases **********

			// Return all uris
			if (subject == String.Empty && predicate == String.Empty && _object == String.Empty) {
				ICollection hits = GetAllHitsByUri ().Values;
				foreach (Hit hit in hits)
					foreach (Property text_link_property in GetTextLinks (hit.Uri, text_cache))
						hit.AddProperty (text_link_property);
				return hits;
			}

			// Normal query
			if (subject == String.Empty && predicate == String.Empty && _object != String.Empty) {
				QueryPart_Text part = new QueryPart_Text ();
				part.Text = _object;
				part.SearchFullText = false; // We only search properties in RDF query
				query.AddPart (part);
				return DoLowLevelRDFQuery (query, pred_type, predicate, _object, text_cache);
			}

			// Return uris for all documents with this property
			if (subject == String.Empty && predicate != String.Empty && _object == String.Empty) {
				string field_name = PropertyToFieldName (pred_type, predicate);

				QueryPart_Property part = new QueryPart_Property ();
				part.Type = PropertyType.Internal;
				part.Key = "Properties";
				part.Value = field_name;
				query.AddPart (part);

				return DoLowLevelRDFQuery (query, pred_type, predicate, null, text_cache);
			}

			// Property query
			if (subject == String.Empty && predicate != String.Empty && _object != String.Empty) {
				QueryPart_Property part = new QueryPart_Property ();
				part.Type = pred_type;
				part.Key = predicate;
				part.Value = _object;
				query.AddPart (part);
				return DoLowLevelRDFQuery (query, pred_type, predicate, _object, text_cache);
			}

			// Return if the URI exists
			if (subject != String.Empty && predicate == String.Empty && _object == String.Empty) {
				QueryPart_Uri part = new QueryPart_Uri ();
				part.Uri = UriFu.UserUritoEscapedUri (subject); // better be URI!
				query.AddPart (part);
				// FIXME: Which properties to return in the hit? All or none ?
				return DoLowLevelRDFQuery (query, pred_type, predicate, null, text_cache);
			}

			// Normal query in the document with this URI
			if (subject != String.Empty && predicate == String.Empty && _object != String.Empty) {
				QueryPart_Uri uri_part = new QueryPart_Uri ();
				uri_part.Uri = UriFu.UserUritoEscapedUri (subject); // better be URI!
				query.AddPart (uri_part);

				QueryPart_Text part = new QueryPart_Text ();
				part.Text = _object;
				part.SearchFullText = false; // We only search properties in RDF query
				query.AddPart (part);

				return DoLowLevelRDFQuery (query, pred_type, predicate, _object, text_cache);
			}

			// Return URI if the document with this URI contains this property
			if (subject != String.Empty && predicate != String.Empty && _object == String.Empty) {
				ArrayList returned_uris = new ArrayList (1);

				ArrayList uri_list = new ArrayList (1);
				uri_list.Add (UriFu.UserUritoEscapedUri (subject));

				string field_name = PropertyToFieldName (pred_type, predicate);
				FieldSelector fields = new MapFieldSelector (new string[] { "Uri", "Timestamp", field_name });
				ICollection hits = GetHitsForUris (uri_list, fields);
				if (predicate == "TextLinks") {
					foreach (Hit hit in hits)
						foreach (Property text_link_property in GetTextLinks (hit.Uri, text_cache))
							hit.AddProperty (text_link_property);
				}

				return hits;
			}

			// Property query in the document with this URI
			if (subject != String.Empty && predicate != String.Empty && _object != String.Empty) {
				QueryPart_Uri uri_part = new QueryPart_Uri ();
				uri_part.Uri = UriFu.UserUritoEscapedUri (subject); // better be URI!
				query.AddPart (uri_part);

				QueryPart_Property part = new QueryPart_Property ();
				part.Type = pred_type;
				part.Key = predicate;
				part.Value = _object;
				query.AddPart (part);

				return DoLowLevelRDFQuery (query, pred_type, predicate, _object, text_cache);
			}

			throw new Exception ("Never reaches");
		}

		private ICollection DoLowLevelRDFQuery (Query query,
							PropertyType pred_type,
							string predicate,
							string field_value,
							TextCache text_cache)
		{

			Stopwatch total, a, b, c, d, e, f;

			total = new Stopwatch ();
			a = new Stopwatch ();
			b = new Stopwatch ();
			c = new Stopwatch ();
			d = new Stopwatch ();
			e = new Stopwatch ();
			f = new Stopwatch ();

			total.Start ();
			a.Start ();

			// Assemble all of the parts into a bunch of Lucene queries

			ArrayList primary_required_part_queries;
			ArrayList secondary_required_part_queries;

			LNS.BooleanQuery primary_prohibited_part_query;
			LNS.BooleanQuery secondary_prohibited_part_query;

			AndHitFilter all_hit_filters;

			ArrayList term_list;

			// Assemble all of the parts into a bunch of Lucene queries

			term_list = AssembleQuery (query,
				null,
				null,
				out primary_required_part_queries,
				out secondary_required_part_queries,
				out primary_prohibited_part_query,
				out secondary_prohibited_part_query,
				out all_hit_filters);

			a.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Building queries took {1}", IndexName, a);

			// If we have no required parts, give up.
			if (primary_required_part_queries == null)
				return null;

			b.Start ();
			
			//
			// Now that we have all of these nice queries, let's execute them!
			//

			// Create the searchers that we will need.

			IndexReader primary_reader;
			LNS.IndexSearcher primary_searcher;
			IndexReader secondary_reader;
			LNS.IndexSearcher secondary_searcher;

			// Create the searchers that we will need.
			if (! BuildSearchers (out primary_reader, out primary_searcher, out secondary_reader, out secondary_searcher))
				return null;

			b.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Readers/searchers built in {1}", IndexName, b);

			// Build whitelists and blacklists for search subsets.
			c.Start ();
			
			// Possibly create our whitelists from the search subset.
			LuceneBitArray primary_whitelist, secondary_whitelist;
			CreateQueryWhitelists (null,
				primary_searcher,
				secondary_searcher,
				primary_prohibited_part_query,
				secondary_prohibited_part_query,
				out primary_whitelist,
				out secondary_whitelist);

			c.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Whitelists and blacklists built in {1}", IndexName, c);

			// Now run the low level queries against our indexes.
			d.Start ();

			BetterBitArray primary_matches = null;

			if (primary_required_part_queries != null) {

				if (secondary_searcher != null)
					primary_matches = DoRequiredQueries_TwoIndex (primary_searcher,
										      secondary_searcher,
										      primary_required_part_queries,
										      secondary_required_part_queries,
										      primary_whitelist,
										      secondary_whitelist);
				else
					primary_matches = DoRequiredQueries (primary_searcher,
									     primary_required_part_queries,
									     primary_whitelist);

			} 

			d.Stop ();
			if (Debug)
				Logger.Log.Debug ("###### {0}: Low-level queries finished in {1} and returned {2} matches", IndexName, d, primary_matches.TrueCount);

			e.Start ();

			int count = 0;
			Document doc;
			ArrayList hits = new ArrayList (primary_matches.TrueCount);

			TermDocs secondary_term_docs = null;
			if (secondary_searcher != null)
				secondary_term_docs = secondary_searcher.Reader.TermDocs ();
		
			FieldSelector fields = null;
			if (predicate != null)
				fields = new MapFieldSelector (new string[] { "Uri", "Timestamp", PropertyToFieldName (pred_type, predicate)});

			for (int match_index = primary_matches.GetNextTrueIndex (0);
			     match_index < primary_matches.Count; 
			     match_index = primary_matches.GetNextTrueIndex (++ match_index)) {

				count++;

				// If we have a HitFilter, apply it.
				// RDF FIXME: Ignore Hit Filter for now

				// If predicate was not specified but object was specified,
				// then figure out the right predicate
				if (predicate == null && field_value != null) {
					Hit hit = new Hit ();
					doc = primary_searcher.Doc (match_index);
					hit.Uri = GetUriFromDocument (doc);
					hit.Timestamp = StringFu.StringToDateTime (doc.Get ("Timestamp"));

					bool found_matching_predicate = false;

					foreach (Field field in doc.Fields ()) {
						if (! FieldIsPredicate (field, field_value))
							continue;

						Property prop = new Property ();
						prop.Type = pred_type;
						prop.Key = predicate;
						prop.Value = field_value;
						hit.AddProperty (prop);

						found_matching_predicate = true;
					}

					// Now get the matching predicate from the secondary index
					if (secondary_searcher == null) {
						doc = null;
					} else {
						Term term = new Term ("Uri", doc.Get ("Uri"));
						secondary_term_docs.Seek (term);
						if (secondary_term_docs.Next ())
							doc = secondary_searcher.Doc (secondary_term_docs.Doc ());
					}

					if (doc != null) {
						foreach (Field field in doc.Fields ()) {
							if (! FieldIsPredicate (field, field_value))
								continue;

							Property prop = new Property ();
							prop.Type = pred_type;
							prop.Key = predicate;
							prop.Value = field_value;
							hit.AddProperty (prop);

							found_matching_predicate = true;
						}
					}

					if (! found_matching_predicate) {
						// No matching predicate found
						// This means some unstored field matched the query
						// FIXME: Add a synthetic property #text
						hit.AddProperty (Property.New ("#text", field_value));
					}
					
					hits.Add (hit);
				} else if (predicate == "TextLinks") {
					// Special treatment: TextLinks is not stored but can be queried
					doc = primary_searcher.Doc (match_index, fields_timestamp_uri);
					Hit hit = CreateHit (doc, secondary_reader, secondary_term_docs, fields);
					if (field_value != null)
						hit.AddProperty (Property.New ("TextLinks", field_value));
					else {
						foreach (Property text_link_property in GetTextLinks (hit.Uri, text_cache))
							hit.AddProperty (text_link_property);
					}
					hits.Add (hit);
				} else {
					doc = primary_searcher.Doc (match_index, fields);
					Hit hit = CreateHit (doc, secondary_reader, secondary_term_docs, fields);
					foreach (Property prop in hit.Properties) {
						if (prop.Key == predicate)
							prop.Value = field_value;
					}

					hits.Add (hit);
				}
			}

			e.Stop ();

			if (Debug)
				Log.Debug ("###### {0}: Query results generated in {1}", IndexName, e);

			//
			// Finally, we clean up after ourselves.
			//

			f.Start ();
			CloseSearchers (primary_reader, primary_searcher, secondary_reader, secondary_searcher);
			f.Stop ();
			
			if (Debug)
				Log.Debug ("###### {0}: Readers/searchers released in {1}", IndexName, f);

			total.Stop ();
			if (Debug) {
				Log.Debug ("###### {0}: Query time breakdown:", IndexName);
				Log.Debug ("###### {0}:    Build queries {1,6} ({2:0.0}%)", IndexName, a, 100 * a.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:      Got readers {1,6} ({2:0.0}%)", IndexName, b, 100 * b.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:       Whitelists {1,6} ({2:0.0}%)", IndexName, c, 100 * c.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:          Queries {1,6} ({2:0.0}%)", IndexName, d, 100 * d.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:    Gen'd Results {1,6} ({2:0.0}%)", IndexName, e, 100 * e.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:   Reader cleanup {1,6} ({2:0.0}%)", IndexName, f, 100 * f.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:            TOTAL {1,6}", IndexName, total);

				Logger.Log.Debug ("###### {0}: Total query run in {1}", IndexName, total);
			}

			return hits;
		}

		// FIXME: This basically queries the value against the field
		// and is really really slow!
		private bool FieldIsPredicate (Field field, string value)
		{
			string field_name = field.Name ();
			string field_value = field.StringValue ();
			Console.WriteLine ("Reverse searching for '{0}' value in {1}='{2}'", value, field_name, field_value);
			// Simply run the value of the property against the right analyzer
			// and check if there is any match
			TokenStream source = IndexingAnalyzer.TokenStream (field_name, new StringReader (field_value));
			StringBuilder sb = new StringBuilder ();
			try {
				Lucene.Net.Analysis.Token token;
				while (true) {
					token = source.Next ();
					if (token == null)
						break;
					sb.Append (token.TermText ());
					sb.Append (" ");
					break;
				}
			} finally {
				try {
					source.Close ();
				} catch { }
			}

			string field_analyzed = sb.ToString ();
			sb.Length = 0;

			source = QueryAnalyzer.TokenStream (field_name, new StringReader (value));
			try {
				Lucene.Net.Analysis.Token token;
				while (true) {
					token = source.Next ();
					if (token == null)
						break;
					sb.Append (token.TermText ());
					sb.Append (" ");
					break;
				}
			} finally {
				try {
					source.Close ();
				} catch { }
			}

			string value_analyzed = sb.ToString ();
			return field_analyzed.Contains (value_analyzed);
		}

		private IEnumerable GetTextLinks (Uri uri, TextCache text_cache)
		{
			if (text_cache == null)
				yield break;

			IList<string> links = text_cache.GetLinks (uri);
			if (links == null)
				yield break;

			foreach (string link in links)
				yield return Property.NewKeyword ("TextLinks", link);
		}
#endif
		////////////////////////////////////////////////////////////////

		public int DoCountMatchQuery (Query query, QueryPartHook query_part_hook)
		{
			if (Debug)
				Logger.Log.Debug ("###### {0}: Starting low-level queries", IndexName);

			Stopwatch total;
			total = new Stopwatch ();
			total.Start ();

			ArrayList primary_required_part_queries;
			ArrayList secondary_required_part_queries;

			LNS.BooleanQuery primary_prohibited_part_query;
			LNS.BooleanQuery secondary_prohibited_part_query;

			AndHitFilter all_hit_filters;

			ArrayList term_list;
			term_list = AssembleQuery ( query,
						    query_part_hook,
						    null,
						    out primary_required_part_queries,
						    out secondary_required_part_queries,
						    out primary_prohibited_part_query,
						    out secondary_prohibited_part_query,
						    out all_hit_filters);

			// If we have no required parts, give up.
			if (primary_required_part_queries == null)
				return 0;

			IndexReader primary_reader;
			LNS.IndexSearcher primary_searcher;
			IndexReader secondary_reader;
			LNS.IndexSearcher secondary_searcher;

			if (! BuildSearchers (out primary_reader, out primary_searcher, out secondary_reader, out secondary_searcher))
				return 0;

			// Build whitelists and blacklists for search subsets.
			LuceneBitArray primary_whitelist, secondary_whitelist;
			CreateQueryWhitelists (null,
				primary_searcher,
				secondary_searcher,
				primary_prohibited_part_query,
				secondary_prohibited_part_query,
				out primary_whitelist,
				out secondary_whitelist);

			// Now run the low level queries against our indexes.
			BetterBitArray primary_matches = null;
			if (primary_required_part_queries != null) {

				if (secondary_searcher != null)
					primary_matches = DoRequiredQueries_TwoIndex (primary_searcher,
										      secondary_searcher,
										      primary_required_part_queries,
										      secondary_required_part_queries,
										      primary_whitelist,
										      secondary_whitelist);
				else
					primary_matches = DoRequiredQueries (primary_searcher,
									     primary_required_part_queries,
									     primary_whitelist);

			} 

			int result = 0;
			// FIXME: Pass the count through uri-filter and other validation checks
			if (primary_matches != null)
				result = primary_matches.TrueCount;

			CloseSearchers (primary_reader, primary_searcher, secondary_reader, secondary_searcher);

			total.Stop ();
			if (Debug)
				Logger.Log.Debug ("###### {0}: Total query run in {1}", IndexName, total);

			return result;
		}

		////////////////////////////////////////////////////////////////

		public void DoQuery (Query               query,
				     IQueryResult        result,
				     ICollection         search_subset_uris, // should be internal uris
				     QueryPartHook       query_part_hook,
				     HitFilter           hit_filter)
		{
			if (Debug)
				Logger.Log.Debug ("###### {0}: Starting low-level queries", IndexName);

			Stopwatch total, a, b, c, d, e, f;

			total = new Stopwatch ();
			a = new Stopwatch ();
			b = new Stopwatch ();
			c = new Stopwatch ();
			d = new Stopwatch ();
			e = new Stopwatch ();
			f = new Stopwatch ();

			total.Start ();
			a.Start ();

			ArrayList primary_required_part_queries;
			ArrayList secondary_required_part_queries;

			LNS.BooleanQuery primary_prohibited_part_query;
			LNS.BooleanQuery secondary_prohibited_part_query;

			AndHitFilter all_hit_filters;

			ArrayList term_list;

			// Assemble all of the parts into a bunch of Lucene queries

			term_list = AssembleQuery (query,
				query_part_hook,
				hit_filter,
				out primary_required_part_queries,
				out secondary_required_part_queries,
				out primary_prohibited_part_query,
				out secondary_prohibited_part_query,
				out all_hit_filters);

			a.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Building queries took {1}", IndexName, a);

			// If we have no required parts, give up.
			if (primary_required_part_queries == null)
				return;

			b.Start ();
			
			//
			// Now that we have all of these nice queries, let's execute them!
			//

			IndexReader primary_reader;
			LNS.IndexSearcher primary_searcher;
			IndexReader secondary_reader;
			LNS.IndexSearcher secondary_searcher;

			// Create the searchers that we will need.
			if (! BuildSearchers (out primary_reader, out primary_searcher, out secondary_reader, out secondary_searcher))
				return;

			b.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Readers/searchers built in {1}", IndexName, b);

			// Build whitelists and blacklists for search subsets.
			c.Start ();

			// Possibly create our whitelists from the search subset.
			LuceneBitArray primary_whitelist, secondary_whitelist;
			CreateQueryWhitelists (search_subset_uris,
				primary_searcher,
				secondary_searcher,
				primary_prohibited_part_query,
				secondary_prohibited_part_query,
				out primary_whitelist,
				out secondary_whitelist);

			c.Stop ();
			if (Debug)
				Log.Debug ("###### {0}: Whitelists and blacklists built in {1}", IndexName, c);

			// Now run the low level queries against our indexes.
			d.Start ();

			BetterBitArray primary_matches = null;

			if (primary_required_part_queries != null) {

				if (secondary_searcher != null)
					primary_matches = DoRequiredQueries_TwoIndex (primary_searcher,
										      secondary_searcher,
										      primary_required_part_queries,
										      secondary_required_part_queries,
										      primary_whitelist,
										      secondary_whitelist);
				else
					primary_matches = DoRequiredQueries (primary_searcher,
									     primary_required_part_queries,
									     primary_whitelist);

			} 

			d.Stop ();
			if (Debug)
				Logger.Log.Debug ("###### {0}: Low-level queries finished in {1}", IndexName, d);

			e.Start ();
			// Only generate results if we got some matches
			if (primary_matches != null && primary_matches.ContainsTrue ()) {
				GenerateQueryResults (primary_reader,
						      secondary_reader,
						      primary_matches,
						      result,
						      term_list,
						      query.MaxHits,
						      new HitFilter (all_hit_filters.HitFilter),
						      IndexName);
			}

			e.Stop ();

			if (Debug)
				Log.Debug ("###### {0}: Query results generated in {1}", IndexName, e);

			//
			// Finally, we clean up after ourselves.
			//

			f.Start ();
			CloseSearchers (primary_reader, primary_searcher, secondary_reader, secondary_searcher);
			f.Stop ();
			
			if (Debug)
				Log.Debug ("###### {0}: Readers/searchers released in {1}", IndexName, f);

			total.Stop ();
			if (Debug) {
				Log.Debug ("###### {0}: Query time breakdown:", IndexName);
				Log.Debug ("###### {0}:    Build queries {1,6} ({2:0.0}%)", IndexName, a, 100 * a.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:      Got readers {1,6} ({2:0.0}%)", IndexName, b, 100 * b.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:       Whitelists {1,6} ({2:0.0}%)", IndexName, c, 100 * c.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:          Queries {1,6} ({2:0.0}%)", IndexName, d, 100 * d.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:    Gen'd Results {1,6} ({2:0.0}%)", IndexName, e, 100 * e.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:   Reader cleanup {1,6} ({2:0.0}%)", IndexName, f, 100 * f.ElapsedTime / total.ElapsedTime);
				Log.Debug ("###### {0}:            TOTAL {1,6}", IndexName, total);

				Logger.Log.Debug ("###### {0}: Total query run in {1}", IndexName, total);
			}

		}

		////////////////////////////////////////////////////////////////

		//
		// Special logic for handling our set of required queries
		//

		// This is the easy case: we just combine all of the queries
		// into one big BooleanQuery.
		private static BetterBitArray DoRequiredQueries (LNS.IndexSearcher primary_searcher,
								 ArrayList primary_queries,
								 BetterBitArray primary_whitelist)
		{
			LNS.BooleanQuery combined_query;
			combined_query = new LNS.BooleanQuery ();
			foreach (LNS.Query query in primary_queries)
				combined_query.Add (query, LNS.BooleanClause.Occur.MUST);

			LuceneBitArray matches;
			matches = new LuceneBitArray (primary_searcher, combined_query);
			if (primary_whitelist != null)
				matches.And (primary_whitelist);

			return matches;
		}

		// This code attempts to execute N required queries in the
		// most efficient order to minimize the amount of time spent
		// joining between the two indexes.  It returns a joined bit
		// array of matches against the primary index.

		private class MatchInfo : IComparable {

			public LuceneBitArray PrimaryMatches = null;
			public LuceneBitArray SecondaryMatches = null;
			public int UpperBound = 0;

			public void Join ()
			{
				PrimaryMatches.Join (SecondaryMatches);
			}

			public void RestrictBy (MatchInfo joined)
			{
				if (joined != null) {
					this.PrimaryMatches.And (joined.PrimaryMatches);
					this.SecondaryMatches.And (joined.SecondaryMatches);
				}

				UpperBound = 0;
				UpperBound += PrimaryMatches.TrueCount;
				UpperBound += SecondaryMatches.TrueCount;
			}

			public int CompareTo (object obj)
			{
				MatchInfo other = (MatchInfo) obj;
				return this.UpperBound - other.UpperBound;
			}
		}

		// Any whitelists that are passed in must be fully joined, or
		// query results will be incorrect.
		private static BetterBitArray DoRequiredQueries_TwoIndex (LNS.IndexSearcher primary_searcher,
									  LNS.IndexSearcher secondary_searcher,
									  ArrayList primary_queries,
									  ArrayList secondary_queries,
									  BetterBitArray primary_whitelist,
									  BetterBitArray secondary_whitelist)
		{
			ArrayList match_info_list;
			match_info_list = new ArrayList ();

			// First, do all of the low-level queries
			// and store them in our MatchInfo 
			for (int i = 0; i < primary_queries.Count; ++i) {
				LNS.Query pq, sq;
				pq = primary_queries [i] as LNS.Query;
				sq = secondary_queries [i] as LNS.Query;

				LuceneBitArray p_matches = null, s_matches = null;
				p_matches = new LuceneBitArray (primary_searcher);
				if (pq != null) {
					p_matches.Or (pq);
					if (primary_whitelist != null)
						p_matches.And (primary_whitelist);
				}

				s_matches = new LuceneBitArray (secondary_searcher);
				if (sq != null) {
					s_matches.Or (sq);
					if (secondary_whitelist != null)
						s_matches.And (secondary_whitelist);
				}

				MatchInfo info;
				info = new MatchInfo ();
				info.PrimaryMatches = p_matches;
				info.SecondaryMatches = s_matches;
				info.RestrictBy (null); // a hack to initialize the UpperBound
				match_info_list.Add (info);
			}

			// We want to be smart about the order we do this in,
			// to minimize the expense of the Join.
			while (match_info_list.Count > 1) {

				// linear scan to find the minimum
				int index_min = 0;
				for (int i = 1; i < match_info_list.Count; ++i)
					if (((MatchInfo) match_info_list [i]).CompareTo ((MatchInfo) match_info_list [index_min]) < 0)
						index_min = i;

				MatchInfo smallest;
				smallest = match_info_list [index_min] as MatchInfo;
				match_info_list.RemoveAt (index_min);

				// We can short-circuit if our smallest set of
				// matches is empty.
				if (smallest.UpperBound == 0)
					return smallest.PrimaryMatches; // this must be an empty array.

				smallest.Join ();

				foreach (MatchInfo info in match_info_list)
					info.RestrictBy (smallest);
			}
			
			// For the final pair, we don't need to do a full join:
			// mapping the secondary onto the primary is sufficient
			MatchInfo last;
			last = match_info_list [0] as MatchInfo;
			last.SecondaryMatches.ProjectOnto (last.PrimaryMatches);

			return last.PrimaryMatches;
		}		

		////////////////////////////////////////////////////////////////

		static private void ScoreHits (Dictionary<int, Hit>   hits_by_id,
					       IndexReader reader,
					       ICollection term_list)
		{
			LNS.Similarity similarity;
			similarity = LNS.Similarity.GetDefault ();

			TermDocs term_docs = reader.TermDocs ();
			Hit hit;

			foreach (Term term in term_list) {

				double idf;
				idf = similarity.Idf (reader.DocFreq (term), reader.MaxDoc ());

				int hit_count;
				hit_count = hits_by_id.Count;

				term_docs.Seek (term);
				while (term_docs.Next () && hit_count > 0) {
					
					int id;
					id = term_docs.Doc ();

					if (hits_by_id.TryGetValue (id, out hit)) {
						double tf;
						tf = similarity.Tf (term_docs.Freq ());
						hit.Score += tf * idf;
						--hit_count;
					}
				}
			}

			term_docs.Close ();
		}

		////////////////////////////////////////////////////////////////

		private class DocAndId {
			public Document Doc;
			public int Id;
		}

		//
		// Given a set of hits, broadcast some set out as our query
		// results.
		//

		// Two arrays we need for quickly creating lucene documents and check if they are valid
		static FieldSelector fields_timestamp_uri = new MapFieldSelector (new string[] {"Uri", "Timestamp"});
		static FieldSelector fields_uri = new MapFieldSelector (new string[] {"Uri"});

		private static void GenerateQueryResults (IndexReader       primary_reader,
							  IndexReader       secondary_reader,
							  BetterBitArray    primary_matches,
							  IQueryResult      result,
							  ICollection       query_term_list,
							  int               max_results,
							  HitFilter         hit_filter,
							  string            index_name)
		{
			int num_hits;

			if (Debug)
				Logger.Log.Debug (">>> {0}: Initially handed {1} matches", index_name, primary_matches.TrueCount);

			if (primary_matches.TrueCount <= max_results) {
				if (Debug)
					Logger.Log.Debug (">>> {0}: Initial count is within our limit of {1}", index_name, max_results);
				num_hits = primary_matches.TrueCount;
			} else {
				if (Debug)
					Logger.Log.Debug (">>> {0}: Number of hits is capped at {1}", index_name, max_results);
				num_hits = max_results;
			}

			Stopwatch total, d, e;
			total = new Stopwatch ();
			d = new Stopwatch ();
			e = new Stopwatch ();

			total.Start ();

			ArrayList final_list_of_hits = null;

			// This is used only for scoring
			Dictionary<int, Hit> hits_by_id = new Dictionary<int, Hit> (num_hits);

			int total_number_of_matches = primary_matches.TrueCount;

			if (primary_matches.TrueCount > max_results)
				final_list_of_hits = ScanRecentDocs (primary_reader,
					secondary_reader,
					primary_matches,
					hits_by_id,
					max_results,
					ref total_number_of_matches,
					hit_filter,
					index_name);

			if (final_list_of_hits == null)
				final_list_of_hits = FindRecentResults (primary_reader,
					secondary_reader,
					primary_matches,
					hits_by_id,
					max_results,
					ref total_number_of_matches,
					hit_filter,
					index_name);

			d.Start ();

			ScoreHits (hits_by_id, primary_reader, query_term_list);
			hits_by_id = null;

			d.Stop ();

			if (Debug)
				Log.Debug (">>> {0}: Scored hits in {1}", index_name, d);

			e.Start ();

			// 25 hits seems to be the sweet spot: anything lower
			// and serialization overhead gets us, higher takes
			// longer to send out.
			const int MAX_QUEUED_HITS = 25;
			int sent_index = 0;

			// Break up the hits into reasonably sized chunks for
			// sending over the wire.
			for (int i = 0; i < final_list_of_hits.Count; ++i) {
				// Flush our hits
				if (i > 0 && i % MAX_QUEUED_HITS == 0) {
					result.Add (final_list_of_hits.GetRange (0, MAX_QUEUED_HITS));
					final_list_of_hits.RemoveRange (0, MAX_QUEUED_HITS);
					i -= MAX_QUEUED_HITS;
				}
			}

			// Flush the remaining hits
			result.Add (final_list_of_hits, total_number_of_matches);
			final_list_of_hits = null;

			e.Stop ();

			if (Debug)
				Log.Debug (">>> {0}: Hit filters executed and results sent in {1}", index_name, e);

			total.Stop ();

			if (Debug) {
				Logger.Log.Debug (">>> {0}: GenerateQueryResults time statistics:", index_name);
				//Logger.Log.Debug (">>> {0}:   Short circuit {1,6} ({2:0.0}%)", index_name, a == null ? "N/A" : a.ToString (), a == null ? 0.0 : 100 * a.ElapsedTime / total.ElapsedTime);
				//Logger.Log.Debug (">>> {0}:     Create docs {1,6} ({2:0.0}%)", index_name, b, 100 * b.ElapsedTime / total.ElapsedTime);
				//Logger.Log.Debug (">>> {0}:    Hit assembly {1,6} ({2:0.0}%)", index_name, c, 100 * c.ElapsedTime / total.ElapsedTime);
				Logger.Log.Debug (">>> {0}:     Scored hits {1,6} ({2:0.0}%)", index_name, d, 100 * d.ElapsedTime / total.ElapsedTime);
				Logger.Log.Debug (">>> {0}:    Results sent {1,6} ({2:0.0}%)", index_name, e, 100 * e.ElapsedTime / total.ElapsedTime);
				Logger.Log.Debug (">>> {0}:           TOTAL {1,6}", index_name, total);
			}
		}

		// There are two ways we can determine the max_results
		// most recent items:  
		//
		// One is to instantiate Lucene documents for each of
		// the document IDs in primary_matches.  This is a
		// fairly expensive operation.
		//
		// The other is to walk through the list of all
		// document IDs in descending time order.  This is
		// a less expensive operation, but adds up over time
		// on large data sets.
		//
		// We can walk about 2.5 docs for every Document we
		// instantiate.  So what we'll do, if we have more
		// matches than available hits, is walk (m * 1.25)
		// docs to see if we can fill out the top 100 hits.
		// If not, we'll fall back to creating documents
		// for all of them.

		private static ArrayList ScanRecentDocs (IndexReader	    primary_reader,
						    IndexReader		    secondary_reader,
						    BetterBitArray	    primary_matches,
						    Dictionary<int, Hit>    hits_by_id,
						    int			    max_results,
						    ref int		    total_number_of_matches,
						    HitFilter		    hit_filter,
						    string		    index_name)
		{
			Stopwatch a = new Stopwatch ();
			a.Start ();

			TermDocs docs = primary_reader.TermDocs ();
			TermEnum enumerator = primary_reader.Terms (new Term ("InvertedTimestamp", String.Empty));
			ArrayList results = new ArrayList (max_results);
			int docs_found = 0;
			int docs_walked = 0;
			int hit_filter_removed = 0;
			int max_docs = (int) (primary_matches.TrueCount * 1.25);

			Term term;
			TermDocs secondary_term_docs = null;
			if (secondary_reader != null)
				secondary_term_docs = secondary_reader.TermDocs ();

			do {
				term = enumerator.Term ();
			
				if (term.Field () != "InvertedTimestamp")
					break;

				docs.Seek (enumerator);

				while (docs.Next ()
				       && docs_found < max_results
				       && docs_walked < max_docs) {
					int doc_id = docs.Doc ();

					if (primary_matches.Get (doc_id)) {
						Document doc = primary_reader.Document (doc_id);
						Hit hit = CreateHit (doc, secondary_reader, secondary_term_docs);

						// If we have a HitFilter, apply it.
						if (hit_filter != null && ! hit_filter (hit)) {
							if (Debug)
								Log.Debug ("Filtered out {0}", hit.Uri);
							hit_filter_removed ++;
							continue;
						}
						hits_by_id [doc_id] = hit;
						// Add the result, last modified first
						results.Add (hit);
						docs_found++;
					}
			
					docs_walked++;
				}
			} while (enumerator.Next ()
				 && docs_found < max_results
				 && docs_walked < max_docs);

			docs.Close ();
			if (secondary_term_docs != null)
				secondary_term_docs.Close ();

			// If we've found all the docs we can return in a subset!
			// Fantastic, we've probably short circuited a slow search.
			if (docs_found != max_results) {
				// Otherwise bad luck! Not all docs found
				// Start afresh - this time traversing all results
				results = null;
			} else {
				// Adjust total_number_of_matches. We need to do this to avoid scenarios like the following:
				// max_hits = 100. Matched 100 results. But hit filter removed 30. So 70 results will be returned.
				// We want to avoid saying "Showing top 70 of 100". Note that since we are not passing
				// every document in the index through the hit_filter, when we say "Showing top 100 of 1234", the
				// 1234 could actually be much less. But since max_hits was 100, that will not mislead the user.
				total_number_of_matches -= hit_filter_removed;
			}

			a.Stop ();
			if (Debug) {
				Log.Debug (">>> {0}: Walked {1} items, populated an enum with {2} items in {3}", index_name, docs_walked, docs_found, a);
				
				if (docs_found == max_results)
					Log.Debug (">>> {0}: Successfully short circuited timestamp ordering!", index_name);
			}

			return results;
		}

		private static ArrayList   FindRecentResults (IndexReader	    primary_reader,
							      IndexReader	    secondary_reader,
							      BetterBitArray	    primary_matches,
							      Dictionary<int, Hit>  hits_by_id,
							      int		    max_results,
							      ref int		    total_number_of_matches,
							      HitFilter		    hit_filter,
							      string		    index_name)
		{
			Stopwatch b = new Stopwatch ();
			b.Start ();
			
			int count = 0;
			Document doc;

			ArrayList all_docs = null;
			TopScores top_docs = null;
			TermDocs term_docs = null;

			if (primary_matches.TrueCount > max_results)
				top_docs = new TopScores (max_results);
			else
				all_docs = new ArrayList (primary_matches.TrueCount);

			if (secondary_reader != null)
				term_docs = secondary_reader.TermDocs ();

			for (int match_index = primary_matches.Count; ; match_index --) {
				// Walk across the matches backwards, since newer
				// documents are more likely to be at the end of
				// the index.
				match_index = primary_matches.GetPreviousTrueIndex (match_index);
				if (match_index < 0)
					break;

				count++;

				doc = primary_reader.Document (match_index, fields_timestamp_uri);

				// Check the timestamp --- if we have already reached our
				// limit, we might be able to reject it immediately.
				string timestamp_str;
				long timestamp_num = 0;

				timestamp_str = doc.Get ("Timestamp");
				if (timestamp_str == null) {
					Logger.Log.Warn ("No timestamp on {0}!", GetUriFromDocument (doc));
				} else {
					timestamp_num = Int64.Parse (doc.Get ("Timestamp"));
					if (top_docs != null && ! top_docs.WillAccept (timestamp_num))
						continue;
				}

				// Get the actual hit now
				// doc was created with only 2 fields, so first get the complete lucene document for primary document.
				// Also run our hit_filter now, if we have one. Since we insist of returning max_results
				// most recent hits, any hits that would be filtered out should happen now and not later.
				Hit hit = CreateHit (primary_reader.Document (match_index), secondary_reader, term_docs);
				if (hit_filter != null && ! hit_filter (hit)) {
					if (Debug)
						Log.Debug ("Filtered out {0}", hit.Uri);
					total_number_of_matches --;
					continue;
				}

				hits_by_id [match_index] = hit;

				// Add the document to the appropriate data structure.
				// We use the timestamp_num as the score, so high
				// scores correspond to more-recent timestamps.
				if (all_docs != null)
					all_docs.Add (hit);
				else
					top_docs.Add (timestamp_num, hit);
			}

			if (term_docs != null)
				term_docs.Close ();

			b.Stop ();

			if (Debug)
				Log.Debug (">>> {0}: Instantiated and scanned {1} documents in {2}", index_name, count, b);

			if (all_docs != null) {
				// Sort results before sending
				all_docs.Sort ();
				return all_docs;
			} else {
				return top_docs.TopScoringObjects;
			}
		}

		private static Hit CreateHit ( Document primary_doc,
					IndexReader secondary_reader,
					TermDocs term_docs)
		{
			return CreateHit ( primary_doc,
					secondary_reader,
					term_docs,
					null);
		}

		private static Hit CreateHit ( Document primary_doc,
					IndexReader secondary_reader,
					TermDocs term_docs,
					FieldSelector fields)
		{
			Hit hit = DocumentToHit (primary_doc);

			if (secondary_reader == null)
				return hit;

			// Get the stringified version of the URI
			// exactly as it comes out of the index.
			Term term = new Term ("Uri", primary_doc.Get ("Uri"));
			term_docs.Seek (term);

			// Move to the first (and only) matching term doc
			term_docs.Next ();
			Document secondary_doc =
				(fields == null) ?
				secondary_reader.Document (term_docs.Doc ()) :
				secondary_reader.Document (term_docs.Doc (), fields);

			// If we are using the secondary index, now we need to
			// merge the properties from the secondary index
			AddPropertiesToHit (hit, secondary_doc, false);

			return hit;
		}
	}
}
