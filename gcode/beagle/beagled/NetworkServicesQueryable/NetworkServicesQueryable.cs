//
//  NetworkServicesQueryable.cs
//
//  Copyright (c) 2007 Lukas Lipka <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;

using Beagle;
using Beagle.Util;

namespace Beagle.Daemon.NetworkServicesQueryable {

	[QueryableFlavor (Name="NetworkServices", Domain=QueryDomain.Neighborhood, RequireInotify=false)]
	public class NetworkServicesQueryable : IQueryable {

		public NetworkServicesQueryable ()
		{
		}

		public void Start ()
		{
		}

		public bool AcceptQuery (Query query)
		{
			List<string[]> services = Conf.Networking.GetListOptionValues (Conf.Names.NetworkServices);
			return (services != null && services.Count > 0);
		}

#if ENABLE_RDF_ADAPTER
		public ICollection DoRDFQuery (Query query)
		{
			return null;
		}
#endif

		public void DoQuery (Query query, IQueryResult result, IQueryableChangeData data)
		{
			// Get rid of the standard UnixTransport so that we can
			// forward our local query to remote hosts.
			query.Transports.Clear ();

			List<string[]> network_services = Conf.Networking.GetListOptionValues (Conf.Names.NetworkServices);
			if (network_services != null) {
				foreach (string[] service in network_services)
					query.RegisterTransport (new HttpTransport (service [1]));
			}

			// Anonymous delegates cannot be un-registered ... hence
			Query.HitsAdded hits_added_handler;
			hits_added_handler = delegate (HitsAddedResponse response) {
								//Console.WriteLine ("Adding hits added response");
								result.Add (response.Hits, response.NumMatches);
						};

			Query.HitsSubtracted hits_subtracted_handler;
			hits_subtracted_handler = delegate (HitsSubtractedResponse response) {
								// Console.WriteLine ("Adding hits subtracted response");
								result.Subtract (response.Uris);
						    };

			Query.Finished finished_handler;
			finished_handler = delegate (FinishedResponse response) {
							//Console.WriteLine ("Adding finished response");
							// NO-OP
					    };

			// FIXME: ClosedEvent ? Should be handled by HttpTransport but should we do something more

			query.HitsAddedEvent += hits_added_handler;
			query.HitsSubtractedEvent += hits_subtracted_handler;
			query.FinishedEvent += finished_handler;

			Exception throw_me = null;

			try {
				query.SendAsyncBlocking ();
			} catch (Exception ex) {
				throw_me = ex;
			}

			// FIXME FIXME FIXME: Live query does not work!

			query.HitsAddedEvent -= hits_added_handler;
			query.HitsSubtractedEvent -= hits_subtracted_handler;
			query.FinishedEvent -= finished_handler;
			query.Transports.Clear ();

			if (throw_me != null)
				throw throw_me;

			return;
		}

		public int DoCountMatchQuery (Query query)
		{
			return 0;
		}

		public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			string source = hit ["beagle:Source"];
			hit ["beagle:Source"] = hit ["beagle:OrigSource"];

			string network_node = hit ["beagle:NetworkNode"];
			SnippetReader snippet_reader = null;

			// FIXME: Creating a snippet request, registering transports, all of this
			// doing everytime for hundreds of hits may become quite expensive.
			// In that case, pre generate one snippetrequest and use it over and over.

			// Form a correct snippet request
			SnippetRequest sreq = new SnippetRequest ();
			sreq.Hit = hit;
			sreq.QueryTerms = query_terms;
			sreq.FullText = full_text;
			sreq.ContextLength = ctx_length;
			sreq.SnippetLength = snp_length;

			// fake a blocking snippet retrieval
			sreq.RegisterAsyncResponseHandler (typeof (SnippetResponse),
							   delegate (ResponseMessage response) {
				if (response is ErrorResponse) {
					Log.Error ("Error retrieval snippet for {0} from network node {1}", hit.Uri, network_node);
					return;
				}

				snippet_reader = new SnippetReader ((SnippetResponse) response);
			});

			List<string[]> network_services = Conf.Networking.GetListOptionValues (Conf.Names.NetworkServices);
			foreach (string[] service in network_services) {
				if (network_node != service [0])
					continue;

				sreq.Transports.Clear ();
				sreq.RegisterTransport (new HttpTransport (service [1]));

				// fake a blocking snippet retrieval
				try {
					sreq.SendAsyncBlocking ();
				} catch (Exception e) {
					Log.Debug (e, "Error while requesting snippet from {0} for {1}", service [1], hit.Uri);
				}
				break;
			}

			hit ["beagle:Source"] = source; // reset source
			return snippet_reader;
		}

		public QueryableStatus GetQueryableStatus ()
		{
			QueryableStatus status = new QueryableStatus ();
			return status;
		}

		private class SnippetReader : ISnippetReader {
			private SnippetResponse response;
			public SnippetReader (SnippetResponse response)
			{
				this.response = response;
			}

			public IEnumerable GetSnippet ()
			{
				return response.SnippetList.Snippets;
			}

			private System.IO.StringReader fulltext_reader = null;

			private bool ReadLineInit ()
			{
				if (response.SnippetList.Snippets == null)
					return false;

				foreach (SnippetLine line in response.SnippetList.Snippets) {
					if (line.Fragments == null || line.Fragments.Count == 0)
						return false;

					Fragment fragment = (Fragment) line.Fragments [0];
					fulltext_reader = new System.IO.StringReader (fragment.Text);
					return true;
				}

				return false; // make gmcs happy
			}

			public string ReadLine ()
			{
				if (fulltext_reader == null && ! ReadLineInit ())
					return null;

				return fulltext_reader.ReadLine ();
			}

			public void Close ()
			{
			}
		}
	}
}
