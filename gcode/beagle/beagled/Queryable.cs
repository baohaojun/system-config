//
// Queryable.cs
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

using Beagle.Util;
using Beagle;

namespace Beagle.Daemon {

	public class Queryable {

		private QueryableFlavor flavor = null;
		private IQueryable iqueryable = null;

		public Queryable (QueryableFlavor flavor, IQueryable iqueryable)
		{
			this.flavor = flavor;
			this.iqueryable = iqueryable;
		}
		
		public void Start ()
		{
			if (! Shutdown.ShutdownRequested)
				iqueryable.Start ();
		}

		public string Name {
			get { return flavor.Name; }
		}

		public QueryDomain Domain {
			get { return flavor.Domain; }
		}

		public string DependsOn {
			get { return flavor.DependsOn; }
		}

		public IQueryable IQueryable {
			get { return iqueryable; }
		}

		public bool AcceptQuery (Query query)
		{
			return query != null
				&& (query.IsIndexListener || ! query.IsEmpty)
				&& query.AllowsDomain (Domain)
				&& iqueryable.AcceptQuery (query);
		}
				    
		public void DoQuery (Query query, IQueryResult result, IQueryableChangeData change_data)
		{
			try {
				iqueryable.DoQuery (query, result, change_data);
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception calling DoQuery on '{0}'", Name);
			}
		}


		public int DoCountMatchQuery (Query query)
		{
			try {
				return iqueryable.DoCountMatchQuery (query);
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception calling DoCountMatchQuery on '{0}'", Name);
				return 0;
			}
		}

		public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			if (hit == null)
				return null;

			// Sanity-check: make sure this Hit actually came out of this Queryable
			if (QueryDriver.GetQueryable (hit.Source) != this) {
				string msg = String.Format ("Queryable mismatch in GetSnippet: {0} vs {1}", hit.Source, this);
				throw new Exception (msg);
			}

			try {
				return iqueryable.GetSnippet (query_terms, hit, full_text, ctx_length, snp_length);
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception calling DoQuery on '{0}'", Name);
			}
			
			return null;
		}

		public QueryableStatus GetQueryableStatus ()
		{
		        QueryableStatus status = iqueryable.GetQueryableStatus ();

			if (status == null)
				status = new QueryableStatus ();

			status.Name = Name;

			return status;
		}
	}
}

