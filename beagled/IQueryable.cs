//
// IQueryable.cs
//
// Copyright (C) 2004 Novell, Inc.
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

namespace Beagle.Daemon {

	public interface IQueryable {

		void Start ();

		// Should not modify query
		bool AcceptQuery (Query query);

		// Should not modify query
		void DoQuery (Query query,
			      IQueryResult result,
			      IQueryableChangeData data);

#if ENABLE_RDF_ADAPTER
		ICollection DoRDFQuery (Query query);
#endif
		// Just return the number of matches
		int DoCountMatchQuery (Query query);

		ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int context_length, int snippet_length);

		QueryableStatus GetQueryableStatus ();
	}

	public interface IQueryableChangeData { 
	
	}

	[AttributeUsage (AttributeTargets.Assembly)]
	public class IQueryableTypesAttribute : TypeCacheAttribute {
		public IQueryableTypesAttribute (params Type[] queryable_types) : base (queryable_types) { }
	}
}
