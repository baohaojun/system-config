//
// QueryResult.cs
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
using Debug = System.Diagnostics.Debug;
using System.Threading;
using Beagle.Util;

namespace Beagle.Daemon {

	public class QueryResult : IQueryResult, IDisposable {

		public delegate void StartedHandler (QueryResult source);
		public event StartedHandler StartedEvent;

		public delegate void HitsAddedHandler (QueryResult source, ICollection someHits, int total_results);
		public event HitsAddedHandler HitsAddedEvent;

		public delegate void HitsSubtractedHandler (QueryResult source, ICollection someUris);
		public event HitsSubtractedHandler HitsSubtractedEvent;

		public delegate void FinishedHandler (QueryResult source);
		public event FinishedHandler FinishedEvent;

		//////////////////////////////////

		int workers = 0;
		bool cancelled = false;
		Hashtable uri_hash = UriFu.NewHashtable ();
		DateTime started_time;
		DateTime finished_time;
		Hashtable per_worker_started_time = new Hashtable ();
		bool is_index_listener = false;


		public QueryResult ()
		{

		}

		//////////////////////////////////

		public void Dispose () 
		{
			lock (this) {
				if (cancelled)
					return;
				cancelled = true;
			}	
		}

		//////////////////////////////////

		public bool Active {
			get { return workers > 0 && ! cancelled; }
		}

		public bool Cancelled {
			get { return cancelled; }
		}

		public bool IsIndexListener {
			get { return is_index_listener; }
			set { is_index_listener = value; }
		}

		public void Cancel ()
		{
			lock (this) {
				if (cancelled)
					return;
				cancelled = true;
				Monitor.Pulse (this);
			}
		}

		public void Add (ICollection some_hits)
		{
			Add (some_hits, -1);
		}

		// Note: some_hits is allowed to contain null.
		// They are silently ignored.
		public void Add (ICollection some_hits, int total_results)
		{
			lock (this) {
				if (cancelled)
					return;

				Debug.Assert (workers > 0, "Adding Hits to idle QueryResult");

				if (some_hits.Count == 0)
					return;

				if (IsIndexListener) {
					if (HitsAddedEvent != null)
						HitsAddedEvent (this, some_hits, total_results);
					return;
				}

				// Be careful not to report the same hit twice.
				ArrayList hits_to_report;
				hits_to_report = new ArrayList ();
				foreach (Hit hit in some_hits) {
					if (hit != null && ! uri_hash.Contains (hit.Uri)) {
						uri_hash [hit.Uri] = hit.Uri;
						hits_to_report.Add (hit);
					}
				}
				
				if (HitsAddedEvent != null && hits_to_report.Count > 0)
					HitsAddedEvent (this, hits_to_report, total_results);
			}
		}

		// Note: some_uris is allowed to contain null.
		// They are silently ignored.
		public void Subtract (ICollection some_uris)
		{
			lock (this) {
				if (cancelled)
					return;

				Debug.Assert (workers > 0, "Subtracting Hits from idle QueryResult");

				if (some_uris.Count == 0)
					return;
				
				if (IsIndexListener) {
					if (HitsSubtractedEvent != null)
						HitsSubtractedEvent (this, some_uris);
					return;
				}

				ArrayList filtered_uris = new ArrayList ();

				// We only get to subtract a URI if it was previously added.
				foreach (Uri uri in some_uris) {
					if (uri != null && uri_hash.Contains (uri)) {
						filtered_uris.Add (uri);
						uri_hash.Remove (uri);
					}
				}

				if (HitsSubtractedEvent != null && filtered_uris.Count > 0)
					HitsSubtractedEvent (this, filtered_uris);
			}
		}

		//////////////////////////////////////////////////////////////////////////////////////

		class QueryWorkerClosure {
			IQueryWorker worker;
			QueryResult result;

			public QueryWorkerClosure (IQueryWorker _worker, QueryResult _result)
			{
				worker = _worker;
				result = _result;
			}

			public void Start ()
			{
				try {
					worker.DoWork ();
				} catch (Exception e) {
					Logger.Log.Error (e, "QueryWorker '{0}' threw an exception", worker);
				}
				try {
					result.WorkerFinished (worker);
				} catch (Exception e) {
					Logger.Log.Error ("QueryResult threw an exception while calling WorkerFinished for '{0}'",
							  worker);
				}
			}
		}

		public void AttachWorker (IQueryWorker worker)
		{
			lock (this) {
				if (cancelled)
					return;

				QueryWorkerClosure qwc;
				qwc = new QueryWorkerClosure (worker, this);

				// QueryDriver has an enclosing WorkerStart,
				// so if we call WorkerStart in this thread, 
				// all the workers will have a chance 
				// to start before Finished is called
				
				if (!WorkerStartNoLock (worker)) 
					return;

				// Run our queries serially rather than in
				// parallel with threads.  Since 98% of the
				// time indexes will be stored on the same disk
				// we gain nothing by parallelizing, even on
				// multiprocessor/multicore systems.
				//
				// This also reduces memory usage considerably.

				//ExceptionHandlingThread.Start (new ThreadStart (qwc.Start));
				qwc.Start ();
			}
		}

		private bool WorkerStartNoLock (object o)
		{
			if (!Shutdown.WorkerStart (o))
				return false;

			DateTime now = DateTime.Now;
			per_worker_started_time [o] = now;
			++workers;
			if (workers == 1) {
				started_time = now;
				if (StartedEvent != null) 
					StartedEvent (this);
			}


			return true;
			
		}

		internal bool WorkerStart (object o)
		{
			lock (this) {
				return WorkerStartNoLock (o);
			}
		}

		internal void WorkerFinished (object o)
		{
			lock (this) {
				Debug.Assert (workers > 0, "Too many calls to WorkerFinished");
				--workers;


				DateTime now = DateTime.Now;

				//DateTime then = (DateTime) per_worker_started_time [o];
				//Logger.Log.Debug ("{0} finished in {1:0.00}s", o, (now - then).TotalSeconds);

				per_worker_started_time.Remove (o);

				if (workers == 0) {
					finished_time = now;
					//Logger.Log.Debug ("Last worker finished {0:0.00}s after start",
					//(finished_time - started_time).TotalSeconds);
					if (FinishedEvent != null)
						FinishedEvent (this);
					Monitor.Pulse (this);
				}
			}
			Shutdown.WorkerFinished (o);
		}

		public void Wait ()
		{
			lock (this) {
				while (true) {
					if (cancelled || workers == 0)
						return;
					Monitor.Wait (this);
				}
			}
		}
	}
}
