//
// QueryDriver.cs
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
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Threading;
using Beagle.Util;
using System.Xml.Serialization;

namespace Beagle.Daemon {
	
	public static class QueryDriver {

		// Contains list of queryables explicitly asked by --allow-backend or --backend name
		// --allow-backend/--backend name : dont read config information and only start backend 'name'
		static ArrayList excl_allowed_queryables = new ArrayList ();

		// Contains list of denied queryables from config/arguments (every queryable is enabled by default)
		// Unless overruled by --allow-backend/--backend name, deny backend only if names appears here.
		static ArrayList denied_queryables = new ArrayList ();
		
		static bool to_read_conf = true; // read backends from conf if true
		static bool done_reading_conf = false;

		static private void ReadBackendsFromConf ()
		{
			if (! to_read_conf || done_reading_conf)
				return;

			// set flag here to stop Allow() from calling ReadBackendsFromConf() again
			done_reading_conf = true;

			Config config = Conf.Get (Conf.Names.DaemonConfig);

			// To allow static indexes, "static" should be in allowed_queryables
			if (config.GetOption (Conf.Names.AllowStaticBackend, false))
				Allow ("static");

			List<string[]> values = config.GetListOptionValues (Conf.Names.DeniedBackends);
			if (values == null)
				return;
			
			foreach (string[] name in values)
				denied_queryables.Add (name [0].ToLower ());
		}

		static public void OnlyAllow (string name)
		{
			excl_allowed_queryables.Add (name.ToLower ());
			to_read_conf = false;
		}
		
		static public void Allow (string name)
		{
			if (! done_reading_conf && to_read_conf)
				ReadBackendsFromConf ();

			denied_queryables.Remove (name.ToLower ());
		}
		
		static public void Deny (string name)
		{
			if (! done_reading_conf && to_read_conf)
				ReadBackendsFromConf ();

			name = name.ToLower ();
			if (!denied_queryables.Contains (name))
				denied_queryables.Add (name);
		}

		static private bool UseQueryable (string name)
		{
			name = name.ToLower ();

			if (excl_allowed_queryables.Contains (name))
				return true;
			if (excl_allowed_queryables.Count != 0)
				return false;

			if (denied_queryables.Contains (name))
				return false;

			return true;
		}

		//////////////////////////////////////////////////////////////////////////////////////

		// Paths to static queryables

		static ArrayList static_queryables = new ArrayList ();
		
		static public void AddStaticQueryable (string path) {

			if (! static_queryables.Contains (path))
				static_queryables.Add (path);
		}

		//////////////////////////////////////////////////////////////////////////////////////

		// Delay before starting the indexing process

		static int indexing_delay = 60;  // Default to 60 seconds

		public static int IndexingDelay {
			set { indexing_delay = value; }
			get { return indexing_delay; }
		}

		//////////////////////////////////////////////////////////////////////////////////////

		// Use introspection to find all classes that implement IQueryable, the construct
		// associated Queryables objects.

		// To protect the list of iqueryables, since now we support adding/removing of
		// static queryables at runtime
		static ReaderWriterLock iqueryable_lock = new ReaderWriterLock ();

		static Hashtable iqueryable_to_queryable = new Hashtable ();
		static ICollection Queryables {
			get { return iqueryable_to_queryable.Values; }
		}

		// (1) register themselves in AssemblyInfo.cs:IQueryableTypes and
		// (2) has a QueryableFlavor attribute attached
		// assemble a Queryable object and stick it into our list of queryables.
		static void ScanAssemblyForQueryables (Assembly assembly)
		{
			int count = 0;

			foreach (Type type in ReflectionFu.GetTypesFromAssemblyAttribute (assembly, typeof (IQueryableTypesAttribute))) {
				foreach (QueryableFlavor flavor in ReflectionFu.ScanTypeForAttribute (type, typeof (QueryableFlavor))) {
					if (! UseQueryable (flavor.Name))
						continue;

					if (flavor.RequireInotify && ! Inotify.Enabled) {
						Logger.Log.Warn ("Can't start backend '{0}' without inotify", flavor.Name);
						continue;
					}

					if (flavor.RequireExtendedAttributes && ! ExtendedAttribute.Supported) {
						Logger.Log.Warn ("Can't start backend '{0}' without extended attributes", flavor.Name);
						continue;
					}

					IQueryable iq = null;
					try {
						iq = Activator.CreateInstance (type) as IQueryable;
					} catch (Exception e) {
						Logger.Log.Error (e, "Caught exception while instantiating {0} backend", flavor.Name);
					}

					if (iq != null) {
						Queryable q = new Queryable (flavor, iq);
						iqueryable_to_queryable [iq] = q;
						++count;
						break;
					}
				}
			}
			Logger.Log.Debug ("Found {0} backends in {1}", count, assembly.Location);
		}

		////////////////////////////////////////////////////////

		// Scans PathFinder.SystemIndexesDir after available 
		// system-wide indexes.
		static void LoadSystemIndexes () 
		{
			if (!Directory.Exists (PathFinder.SystemIndexesDir))
				return;
			
			Logger.Log.Info ("Loading system static indexes.");

			int count = 0;

			Queryable queryable;
			foreach (DirectoryInfo index_dir in new DirectoryInfo (PathFinder.SystemIndexesDir).GetDirectories ()) {
				if (! UseQueryable (index_dir.Name))
					continue;
				
				queryable = StaticQueryable.LoadStaticQueryable (index_dir, QueryDomain.System);
				if (queryable != null) {
					iqueryable_to_queryable [queryable.IQueryable] = queryable;
					count++;
				}
			}

			Logger.Log.Info ("Found {0} system-wide indexes.", count);
		}

		// Scans configuration for user-specified index paths 
		// to load StaticQueryables from.
		static void LoadStaticQueryables () 
		{
			int count = 0;

			if (UseQueryable ("static")) {
				Logger.Log.Info ("Loading user-configured static indexes.");
				List<string[]> values = Conf.Daemon.GetListOptionValues (Conf.Names.StaticQueryables);
				if (values != null) {
					foreach (string[] path in values)
						static_queryables.Add (path [0]);
				}
			}

			Queryable queryable;
			foreach (string path in static_queryables) {
				DirectoryInfo index_dir = new DirectoryInfo (StringFu.SanitizePath (path));

				if (!index_dir.Exists)
					continue;
				
				// FIXME: QueryDomain might be other than local
				queryable = StaticQueryable.LoadStaticQueryable (index_dir, QueryDomain.Local);
				if (queryable != null) {
					iqueryable_to_queryable [queryable.IQueryable] = queryable;
					count++;
				}
			}

			Logger.Log.Info ("Found {0} user-configured static indexes..", count);

			static_queryables = null;
		}

		////////////////////////////////////////////////////////

		private static ArrayList assemblies = null;

		// Perform expensive initialization steps all at once.
		// Should be done before SignalHandler comes into play.
		static public void Init ()
		{
			ReadBackendsFromConf ();
			assemblies = ReflectionFu.ScanEnvironmentForAssemblies ("BEAGLE_BACKEND_PATH", PathFinder.BackendDir);
		}

		////////////////////////////////////////////////////////

		private static bool queryables_started = false;

		static public void Start ()
		{
			// Only add the executing assembly if we haven't already loaded it.
			if (assemblies.IndexOf (Assembly.GetExecutingAssembly ()) == -1)
				assemblies.Add (Assembly.GetExecutingAssembly ());

			foreach (Assembly assembly in assemblies) {
				ScanAssemblyForQueryables (assembly);

				// This allows backends to define their
				// own executors.
				Server.ScanAssemblyForExecutors (assembly);
			}
			
			assemblies = null;

			PropertyKeywordFu.ReadKeywordMappings ();

			LoadSystemIndexes ();
			LoadStaticQueryables ();

			if (indexing_delay < 0)
				return;

			if (indexing_delay == 0 || Environment.GetEnvironmentVariable ("BEAGLE_EXERCISE_THE_DOG") != null)
				StartQueryables ();
			else {
				Logger.Log.Debug ("Waiting {0} seconds before starting queryables", indexing_delay);
				GLib.Timeout.Add ((uint) indexing_delay * 1000, new GLib.TimeoutHandler (StartQueryables));
			}
		}

		static private bool StartQueryables ()
		{
			Logger.Log.Debug ("Starting queryables");

			ArrayList started_queryables = new ArrayList ();
			ArrayList delayed_queryables = new ArrayList ();

			ICollection queryables_to_start = Queryables;
			int last_count;

			do {
				foreach (Queryable q in queryables_to_start) {
					bool ready_to_start = true;

					if (q.DependsOn != null && started_queryables.IndexOf (q.DependsOn) == -1) {
						Log.Info ("Delaying backend '{0}' until after backend '{1}'",
							  q.Name, q.DependsOn);
						ready_to_start = false;
						delayed_queryables.Add (q);
					}

					if (! ready_to_start)
						continue;

					Logger.Log.Info ("Starting backend: '{0}'", q.Name);
					q.Start ();

					started_queryables.Add (q.Name);
				}

				last_count = queryables_to_start.Count;

				queryables_to_start = delayed_queryables;
				delayed_queryables = new ArrayList ();
			} while (queryables_to_start.Count > 0 && queryables_to_start.Count != last_count);

			if (queryables_to_start.Count > 0) {
				Log.Info ("Unable to start backends due to missing or circular dependencies:");

				foreach (Queryable q in queryables_to_start) {
					Log.Info ("  - {0}", q.Name);
					iqueryable_to_queryable.Remove (q.IQueryable);
				}
			}

			queryables_started = true;

			return false;
		}

		////////////////////////////////////////////////////////

		public static string ListBackends ()
		{
			ArrayList assemblies = ReflectionFu.ScanEnvironmentForAssemblies ("BEAGLE_BACKEND_PATH", PathFinder.BackendDir);

			// Only add the executing assembly if we haven't already loaded it.
			if (assemblies.IndexOf (Assembly.GetExecutingAssembly ()) == -1)
				assemblies.Add (Assembly.GetExecutingAssembly ());

			string ret = "User:\n";

			foreach (Assembly assembly in assemblies) {
				foreach (Type type in ReflectionFu.GetTypesFromAssemblyAttribute (assembly, typeof (IQueryableTypesAttribute))) {
					foreach (QueryableFlavor flavor in ReflectionFu.ScanTypeForAttribute (type, typeof (QueryableFlavor))) {
						ret += String.Format (" - {0}", flavor.Name);
						if (flavor.DependsOn != null)
							ret += String.Format (" (depends on {0})", flavor.DependsOn);

						ret += "\n";
					}
				}
			}
			
			if (!Directory.Exists (PathFinder.SystemIndexesDir)) 
				return ret;
			
			ret += "System:\n";
			foreach (DirectoryInfo index_dir in new DirectoryInfo (PathFinder.SystemIndexesDir).GetDirectories ()) {
				ret += String.Format (" - {0}\n", index_dir.Name);
			}

			return ret;
		}

		static public Queryable GetQueryable (string name)
		{
			foreach (Queryable q in Queryables) {
				if (q.Name == name)
					return q;
			}

			return null;
		}

		static public Queryable GetQueryable (IQueryable iqueryable)
		{
			return (Queryable) iqueryable_to_queryable [iqueryable];
		}

		////////////////////////////////////////////////////////

		public delegate void ChangedHandler (Queryable            queryable,
						     IQueryableChangeData changeData);

		static public event ChangedHandler ChangedEvent;

		// A method to fire the ChangedEvent event.
		static public void QueryableChanged (IQueryable           iqueryable,
						     IQueryableChangeData change_data)
		{
			if (ChangedEvent != null) {
				Queryable queryable = iqueryable_to_queryable [iqueryable] as Queryable;
				ChangedEvent (queryable, change_data);
			}
		}

		////////////////////////////////////////////////////////

		private class QueryClosure : IQueryWorker {

			Queryable queryable;
			Query query;
			IQueryResult result;
			IQueryableChangeData change_data;
			
			public QueryClosure (Queryable            queryable,
					     Query                query,
					     QueryResult          result,
					     IQueryableChangeData change_data)
			{
				this.queryable = queryable;
				this.query = query;
				this.result = result;
				this.change_data = change_data;
			}

			public void DoWork ()
			{
				queryable.DoQuery (query, result, change_data);
			}
		}

		static public void DoOneQuery (Queryable            queryable,
					       Query                query,
					       QueryResult          result,
					       IQueryableChangeData change_data)
		{
			try {
				if (queryable.AcceptQuery (query)) {
					QueryClosure qc = new QueryClosure (queryable, query, result, change_data);
					result.AttachWorker (qc);
				}
			} catch (Exception ex) {
				Logger.Log.Warn (ex, "Caught exception calling DoOneQuery on '{0}'", queryable.Name);
			}
		}

		static void AddSearchTermInfo (QueryPart          part,
					       SearchTermResponse response, StringBuilder sb)
		{
			if (part.Logic == QueryPartLogic.Prohibited)
				return;

			if (part is QueryPart_Or) {
				ICollection sub_parts;
				sub_parts = ((QueryPart_Or) part).SubParts;
				foreach (QueryPart qp in sub_parts)
					AddSearchTermInfo (qp, response, sb);
				return;
			}

			if (! (part is QueryPart_Text))
				return;

			QueryPart_Text tp;
			tp = (QueryPart_Text) part;

			string [] split;
			split = tp.Text.Split (' ');
 
			// First, remove stop words
			for (int i = 0; i < split.Length; ++i)
				if (LuceneCommon.IsStopWord (split [i]))
					split [i] = null;

			// Assemble the phrase minus stop words
			sb.Length = 0;
			for (int i = 0; i < split.Length; ++i) {
				if (split [i] == null)
					continue;
				if (sb.Length > 0)
					sb.Append (' ');
				sb.Append (split [i]);
			}
			response.ExactText.Add (sb.ToString ());

			// Now assemble a stemmed version
			sb.Length = 0; // clear the previous value
			for (int i = 0; i < split.Length; ++i) {
				if (split [i] == null)
					continue;
				if (sb.Length > 0)
					sb.Append (' ');
				sb.Append (LuceneCommon.Stem (split [i].ToLower ()));
			}
			response.StemmedText.Add (sb.ToString ());
		}

		////////////////////////////////////////////////////////

		static private void DehumanizeQuery (Query query)
		{
			// We need to remap any QueryPart_Human parts into
			// lower-level part types.  First, we find any
			// QueryPart_Human parts and explode them into
			// lower-level types.
			ArrayList new_parts = null;
			foreach (QueryPart abstract_part in query.Parts) {
				if (abstract_part is QueryPart_Human) {
					QueryPart_Human human = abstract_part as QueryPart_Human;
					if (new_parts == null)
						new_parts = new ArrayList ();
					foreach (QueryPart sub_part in QueryStringParser.Parse (human.QueryString))
						new_parts.Add (sub_part);
				}
			}

			// If we found any QueryPart_Human parts, copy the
			// non-Human parts over and then replace the parts in
			// the query.
			if (new_parts != null) {
				foreach (QueryPart abstract_part in query.Parts) {
					if (! (abstract_part is QueryPart_Human))
						new_parts.Add (abstract_part);
				}
				
				query.ClearParts ();
				foreach (QueryPart part in new_parts)
					query.AddPart (part);
			}

		}

		static private SearchTermResponse AssembleSearchTermResponse (Query query)
		{
			StringBuilder sb = new StringBuilder ();
			SearchTermResponse search_term_response;
			search_term_response = new SearchTermResponse ();
			foreach (QueryPart part in query.Parts)
				AddSearchTermInfo (part, search_term_response, sb);
			return search_term_response;
		}

		static private void QueryEachQueryable (Query query, QueryResult result)
		{
			// The extra pair of calls to WorkerStart/WorkerFinished ensures:
			// (1) that the QueryResult will fire the StartedEvent
			// and FinishedEvent, even if no queryable accepts the
			// query.
			// (2) that the FinishedEvent will only get called when all of the
			// backends have had time to finish.

			object dummy_worker = new object ();

			if (! result.WorkerStart (dummy_worker))
				return;

			try {
				iqueryable_lock.AcquireReaderLock (System.Threading.Timeout.Infinite);

				foreach (Queryable queryable in Queryables)
					DoOneQuery (queryable, query, result, null);
			} finally {
				iqueryable_lock.ReleaseReaderLock ();
			}
			
			result.WorkerFinished (dummy_worker);
		}

		static public void DoQueryLocal (Query       query,
						 QueryResult result)
		{
			DehumanizeQuery (query);

			SearchTermResponse search_term_response;
			search_term_response = AssembleSearchTermResponse (query);
			query.ProcessSearchTermResponse (search_term_response);

			QueryEachQueryable (query, result);
		}

		static public void DoQuery (Query                                query,
					    QueryResult                          result,
					    RequestMessageExecutor.AsyncResponse send_response)
		{
			DehumanizeQuery (query);

			SearchTermResponse search_term_response;
			search_term_response = AssembleSearchTermResponse (query);
			send_response (search_term_response);

			QueryEachQueryable (query, result);
		}


		////////////////////////////////////////////////////////

		static public int DoCountMatchQuery (CountMatchQuery query)
		{
			DehumanizeQuery (query);

			int num_matches = 0;

			try {
				iqueryable_lock.AcquireReaderLock (System.Threading.Timeout.Infinite);

				foreach (Queryable q in Queryables) {
					if (! q.AcceptQuery (query))
						continue;

					num_matches += q.DoCountMatchQuery (query);
				}
			} finally {
				iqueryable_lock.ReleaseReaderLock ();
			}

			return num_matches;
		}

		////////////////////////////////////////////////////////

		static public IEnumerable GetIndexInformation ()
		{
			try {
				iqueryable_lock.AcquireReaderLock (System.Threading.Timeout.Infinite);

				foreach (Queryable q in Queryables)
					yield return q.GetQueryableStatus ();
			} finally {
				iqueryable_lock.ReleaseReaderLock ();
			}
		}

		////////////////////////////////////////////////////////

		static public bool IsIndexing {
			get {
				// If the backends haven't been started yet,
				// there is at least the initial setup.  Just
				// assume all the backends are indexing.
				if (! queryables_started)
					return true;

				try {
					iqueryable_lock.AcquireReaderLock (System.Threading.Timeout.Infinite);

					foreach (Queryable q in Queryables) {
						QueryableStatus status = q.GetQueryableStatus ();

						if (status == null)
							return false;

						if (status.IsIndexing)
							return true;
					}

					return false;
				} finally {
					iqueryable_lock.ReleaseReaderLock ();
				}
			}
		}

		/////////////////////////////////////////////////////////

		// Removable Index related stuff

		private static Dictionary<string, Queryable> removable_queryables = new Dictionary<string, Queryable> (4); // small number

		internal static ResponseMessage HandleRemovableIndexRequest (bool to_mount, string index_dir, string mnt_dir)
		{
			lock (removable_queryables) {
				if (to_mount)
					return AddRemovableIndex (index_dir, mnt_dir);
				else
					return RemoveRemovableIndex (index_dir, mnt_dir);
			}
		}

		private static ResponseMessage AddRemovableIndex (string path, string mnt_dir)
		{
			DirectoryInfo index_dir = new DirectoryInfo (StringFu.SanitizePath (path));
			if (! index_dir.Exists) {
				ErrorResponse msg;
				msg = new ErrorResponse ();
				msg.ErrorMessage = "Adding removable index failed";
				msg.Details = String.Format ("'{0}' does not exist.", path);
				return msg;
			}

			// Allow late loading of mount dir ?
			mnt_dir = StringFu.SanitizePath (mnt_dir);
			if (! Directory.Exists (mnt_dir)) {
				ErrorResponse msg;
				msg = new ErrorResponse ();
				msg.ErrorMessage = "Adding removable index failed";
				msg.Details = String.Format ("Mount directory '{0}' does not exist.", mnt_dir);
				return msg;
			}

			if (removable_queryables.ContainsKey (path)) {
				ErrorResponse msg;
				msg = new ErrorResponse ();
				msg.ErrorMessage = "Adding removable index failed";
				msg.Details = String.Format ("'{0}' already added.", path);
				return msg;
			}

			Queryable removable_queryable = null;

			try {
				iqueryable_lock.AcquireWriterLock (System.Threading.Timeout.Infinite);
				removable_queryable = StaticQueryable.LoadRemovableQueryable (index_dir, mnt_dir);
			} finally {
				iqueryable_lock.ReleaseWriterLock ();
			}

			if (removable_queryable == null)
				return new ErrorResponse ("Adding removable index failed");

			iqueryable_to_queryable [removable_queryable.IQueryable] = removable_queryable;
			removable_queryables [path] = removable_queryable;

			RemovableIndexResponse resp = new RemovableIndexResponse ();
			resp.Source = removable_queryable.Name;
			Log.Info ("Adding removable index '{0}' from {1}", resp.Source, path);
			return resp;
		}

		private static ResponseMessage RemoveRemovableIndex (string path, string mnt_dir)
		{
			if (! removable_queryables.ContainsKey (path)) {
				ErrorResponse msg;
				msg = new ErrorResponse ();
				msg.ErrorMessage = "Removing removable-index failed";
				msg.Details = String.Format ("'{0}' was not added.", path);
				return msg;
			}

			Queryable removable_queryable = removable_queryables [path];

			// Assert
			if (removable_queryable == null ||
			    ! (removable_queryable.IQueryable is StaticQueryable)) {
				ErrorResponse msg = new ErrorResponse ("Removing removable-index failed");
				return msg;
			}

			StaticQueryable static_queryable = (StaticQueryable) removable_queryable.IQueryable;
			if (static_queryable.RemovableMountDir != mnt_dir) {
				ErrorResponse msg;
				msg = new ErrorResponse ();
				msg.ErrorMessage = "Removing removable-index failed";
				msg.Details = String.Format ("No index mounted at {0}.", mnt_dir);
				return msg;
			}

			static_queryable.Close ();

			removable_queryables.Remove (path);

			try {
				iqueryable_lock.AcquireWriterLock (System.Threading.Timeout.Infinite);
				iqueryable_to_queryable [removable_queryable.IQueryable] = null;
				iqueryable_to_queryable.Remove (removable_queryable.IQueryable);
			} finally {
				iqueryable_lock.ReleaseWriterLock ();
			}

			RemovableIndexResponse resp = new RemovableIndexResponse ();
			resp.Source = removable_queryable.Name;
			Log.Info ("Removed removable-index '{0}' at {1}", removable_queryable.Name, path);
			return resp;
		}

		/////////////////////////////////////////////////////////

		// Various debug hooks; especially persistant state information and memory issues

		public static void DebugHook ()
		{
			Log.Debug ("Debughook called:");
			LuceneQueryable l;

			foreach (Queryable q in Queryables) {
				l = q.IQueryable as LuceneQueryable;
				if (l == null)
					continue;
				try {
					l.DebugHook ();
				} catch (Exception e) {
					Log.Debug (e, "Exception during Debughook for {0}", q.Name);
				}
			}
		}
	}
}
