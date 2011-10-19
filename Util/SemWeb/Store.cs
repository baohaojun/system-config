using System;
#if !DOTNET2
using System.Collections;
#else
using System.Collections.Generic;
#endif

using SemWeb.Inference;
using SemWeb.Util;

#if !DOTNET2
using SourceList = System.Collections.ArrayList;
using NamedSourceMap = System.Collections.Hashtable;
using ReasonerList = System.Collections.ArrayList;
#else
using SourceList = System.Collections.Generic.List<SemWeb.SelectableSource>;
using NamedSourceMap = System.Collections.Generic.Dictionary<string, SemWeb.SelectableSource>;
using ReasonerList = System.Collections.Generic.List<SemWeb.Inference.Reasoner>;
#endif

namespace SemWeb {
	
	public class Store : StatementSource, StatementSink,
		SelectableSource, QueryableSource, StaticSource, ModifiableSource,
		IDisposable {
		
		// Static helper methods for creating data sources and sinks
		// from spec strings.
		
		public static Store Create(string spec) {
			Store ret = new Store();
			
			bool rdfs = false, euler = false;
			
			if (spec.StartsWith("rdfs+")) {
				rdfs = true;
				spec = spec.Substring(5);
			}
			if (spec.StartsWith("euler+")) {
				euler = true;
				spec = spec.Substring(6);
			}
				
			foreach (string spec2 in spec.Split('\n', '|')) {
				StatementSource s = CreateForInput(spec2.Trim());
				if (s is SelectableSource)
					ret.AddSource((SelectableSource)s);
				else
					ret.AddSource(new MemoryStore(s));
			}
			
			if (rdfs)
				ret.AddReasoner(new RDFS(ret));
#if ENABLE_RDF_ADAPTER
			if (euler)
				ret.AddReasoner(new Euler(ret)); // loads it all into memory!
#endif
			
			return ret;
		}
		
		public static StatementSource CreateForInput(string spec) {
			if (spec.StartsWith("debug+")) {
				StatementSource s = CreateForInput(spec.Substring(6));
				if (!(s is SelectableSource)) s = new MemoryStore(s);
				return new SemWeb.Stores.DebuggedSource((SelectableSource)s, System.Console.Error);
			}
			return (StatementSource)Create(spec, false);
		}		
		
		public static StatementSink CreateForOutput(string spec) {
			return (StatementSink)Create(spec, true);
		}
		
		private static object Create(string spec, bool output) {
			string type = spec;
			
			int c = spec.IndexOf(':');
			if (c != -1) {
				type = spec.Substring(0, c);
				spec = spec.Substring(c+1);
			} else {
				spec = "";
			}
			
			Type ttype;
			
			switch (type) {
				case "mem":
					return new MemoryStore();
				case "xml":
					if (spec == "") throw new ArgumentException("Use: xml:filename");
					if (output) {
						#if !SILVERLIGHT
							return new RdfXmlWriter(spec);
						#else
							throw new NotSupportedException("RDF/XML output is not supported in the Silverlight build of the SemWeb library.");
						#endif
					} else {
						return new RdfXmlReader(spec);
					}
				case "n3":
				case "ntriples":
				case "nt":
				case "turtle":
					if (spec == "") throw new ArgumentException("Use: format:filename");
					if (output) {
						N3Writer ret = new N3Writer(spec); // turtle is default format
						switch (type) {
							case "nt": case "ntriples":
								ret.Format = N3Writer.Formats.NTriples;
								break;
						}
						return ret;
					} else {
						return new N3Reader(spec);
					}
				
				case "null":
					if (!output) throw new ArgumentException("The null sink does not support reading.");
					return new StatementCounterSink();
				
				/*case "file":
					if (spec == "") throw new ArgumentException("Use: format:filename");
					if (output) throw new ArgumentException("The FileStore does not support writing.");
					return new SemWeb.Stores.FileStore(spec);*/

				case "sqlite":
				case "mysql":
				case "postgresql":
				case "sqlserver":
					if (spec == "") throw new ArgumentException("Use: sqlite|mysql|postgresql|sqlserver:table:connection-string");
				
					c = spec.IndexOf(':');
					if (c == -1) throw new ArgumentException("Invalid format for SQL spec parameter (table:constring).");
					string table = spec.Substring(0, c);
					spec = spec.Substring(c+1);
					
					string classtype = null;
					if (type == "sqlite") {
						classtype = "SemWeb.Stores.SqliteStore, SemWeb.SqliteStore";
						spec = spec.Replace(";", ",");
					} else if (type == "mysql") {
						classtype = "SemWeb.Stores.MySQLStore, SemWeb.MySQLStore";
					} else if (type == "postgresql") {
						classtype = "SemWeb.Stores.PostgreSQLStore, SemWeb.PostgreSQLStore";
					} else if( type == "sqlserver" ) {
						classtype = "SemWeb.Stores.SQLServerStore, SemWeb.SQLServerStore";
					}
					ttype = Type.GetType(classtype);
					if (ttype == null)
						throw new NotSupportedException("The storage type in <" + classtype + "> could not be found.");
					return Activator.CreateInstance(ttype, new object[] { spec, table });
				/*case "bdb":
					return new SemWeb.Stores.BDBStore(spec);*/
				case "sparql-http":
					#if false
					return new SemWeb.Remote.SparqlHttpSource(spec);
					#else
					throw new NotSupportedException("The SparqlHttpSource class is not available in the Silverlight build of SemWeb.");
					#endif
				case "class":
					ttype = Type.GetType(spec);
					if (ttype == null)
						throw new NotSupportedException("The class <" + spec + "> could not be found.");
					return Activator.CreateInstance(ttype);
				default:
					throw new ArgumentException("Unknown parser type: " + type);
			}
		}
		
		// START OF ACTUAL STORE IMPLEMENTATION
		
		readonly Entity rdfType = new Entity("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

		SourceList unnamedgraphs = new SourceList(); // a list of SelectableSources that aren't associated with graph URIs
		NamedSourceMap namedgraphs = new NamedSourceMap(); // a mapping from graph URIs to a selectable source that represents that graph
		
		SourceList allsources = new SourceList(); // a list of the sources in unnamed graphs and namedgraphs
		
		ReasonerList reasoners = new ReasonerList(); // a list of reasoning engines applied to this data model, which are run in order
		
		// GENERAL METHODS
		
		public Store() {
		}
		
		public Store(StatementSource source) {
			AddSource(new MemoryStore.StoreImpl(source));
		}
		
		public Store(SelectableSource source) {
			AddSource(source);
		}
		
		#if !DOTNET2
		public IList DataSources {
			get {
				return ArrayList.ReadOnly(allsources);
			}
		}
		#else
		public IList<SelectableSource> DataSources {
			get {
				return new List<SelectableSource>(allsources);
			}
		}
		#endif
				
		public virtual void AddSource(SelectableSource source) {
			if (source is MemoryStore) source = ((MemoryStore)source).impl;
			unnamedgraphs.Add(source);
			allsources.Add(source);
		}
		
		public virtual void AddSource(SelectableSource source, string uri) {
			if (namedgraphs.ContainsKey(uri))
				throw new ArgumentException("URI has already been associated with a data source.");
			if (source is MemoryStore) source = ((MemoryStore)source).impl;
			namedgraphs[uri] = source;
			allsources.Add(source);
		}
		
		internal void AddSource2(SelectableSource store) {
			// Used by MemoryStore only!
			unnamedgraphs.Add(store);
			allsources.Add(store);
		}
		
		public virtual void AddReasoner(Reasoner reasoner) {
			reasoners.Add(reasoner);
		}
		
		public void Write(System.IO.TextWriter writer) {
			using (RdfWriter w = new N3Writer(writer)) {
				Select(w);
			}
		}

		// INTERFACE IMPLEMENTATIONS and related methods
		
		// IDisposable
		
		public void Dispose() {
			foreach (SelectableSource s in allsources)
				if (s is IDisposable)
					((IDisposable)s).Dispose();
			foreach (Reasoner r in reasoners)
				if (r is IDisposable)
					((IDisposable)r).Dispose();
		}
		
		// StatementSource
		
		public bool Distinct {
			get {
				if (allsources.Count > 1) return false;
				foreach (Reasoner r in reasoners)
					if (!r.Distinct)
						return false;
				if (allsources.Count == 0) return true;
				return ((SelectableSource)allsources[0]).Distinct;
			}
		}
		
		public void Select(StatementSink result) {
			Select(Statement.All, result);
		}

		// SelectableSource
		
		private SelectableSource[] GetSources(ref Entity graph) {
			if (graph == null || namedgraphs.Count == 0)
				#if !DOTNET2
					return (SelectableSource[])allsources.ToArray(typeof(SelectableSource));
				#else
					return allsources.ToArray();
				#endif
			else if (graph == Statement.DefaultMeta)
				#if !DOTNET2
					return (SelectableSource[])unnamedgraphs.ToArray(typeof(SelectableSource));
				#else
					return unnamedgraphs.ToArray();
				#endif
			else if (graph.Uri != null && namedgraphs.ContainsKey(graph.Uri)) {
				graph = Statement.DefaultMeta;
				return new SelectableSource[] { (SelectableSource)namedgraphs[graph.Uri] };
			} else
				return null;
		}

		public bool Contains(Resource resource) {
			foreach (SelectableSource s in allsources)
				if (s.Contains(resource))
					return true;
			return false;
			/*return (resource is Entity && Contains(new Statement((Entity)resource, null, null, null)))
				|| (resource is Entity && Contains(new Statement(null, (Entity)resource, null, null)))
				|| (                      Contains(new Statement(null, null, resource, null)))
				|| (resource is Entity && Contains(new Statement(null, null, null, (Entity)resource)));*/
		}
		
		public bool Contains(Statement template) {
			// If reasoning is applied, use DefaultContains so that
			// we use a Select call which will delegate the query
			// to the reasoner.
			ReasoningHelper rh = GetReasoningHelper(null);
			if (rh != null)
				return DefaultContains(this, template);
			
			SelectableSource[] sources = GetSources(ref template.Meta);
			if (sources == null) return false;
			foreach (SelectableSource s in sources)
				if (s.Contains(template))
					return true;
			return false;
		}
			
		public static bool DefaultContains(SelectableSource source, Statement template) {
			StatementExistsSink sink = new StatementExistsSink();
			SelectFilter filter = new SelectFilter(template);
			filter.Limit = 1;
			source.Select(filter, sink);
			return sink.Exists;
		}
		
		private class ReasoningHelper {
			public Reasoner reasoner;
			public Store nextStore;
		}
		
		private ReasoningHelper GetReasoningHelper(SelectableSource[] sources) {
			if (reasoners.Count == 0)
				return null;
		
			ReasoningHelper ret = new ReasoningHelper();
			
			ret.reasoner = (Reasoner)reasoners[reasoners.Count-1];
			
			ret.nextStore = new Store();
			if (sources == null) {
				ret.nextStore.unnamedgraphs = unnamedgraphs; // careful...
				ret.nextStore.namedgraphs = namedgraphs;
				ret.nextStore.allsources = allsources;
			} else {
				ret.nextStore.unnamedgraphs.AddRange(sources);
				ret.nextStore.allsources.AddRange(sources);
			}
			for (int i = 0; i < reasoners.Count-1; i++)
				ret.nextStore.reasoners.Add(reasoners[i]);
			return ret;
		}
		
		public void Select(Statement template, StatementSink result) {
			// If reasoning is applied, delegate this call to the last reasoner
			// and pass it a clone of this store but with itself removed.
			ReasoningHelper rh = GetReasoningHelper(null);
			if (rh != null) {
				rh.reasoner.Select(template, rh.nextStore, result);
				return;
			}
		
			SelectableSource[] sources = GetSources(ref template.Meta);
			if (sources == null) return;
			foreach (SelectableSource s in sources)
				s.Select(template, result);
		}
		
		public void Select(SelectFilter filter, StatementSink result) {
			Entity[] scanMetas = filter.Metas;
			if (scanMetas == null || namedgraphs.Count == 0) scanMetas = new Entity[] { null };
			foreach (Entity meta in scanMetas) {
				Entity meta2 = meta;
				SelectableSource[] sources = GetSources(ref meta2);
				if (sources == null) continue;
				
				if (meta2 == null)
					filter.Metas = null;
				else
					filter.Metas = new Entity[] { meta2 };

				// If reasoning is applied, delegate this call to the last reasoner
				// and pass it either:
				// 		a clone of this store but with itself removed, if the meta we are processing now is null, or
				//		that, but only with the sources that apply to this meta
				ReasoningHelper rh = GetReasoningHelper(sources);
				if (rh != null) {
					rh.reasoner.Select(filter, rh.nextStore, result);
					continue;
				}
				
				foreach (SelectableSource s in sources)
					s.Select(filter, result);
			}
		}
		
		public static void DefaultSelect(SelectableSource source, SelectFilter filter, StatementSink sink) {
			// This method should really be avoided...
			if (filter.LiteralFilters != null)
				sink = new SemWeb.Filters.FilterSink(filter.LiteralFilters, sink, source);
			foreach (Entity subject in filter.Subjects == null ? new Entity[] { null } : filter.Subjects)
			foreach (Entity predicate in filter.Predicates == null ? new Entity[] { null } : filter.Predicates)
			foreach (Resource objct in filter.Objects == null ? new Resource[] { null } : filter.Objects)
			foreach (Entity meta in filter.Metas == null ? new Entity[] { null } : filter.Metas)
				source.Select(new Statement(subject, predicate, objct, meta), sink);
		}		
		
		public SelectResult Select(Statement template) {
			return new SelectResult.Single(this, template);
		}
		
		public SelectResult Select(SelectFilter filter) {
			return new SelectResult.Multi(this, filter);
		}
		
		public Resource[] SelectObjects(Entity subject, Entity predicate) {
			if (predicate.Uri != null && predicate.Uri == NS.RDFS + "member") {
				ResourceCollector2 collector = new ResourceCollector2();
				Select(new Statement(subject, predicate, null, null), collector);
				return collector.GetItems();
			} else {
				ResSet resources = new ResSet();
				ResourceCollector collector = new ResourceCollector();
				collector.SPO = 2;
				collector.Table = resources;
				Select(new Statement(subject, predicate, null, null), collector);
				return resources.ToArray();
			}
		}
		public Entity[] SelectSubjects(Entity predicate, Resource @object) {
			ResSet resources = new ResSet();
			ResourceCollector collector = new ResourceCollector();
			collector.SPO = 0;
			collector.Table = resources;
			Select(new Statement(null, predicate, @object, null), collector);
			return resources.ToEntityArray();
		}
		class ResourceCollector : StatementSink {
			public ResSet Table;
			public int SPO;
			public bool Add(Statement s) {
				if (SPO == 0) Table.Add(s.Subject);
				if (SPO == 2) Table.Add(s.Object);
				return true;
			}
		}
		class ResourceCollector2 : StatementSink {
			System.Collections.ArrayList items = new System.Collections.ArrayList();
			ResSet other = new ResSet();
			public bool Add(Statement s) {
				if (s.Predicate.Uri == null || !s.Predicate.Uri.StartsWith(NS.RDF + "_")) {
					other.Add(s.Object);
				} else {
					string num = s.Predicate.Uri.Substring(NS.RDF.Length+1);
					try {
						int idx = int.Parse(num);
						items.Add(new Item(s.Object, idx));
					} catch {
						other.Add(s.Object);
					}
				}
				return true;
			}
			public Resource[] GetItems() {
				items.Sort();
				Resource[] ret = new Resource[items.Count + other.Count];
				int ctr = 0;
				foreach (Item item in items)
					ret[ctr++] = item.r;
				foreach (Resource item in other)
					ret[ctr++] = item;
				return ret;
			}
			class Item : IComparable {
				public Resource r;
				int idx;
				public Item(Resource r, int idx) { this.r = r; this.idx = idx; }
				public int CompareTo(object other) {
					return idx.CompareTo(((Item)other).idx);
				}
			}
		}
		
		// QueryableSource
		
		public SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options) {
			// If reasoning is applied, delegate this call to the last reasoner
			// and pass it a clone of this store but with itself removed.
			ReasoningHelper rh = GetReasoningHelper(null);
			if (rh != null)
				return rh.reasoner.MetaQuery(graph, options, rh.nextStore);
			
			// Special case for one wrapped data source that supports QueryableSource:
			if (allsources.Count == 1 && allsources[0] is QueryableSource)
				return ((QueryableSource)allsources[0]).MetaQuery(graph, options);
		
			return new SemWeb.Inference.SimpleEntailment().MetaQuery(graph, options, this);
		}

		public void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SemWeb.Query.QueryResultSink sink) {
			// If reasoning is applied, delegate this call to the last reasoner
			// and pass it a clone of this store but with that reasoner removed.
			ReasoningHelper rh = GetReasoningHelper(null);
			if (rh != null) {
				rh.reasoner.Query(graph, options, rh.nextStore, sink);
				return;
			}

			// Special case for one wrapped data source that supports QueryableSource:
			if (allsources.Count == 1 && allsources[0] is QueryableSource) {
				((QueryableSource)allsources[0]).Query(graph, options, sink);
				return;
			}

			// Chunk the query graph as best we can.
			SemWeb.Query.GraphMatch.QueryPart[] chunks = ChunkQuery(graph, options, sink);
			
			// If we couldn't chunk the graph, then just use the default GraphMatch implementation.
			if (chunks == null) {
				new SemWeb.Inference.SimpleEntailment().Query(graph, options, this, sink);
				return;
			}
			
			SemWeb.Query.GraphMatch.RunGeneralQuery(chunks, options.VariableKnownValues, options.VariableLiteralFilters, options.DistinguishedVariables,
				0, options.Limit, true, sink);
		}
		
		private SemWeb.Query.GraphMatch.QueryPart[] ChunkQuery(Statement[] query, SemWeb.Query.QueryOptions options, SemWeb.Query.QueryResultSink sink) {
			// MetaQuery the data sources to get their capabilities.
			SemWeb.Query.MetaQueryResult[] mq = new SemWeb.Query.MetaQueryResult[allsources.Count];
			for (int i = 0; i < allsources.Count; i++) {
				if (!(allsources[i] is QueryableSource))
					return null;
				mq[i] = ((QueryableSource)allsources[i]).MetaQuery(query, options);
				if (!mq[i].QuerySupported)
					return null;
			}
			
			// Establish which statements can be answered definitively by which data sources.
			bool[,] definitive = new bool[query.Length, allsources.Count];
			for (int j = 0; j < query.Length; j++) {
				// Find a definitive source for this statement
				for (int i = 0; i < mq.Length; i++) {
					if (mq[i].IsDefinitive != null && mq[i].IsDefinitive[j]) {
						definitive[j,i] = true;
						sink.AddComments("Data source '" + allsources[i] + "' definitively answers:  " + query[j]);
					}
				}
				
				// See if only one source can answer this statement.
				System.Collections.ArrayList answerables = new System.Collections.ArrayList();
				for (int i = 0; i < mq.Length; i++) {
					if (mq[i].NoData != null && mq[i].NoData[j]) continue;
					answerables.Add(i);
				}
				if (answerables.Count == 0) {
					sink.AddComments("No data source could answer a part of the query: " + query[j]);
					return null;
				}
				
				if (answerables.Count == 1) {
					//sink.AddComments("Only '" + allsources[(int)answerables[0]] + "' could answer:  " + query[j]);
					definitive[j,(int)answerables[0]] = true;
				}
			}
			
			// Create a table that indicates preferred grouping: two statements that can be
			// definitively answered by the same data source prefer to be grouped together.
			bool[,] group = new bool[query.Length, query.Length];
			for (int i = 0; i < query.Length; i++) {
				for (int j = 0; j < query.Length; j++) {
					if (i == j) continue;
					
					for (int k = 0; k < mq.Length; k++) {
						if (definitive[i,k] && definitive[j,k]) {
							group[i,j] = true;
							group[j,i] = true;
						}
					}
				}
			}
			
			// Reorder the statements. Then run MetaQuery again because the order of statements changed.
			query = SemWeb.Query.GraphMatch.ReorderQuery(query, SemWeb.Query.GraphMatch.toArray(options.VariableKnownValues), this, group);
			for (int i = 0; i < allsources.Count; i++)
				mq[i] = ((QueryableSource)allsources[i]).MetaQuery(query, options);

			// Chunk the statements.
			
			System.Collections.ArrayList chunks = new System.Collections.ArrayList();
			
			int curSource = -1;
			System.Collections.ArrayList curStatements = new System.Collections.ArrayList();
			
			for (int j = 0; j < query.Length; j++) {
				if (curSource != -1) {
					// If we have a curSource and it definitively answers this
					// statement in the graph, include this statement in the
					// current chunk.
					if (mq[curSource].IsDefinitive != null && mq[curSource].IsDefinitive[j]) {
						curStatements.Add(query[j]);
						continue;
					}
					
					// If we have a curSource and no other source answers this
					// statement, also include this statement in the current chunk.
					bool foundOther = false;
					for (int i = 0; i < mq.Length; i++) {
						if (i == curSource) continue;
						if (mq[i].NoData != null && mq[i].NoData[j]) continue;
						foundOther = true;
						break;
					}
					if (!foundOther) {
						curStatements.Add(query[j]);
						continue;
					}
					
					// Some other source could possibly answer this statement,
					// so we complete the chunk we started.
					SemWeb.Query.GraphMatch.QueryPart c = new SemWeb.Query.GraphMatch.QueryPart(
						(Statement[])curStatements.ToArray(typeof(Statement)),
						(QueryableSource)allsources[curSource]
						);
					chunks.Add(c);
					
					curSource = -1;
					curStatements.Clear();
				}
			
				// Find a definitive source for this statement
				for (int i = 0; i < mq.Length; i++) {
					if (mq[i].IsDefinitive != null && mq[i].IsDefinitive[j]) {
						curSource = i;
						curStatements.Add(query[j]);
						break;
					}
				}
				if (curSource != -1) // found a definitive source
					continue;
					
				// See if only one source can answer this statement.
				// Also build a list of sources that can answer the
				// statement, so don't break out of this loop early.
				System.Collections.ArrayList answerables = new System.Collections.ArrayList();
				int findSource = -1;
				for (int i = 0; i < mq.Length; i++) {
					if (mq[i].NoData != null && mq[i].NoData[j]) continue;
					answerables.Add(allsources[i]);
					if (findSource == -1)
						findSource = i;
					else
						findSource = -2; // found a second source that can answer this
				}
				if (findSource >= 0) {
					curSource = findSource;
					curStatements.Add(query[j]);
					continue;
				}
				if (answerables.Count == 0) {
					return null;
				}
				
				// More than one source can answer this, so make a one-statement chunk.
				SemWeb.Query.GraphMatch.QueryPart cc = new SemWeb.Query.GraphMatch.QueryPart(
					query[j],
					(QueryableSource[])answerables.ToArray(typeof(QueryableSource))
					);
				chunks.Add(cc);
			}

			if (curSource != -1) {
				SemWeb.Query.GraphMatch.QueryPart c = new SemWeb.Query.GraphMatch.QueryPart(
					(Statement[])curStatements.ToArray(typeof(Statement)),
					(QueryableSource)allsources[curSource]
					);
				chunks.Add(c);
			}
			
			return (SemWeb.Query.GraphMatch.QueryPart[])chunks.ToArray(typeof(SemWeb.Query.GraphMatch.QueryPart));
		}

		public
		#if !DOTNET2
		ICollection
		#else
		ICollection<SemWeb.Query.VariableBindings>
		#endif
		Query(Statement[] graph) {
			SemWeb.Query.QueryOptions options = new SemWeb.Query.QueryOptions();
			options.Limit = 1;
			SemWeb.Query.QueryResultBuffer sink = new SemWeb.Query.QueryResultBuffer();
			Query(graph, options, sink);
			return sink.Bindings;
		}
		
		// StaticSource
		
		public int StatementCount {
			get {
				int ret = 0;
				foreach (StatementSource s in allsources) {
					if (s is StaticSource)
						ret += ((StaticSource)s).StatementCount;
					else
						throw new InvalidOperationException("Not all data sources are support StatementCount.");
				}
				return ret;
			}
		}
		
		public Entity[] GetEntities() {
			ResSet h = new ResSet();
			foreach (StatementSource s in allsources) {
				if (s is StaticSource) {
					foreach (Resource r in ((StaticSource)s).GetEntities())
						h.Add(r);
				} else {
					throw new InvalidOperationException("Not all data sources support GetEntities.");
				}
			}
			return h.ToEntityArray();
		}
		
		public Entity[] GetPredicates() {
			ResSet h = new ResSet();
			foreach (StatementSource s in allsources) {
				if (s is StaticSource) {
					foreach (Resource r in ((StaticSource)s).GetPredicates())
						h.Add(r);
				} else {
					throw new InvalidOperationException("Not data sources support GetPredicates.");
				}
			}
			return h.ToEntityArray();
		}

		public Entity[] GetMetas() {
			ResSet h = new ResSet();
			foreach (StatementSource s in allsources) {
				if (s is StaticSource) {
					foreach (Resource r in ((StaticSource)s).GetMetas())
						h.Add(r);
				} else {
					throw new InvalidOperationException("Not all data sources support GetMetas.");
				}
			}
			return h.ToEntityArray();
		}

		public Entity[] GetEntitiesOfType(Entity type) {
			return SelectSubjects(rdfType, type);
		}
		
		public string GetPersistentBNodeId(BNode node) {
			foreach (SelectableSource source in allsources) {
				if (source is StaticSource) {
					string id = ((StaticSource)source).GetPersistentBNodeId(node);
					if (id != null) return id;
				}
			}
			return null;
		}
		
		public BNode GetBNodeFromPersistentId(string persistentId) {
			foreach (SelectableSource source in allsources) {
				if (source is StaticSource) {
					BNode node = ((StaticSource)source).GetBNodeFromPersistentId(persistentId);
					if (node != null) return node;
				}
			}
			return null;
		}
		
		
		// StatementSink

		bool StatementSink.Add(Statement statement) {
			Add(statement);
			return true;
		}
		
		public void Add(Statement statement) {
			if (statement.AnyNull) throw new ArgumentNullException();
			// We don't know where to put it unless we are wrapping just one store.
			SelectableSource[] sources = GetSources(ref statement.Meta);
			if (sources == null || sources.Length != 1) throw new InvalidOperationException("I don't know which data source to put the statement into.");
			if (!(sources[0] is ModifiableSource)) throw new InvalidOperationException("The data source is not modifiable.");
			((ModifiableSource)sources[0]).Add(statement);
		}
		
		// ModifiableSource
		
		public void Clear() {
			if (allsources.Count > 1) throw new InvalidOperationException("The Clear() method is not supported when multiple data sources are added to a Store.");
			if (!(allsources[0] is ModifiableSource)) throw new InvalidOperationException("The data source is not modifiable.");
			((ModifiableSource)allsources[0]).Clear();
		}
		
		ModifiableSource[] GetModifiableSources(ref Entity graph) {
			SelectableSource[] sources = GetSources(ref graph);
			if (sources == null) return null;
			
			// check all are modifiable first
			foreach (SelectableSource source in sources)
				if (!(source is ModifiableSource)) throw new InvalidOperationException("Not all of the data sources are modifiable.");
				
			ModifiableSource[] sources2 = new ModifiableSource[sources.Length];
			sources.CopyTo(sources2, 0);
			return sources2;
		}

		public void Remove(Statement template) {
			ModifiableSource[] sources = GetModifiableSources(ref template.Meta);
			if (sources == null) return;
			
			foreach (ModifiableSource source in sources)
				source.Remove(template);
		}

		public void Import(StatementSource source) {
			// We don't know where to put the data unless we are wrapping just one store.
			if (allsources.Count != 1) throw new InvalidOperationException("I don't know which data source to put the statements into.");
			if (!(allsources[0] is ModifiableSource)) throw new InvalidOperationException("The data source is not modifiable.");
			((ModifiableSource)allsources[0]).Import(source);
		}
		
		public void RemoveAll(Statement[] templates) {
			// Not tested...
			
			System.Collections.ArrayList metas = new System.Collections.ArrayList();
			foreach (Statement t in templates)
				if (!metas.Contains(t.Meta))
					metas.Add(t.Meta);
					
			foreach (Entity meta in metas) {
				Entity meta2 = meta;
				ModifiableSource[] sources = GetModifiableSources(ref meta2);
				if (sources == null) continue;
				
				StatementList templates2 = new StatementList();
				foreach (Statement t in templates) {
					if (t.Meta == meta) {
						Statement t2 = t;
						t2.Meta = meta2;
						templates2.Add(t2);
					}
				}
					
				foreach (ModifiableSource source in sources)
					source.RemoveAll(templates2);
			}
		}
		
		public void Replace(Entity find, Entity replacement) {
			foreach (SelectableSource source in allsources)
				if (!(source is ModifiableSource)) throw new InvalidOperationException("Not all of the data sources are modifiable.");

			foreach (ModifiableSource source in allsources)
				source.Replace(find, replacement);
		}
		
		public void Replace(Statement find, Statement replacement) {
			ModifiableSource[] sources = GetModifiableSources(ref find.Meta);
			if (sources == null) return;
				
			foreach (ModifiableSource source in sources)
				source.Replace(find, replacement);
		}

		public static void DefaultReplace(ModifiableSource source, Entity find, Entity replacement) {
			MemoryStore deletions = new MemoryStore();
			MemoryStore additions = new MemoryStore();
			
			source.Select(new Statement(find, null, null, null), deletions);
			source.Select(new Statement(null, find, null, null), deletions);
			source.Select(new Statement(null, null, find, null), deletions);
			source.Select(new Statement(null, null, null, find), deletions);
			
			foreach (Statement s in deletions) {
				source.Remove(s);
				additions.Add(s.Replace(find, replacement));
			}
			
			foreach (Statement s in additions) {
				source.Add(s);
			}
		}
		
		public static void DefaultReplace(ModifiableSource source, Statement find, Statement replacement) {
			source.Remove(find);
			source.Add(replacement);
		}
		
		
	}

	public abstract class SelectResult : StatementSource, 
#if DOTNET2
	System.Collections.Generic.IEnumerable<Statement>
#else
	IEnumerable
#endif
	{
		internal Store source;
		MemoryStore ms;
		internal SelectResult(Store source) { this.source = source; }
		public bool Distinct { get { return source.Distinct; } }
		public abstract void Select(StatementSink sink);
#if DOTNET2
		System.Collections.Generic.IEnumerator<Statement> System.Collections.Generic.IEnumerable<Statement>.GetEnumerator() {
			return ((System.Collections.Generic.IEnumerable<Statement>)Buffer()).GetEnumerator();
		}
#endif
		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() {
			return ((System.Collections.IEnumerable)Buffer()).GetEnumerator();
		}
		public long StatementCount { get { return Buffer().StatementCount; } }
		public MemoryStore Load() { return Buffer(); }
		public Statement[] ToArray() { return Load().ToArray(); }
		private MemoryStore Buffer() {
			if (ms != null) return ms;
			ms = new MemoryStore();
			ms.allowIndexing = false;
			Select(ms);
			return ms;
		}
		
		internal class Single : SelectResult {
			Statement template;
			public Single(Store source, Statement template) : base(source) {
				this.template = template;
			}
			public override void Select(StatementSink sink) {
				source.Select(template, sink);
			}
		}
		
		internal class Multi : SelectResult {
			SelectFilter filter;
			public Multi(Store source, SelectFilter filter)
				: base(source) {
				this.filter = filter;
			}
			public override void Select(StatementSink sink) {
				source.Select(filter, sink);
			}
		}
	}
}
		



/////// AUXILIARY STORE WRAPPERS /////////
	
namespace SemWeb.Stores {

	#if DOTNET2
	using System.Collections;
	#endif

	public abstract class SimpleSourceWrapper : SelectableSource {
	
		public virtual bool Distinct { get { return true; } }
		
		public virtual void Select(StatementSink sink) {
			// The default implementation does not return
			// anything for this call.
		}
		
		public abstract bool Contains(Resource resource);

		public virtual bool Contains(Statement template) {
			template.Object = null; // reduce to another case (else there would be recursion)
			return Store.DefaultContains(this, template);
		}
		
		protected virtual void SelectAllSubject(Entity subject, StatementSink sink) {
		}

		protected virtual void SelectAllObject(Resource @object, StatementSink sink) {
		}

		protected virtual void SelectRelationsBetween(Entity subject, Resource @object, StatementSink sink) {
		}
		
		protected virtual void SelectAllPairs(Entity predicate, StatementSink sink) {
		}

		protected virtual void SelectSubjects(Entity predicate, Resource @object, StatementSink sink) {
		}

		protected virtual void SelectObjects(Entity subject, Entity predicate, StatementSink sink) {
		}

		public void Select(Statement template, StatementSink sink) {
			if (template.Meta != null && template.Meta != Statement.DefaultMeta) return;
			if (template.Predicate != null && template.Predicate.Uri == null) return;
			
			if (template.Subject == null && template.Predicate == null && template.Object == null) {
				Select(sink);
			} else if (template.Subject != null && template.Predicate != null && template.Object != null) {
				template.Meta = Statement.DefaultMeta;
				if (Contains(template))
					sink.Add(template);
			} else if (template.Predicate == null) {
				if (template.Subject == null)
					SelectAllObject(template.Object, sink); 
				else if (template.Object == null)
					SelectAllSubject(template.Subject, sink); 
				else
					SelectRelationsBetween(template.Subject, template.Object, sink);
			} else if (template.Subject != null && template.Object == null) {
				SelectObjects(template.Subject, template.Predicate, sink);
			} else if (template.Subject == null && template.Object != null) {
				SelectSubjects(template.Predicate, template.Object, sink);
			} else if (template.Subject == null && template.Object == null) {
				SelectAllPairs(template.Predicate, sink);
			}
		}
	
		public void Select(SelectFilter filter, StatementSink sink) {
			Store.DefaultSelect(this, filter, sink);
		}
	}
	
	public class DebuggedSource : QueryableSource {
		SelectableSource source;
		System.IO.TextWriter output;
		
		public DebuggedSource(SelectableSource source, System.IO.TextWriter output) {
			this.source = source;
			this.output = output;
		}
		
		public bool Distinct { get { return source.Distinct; } }
		
		public void Select(StatementSink sink) {
			Select(Statement.All, sink);
		}
		
		public bool Contains(Resource resource) {
			output.WriteLine("CONTAINS: " + resource);
			return source.Contains(resource);
		}

		public bool Contains(Statement template) {
			output.WriteLine("CONTAINS: " + template);
			return source.Contains(template);
		}
		
		public void Select(Statement template, StatementSink sink) {
			output.WriteLine("SELECT: " + template);
			source.Select(template, sink);
		}
	
		public void Select(SelectFilter filter, StatementSink sink) {
			output.WriteLine("SELECT: " + filter);
			source.Select(filter, sink);
		}

		public virtual SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options) {
			if (source is QueryableSource)
				return ((QueryableSource)source).MetaQuery(graph, options);
			else
				return new SemWeb.Query.MetaQueryResult(); // QuerySupported is by default false
		}

		public void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SemWeb.Query.QueryResultSink sink) {
			output.WriteLine("QUERY: " + source);
			foreach (Statement s in graph)
				output.WriteLine("\t" + s);
			if (options.VariableKnownValues != null) {
				#if !DOTNET2
				foreach (System.Collections.DictionaryEntry ent in options.VariableKnownValues)
				#else
				foreach (KeyValuePair<Variable,ICollection<Resource>> ent in options.VariableKnownValues)
				#endif
					output.WriteLine("\twhere " + ent.Key + " in " + ToString((ICollection)ent.Value));	
			}
			if (source is QueryableSource) 
				((QueryableSource)source).Query(graph, options, sink);
			else
				throw new NotSupportedException("Underlying source " + source + " is not a QueryableSource.");
		}
		
		string ToString(ICollection resources) {
			ArrayList s = new ArrayList();
			foreach (Resource r in resources)
				s.Add(r.ToString());
			return String.Join(",", (string[])s.ToArray(typeof(string)));
		}
	}
	
	public class CachedSource : SelectableSource {
		SelectableSource source;

		Hashtable containsresource = new Hashtable();
		StatementMap containsstmtresults = new StatementMap();
		StatementMap selectresults = new StatementMap();
		Hashtable selfilterresults = new Hashtable();
	
		public CachedSource(SelectableSource s) { source = s; }
	
		public bool Distinct { get { return source.Distinct; } }
		
		public void Select(StatementSink sink) {
			Select(Statement.All, sink);
		}

		public bool Contains(Resource resource) {
			if (source == null) return false;
			if (!containsresource.ContainsKey(resource))
				containsresource[resource] = source.Contains(resource);
			return (bool)containsresource[resource];
		}

		public bool Contains(Statement template) {
			if (source == null) return false;
			if (!containsstmtresults.ContainsKey(template))
				containsstmtresults[template] = source.Contains(template);
			return (bool)containsstmtresults[template];
		}
		
		public void Select(Statement template, StatementSink sink) {
			if (source == null) return;
			if (!selectresults.ContainsKey(template)) {
				MemoryStore s = new MemoryStore();
				source.Select(template, s);
				selectresults[template] = s;
			}
			((MemoryStore)selectresults[template]).Select(sink);
		}
	
		public void Select(SelectFilter filter, StatementSink sink) {
			if (source == null) return;
			if (!selfilterresults.ContainsKey(filter)) {
				MemoryStore s = new MemoryStore();
				source.Select(filter, s);
				selfilterresults[filter] = s;
			}
			((MemoryStore)selfilterresults[filter]).Select(sink);
		}

	}
	
	internal class DecoupledStatementSource : StatementSource {
		StatementSource source;
		int minbuffersize = 2000;
		int maxbuffersize = 10000;
		
		bool bufferWanted = false;
		System.Threading.AutoResetEvent bufferMayAcquire = new System.Threading.AutoResetEvent(false);
		System.Threading.AutoResetEvent bufferReleased = new System.Threading.AutoResetEvent(false);

		System.Threading.Thread sourceThread;
		
		StatementList buffer = new StatementList();
		bool sourceFinished = false;
		
		public DecoupledStatementSource(StatementSource source) {
			this.source = source;
		}
		
		public bool Distinct { get { return source.Distinct; } }

		public void Select(StatementSink sink) {
			bufferWanted = false;
			
			sourceThread = new System.Threading.Thread(Go);
			sourceThread.Start();
			
			while (true) {
				bufferWanted = true;
				if (!sourceFinished) bufferMayAcquire.WaitOne(); // wait until we can have the buffer
				bufferWanted = false;
				
				Statement[] statements = buffer.ToArray();
				buffer.Clear();
				
				bufferReleased.Set(); // notify that we don't need the buffer anymore

				if (sourceFinished && statements.Length == 0) break;
				
				foreach (Statement s in statements)
					sink.Add(s);
			}
		}
		
		private void Go() {
			source.Select(new MySink(this));
			sourceFinished = true;
			bufferMayAcquire.Set(); // for the last batch
		}
		
		private void SourceAdd(Statement s) {
			if ((bufferWanted && buffer.Count > minbuffersize) || buffer.Count >= maxbuffersize) {
				bufferMayAcquire.Set();
				bufferReleased.WaitOne();
			}
			buffer.Add(s);
		}
		private void SourceAdd(Statement[] s) {
			if ((bufferWanted && buffer.Count > minbuffersize) || buffer.Count >= maxbuffersize) {
				bufferMayAcquire.Set();
				bufferReleased.WaitOne();
			}
			foreach (Statement ss in s)
				buffer.Add(ss);
		}
		
		private class MySink : StatementSink {
			DecoupledStatementSource x;
			public MySink(DecoupledStatementSource x) { this.x = x; }
			public bool Add(Statement s) {
				x.SourceAdd(s);
				return true;
			}
			public bool Add(Statement[] s) {
				x.SourceAdd(s);
				return true;
			}
		}
	}
}
