using System;
using System.Collections;

using SemWeb;
using SemWeb.Query;
using SemWeb.Util;

namespace SemWeb.Algos {

	class SubtractionSource : SelectableSource {
		SelectableSource a, b;
		public SubtractionSource(SelectableSource a, SelectableSource b) {
			this.a = a;
			this.b = b;
		}
		public bool Distinct { get { return a.Distinct; } }
		public bool Contains(Resource resource) {
			return a.Contains(resource) || b.Contains(resource);
		}
		public bool Contains(Statement template) {
			return Store.DefaultContains(this, template);
		}
		public void Select(StatementSink sink) {
			Select(Statement.All, sink);
		}
		public void Select(Statement template, StatementSink sink) {
			a.Select(template, new Tester(b, sink));
		}
		public void Select(SelectFilter filter, StatementSink sink) {
			a.Select(filter, new Tester(b, sink));
		}
		class Tester : StatementSink {
			SelectableSource b;
			StatementSink c;
			public Tester(SelectableSource b, StatementSink c) { this.b = b; this.c = c;}
			public bool Add(Statement s) {
				if (b.Contains(s)) return true;
				return c.Add(s);
			}
		}
	}

	// This class makes a graph lean.
	public class Lean {
		// A graph g is not lean if it can be decomposed
		// into a and b such that a entails b.  (where
		// 'decomposed' means a and b don't overlap
		// and their union is g.)
		// One graph a entails another graph b when:
		//   Let V be the set of variables, which is the
		//   set of blank nodes that are in b but not in a.
		//   Let M be a mapping from nodes to nodes taking
		//   nodes that aren't in V to themselves.
		//   Let M* be a mapping from graphs to graphs that
		//   maps a graph to the same graph except where 
		//   each node x is replaced by M(x).
		//   If there exists an M such that M*(b) is a
		//   subgraph of a, then a entails b.
		// Let a and b be a decomposition of g, and V be
		// the variables in b w.r.t. a (as defined above).

		// Assume a entails b.
		// |a| >= |b|.
		// Since a and b are nonoverlapping, every statement
		// in b must have a variable.  b therefore contains
		// all and only the statements in g that mention a
		// variable.  (If b had a statement without a variable,
		// M*(b) would still have that statement, so it could
		// not be a subgraph of a.)
		
		// Define a N-decomposition as a decomposition of
		//   a graph g into g1 and g2 such that the nodes of
		//   N each appear in either g1 or g2, but not both.
		//   In such a decomposition, there is no statement
		//   in g that mentions a node from N and g1 and
		//   also mention a node from N and g2.
		
		// Assume b has a V-decomposition into b1 and b2.
		// Then if a entails b, a entails b1 and a entails b2.
		// Thus, if b has a V-decomposition, b need not be
		// considered as its decomposed parts will be considered.
		
		// Define 'directly connected' as a relation between
		// two nodes and a graph that is true iff there is
		// a statement in the graph that mentions both nodes.
		// Define connected (generally) as a relation between
		// two nodes x and y, a graph g, and a set S that is true
		// iff x and y are directly connected in g or else there
		// exists another node z in S such that x and z are
		// connected and z and y are connected, in g with S.
		
		// If b has a V-decomposition, then V can be decomposed
		// into V1 and V2 and b can be decomposed into b1 and b2
		// such that all nodes in V1 appear in b1 and all nodes
		// in V2 appear in b2.  It can be seen that a node in
		// V1 cannot be connected to a node in V2 w.r.t. b and V.
		// Therefore iff every node in V is connected to every
		// other node in V, then b has no V-decomposition.
		
		// The only b's to consider are those whose variables V
		// are all connected to each other in b w.r.t. V.
		
		// The plan then is first to consider MSGs, and then
		// look at their subgraphs.
	
		public static void MakeLean(Store store) {
			MakeLean(store, null, null);
		}

		public static void MakeLean(Store store, SelectableSource relativeTo) {
			MakeLean(store, relativeTo, null);
		}
	
		public static void MakeLean(Store store, SelectableSource relativeTo, StatementSink removed) {
			// Break the data source into MSGs.  Make each MSG
			// lean first (in isolation).  Then check each lean MSG
			// to see if it's already entailed by the whole store,
			// or by relativeTo if it's provided (not null).
		
			MSG.Graph[] msgs = MSG.FindMSGs(store, true);
			
			foreach (MSG.Graph msgg in msgs) {
				// Load the MSG into memory.
				MemoryStore msg = new MemoryStore(msgg); // unnecessary duplication...

				// Make this MSG lean.  The "right" thing to do is
				// to consider all of the 'connected' subgraphs of MSG
				// against the whole store, rather than the MSG in
				// isolation.  But that gets much too expensive.
				MemoryStore msgremoved = new MemoryStore();
				MakeLeanMSG(new Store(msg), msgg.GetBNodes(), msgremoved);
				
				// Whatever was removed from msg, remove it from the main graph.
				store.RemoveAll(msgremoved.ToArray());
				
				// And track what was removed.
				if (removed != null) msgremoved.Select(removed);
				
				// If this MSG is now (somehow) empty (shouldn't happen,
				// but one never knows), don't test for entailment.
				if (msg.StatementCount == 0) continue;

				// Remove this MSG if it is already entailed.
				
				// The GraphMatch will treat all blank nodes in
				// msg as variables.
				GraphMatch match = new GraphMatch(msg);
				QueryResultBuffer sink = new QueryResultBuffer();
				match.Run(new SubtractionSource(store, msg), sink);
				if (sink.Bindings.Count > 0) {
					// This MSG can be removed.
					store.RemoveAll(msg.ToArray());
					if (removed != null) msg.Select(removed);
				} else if (relativeTo != null) {
					match.Run(relativeTo, sink);
					if (sink.Bindings.Count > 0) {
						// This MSG can be removed.
						store.RemoveAll(msg.ToArray());
						if (removed != null) msg.Select(removed);
					}
				}
			}
		}
		
		private static void MakeLeanMSG(Store msg, ICollection bnodecollection, StatementSink removed) {
			// To make any graph lean, we try to eliminate duplicate
			// paths through the graph, where duplicate means we
			// take some subset of the bnodes and call them variables,
			// and we relabel them as other bnodes from the remaining
			// set (the fixed nodes).  But there are 2^N subsets of bnodes
			// we could choose as variables (N=number of bnodes), so we can't
			// reasonably iterate through them.
			
			// I'll make a simplifying assumption that bnode predicates
			// in the graph will be considered always fixed.
			// This lets us view the graph as actually a graph (with
			// nodes and edges), and then we can make the observation that
			// if variable node V is part of a subgraph that can be removed,
			// if V directly connects to fixed node F via an edge labeled P,
			// then F must connect to a fixed node G via an edge also
			// labeled P.  That is, we can start our search looking for
			// nodes that project two edges with the same label.
			
			// Also, we only want to consider contiguous 'paths' -- subsets
			// of the bnodes connected only through those nodes --
			// to see if there is another path in the MSG if we
			// map bnodes in the first path to nodes in the MSG.
			
			// So the strategy is to start at each node in the graph
			// and consider it fixed.  If it has two outgoing
			// edges with the same property and one terminates on a
			// bnode, this is the beginning of a possible pair
			// of redundant paths (the one with the bnode being
			// eliminable).
			
			// However, the path with the bnode
			// has to be incremented with all of that bnode's
			// outgoing edges.  The other path has to be
			// incremented in parallel, following the same predicates
			// to other nodes.  If that can't be done, then these
			// paths are not duplicates.  If the parallel predicates
			// terminate on the very same nodes, the bnode and its edges can
			// be removed.
			
			// From there, each of the nodes the bnode edges terminate on,
			// besides the initial node, can be considered fixed or
			// a variable.  If it's a variable it might be able to have
			// one of many possible values, but then the path has to
			// be expanded to include all of the outgoing edges for this
			// variable.
		
			// Ok, here we go.
			
			// If there is only one bnode in the MSG, then
			// there are no subgraphs to check.  That's nice.
			if (bnodecollection.Count == 1) return;
			
			// Remember which bnodes have been removed in
			// due course.
			ResSet nodesremoved = new ResSet();
			
			// Remember which nodes are predicates and can't
			// be considered variable.
			ResSet predicates = new ResSet();
			foreach (Statement s in msg.Select(Statement.All))
				predicates.Add(s.Predicate);
			
			// Start with each bnode to consider fixed.
			foreach (BNode b in bnodecollection) {
				if (nodesremoved.Contains(b)) continue;
				MakeLeanMSG2(msg, predicates, removed, nodesremoved, b);
			}
		}
		
		private static void MakeLeanMSG2(Store msg, ResSet predicates, StatementSink removed,
			ResSet nodesremoved, BNode startingnode) {
			
			// Find every pair of two distinct outgoing edges from startingnode
			// with the same predicate, targeting entities only.
			
			MultiMap edges = new MultiMap();
			
			foreach (Statement s in msg.Select(new Statement(startingnode, null, null)))
				if (s.Object is Entity)
					edges.Put(new Edge(true, startingnode, s.Predicate, null), s.Object);
			foreach (Statement s in msg.Select(new Statement(null, null, startingnode)))
				edges.Put(new Edge(false, startingnode, s.Predicate, null), s.Subject);
			
			foreach (Edge e in edges.Keys) {
				// Make sure we have a distinct set of targets.
				ResSet targets_set = new ResSet();
				foreach (Entity r in edges.Get(e))
					targets_set.Add(r);
				if (targets_set.Count == 1) continue;
				
				IList targets = targets_set.ToEntityArray();
				
				// Take every pair of targets, provided
				// one is a bnode that can be a variable.
				for (int i = 0; i < targets.Count; i++) {
					if (!(targets[i] is BNode) || predicates.Contains((BNode)targets[i])) continue;
					if (nodesremoved.Contains((BNode)targets[i])) continue;
					for (int j = 0; j < targets.Count; j++) {
						if (i == j) continue;
						// Create a new synchronous-path object.
						SyncPath p = new SyncPath();
						p.FixedNodes.Add((Resource)targets[j]);
						p.FrontierVariables.Add((Resource)targets[i]);
						p.Mapping[targets[i]] = targets[j];
						p.Path[new Edge(e.Direction, e.Start, e.Predicate, (BNode)targets[i])] = p.Path;
						if (MakeLeanMSG3(msg, predicates, removed, nodesremoved, p))
							break; // the target was removed
					}
				}
			}
		}
		
		private static bool MakeLeanMSG3(Store msg, ResSet predicates, StatementSink removed,
			ResSet nodesremoved, SyncPath path) {
			// The variable path has to be expanded by including the statements
			// connected to the variables on the frontier.  Statements
			// mentioning a variable node have already been considered.
			// The target of each such statement can be considered fixed
			// or variable. If a variable is considered fixed, the edge
			// must exist in the MSG substituting the variables for their
			// values.  If it's variable, it has to have at least one
			// match in the MSG but not as any of the variable nodes.
			// If all targets are considered fixed (and have matches),
			// then the variables so far (and their edges) can all be
			// removed and no more processing needs to be done.
			// There are (2^N)-1 other considerations.  For each of those,
			// the targets considered variables all become the new
			// frontier, and this is repeated. 
			
			// First, get a list of edges from the frontier that we
			// haven't considered yet.
			
			ArrayList alledges = new ArrayList();
			foreach (BNode b in path.FrontierVariables) {
				// Make sure all edges are kept because even the ones
				// to literals have to be removed when duplication is found.
				foreach (Statement s in msg.Select(new Statement(b, null, null)))
					alledges.Add(new Edge(true, b, s.Predicate, s.Object));
				foreach (Statement s in msg.Select(new Statement(null, null, b)))
					alledges.Add(new Edge(false, b, s.Predicate, s.Subject));
			}
			
			ArrayList newedges = new ArrayList();
			ResSet alltargets = new ResSet();
			ResSet fixabletargetsset = new ResSet(); // can be fixed
			ResSet variabletargetsset = new ResSet(); // must be variable
			foreach (Edge e in alledges) {
				if (path.Path.ContainsKey(e)) continue;
				path.Path[e] = e;
				
				// This checks if we can keep the target of this edge
				// fixed, given the variable mappings we have so far.
				bool isTargetFixable =
					msg.Contains(e.AsStatement().Replace(path.Mapping));

				// If the target of e is any of the following, we
				// can check immediately if the edge is supported
				// by the MSG under the variable mapping we have so far:
				//    a named node, literal, fixed node, or predicate
				//    a variable we've seen already
				// If it's not supported, this path fails.  If it is
				// supported, we're done with this edge.
				if (!(e.End is BNode)
					|| path.FixedNodes.Contains(e.End)
					|| predicates.Contains(e.End)
					|| path.VariableNodes.Contains(e.End)) {
					if (!isTargetFixable) return false;
					continue; // this edge is supported, so we can continue
				}
				
				// The target of e is a new BNode.
				// If this target is not fixable via this edge, it's
				// not fixable at all.
				
				if (!isTargetFixable) {
					fixabletargetsset.Remove(e.End);
					variabletargetsset.Add(e.End);
				}
				
				if (!alltargets.Contains(e.End)) {
					alltargets.Add(e.End);
					fixabletargetsset.Add(e.End);
				}
				
				newedges.Add(e);
			}
			
			// If all of the targets were fixable (trivially true also
			// if there simple were no new edges/targets), then we've reached
			// the end of this path.  We can immediately remove
			// the edges we've seen so far, under the variable mapping
			// we've chosen.
			if (variabletargetsset.Count == 0) {
				foreach (Edge e in path.Path.Keys) {
					Statement s = e.AsStatement();
					msg.Remove(s);
					if (removed != null) removed.Add(s);
				}
				foreach (Entity e in path.Mapping.Keys)
					nodesremoved.Add(e);
				return true;
			}
			
			// At this point, at least one target must be a variable
			// and we'll have to expand the path in that direction.
			// We might want to permute through the ways we can
			// take fixable nodes as either fixed or variable, but
			// we'll be greedy and assume everything fixable is
			// fixed and everything else is a variable.
			
			path.FixedNodes.AddRange(fixabletargetsset);
			path.VariableNodes.AddRange(variabletargetsset);

			// But we need to look at all the ways each variable target
			// can be mapped to a new value, which means intersecting
			// the possible matches for each relevant edge.
			Entity[] variables = variabletargetsset.ToEntityArray();
			ResSet[] values = new ResSet[variables.Length];
			Entity[][] values_array = new Entity[variables.Length][];
			int[] choices = new int[variables.Length];
			for (int i = 0; i < variables.Length; i++) {
				foreach (Edge e in newedges) {
					if (e.End != variables[i]) continue;
					
					// Get the possible values this edge allows
					Resource[] vr;
					if (e.Direction)
						vr = msg.SelectObjects((Entity)path.Mapping[e.Start], e.Predicate);
					else
						vr = msg.SelectSubjects(e.Predicate, (Entity)path.Mapping[e.Start]);
					
					// Filter out literals and any variables
					// on the path!  The two paths can't intersect
					// except at fixed nodes.
					ResSet v = new ResSet();
					foreach (Resource r in vr) {
						if (r is Literal) continue;
						if (path.Mapping.ContainsKey(r)) continue;
						v.Add(r);
					}
					
					// Intersect these with the values we have already.
					if (values[i] == null)
						values[i] = v;
					else
						values[i].RetainAll(v);
						
					// If no values are available for this variable,
					// we're totally done.
					if (values[i].Count == 0) return false;
				}
				
				choices[i] = values[i].Count;
				values_array[i] = values[i].ToEntityArray();
			}
			
			// Now we have to permute through the choice of values.
			// Make an array of the number of choices for each variable.
			Permutation p = new Permutation(choices);
			int[] pstate;
			while ((pstate = p.Next()) != null) {
				SyncPath newpath = new SyncPath();
				newpath.FixedNodes.AddRange(path.FixedNodes);
				newpath.VariableNodes.AddRange(path.VariableNodes);
				newpath.Mapping = (Hashtable)path.Mapping.Clone();
				newpath.Path = (Hashtable)path.Path.Clone();
				
				newpath.FrontierVariables = variabletargetsset;
				
				for (int i = 0; i < variables.Length; i++) {
					Entity value = values_array[i][pstate[i]];
					newpath.Mapping[variables[i]] = value;
					newpath.FixedNodes.Add(value);
				}

				if (MakeLeanMSG3(msg, predicates, removed,
					nodesremoved, newpath)) return true;
			}
			
			return false;
		}
		
		private class Edge {
			bool direction;
			Entity start, predicate;
			Resource end;
			public Edge(bool direction, Entity start, Entity predicate, Resource end) {
				this.direction = direction;
				this.start = start;
				this.predicate = predicate;
				this.end = end;
			}
			public bool Direction { get { return direction; } }
			public Entity Start { get { return start; } }
			public Entity Predicate { get { return predicate; } }
			public Resource End { get { return end; } }
			public override int GetHashCode() { return predicate.GetHashCode(); }
			public override bool Equals(object other) {
				Edge e = (Edge)other;
				return Direction == e.Direction
					&& Start == e.Start
					&& Predicate == e.Predicate
					&& End == e.End;
			}
			public Statement AsStatement() {
				if (Direction) return new Statement(Start, Predicate, End);
				else return new Statement((Entity)End, Predicate, Start);
			}
		}
		
		private class SyncPath {
			public ResSet FixedNodes = new ResSet();
			public ResSet VariableNodes = new ResSet();
			public ResSet FrontierVariables = new ResSet();
			public Hashtable Mapping = new Hashtable();
			public Hashtable Path = new Hashtable();
		}

		private class Sink : StatementSink {
			ResSet variables;
			Store store;
			public Sink(ResSet variables, Store store) {
				this.variables = variables;
				this.store = store;
			}
			public bool Add(Statement s) {
				s.Meta = Statement.DefaultMeta;
				if (store.Contains(s)) return true;
				if (variables.Contains(s.Subject)
					|| variables.Contains(s.Predicate)
					|| variables.Contains(s.Object))
					store.Add(s);
				return true;
			}
		}
	}

	public class MSG {

		// These methods find minimal self-contained graphs
		// in a graph by recursively expanding a subgraph.
	
		public static MemoryStore FindMSG(SelectableSource store, Entity node) {
			MemoryStore ret = new MemoryStore();
			FindMSG(store, node, ret);
			return ret;
		}
		
		public static void FindMSG(SelectableSource store, Entity node, StatementSink msg) {
			if (node.Uri != null) throw new ArgumentException("node must be anonymous");
			
			ResSet nodesSeen = new ResSet();
			ResSet nodesToAdd = new ResSet();
			
			nodesToAdd.Add(node);
			
			while (nodesToAdd.Count > 0) {
				ResSet nodes = nodesToAdd;
				nodesToAdd = new ResSet();
				
				Sink sink = new Sink(msg, nodesToAdd);
				foreach (Entity n in nodes) {
					if (nodesSeen.Contains(n)) continue;
					nodesSeen.Add(n);
					store.Select(new Statement(n, null, null, null), sink);
					store.Select(new Statement(null, n, null, null), sink);
					store.Select(new Statement(null, null, n, null), sink);
				}
			}
		}
		
		private class Sink : StatementSink {
			StatementSink msg;
			ResSet add;
			public Sink(StatementSink msg, ResSet add) {
				this.msg = msg;
				this.add = add;
			}
			public bool Add(Statement s) {
				if (msg is SelectableSource && ((SelectableSource)msg).Contains(s)) return true;
				msg.Add(s);
				if (s.Subject.Uri == null) add.Add(s.Subject);
				if (s.Predicate.Uri == null) add.Add(s.Predicate);
				if (s.Object is Entity && s.Object.Uri == null) add.Add(s.Object);
				return true;
			}
		}
		
		// This method finds all minimal self-contained graphs
		// by painting nodes colors (the colors happen to be
		// objects) in one pass over the statements and then doing
		// a second pass to put each statement mentioning a bnode
		// into the appropriate graph structure.
		public static Graph[] FindMSGs(SelectableSource source, bool loadIntoMemory) {
			FindMSGsSink sink = new FindMSGsSink(source, loadIntoMemory);
			source.Select(Statement.All, sink);
			ArrayList graphs = new ArrayList(sink.colors.Keys);
			return (Graph[])graphs.ToArray(typeof(Graph));
		}
		
		public class Graph : StatementSource {
			SelectableSource source;
			internal ResSet entities = new ResSet();
			internal MemoryStore statements;

			internal Graph(SelectableSource source)  {
				this.source = source;
			}
			
			public bool Distinct { get { return source.Distinct; } }
			public bool Contains(Entity e) {
				return entities.Contains(e);
			}
			public ICollection GetBNodes() {
				return entities.Items;
			}
			public void Select(StatementSink s) {
				if (statements == null)
					source.Select(Statement.All, new Sink(this, s));
				else
					statements.Select(s);
			}

			private class Sink : StatementSink {
				Graph g;
				StatementSink s;
				public Sink(Graph g, StatementSink s) {
					this.g = g;
					this.s = s;
				}
				public bool Add(Statement s) {
					if (g.Contains(s.Subject)
						|| g.Contains(s.Predicate)
						|| (s.Object is Entity && g.Contains((Entity)s.Object)))
						return this.s.Add(s);
					return true;
				}
			}
			public static void LoadGraphs(Graph[] graphs) {
			}
		}
		
		class FindMSGsSink : StatementSink {
			SelectableSource source;
			bool loadin;
			Hashtable bnodecolors = new Hashtable();
			public Hashtable colors = new Hashtable();
			public FindMSGsSink(SelectableSource source, bool loadIntoMem) { this.source = source; loadin = loadIntoMem; }
			public bool Add(Statement s) {
				// Get the color of any painted entity in the statement.
				int numcon = 0;
				Graph color = null;
				if (s.Subject.Uri == null) { Go1(s.Subject, ref color); numcon++; }
				if (s.Predicate.Uri == null) { Go1(s.Predicate, ref color); numcon++; }
				if (s.Object.Uri == null && s.Object is Entity) { Go1((Entity)s.Object, ref color); numcon++; }
				
				// If there isn't a blank node here, nothing to do.
				if (numcon == 0)
					return true;
				
				// No nodes were colored yet, so pick a new color.
				if (color == null) {
					color = new Graph(source);
					if (loadin)
						color.statements = new MemoryStore();
					colors[color] = color;
				}
				
				// Apply that color to all of the nodes.
				if (s.Subject.Uri == null) Go2(s.Subject, ref color);
				if (s.Predicate.Uri == null) Go2(s.Predicate, ref color);
				if (s.Object.Uri == null && s.Object is Entity) Go2((Entity)s.Object, ref color);
				
				// And put this statement into that color
				if (loadin)
					color.statements.Add(s);
				
				return true;
			}
			void Go1(Entity e, ref Graph color) {
				if (color == null && bnodecolors.ContainsKey(e)) {
					color = (Graph)bnodecolors[e];
				}
			}
			void Go2(Entity e, ref Graph color) {
				if (bnodecolors.ContainsKey(e)) {
					Graph curcolor = (Graph)bnodecolors[e];
					if (curcolor != color) {
						// Everyone that has the color curcolor
						// has to switch to the color color.
						foreach (Entity e2 in curcolor.entities)
							bnodecolors[e2] = color;

						color.entities.AddRange(curcolor.entities);
						if (loadin)
							foreach (Statement s in curcolor.statements)
								color.statements.Add(s);
								
						colors.Remove(curcolor);
					}
				} else {
					bnodecolors[e] = color;
					color.entities.Add(e);
				}
			}
		}
		
	}
	
	public class Connectivity {
	
		public static void Build(StatementSource graph, out bool[,] connectivity, Hashtable indexes) {
			connectivity = new bool[indexes.Count, indexes.Count];
			graph.Select(new Sink(connectivity, indexes));
		}
		
		class Sink : StatementSink {
			bool[,] connectivity;
			Hashtable indexes;
			public Sink(bool[,] connectivity, Hashtable indexes) {
				this.connectivity = connectivity;
				this.indexes = indexes;
			}
			public bool Add(Statement st) {
				int s = indexes.ContainsKey(st.Subject) ? (int)indexes[st.Subject] : -1;
				int p = indexes.ContainsKey(st.Predicate) ? (int)indexes[st.Predicate] : -1;
				int o = indexes.ContainsKey(st.Object) ? (int)indexes[st.Object] : -1;
				if (s != -1 && p != -1) { connectivity[s,p]=true; connectivity[p,s]=true; }
				if (s != -1 && o != -1) { connectivity[s,o]=true; connectivity[o,s]=true; }
				if (p != -1 && o != -1) { connectivity[p,o]=true; connectivity[o,p]=true; }
				return true;
			}
		}
	}

	// This class uses a connectivity matrix to iterate
	// through the connected subsets of the nodes, that is,
	// subsets of the nodes that are connected by traveling
	// just through those nodes.  The Next() method returns
	// a bool[] indicating the nodes in the subgraph.
	public class SubgraphIterator {
		// This is based on something I read.
		// We'll maintain a queue of connected
		// subgraphs to process.  The queue will
		// start with a one-node subgraph for each
		// bnode.  Then each time we process a
		// subgraph, we'll extend the graph by one
		// node every way we can and add all of those
		// new subgraphs into the queue -- unless we've
		// already processed the subgraph.  
	
		int n;
		bool[,] conn;
		Queue queue = new Queue();
		Hashtable processed = new Hashtable();
		
		public SubgraphIterator(bool[,] connectivity) {
			this.conn = connectivity;
			n = conn.GetLength(0);
			for (int i = 0; i < n; i++)
				QueueSubgraph(null, i);
		}
		
		void QueueSubgraph(Subgraph a, int b) {
			Subgraph s = new Subgraph();
			s.nodes = new bool[n];
			s.touching = new bool[n];
			if (a != null) {
				a.nodes.CopyTo(s.nodes, 0);
				a.touching.CopyTo(s.touching, 0);
			}
			s.nodes[b] = true;

			s.sum = unchecked((a != null ? a.sum : 0) + b);
			if (processed.ContainsKey(s)) return;
			
			for (int i = 0; i < n; i++)
				if (conn[b,i])
					s.touching[i] = true;
					
			processed[s] = processed;
			queue.Enqueue(s);
		}
		
		public bool[] Next() {
			if (queue.Count == 0) return null;
			Subgraph s = (Subgraph)queue.Dequeue();
			
			// Create a new s for every node touching
			// s but not in s.
			for (int i = 0; i < n; i++)
				if (!s.nodes[i] && s.touching[i])
					QueueSubgraph(s, i);
			
			return s.nodes;
		}
		
		class Subgraph {
			public bool[] nodes;
			public bool[] touching;
			public int sum;
			
			public override int GetHashCode() { return sum; }
			public override bool Equals(object o) {
				Subgraph g = (Subgraph)o;
				for (int i = 0; i < nodes.Length; i++)
					if (nodes[i] != g.nodes[i])
						return false;
				return true;
			}
		}
	}
		
}
