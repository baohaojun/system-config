using System;
using System.Collections;

using SemWeb;
using SemWeb.Stores;
using SemWeb.Util;

#if !DOTNET2
using ResourceList = System.Collections.ICollection;
using VarKnownValuesType = System.Collections.Hashtable;
#else
using ResourceList = System.Collections.Generic.ICollection<SemWeb.Resource>;
using VarKnownValuesType = System.Collections.Generic.Dictionary<SemWeb.Variable,System.Collections.Generic.ICollection<SemWeb.Resource>>;
#endif

namespace SemWeb.Inference {

	public class RDFS : Reasoner {
		static readonly Entity type = NS.RDF + "type";
		static readonly Entity subClassOf = NS.RDFS + "subClassOf";
		static readonly Entity subPropertyOf = NS.RDFS + "subPropertyOf";
		static readonly Entity domain = NS.RDFS + "domain";
		static readonly Entity range = NS.RDFS + "range";
		static readonly Entity rdfsresource = NS.RDFS + "Resource";
	
		// Each of these hashtables relates an entity
		// to a ResSet of other entities, including itself.
		Hashtable superclasses = new Hashtable();
		Hashtable subclasses = new Hashtable();
		Hashtable superprops = new Hashtable();
		Hashtable subprops = new Hashtable();
		
		// The hashtables relate a property to a ResSet of
		// its domain and range, and from a type to a ResSet
		// of properties it is the domain or range of.
		Hashtable domains = new Hashtable();
		Hashtable ranges = new Hashtable();
		Hashtable domainof = new Hashtable();
		Hashtable rangeof = new Hashtable();
		
		StatementSink schemasink;
		
		public RDFS() {
			schemasink = new SchemaSink(this);
		}
		
		public RDFS(StatementSource schema) : this() {
			LoadSchema(schema);
		}
		
		public StatementSink Schema { get { return schemasink; } }
		
		class SchemaSink : StatementSink {
			RDFS rdfs;
			public SchemaSink(RDFS parent) { rdfs = parent; }
			bool StatementSink.Add(Statement s) { rdfs.AddAxiom(s); return true; }
		}
		
		void AddAxiom(Statement schemastatement) {
			if (schemastatement.Predicate == subClassOf && schemastatement.Object is Entity) {
				AddRelation(schemastatement.Subject, (Entity)schemastatement.Object, superclasses, subclasses);
				AddRelation(schemastatement.Subject, rdfsresource, superclasses, subclasses);
				AddRelation((Entity)schemastatement.Object, rdfsresource, superclasses, subclasses);
			}
			if (schemastatement.Predicate == subPropertyOf && schemastatement.Object is Entity)
				AddRelation(schemastatement.Subject, (Entity)schemastatement.Object, superprops, subprops);
			if (schemastatement.Predicate == domain && schemastatement.Object is Entity) {
				AddRelation(schemastatement.Subject, (Entity)schemastatement.Object, domains, domainof);
				AddRelation((Entity)schemastatement.Object, rdfsresource, superclasses, subclasses);
			}
			if (schemastatement.Predicate == range && schemastatement.Object is Entity) {
				AddRelation(schemastatement.Subject, (Entity)schemastatement.Object, ranges, rangeof);
				AddRelation((Entity)schemastatement.Object, rdfsresource, superclasses, subclasses);
			}
		}
		
		void AddRelation(Entity a, Entity b, Hashtable supers, Hashtable subs) {
			AddRelation(a, b, supers);
			AddRelation(b, a, subs);
		}
		
		void AddRelation(Entity a, Entity b, Hashtable h) {
			ResSet r = (ResSet)h[a];
			if (r == null) {
				r = new ResSet();
				h[a] = r;
			}
			r.Add(b);
		}
		
		public void LoadSchema(StatementSource source) {
			if (source is SelectableSource) {
				((SelectableSource)source).Select(
					new SelectFilter(
					null,
					new Entity[] { subClassOf, subPropertyOf, domain, range },
					null, null), Schema);
			} else {
				source.Select(Schema);
			}
		}
		
		public override bool Distinct { get { return false; } }
		
		public override void Select(SelectFilter filter, SelectableSource data, StatementSink sink) {
			if (filter.Predicates == null || filter.LiteralFilters != null) {
				data.Select(filter, sink);
				return;
			}
			
			ResSet remainingPredicates = new ResSet();
			
			Entity[] subjects = filter.Subjects;
			Entity[] predicates = filter.Predicates;
			Resource[] objects = filter.Objects;
			Entity[] metas = filter.Metas;
			
			foreach (Entity p in predicates) {
				if (p == type) {
					if (objects != null) {
						// Do the subjects have any of the types listed in the objects,
						// or what things have those types?
						
						// Expand objects by the subclass closure of the objects
						data.Select(new SelectFilter(subjects, new Entity[] { p }, GetClosure(objects, subclasses, true), metas), sink);
						
						// Process domains and ranges.
						ResSet dom = new ResSet(), ran = new ResSet();
						Hashtable domPropToType = new Hashtable();
						Hashtable ranPropToType = new Hashtable();
						foreach (Entity e in GetClosure(objects, subclasses, true)) {
							Entity[] dc = GetClosure((ResSet)domainof[e], subprops, true);
							if (dc != null)
							foreach (Entity c in dc) {
								dom.Add(c);
								AddRelation(c, e, domPropToType);
							}
							
							dc = GetClosure((ResSet)rangeof[e], subprops, true);
							if (dc != null)
							foreach (Entity c in dc) {
								ran.Add(c);
								AddRelation(c, e, ranPropToType);
							}
						}
						
						// If it's in the domain of any of these properties,
						// we know its type.  Only do this if subjects are given,
						// since otherwise we have to select for all of the values
						// of all of these properties, and that doesn't scale well.
						if (subjects != null) {
							if (dom.Count > 0) data.Select(new SelectFilter(subjects, dom.ToEntityArray(), null, metas), new ExpandDomRan(0, domPropToType, sink));
							if (ran.Count > 0) data.Select(new SelectFilter(null, ran.ToEntityArray(), subjects, metas), new ExpandDomRan(1, ranPropToType, sink));
						}
						
					} else if (subjects != null) {
						// What types do these subjects have?
						
						// Expand the resulting types by the closure of their superclasses
						data.Select(new SelectFilter(subjects, new Entity[] { p }, objects, metas), new Expand(superclasses, sink));
						
						// Use domains and ranges to get type info
						data.Select(new SelectFilter(subjects, null, null, metas), new Expand3(0, domains, superclasses, sink));
						data.Select(new SelectFilter(null, null, subjects, metas), new Expand3(1, ranges, superclasses, sink));

					} else {
						// What has type what?  We won't answer that question.
						data.Select(filter, sink);
					}

				} else if ((p == subClassOf || p == subPropertyOf)
					&& (metas == null || metas[0] == Statement.DefaultMeta)) {
					
					Hashtable supers = (p == subClassOf) ? superclasses : superprops;
					Hashtable subs = (p == subClassOf) ? subclasses : subprops;
					
					if (subjects != null && objects != null) {
						// Expand objects by the subs closure of the objects.
						data.Select(new SelectFilter(subjects, new Entity[] { p }, GetClosure(objects, subs, true), metas), sink);
					} else if (subjects != null) {
						// get all of the supers of all of the subjects
						foreach (Entity s in subjects)
							foreach (Entity o in GetClosure(s, supers, false))
								sink.Add(new Statement(s, p, o));
					} else if (objects != null) {
						// get all of the subs of all of the objects
						foreach (Resource o in objects) {
							if (o is Literal) continue;
							foreach (Entity s in GetClosure((Entity)o, subs, false))
								sink.Add(new Statement(s, p, (Entity)o));
						}
					} else {
						// What is a subclass/property of what?  We won't answer that.
						data.Select(filter, sink);
					}
				} else {
					remainingPredicates.Add(p);
				}
			}
			
			if (remainingPredicates.Count > 0) {
				// Also query the subproperties of any property
				// being queried, but remember which subproperties
				// came from which superproperties so we can map them
				// back to the properties actually queried.  The closures
				// contain the queried properties themselves too.
				ResSet qprops = new ResSet();
				Hashtable propfrom = new Hashtable();
				foreach (Entity p in remainingPredicates) { 
					foreach (Entity sp in GetClosure(p, subprops, true)) {
						AddRelation(sp, p, propfrom);
						qprops.Add(sp);
					}
				}
				
				//data.Select(subjects, qprops.ToEntityArray(), objects, metas, new LiteralDTMap(ranges, new PredMap(propfrom, sink)));
				
				SelectFilter sf = new SelectFilter(subjects, qprops.ToEntityArray(), objects, metas);
				sf.LiteralFilters = filter.LiteralFilters;
				sf.Limit = filter.Limit;
				
				data.Select(sf, new PredMap(propfrom, sink));
			}
		}
		
		static Entity[] GetClosure(Entity start, Hashtable table, bool includeStart) {
			return GetClosure( new Resource[] { start } , table, includeStart);
		}

		static Entity[] GetClosure(ResSet starts, Hashtable table, bool includeStarts) {
			if (starts == null) return null;
			return GetClosure(starts.ToArray(), table, includeStarts);
		}
		
		static Entity[] GetClosure(Resource[] starts, Hashtable table, bool includeStarts) {
			ResSet ret = new ResSet();
			ResSet toadd = new ResSet(starts);
			bool firstRound = true;
			while (toadd.Count > 0) {
				ResSet newadd = new ResSet();
				
				foreach (Resource e in toadd) {
					if (!(e is Entity)) continue;
					if (ret.Contains(e)) continue;
					if (!(firstRound && !includeStarts)) ret.Add(e);
					if (table.ContainsKey(e))
						newadd.AddRange((ResSet)table[e]);
				}
				
				toadd.Clear();
				toadd.AddRange(newadd);
				firstRound = false;
			}
			return ret.ToEntityArray();
		}
		
		class Expand : StatementSink {
			Hashtable table;
			StatementSink sink;
			public Expand(Hashtable t, StatementSink s) { table = t; sink = s; }
			public bool Add(Statement s) {
				foreach (Entity e in RDFS.GetClosure(new Resource[] { s.Object }, table, true))
					if (!sink.Add(new Statement(s.Subject, s.Predicate, e, s.Meta)))
						return false;
				return true;
			}
		}

		class ExpandDomRan : StatementSink {
			int domran;
			Hashtable map;
			StatementSink sink;
			public ExpandDomRan(int dr, Hashtable propToType, StatementSink s) {
				if (s == null) throw new ArgumentNullException();
				domran = dr;
				map = propToType;
				sink = s; 
			}
			public bool Add(Statement s) {
				if (s.AnyNull) throw new ArgumentNullException();
				if (domran == 1 && !(s.Object is Entity)) return true;
				if (!map.ContainsKey(s.Predicate)) return true; // shouldn't really happen
				foreach (Entity e in (ResSet)map[s.Predicate]) {
					Statement s1 = new Statement(
						domran == 0 ? s.Subject : (Entity)s.Object,
						type,
						e,
						s.Meta);
					if (!sink.Add(s1))
						return false;
				}
				return true;
			}
		}

		class Expand3 : StatementSink {
			int domran;
			Hashtable table;
			Hashtable superclasses;
			StatementSink sink;
			public Expand3(int dr, Hashtable t, Hashtable sc, StatementSink s) { domran = dr; table = t; superclasses = sc; sink = s; }
			public bool Add(Statement s) {
				if (domran == 1 && !(s.Object is Entity)) return true;
				ResSet rs = (ResSet)table[s.Predicate];
				if (rs == null) return true;
				foreach (Entity e in RDFS.GetClosure(rs, superclasses, true)) {
					Statement s1 = new Statement(
						domran == 0 ? s.Subject : (Entity)s.Object,
						type,
						e,
						s.Meta);
					if (!sink.Add(s1))
						return false;
				}
				return true;
			}
		}
		
		class PredMap : StatementSink {
			Hashtable table;
			StatementSink sink;
			public PredMap(Hashtable t, StatementSink s) { table = t; sink = s; }
			public bool Add(Statement s) {
				if (table[s.Predicate] == null) {
					return sink.Add(s);
				} else {
					foreach (Entity e in (ResSet)table[s.Predicate])
						if (!sink.Add(new Statement(s.Subject, e, s.Object, s.Meta)))
							return false;
				}
				return true;
			}
		}

		class LiteralDTMap : StatementSink {
			Hashtable ranges;
			StatementSink sink;
			public LiteralDTMap(Hashtable t, StatementSink s) { ranges = t; sink = s; }
			public bool Add(Statement s) {
				ranges.ToString(); // avoid warning about not using variable
				if (s.Object is Literal && ((Literal)s.Object).DataType == null) {
					// TODO: Look at the superproperty closure of the predicate
					// and apply the first range found to the literal.  While
					// more than one range may apply, we can only assign one.
					// It would be best to assign the most specific, but we
					// don't have that info.  And, don't ever assign the rdfs:Literal
					// or rdfs:Resource classes as the data type -- and there may be
					// others -- that are consistent but just not data types.
					// Also, assign the most specific data type if we have
					// the class relations among them.
					return sink.Add(s);
				} else {
					return sink.Add(s);
				}
			}
		}
		
		public override SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options, SelectableSource data) {
			Statement[] graph2;
			SemWeb.Query.QueryOptions options2;
			RewriteGraph(graph, options, out graph2, out options2, null);
			
			if (!(data is QueryableSource))
				return new SimpleEntailment().MetaQuery(graph2, options2, data);
			else
				return ((QueryableSource)data).MetaQuery(graph2, options2);
		}

		public override void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SelectableSource data, SemWeb.Query.QueryResultSink sink) {
			Statement[] graph2;
			SemWeb.Query.QueryOptions options2;
			RewriteGraph(graph, options, out graph2, out options2, sink);
			
			// TODO: Because we add variables to the query when we replace things with closures,
			// we should filter the query results so we don't pass back the bindings for those
			// variables to the caller.
		
			if (!(data is QueryableSource))
				new SimpleEntailment().Query(graph2, options2, data, sink);
			else
				((QueryableSource)data).Query(graph2, options2, sink);
		}

		void RewriteGraph(Statement[] graph, SemWeb.Query.QueryOptions options, out Statement[] graph2, out SemWeb.Query.QueryOptions options2, SemWeb.Query.QueryResultSink sink) {
			graph2 = new Statement[graph.Length];
			options2 = new SemWeb.Query.QueryOptions();
			
			options2.DistinguishedVariables = options.DistinguishedVariables;
			options2.Limit = options.Limit;
			options2.VariableKnownValues = (options.VariableKnownValues == null ? new VarKnownValuesType() : new VarKnownValuesType(options.VariableKnownValues));
			options2.VariableLiteralFilters = options.VariableLiteralFilters;
			
			for (int i = 0; i < graph.Length; i++) {
				graph2[i] = graph[i];
			
				//ResSet subj = GetQueryRes(graph[i], 0, options);
				ResSet pred = GetQueryRes(graph[i], 1, options);
				ResSet obj = GetQueryRes(graph[i], 2, options);
				
				if (pred.Count == 1 && pred.Contains(type)) {
					// in an ?x rdf:type ___ query, replace ___ with the subclass closure of ___.
					if (obj.Count > 0) {
						Entity[] sc = GetClosure(obj, subclasses, true);
						if (sc.Length != obj.Count && sink != null)
							sink.AddComments("Expanding object of " + graph[i] + " with subclass closure to [" + ToString(sc) + "]");
						SetQueryRes(ref graph2[i], 2, options2, sc);
					}
				}
				
				// expand properties into subproperties after the above tests,
				// because we want to be sure the property was originally
				// just one of the recognized properties

				if (pred.Count > 0) {
					Entity[] pc = GetClosure(pred, subprops, true);
					SetQueryRes(ref graph2[i], 1, options2, pc);
					if (pc.Length != pred.Count && sink != null)
						sink.AddComments("Expanding predicate of " + graph[i] + " with subproperty closure to [" + ToString(pc) + "]");
				}
			}
		}

		ResSet GetQueryRes(Statement s, int i, SemWeb.Query.QueryOptions options) {
			ResSet ret = new ResSet();
			Resource r = s.GetComponent(i);
			if (r == null) return ret;

			if (!(r is Variable)) ret.Add(r);
			
			if (options.VariableKnownValues != null && r is Variable
#if !DOTNET2
				&& options.VariableKnownValues.Contains((Variable)r)) {
#else
				&& options.VariableKnownValues.ContainsKey((Variable)r)) {
#endif
				ret.AddRange((ResourceList)options.VariableKnownValues[(Variable)r]);
			}
			return ret;
		}
		
		void SetQueryRes(ref Statement s, int i, SemWeb.Query.QueryOptions options, Entity[] values) {
			// TODO: what if s had originally a variable in position i?
			if (values.Length == 0)
				s.SetComponent(i, null);
			else if (values.Length == 1)
				s.SetComponent(i, values[0]);
			else {
				Variable v = new Variable();
				s.SetComponent(i, v);
				options.VariableKnownValues[v] = values;
			}
		}
		
		string ToString(Entity[] ents) {
			string[] names = new string[ents.Length];
			for (int i = 0; i < ents.Length; i++)
				names[i] = ents[i].ToString();
			return String.Join(" , ", names);
		}
	}

}
