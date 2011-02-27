using System;
using System.IO;

#if !DOTNET2
using System.Collections;
#else
using System.Collections.Generic;
#endif

using SemWeb;
using SemWeb.Filters;
using SemWeb.Stores;
using SemWeb.Util;

#if !DOTNET2
using VariableList = System.Collections.ArrayList;
using BindingList = System.Collections.ArrayList;
using VarKnownValuesType = System.Collections.Hashtable;
using VarKnownValuesType2 = System.Collections.IDictionary;
using LitFilterList = System.Collections.ArrayList;
using LitFilterMap = System.Collections.Hashtable;
using LitFilterMap2 = System.Collections.IDictionary;
#else
using VariableList = System.Collections.Generic.List<SemWeb.Variable>;
using BindingList = System.Collections.Generic.List<SemWeb.Query.VariableBindings>;
using VarKnownValuesType = System.Collections.Generic.Dictionary<SemWeb.Variable,System.Collections.Generic.ICollection<SemWeb.Resource>>;
using VarKnownValuesType2 = System.Collections.Generic.IDictionary<SemWeb.Variable,System.Collections.Generic.ICollection<SemWeb.Resource>>;
using LitFilterList = System.Collections.Generic.List<SemWeb.LiteralFilter>;
using LitFilterMap = System.Collections.Generic.Dictionary<SemWeb.Variable,System.Collections.Generic.ICollection<SemWeb.LiteralFilter>>;
using LitFilterMap2 = System.Collections.Generic.IDictionary<SemWeb.Variable,System.Collections.Generic.ICollection<SemWeb.LiteralFilter>>;
#endif

namespace SemWeb.Query {

	public class GraphMatch : Query {
		private static Entity qLimit = "http://purl.oclc.org/NET/rsquary/returnLimit";
		private static Entity qStart = "http://purl.oclc.org/NET/rsquary/returnStart";
		private static Entity qDistinctFrom = "http://purl.oclc.org/NET/rsquary/distinctFrom";
		private static Entity qOptional = "http://purl.oclc.org/NET/rsquary/optional";
		
		StatementList graph = new StatementList();
		VarKnownValuesType2 knownValues = new VarKnownValuesType();
		LitFilterMap litFilters = new LitFilterMap();
		VariableList distinguishedVars = new VariableList();
	
		public GraphMatch() {
		}
		
		public GraphMatch(RdfReader query) :
			this(new Store(query),
				query.BaseUri == null ? null : new Entity(query.BaseUri)) {
		}

		public GraphMatch(StatementSource queryModel) : this(new Store(queryModel), null) {
		}
		
		private GraphMatch(Store queryModel, Entity queryNode) {
			// Find the query options
			if (queryNode != null) {
				ReturnStart = GetIntOption(queryModel, queryNode, qStart);
				ReturnLimit = GetIntOption(queryModel, queryNode, qLimit);
			}

			foreach (Statement s in queryModel.Select(Statement.All)) {
				if (IsQueryPredicate(s.Predicate)) continue;
				
				if (s.Meta == Statement.DefaultMeta)
					AddGraphStatement(s);
				else
					throw new NotSupportedException("Subgraphs (meta statement relations) are not supported.");
			}
		}
		
		private int GetIntOption(Store queryModel, Entity query, Entity predicate) {
			Resource[] rr = queryModel.SelectObjects(query, predicate);
			if (rr.Length == 0) return -1;
			Resource r = rr[0];
			if (r == null || !(r is Literal)) return -1;
			try {
				return int.Parse(((Literal)r).Value);
			} catch (Exception) {
				return -1;
			}
		}		

		private bool IsQueryPredicate(Entity e) {
			if (e == qDistinctFrom) return true;
			if (e == qLimit) return true;
			if (e == qStart) return true;
			if (e == qOptional) return true;
			return false;
		}
		
		public override string GetExplanation() {
			string ret = "Query:\n";
			foreach (Statement s in graph)
				ret += " " + s + "\n";
			return ret;
		}
		
		public void AddGraphStatement(Statement statement) {
			graph.Add(statement);
		}
		
		#if !DOTNET2
		public void SetVariableRange(Variable variable, ICollection range) {
		#else
		public void SetVariableRange(Variable variable, ICollection<Resource> range) {
		#endif
			knownValues[variable] = range;
		}
		
		public void AddLiteralFilter(Variable variable, LiteralFilter filter) {
			if (!litFilters.ContainsKey(variable))
				litFilters[variable] = new LitFilterList();
			((LitFilterList)litFilters[variable]).Add(filter);
		}
		
		public void SetDistinguishedVariable(Variable variable) {
			distinguishedVars.Add(variable);
		}
		
		public override void Run(SelectableSource targetModel, QueryResultSink result) {
			Statement[] newgraph = ReorderQuery(graph.ToArray(), toArray(knownValues), targetModel, null);
		
			QueryPart[] parts = new QueryPart[newgraph.Length];
			for (int i = 0; i < newgraph.Length; i++)
				parts[i] = new QueryPart(newgraph[i], targetModel);
			
			RunGeneralQuery(parts, knownValues, litFilters, distinguishedVars.Count == 0 ? null : distinguishedVars, ReturnStart, ReturnLimit, false, result);
		}
		
		internal static Variable[] toArray(VarKnownValuesType2 kv) {
			Variable[] ret = new Variable[kv.Count];
			int ctr = 0;
			foreach (Variable v in kv.Keys)
				ret[ctr++] = v;
			return ret;
		}

		internal struct QueryPart {
			public readonly Statement[] Graph;
			public readonly SelectableSource[] Sources;
			
			public QueryPart(Statement s, SelectableSource src) {
				Graph = new Statement[] { s };
				Sources = new SelectableSource[] { src };
			}

			public QueryPart(Statement s, SelectableSource[] sources) {
				Graph = new Statement[] { s };
				Sources = sources;
			}

			public QueryPart(Statement[] graph, QueryableSource src) {
				Graph = graph;
				Sources = new SelectableSource[] { src };
			}
		}
		
		struct BindingSet {
			public Variable[] Variables;
			public BindingList Rows;
		}
		
		internal static void RunGeneralQuery(QueryPart[] queryParts,
				VarKnownValuesType2 knownValues, LitFilterMap2 litFilters,
				#if !DOTNET2
				ICollection distinguishedVars,
				#else
				ICollection<Variable> distinguishedVars,
				#endif
				int returnStart, int returnLimit,
				bool allowQueryableSource,
				QueryResultSink result) {
				
			BindingSet bindings;
			
			// We use a sort of adaptive limiting technique in
			// queries involving intersection. If a limit on
			// A & B is specified, it is obviously incorrect to
			// limit A and B separately, since the parts that
			// intersect may be not at the beginning of either
			// of A and B. However, in many cases such a limit
			// is possible, and so we try the limits. But if
			// that limit doesn't yield enough results, we back
			// off and use a much larger limit the next time.
			// Note that when the query involves no intersection,
			// it *is* correct to pass the limit down, and so
			// we never need to do a back-off in that case.
			
			int adaptiveLimitMultiplier = 1;
			
			// If intersection is involved in this query, then
			// it's likely there will be some holes in the data
			// and we should query more than the final limit
			// from each source to start with. But if there's just
			// one query part, we keep the multiplier at 1 because
			// we want to pass it down.
			if (queryParts.Length > 1)
				adaptiveLimitMultiplier = 2;
			
			while (true) {
			
			bool adaptiveLimitDidLimit = false;
			int localLimit = returnLimit * adaptiveLimitMultiplier;
			
			bindings = new BindingSet();
			bindings.Variables = new Variable[0];
			bindings.Rows = new BindingList();
			bindings.Rows.Add(null); // we need a dummy row for the first intersection
			
			for (int iPart = 0; iPart < queryParts.Length; iPart++) {
				QueryPart part = queryParts[iPart];
			
				// Get the statements in the target model that match this aspect of the query graph.
				
				// get a list of values already found for each variable
				System.Collections.Hashtable foundValues = new System.Collections.Hashtable();
				foreach (Variable v in bindings.Variables)
					foundValues[v] = new ResSet();
				foreach (VariableBindings row in bindings.Rows) {
					if (row == null) continue; // null in first round
					for (int i = 0; i < row.Count; i++)
						((ResSet)foundValues[row.Variables[i]]).Add(row.Values[i]);
				}
				foreach (Variable v in bindings.Variables)
					foundValues[v] = ((ResSet)foundValues[v]).ToArray();
				
				// matches holds the bindings that match this part of the query
				BindingList matches;

				// vars holds an ordered list of variables found in this part of the query
				Variable[] vars;
				
				// Get a set of variables that we care about.  These are distinguished variables
				// in the query plus any variables that we will encounter in a future queryPart.
				// Any other variables are useless to us at this point and we will not do any
				// duplicate row tests based on them.
				ResSet interestingVariables = null;
				if (distinguishedVars != null) {
					interestingVariables = new ResSet();
					interestingVariables.AddRange(distinguishedVars);
					for (int jPart = iPart+1; jPart < queryParts.Length; jPart++) {
						foreach (Statement s in queryParts[jPart].Graph) {
							for (int jc = 0; jc < 4; jc++) {
								if (s.GetComponent(jc) is Variable)
									interestingVariables.Add(s.GetComponent(jc));
							}
						}
					}
				}
				
				// A QueryPart can either be:
				//	A single statement to match against one or more SelectableSources, or one or more QueryableSources
				//  A graph of statements to match against a single QueryableSource
				
				bool allSourcesQueryable = true;
				foreach (SelectableSource source in part.Sources)
					if (!(source is QueryableSource))
						allSourcesQueryable = false;
				
				if (!allowQueryableSource || !allSourcesQueryable) {
					Statement s = part.Graph[0];
					
					matches = new BindingList();
					VariableList varCollector = new VariableList();
					
					// get a list of variables in this part
					// the filter will have null components for variables, except
					// for variables with known values, we plug those values in
					SelectFilter f = new SelectFilter(s);
					for (int i = 0; i < 4; i++) {
						Resource r = s.GetComponent(i);
					
						if (r is Variable) {
							Variable v = (Variable)r;
							
							if (!varCollector.Contains(v))
								varCollector.Add(v);
							
							Resource[] values = null;
							#if DOTNET2
							if (foundValues.ContainsKey(v))
							#endif
								values = (Resource[])foundValues[v];
							if (values == null && 
							#if !DOTNET2
							knownValues[v] != null
							#else
							knownValues.ContainsKey(v)
							#endif
							)
								#if !DOTNET2
								values = (Resource[])new ArrayList((ICollection)knownValues[v]).ToArray(typeof(Resource));
								#else
								values = new List<Resource>(knownValues[v]).ToArray();
								#endif
							
							if (values == null) {
								f.SetComponent(i, null);
							} else if (i != 2) {
								bool fail = false;
								f.SetComponent(i, ToEntArray(values, ref fail));
								if (fail) return;
							} else {
								f.SetComponent(i, values);
							}
						}
					}
					
					#if !DOTNET2
					if (litFilters != null && s.Object is Variable && litFilters[(Variable)s.Object] != null)
						f.LiteralFilters = (LiteralFilter[])((LitFilterList)litFilters[(Variable)s.Object]).ToArray(typeof(LiteralFilter));
					#else
					if (litFilters != null && s.Object is Variable && litFilters.ContainsKey((Variable)s.Object))
						f.LiteralFilters = ((LitFilterList)litFilters[(Variable)s.Object]).ToArray();
					#endif
					
					vars = new Variable[varCollector.Count];
					varCollector.CopyTo(vars, 0);

					// get the matching statements; but if a variable was used twice in s,
					// filter out matching statements that don't respect that (since that info
					// was lost in the SelectFilter).
					foreach (SelectableSource source in part.Sources) {
						if (localLimit > 0) {
							f.Limit = localLimit - matches.Count;
							if (f.Limit <= 0)
								break;
						}
						source.Select(f, new Filter(matches, s, vars));
					}
						
					result.AddComments("SELECT: " + f + " => " + matches.Count);
				
				} else {
					if (part.Sources.Length == 0)
						throw new InvalidOperationException();
					
					// build a query
					
					QueryOptions opts = new QueryOptions();
					
					if (knownValues != null) {
						foreach (Variable v in knownValues.Keys)
							#if !DOTNET2
							opts.SetVariableKnownValues(v, (System.Collections.ICollection)knownValues[v]);
							#else
							opts.SetVariableKnownValues(v, knownValues[v]);
							#endif
					}
					foreach (Variable v in foundValues.Keys)
						if (foundValues[v] != null)
							opts.SetVariableKnownValues(v, (Resource[])foundValues[v]);
					if (litFilters != null)
						foreach (Variable v in litFilters.Keys)
							if (litFilters[v] != null)
								foreach (LiteralFilter f in (System.Collections.ICollection)litFilters[v]) 
									opts.AddLiteralFilter(v, f);
					
					// The distinguished variables for this part are any that are distinguished for
					// the query plus any that we need to tie these results to past and future
					// bindings (any variable used previously or used later and used in this query).
					if (distinguishedVars != null) {
						VariableList dvars = new VariableList();
						dvars.AddRange(distinguishedVars);
						for (int jPart = 0; jPart < queryParts.Length; jPart++) {
							if (jPart == iPart) continue;
							foreach (Statement s in queryParts[jPart].Graph) {
								for (int jc = 0; jc < 4; jc++) {
									if (s.GetComponent(jc) is Variable) // don't bother checking if it's actually used in this query part
										dvars.Add((Variable)s.GetComponent(jc));
								}
							}
						}
						opts.DistinguishedVariables = dvars;
					}
					
					vars = null;
					matches = null;
					string src = "";
					foreach (QueryableSource source in part.Sources) {
						if (src != "") src += ", ";
						src += source;
					
						if (localLimit > 0) {
							opts.Limit = localLimit - (matches == null ? 0 : matches.Count);
							if (opts.Limit <= 0)
								break;
						}

						QueryResultBuffer partsink = new QueryResultBuffer();
						source.Query(part.Graph, opts, partsink);

						foreach (string comment in partsink.Comments)
							result.AddComments(source.ToString() + ": " + comment);
							
						if (vars == null) {
							vars = new Variable[partsink.Variables.Length];
							partsink.Variables.CopyTo(vars, 0);	
							#if !DOTNET2
							matches = new BindingList(partsink.Bindings);
							#else
							matches = partsink.Bindings;
							#endif
						} else {
							// add in the bindings from this query, but the variables might
							// be in a different order this time
							foreach (VariableBindings b in partsink.Bindings) {
								Resource[] vals = new Resource[vars.Length];
								for (int i = 0; i < vars.Length; i++)
									vals[i] = b[vars[i]];
								VariableBindings c = new VariableBindings(vars, vals);
								matches.Add(c);
							}
						}
					}
					
					string qs = "";
					foreach (Statement s in part.Graph)
						qs += "\n\t" + s;

					result.AddComments("QUERY: " + src + qs + (returnLimit > 0 ? " [limit " + returnLimit + "/" + localLimit + "]" : "") + "  => " + matches.Count);
					
					// If we got back at least as many rows as our local (adaptive) limit,
					// then we know that the limiting (possibly) had an effect and we may
					// need to loop again to get all of the rows.
					if (matches.Count >= localLimit)
						adaptiveLimitDidLimit = true;
				}
				
				// Intersect the existing bindings with the new matches.
				// This involves creating binding rows that:
				//    Match on the intersection of variables from the two sets
				//    But include only interestingVariables variables, which
				//    are distinguished variables plus any variables we will
				//    encounter in later parts of the query
				
				// Get a list of variables the old and new have in common, and
				// a list for the common variables of their indexes in the old
				// and new sets
				int nCommonVars;
				int[,] commonVars = IntersectVariables(bindings.Variables, vars, out nCommonVars);
				
				ResSet retainedVariables = null;
				if (interestingVariables != null) {
					retainedVariables = new ResSet();
					foreach (Variable v in bindings.Variables)
						if (interestingVariables.Contains(v))
							retainedVariables.Add(v);
					foreach (Variable v in vars)
						if (interestingVariables.Contains(v))
							retainedVariables.Add(v);
				}
				
				BindingSet newbindings = new BindingSet();
				if (retainedVariables == null)
					newbindings.Variables = new Variable[bindings.Variables.Length + vars.Length - nCommonVars];
				else
					newbindings.Variables = new Variable[retainedVariables.Count];
				
				// Make a list of the variables in the final set, and a mapping
				// from indexes in the old/new set to indexes in the final set.
				int ctr = 0;
				int[] variableMapLeft = new int[bindings.Variables.Length];
				for (int i = 0; i < variableMapLeft.Length; i++) {
					if (retainedVariables == null || retainedVariables.Contains(bindings.Variables[i])) {
						variableMapLeft[i] = ctr;
						newbindings.Variables[ctr++] = bindings.Variables[i];
					} else {
						variableMapLeft[i] = -1;
					}
				}

				int[] variableMapRight = new int[vars.Length];
				for (int i = 0; i < variableMapRight.Length; i++) {
					if ((retainedVariables == null || retainedVariables.Contains(vars[i]))
						&& Array.IndexOf(newbindings.Variables, vars[i]) == -1) {
						variableMapRight[i] = ctr;
						newbindings.Variables[ctr++] = vars[i];
					} else {
						variableMapRight[i] = -1;
					}
				}
				
				newbindings.Rows = new BindingList();
				
				int nMatches = 0;

				if (nCommonVars == 0) {
					// no variables in common, make a cartesian product of the bindings
					foreach (VariableBindings left in bindings.Rows) {
						for (int i = 0; i < matches.Count; i++) {
							VariableBindings right = (VariableBindings)matches[i];
							
							Resource[] newValues = new Resource[newbindings.Variables.Length];
							CopyValues(left == null ? null : left.Values, right.Values, newValues, variableMapLeft, variableMapRight);
							
							nMatches++;
							if (!quickDupCheckIsDup(newbindings, newValues, nMatches))
								newbindings.Rows.Add(new VariableBindings(newbindings.Variables, newValues));
						}
					}
				
				} else {
					// index the new matches by those variables
				
					System.Collections.Hashtable indexedMatches = new System.Collections.Hashtable();
					foreach (VariableBindings right in matches) {
						// traverse down the list of common variables making a tree
						// structure indexing the matches by their variable values
						System.Collections.Hashtable hash = indexedMatches;
						for (int i = 0; i < nCommonVars; i++) {
							Resource value = right.Values[commonVars[i, 1]];
							if (i < nCommonVars - 1) {
								if (hash[value] == null)
									hash[value] = new System.Collections.Hashtable();
								hash = (System.Collections.Hashtable)hash[value];
							} else {
								if (hash[value] == null)
									hash[value] = new BindingList();
								BindingList list = (BindingList)hash[value];
								list.Add(right);
							}
						}
					}
					
					// for each existing binding, find all of the new matches
					// that match the common variables, by traversing the index tree
					foreach (VariableBindings left in bindings.Rows) {
						System.Collections.Hashtable hash = indexedMatches;
						BindingList list = null;
						
						for (int i = 0; i < nCommonVars; i++) {
							Resource value = left.Values[commonVars[i, 0]];
							if (hash[value] == null) break;
							if (i < nCommonVars - 1)
								hash = (System.Collections.Hashtable)hash[value];
							else
								list = (BindingList)hash[value];
						}
						
						// tree traversal didn't go to the end, meaning there was
						// no corresponding match (with the same common variable values)
						if (list == null) continue;
					
						for (int i = 0; i < list.Count; i++) {
							VariableBindings right = (VariableBindings)list[i];
							
							Resource[] newValues = new Resource[newbindings.Variables.Length];
							CopyValues(left.Values, right.Values, newValues, variableMapLeft, variableMapRight);

							nMatches++;
							if (!quickDupCheckIsDup(newbindings, newValues, nMatches))
								newbindings.Rows.Add(new VariableBindings(newbindings.Variables, newValues));
						}
					}
					
				}
				
				bindings = newbindings;
				
				// We go no intersections, so we can't get any results.
				// No need to try further parts of the query.
				if (bindings.Rows.Count == 0)
					break;
			}
			
			// If there's no limit specified, then we aren't doing adaptive limiting.
			if (returnLimit == 0) break;
			
			// If this query has just one part (no intersection), then we aren't doing adaptive limiting.
			if (queryParts.Length == 1) break;
			
			// If we found enough rows, then adaptive limiting worked.
			if (bindings.Rows.Count >= returnLimit) break;
			
			// If the adaptive limit didn't actually limit: that is, we know
			// that there are no more results than the limit for any part
			// of the query, then no larger limit will help.
			if (!adaptiveLimitDidLimit) break;
			
			// Increase the adaptive limit multiplier for next time.
			adaptiveLimitMultiplier *= 5;
			
			} // end of adaptive limiting
			
			result.Init(bindings.Variables);
			
			int counter = 0;
			foreach (VariableBindings row in bindings.Rows) {
				counter++;
				if (returnStart > 0 && counter < returnStart) continue;
				
				if (!result.Add(row))
					break;

				if (returnLimit > 0 && counter >= returnLimit) break;
			}
			
			result.Finished();
		}

		static Entity[] ToEntArray(Resource[] res, ref bool fail) {
			ResSet ents = new ResSet();
			foreach (Resource r in res)
				if (r is Entity)
					ents.Add(r);
			if (ents.Count == 0) { fail = true; return null; }
			return ents.ToEntityArray();
		}
		
		class Filter : StatementSink {
			BindingList matches;
			Statement filter;
			Variable[] vars;
			
			public Filter(BindingList matches, Statement filter, Variable[] vars) { this.matches = matches; this.filter = filter; this.vars = vars; }
			public bool Add(Statement s) {
				if (filter.Subject == filter.Predicate && s.Subject != s.Predicate) return true;
				if (filter.Subject == filter.Object && s.Subject != s.Object) return true;
				if (filter.Subject == filter.Meta && s.Subject != s.Meta) return true;
				if (filter.Predicate == filter.Object && s.Predicate != s.Object) return true;
				if (filter.Predicate == filter.Meta && s.Predicate != s.Meta) return true;
				if (filter.Object == filter.Meta && s.Object != s.Meta) return true;
				
				Resource[] vals = new Resource[vars.Length];
				for (int i = 0; i < vars.Length; i++) {
					if ((object)vars[i] == (object)filter.Subject) vals[i] = s.Subject;
					else if ((object)vars[i] == (object)filter.Predicate) vals[i] = s.Predicate;
					else if ((object)vars[i] == (object)filter.Object) vals[i] = s.Object;
					else if ((object)vars[i] == (object)filter.Meta) vals[i] = s.Meta;
				}
				matches.Add(new VariableBindings(vars, vals));
				return true;
			}
		}
		
		static int[,] IntersectVariables(Variable[] leftVars, Variable[] rightVars, out int nCommonVars) {
			VariableList commonVars = new VariableList();
			foreach (Variable v in leftVars)
				if (Array.IndexOf(rightVars, v) != -1)
					commonVars.Add(v);
			
			int[,] ret = new int[commonVars.Count, 2];
			for (int i = 0; i < commonVars.Count; i++) {
				ret[i,0] = Array.IndexOf(leftVars, commonVars[i]);
				ret[i,1] = Array.IndexOf(rightVars, commonVars[i]);
			}
			
			nCommonVars = commonVars.Count;
			return ret;
		}

		#if !DOTNET2
		static void CopyValues(Resource[] left, Resource[] right, Resource[] newValues, int[] variableMapLeft, int[] variableMapRight) {
		#else
		static void CopyValues(IList<Resource> left, IList<Resource> right, Resource[] newValues, int[] variableMapLeft, int[] variableMapRight) {
		#endif
			for (int i = 0; i < variableMapLeft.Length; i++)
				if (variableMapLeft[i] != -1)
					newValues[variableMapLeft[i]] = left[i];
			for (int i = 0; i < variableMapRight.Length; i++)
				if (variableMapRight[i] != -1)
					newValues[variableMapRight[i]] = right[i];
		}
		
		static bool quickDupCheckIsDup(BindingSet newbindings, Resource[] newValues, int nMatches) {
			// If there is a more than 10-to-1 ratio of rejected duplicates
			// to unique rows, then we check all rows.  Otherwise we check the first 100.

			bool isHighRejectRatio = newbindings.Rows.Count == 0 || (nMatches / newbindings.Rows.Count) > 10;
			for (int i = 0; i < newbindings.Rows.Count; i++) {
				if (i > 100 && !isHighRejectRatio) break;
				bool dup = true;
				for (int j = 0; j < newValues.Length; j++) {
					Resource left = ((VariableBindings)newbindings.Rows[i]).Values[j];
					Resource right = newValues[j];
					if ((object)left == null || (object)right == null) {
						if (!((object)left == null && (object)right == null))
							dup = false;
					} else if (!left.Equals(right)) {
						dup = false;
						break;
					}
				}
				if (dup)
					return true;
			}
			return false;
		}

		public static Statement[] ReorderQuery(Statement[] graph, Variable[] fixedVariables, SelectableSource schema, bool[,] groupingPreference) {
			// Find an optimal order of the statements in the graph to run the
			// query in, assuming each statement is executed one at a time.
			// This is a greedy algorithm: at each step, choose the statement
			// that has the fewest unseen variables, or failing that, if we have
			// schema information, the statement that is expected to have the
			// fewest results based on functional and inverse functional property
			// typing, and failing that if we have a preference for which statements
			// to group together (because they will be answered by the same data source),
			// then grouping that way.
			
			bool[] used = new bool[graph.Length];
			int[] permutation = new int[graph.Length];
			
			ResSet selectedVars = new ResSet();
			selectedVars.AddRange(fixedVariables);
			
			Entity rdf_type = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
			Entity owl_inversefunctional = "http://www.w3.org/2002/07/owl#InverseFunctionalProperty";
			Entity owl_functional = "http://www.w3.org/2002/07/owl#FunctionalProperty";
			
			for (int i = 0; i < graph.Length; i++) {
				// Choose the best alternative.
				int best_index = -1, best_score = -1;
				
				for (int j = 0; j < graph.Length; j++) {
					if (used[j]) continue;
					
					// Compute a 'data complexity' for the graph statement.
					int score = 1;
					
					// For each component of the graph statement...
					for (int k = 0; k < 4; k++) {
						Resource v = graph[j].GetComponent(k);
						if (!(v is Variable)) continue;
						if (selectedVars.Contains(v)) continue;
						
						int cscore = 3;
						if (schema != null) {
							// Don't penalize so much if the predicate is functional or inverse functional and
							// the other side of the statement has already been selected.
							Entity predicate = (Entity)graph[j].GetComponent(1);
							if (k == 0 && selectedVars.Contains(graph[j].GetComponent(2)) && schema.Contains(new Statement(predicate, rdf_type, owl_inversefunctional)))
								cscore = 2;
							if (k == 2 && selectedVars.Contains(graph[j].GetComponent(0)) && schema.Contains(new Statement(predicate, rdf_type, owl_functional)))
								cscore = 2;
						}
						
						score *= cscore;
					}
					
					//Console.WriteLine(j + ") " + score + " " + graph[j] + " " + ((i > 0 && groupingPreference != null
					//	&& groupingPreference[permutation[i-1], j]) ? "GROUP" : ""));
					
					// If we found something with a better score, than use this statement next.
					if (score < best_score || best_index == -1) {
						best_score = score;
						best_index = j;
					}
					
					// If we found something with the same score, but we have a preference for
					// grouping this statement with the previous, than use this statement next.
					if (score == best_score && i > 0 && groupingPreference != null
						&& !groupingPreference[permutation[i-1], best_index]
						&& groupingPreference[permutation[i-1], j]) {
						best_score = score;
						best_index = j;
					}
				}
				
				// At this point we've picked the best statement to use next.
				//Console.WriteLine("Chose " + best_index);
				used[best_index] = true;
				permutation[i] = best_index;
				for (int k = 0; k < 4; k++) {
					Resource v = graph[best_index].GetComponent(k);
					if (!(v is Variable)) continue;
					selectedVars.Add(v);
				}
			}
			
			// We've chosen a new permutation.
			Statement[] neworder = new Statement[graph.Length];
			for (int i = 0; i < graph.Length; i++) {
				neworder[i] = graph[permutation[i]];
			}
			
			return neworder;
		}
		
	}
}
