// Adapted from:
// ---------------------------------------------------------------
// Euler proof mechanism -- Jos De Roo
// version = '$Id: euler.js,v 1.35 2006/12/17 01:25:01 josd Exp $'
// http://eulersharp.cvs.sourceforge.net/eulersharp/2006/02swap/euler.js?view=log
// ---------------------------------------------------------------
// The original Euler code is licensed under the W3C Software License.
// This is a very liberal translation of the original code into C#.

using System;
using System.Collections;

using SemWeb;
using SemWeb.Util;

namespace SemWeb.Inference {
	
	public class Euler : Reasoner {
	
		#if !SILVERLIGHT
		static bool Debug = System.Environment.GetEnvironmentVariable("SEMWEB_DEBUG_EULER") != null;
		#else
		static bool Debug = false;
		#endif

		Hashtable rules;
		
		static Hashtable builtInRelations;
		
		static Euler() {
			RdfRelation[] rs = new RdfRelation[] {
				#if !SILVERLIGHT
				new SemWeb.Inference.Relations.MathAbsoluteValueRelation(), new SemWeb.Inference.Relations.MathCosRelation(), new SemWeb.Inference.Relations.MathDegreesRelation(), new SemWeb.Inference.Relations.MathEqualToRelation(),
				new SemWeb.Inference.Relations.MathNegationRelation(), new SemWeb.Inference.Relations.MathRoundedRelation(), new SemWeb.Inference.Relations.MathSinRelation(), new SemWeb.Inference.Relations.MathSinhRelation(), new SemWeb.Inference.Relations.MathTanRelation(), new SemWeb.Inference.Relations.MathTanhRelation(),
				new SemWeb.Inference.Relations.MathAtan2Relation(), new SemWeb.Inference.Relations.MathDifferenceRelation(), new SemWeb.Inference.Relations.MathExponentiationRelation(), new SemWeb.Inference.Relations.MathIntegerQuotientRelation(),
				new SemWeb.Inference.Relations.MathQuotientRelation(), new SemWeb.Inference.Relations.MathRemainderRelation(),
				new SemWeb.Inference.Relations.MathSumRelation(), new SemWeb.Inference.Relations.MathProductRelation(),
				new SemWeb.Inference.Relations.MathGreaterThanRelation(), new SemWeb.Inference.Relations.MathLessThanRelation(), new SemWeb.Inference.Relations.MathNotGreaterThanRelation(), new SemWeb.Inference.Relations.MathNotLessThanRelation(), new SemWeb.Inference.Relations.MathNotEqualToRelation()
				#endif
			};
		
			builtInRelations = new Hashtable();
			foreach (RdfRelation r in rs)
				builtInRelations[r.Uri] = r;
		}
		
		public Euler(StatementSource rules) {
			this.rules = RulesToCases(rules);
		}
		
		public override bool Distinct { get { return false; } } // not sure...
		
		public override void Select(SelectFilter filter, SelectableSource targetModel, StatementSink sink) {
			if (filter.Subjects == null) filter.Subjects = new Entity[] { new Variable("subject") };
			if (filter.Predicates == null) filter.Predicates = new Entity[] { new Variable("predicate") };
			if (filter.Objects == null) filter.Objects = new Entity[] { new Variable("object") };

			if (filter.Metas == null) filter.Metas = new Entity[] { Statement.DefaultMeta };
			
			foreach (Statement s in filter) { // until we can operate on filter directly
				ArrayList evidence = prove(rules, targetModel, new Statement[] { s }, -1);
				if (evidence == null)
					continue; // not provable (in max number of steps, if that were given)
				
				foreach (EvidenceItem ei in evidence) {
					foreach (Statement h in ei.head) { // better be just one statement
						if (filter.LiteralFilters != null
							&& !LiteralFilter.MatchesFilters(h.Object, filter.LiteralFilters, targetModel))
							continue;
						
						sink.Add(h);
					}
				}
			}
		}
		
		private void QueryCheckArg(Statement[] graph) {
			if (graph == null) throw new ArgumentNullException("graph");
			foreach (Statement s in graph) {
				if (s.Subject == null || s.Predicate == null || s.Object == null || s.Meta == null)
					throw new ArgumentNullException("Graph statements cannot contain a null subject, predicate, or object. Use a Variable instance instead.");
				if (s.Meta != Statement.DefaultMeta && !(s.Meta is Variable))
					throw new NotSupportedException("Graph statements' meta fields must be Statement.DefaultMeta. Other values of meta are not currently supported.");
			}
		}
		
		public override SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options, SelectableSource targetModel) {
			QueryCheckArg(graph);
			SemWeb.Query.MetaQueryResult ret = new SemWeb.Query.MetaQueryResult();
			ret.QuerySupported = true;
			// TODO: Best to check also whether variables in the query are even known to us.
			return ret;
		}
		
		public override void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SelectableSource targetModel, SemWeb.Query.QueryResultSink sink) {
			QueryCheckArg(graph);

			// Try to do the inferencing.
			ArrayList evidence = prove(rules, targetModel, graph, -1);
			if (evidence == null)
				return; // not provable (in max number of steps, if that were given)
			
			// Then send the possible bindings to the QueryResultSink.
			
			// Map variables to indexes.
			Hashtable vars = new Hashtable();
			foreach (Statement s in graph) {
				if (s.Subject is Variable && !vars.ContainsKey(s.Subject)) vars[s.Subject] = vars.Count;
				if (s.Predicate is Variable && !vars.ContainsKey(s.Predicate)) vars[s.Predicate] = vars.Count;
				if (s.Object is Variable && !vars.ContainsKey(s.Object)) vars[s.Object] = vars.Count;
			}
			
			// Prepare the bindings array.
			Variable[] varOrder = new Variable[vars.Count];
			foreach (Variable v in vars.Keys)
				varOrder[(int)vars[v]] = v;
			
			// Initialize the sink.
			sink.Init(varOrder);
			
			// Send a binding set for each piece of evidence.
			foreach (EvidenceItem ei in evidence) {
				// Write a comment to the results with the actual proof. (nifty actually)
				sink.AddComments(ei.ToProof().ToString());
			
				// Create the binding array and send it on
				Resource[] variableBindings = new Resource[varOrder.Length];
				foreach (Variable v in vars.Keys)
					if (ei.env.ContainsKey(v))
						variableBindings[(int)vars[v]] = (Resource)ei.env[v];
				sink.Add(new SemWeb.Query.VariableBindings(varOrder, variableBindings));
			}
			
			// Close the sink.
			sink.Finished();
		}
		
		// Static methods that wrap the Euler engine
		
		private static readonly Entity entLOGIMPLIES = "http://www.w3.org/2000/10/swap/log#implies";
		private static readonly Entity entRDFFIRST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
		private static readonly Entity entRDFREST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
		private static readonly Entity entRDFNIL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";

		private class Sequent {
			public Statement head; // consequent
			public Statement[] body; // antecedent
			public Hashtable callArgs; // encapsulates (...) arguments to user predicates
			
			public Sequent(Statement head, Statement[] body, Hashtable callArgs) {
				this.head = head;
				this.body = body;
				this.callArgs = callArgs;
			}
			public Sequent(Statement head) {
				this.head = head;
				this.body = new Statement[0];
			}
			
			public Rule ToRule() {
				return new Rule(body, new Statement[] { head });
			}
			
			// override to satisfy a warning about overloading ==
			public override bool Equals(object o) {
				return this == (Sequent)o;
			}
			
			// override to satisfy a warning about overloading ==
			public override int GetHashCode() {
				return base.GetHashCode();
			}
			
			public override string ToString() {
				string ret = "{ ";
				foreach (Statement b in body) {
					if (ret != "{ ") ret += ", ";
					ret += b.ToString();
				}
				ret += " } => " + head;
				return ret;
			}
			
			public static bool operator ==(Sequent a, Sequent b) {
				if (object.ReferenceEquals(a, b)) return true;
				if (object.ReferenceEquals(a, null) && object.ReferenceEquals(b, null)) return true;
				if (object.ReferenceEquals(a, null) || object.ReferenceEquals(b, null)) return false;
				if (a.head != b.head) return false;
				if (a.body.Length != b.body.Length) return false;
				for (int i = 0; i < a.body.Length; i++)
					if (a.body[i] != b.body[i]) return false;
				return true;
			}
			public static bool operator !=(Sequent a, Sequent b) {
				return !(a == b);
			}
		}
		
		private class Ground {
			public Sequent src;  // evidence
			public Hashtable env;  // substitution environment: Resource => Resource
			
			public Ground(Sequent src, Hashtable env) {
				this.src = src;
				this.env = env;
			}
			
			public ProofStep ToProofStep() {
				return new ProofStep(src.ToRule(), env);
			}
		}
		
		private class EvidenceItem {
			public Statement[] head;
			public ArrayList body; // of Ground
			public Hashtable env; // substitutions of goal-level variables
			
			public Proof ToProof() {
				ProofStep[] steps = new ProofStep[body.Count];
				for (int i = 0; i < body.Count; i++)
					steps[i] = ((Ground)body[i]).ToProofStep();
				return new Proof(head, steps);
			}
			
			public override string ToString() {
				string ret = "";
				foreach (DictionaryEntry kv in env)
					ret += "\t" + kv.Key + "=>" + kv.Value + "\n";
				return ret;
			}
		}
		
		private class QueueItem {
			public Sequent rule;
			public Sequent src;
			public int ind;
			public QueueItem parent;
			public Hashtable env; // substitution environment: Resource => Resource
			public ArrayList ground;
			
			public int liveChildren = 0;
			public StatementList solutions;
			
			public override string ToString() {
				string ret = "";
				foreach (DictionaryEntry kv in env)
					ret += kv.Key + "=>" + kv.Value + "; ";
				ret += rule.ToString() + " @ " + ind + "/" + rule.body.Length;
				return ret;
			}
		}
		
		public Proof[] Prove(SelectableSource world, Statement[] goal) {
			ArrayList evidence = prove(rules, world, goal, -1);
			if (evidence == null)
				throw new Exception("Could not complete proof within the maximum number of steps allowed.");
			
			ArrayList ret = new ArrayList();
			foreach (EvidenceItem ei in evidence)
				ret.Add(ei.ToProof());

			return (Proof[])ret.ToArray(typeof(Proof));
		}
		
		private static ArrayList prove(Hashtable cases, SelectableSource world, Statement[] goal, int maxNumberOfSteps) {
			// This is the main routine from euler.js.
		
			// cases: Resource (predicate) => IList of Sequent
			
			if (world != null) // cache our queries to the world, if a world is provided
				world = new SemWeb.Stores.CachedSource(world);
		
			StatementMap cached_subproofs = new StatementMap();
			
			// Create the queue and add the first item.
			ArrayList queue = new ArrayList();
			{
				QueueItem start = new QueueItem();
				start.env = new Hashtable();
				start.rule = new Sequent(Statement.All, goal, null);
				start.src = null;
				start.ind = 0;
				start.parent = null;
				start.ground = new ArrayList();
				queue.Add(start);
				if (Debug) Console.Error.WriteLine("Euler: Queue: " + start);
			}
			
			// The evidence array holds the results of this proof.
			ArrayList evidence = new ArrayList();
			
			// Track how many steps we take to not go over the limit.
			int step = 0;
			
			// Process the queue until it's empty or we reach our step limit.
			while (queue.Count > 0) {
				// deal with the QueueItem at the top of the queue
				QueueItem c = (QueueItem)queue[queue.Count-1];
				queue.RemoveAt(queue.Count-1);
				ArrayList g = new ArrayList(c.ground);
				
				// have we done too much?
				step++;
				if (maxNumberOfSteps != -1 && step >= maxNumberOfSteps) {
					if (Debug) Console.Error.WriteLine("Euler: Maximum number of steps exceeded.  Giving up.");
					return null;
				}
				
				// if each statement in the body of the sequent has been proved
				if (c.ind == c.rule.body.Length) {
					// if this is the top-level sequent being proved; we've found a complete evidence for the goal
					if (c.parent == null) {
						EvidenceItem ev = new EvidenceItem();
						ev.head = new Statement[c.rule.body.Length];
						bool canRepresentHead = true;
						for (int i = 0; i < c.rule.body.Length; i++) {
							ev.head[i] = evaluate(c.rule.body[i], c.env);
							if (ev.head[i].AnyNull) // can't represent it: literal in subject position, for instance
								canRepresentHead = false;
						}
						
						ev.body = c.ground;
						ev.env = c.env;

						if (Debug) Console.Error.WriteLine("Euler: Found Evidence: " + ev);
						
						if (!canRepresentHead)
							continue;

						evidence.Add(ev);
					
					// this is a subproof of something; whatever it is a subgroup for can
					// be incremented
					} else {
						// if the rule had no body, it was just an axiom and we represent that with Ground
						if (c.rule.body.Length != 0) g.Add(new Ground(c.rule, c.env));

						// advance the parent being proved and put the advanced
						// parent into the queue; unify the parent variable assignments
						// with this one's
						QueueItem r = new QueueItem();
						r.rule = c.parent.rule;
						r.src = c.parent.src;
						r.ind = c.parent.ind;
						r.parent = c.parent.parent;
						r.env = (Hashtable)c.parent.env.Clone();
						r.ground = g;
						unify(c.rule.head, c.env, r.rule.body[r.ind], r.env, true);
						r.ind++;
						queue.Add(r);
						if (Debug) Console.Error.WriteLine("Euler: Queue Advancement: " + r);
						
						// The number of live children for this parent is decremented since we are
						// done with this subproof, but we store the result for later.
						if (c.parent.solutions == null)
							c.parent.solutions = new StatementList();
						c.parent.solutions.Add(evaluate(r.rule.body[r.ind-1], r.env));
						decrementLife(c.parent, cached_subproofs);
					}
				
				// this sequent still has parts of the body left to be proved; try to
				// find evidence for the next statement in the body, and if we find
				// evidence, queue up that evidence
				} else {
					// this is the next part of the body that we need to try to prove
					Statement t = c.rule.body[c.ind];
					
					// Try to process this predicate with a user-provided custom
					// function that resolves things like mathematical relations.
					// euler.js provides similar functionality, but the system
					// of user predicates is completely different here.
					RdfRelation b = FindUserPredicate(t.Predicate);
					if (b != null) {
						Resource[] args;
						Variable[] unifyResult;
						Resource value = evaluate(t.Object, c.env);
						
						if (!c.rule.callArgs.ContainsKey(t.Subject)) {
							// The array of arguments to this relation is just the subject of the triple itself
							args = new Resource[] { evaluate(t.Subject, c.env) };
							unifyResult = new Variable[1];
							if (t.Subject is Variable) unifyResult[0] = (Variable)t.Subject;
						
						} else {
							// The array of arguments to this relation comes from a pre-grouped arg list.
							args = (Resource[])c.rule.callArgs[t.Subject];
							unifyResult = new Variable[args.Length];
							
							for (int i = 0; i < args.Length; i++) {
								if (args[i] is Variable) {
									unifyResult[i] = (Variable)args[i];
									args[i] = evaluate(args[i], c.env);
								}
							}
						}
						
						// Run the user relation on the array of arguments (subject) and on the object.
						if (b.Evaluate(args, ref value)) {
							// If it succeeds, we press on.
						
							// The user predicate may update the 'value' variable and the argument
							// list array, and so we want to unify any variables in the subject
							// or object of this user predicate with the values given to us
							// by the user predicate.
							Hashtable newenv = (Hashtable)c.env.Clone();
							if (t.Object is Variable) unify(value, null, t.Object, newenv, true);
							for (int i = 0; i < args.Length; i++)
								if (unifyResult[i] != null)
									unify(args[i], null, unifyResult[i], newenv, true);
						
							Statement grnd = evaluate(t, newenv);
							if (grnd != Statement.All) // sometimes not representable, like if literal as subject
								g.Add(new Ground(new Sequent(grnd, new Statement[0], null), new Hashtable()));

							QueueItem r = new QueueItem();
							r.rule = c.rule;
							r.src = c.src;
							r.ind = c.ind;
							r.parent = c.parent;
							r.env = newenv;
							r.ground = g;
							r.ind++;
							queue.Add(r);
							
							// Note: Since we are putting something back in for c, we don't touch the life counter on the parent.
							
						} else {
							// If the predicate fails, decrement the life of the parent.
							decrementLife(c.parent, cached_subproofs);
						}
						continue;
					}
					
					// t can be proved either by the use of a rule
					// or if t literally exists in the world

					Statement t_resolved = evaluate(t, c.env);
					
					// If resolving this statement requires putting a literal in subject or predicate position, we
					// can't prove it.
					if (t_resolved == Statement.All) {
						decrementLife(c.parent, cached_subproofs);
						continue;
					}
						
					ArrayList tcases = new ArrayList();
					
					// See if we have already tried to prove this.
					if (cached_subproofs.ContainsKey(t_resolved)) {
						StatementList cached_solutions = (StatementList)cached_subproofs[t_resolved];
						if (cached_solutions == null) {
							if (Debug) Console.Error.WriteLine("Euler: Dropping queue item because we have already failed to prove it: " + t_resolved);
						} else {
							foreach (Statement s in cached_solutions) {
								if (Debug) Console.Error.WriteLine("Euler: Using Cached Axiom:  " + s);
								Sequent seq = new Sequent(s);
								tcases.Add(seq);
							}
						}
					} else {
						// get all of the rules that apply to the predicate in question
						if (t_resolved.Predicate != null && cases.ContainsKey(t_resolved.Predicate))
							tcases.AddRange((IList)cases[t_resolved.Predicate]);
	
						if (cases.ContainsKey("WILDCARD"))
							tcases.AddRange((IList)cases["WILDCARD"]);
						
						// if t has no unbound variables and we've matched something from
						// the axioms, don't bother looking at the world, and don't bother
						// proving it any other way than by the axiom.
						bool lookAtWorld = true;
						foreach (Sequent seq in tcases) {
							if (seq.body.Length == 0 && seq.head == t_resolved) {
								lookAtWorld = false;
								tcases.Clear();
								tcases.Add(seq);
								break;
							}
						}
	
						// if there is a seprate world, get all of the world
						// statements that witness t
						if (world != null && lookAtWorld) {
							MemoryStore w = new MemoryStore();
						
							if (Debug) Console.Error.WriteLine("Running " + c);
							if (Debug) Console.Error.WriteLine("Euler: Ask World: " + t_resolved);
							world.Select(t_resolved, w);
							foreach (Statement s in w) {
								if (Debug) Console.Error.WriteLine("Euler: World Select Response:  " + s);
								Sequent seq = new Sequent(s);
								tcases.Add(seq);
							}
						}
					}
					
					// If there is no evidence or potential evidence (i.e. rules)
					// for t, then we will dump this QueueItem by not queuing any
					// subproofs.
					
					// Otherwise we try each piece of evidence in turn.
					foreach (Sequent rl in tcases) {
						ArrayList g2 = (ArrayList)c.ground.Clone();
						if (rl.body.Length == 0) g2.Add(new Ground(rl, new Hashtable()));
						
						QueueItem r = new QueueItem();
						r.rule = rl;
						r.src = rl;
						r.ind = 0;
						r.parent = c;
						r.env = new Hashtable();
						r.ground = g2;
						
						if (unify(t, c.env, rl.head, r.env, true)) {
							QueueItem ep = c;  // euler path
						 	while ((ep = ep.parent) != null)
						  		if (ep.src == c.src && unify(ep.rule.head, ep.env, c.rule.head, c.env, false))
						  			break;
						 	if (ep == null) {
								// It is better for caching subproofs to work an entire proof out before
								// going on, which means we want to put the new queue item at the
								// top of the stack.
						 		queue.Add(r);
								c.liveChildren++;
						 		if (Debug) Console.Error.WriteLine("Euler: Queue from Axiom: " + r);
						 	}
						}
					}
					
					// If we did not add anything back into the queue for this item, then
					// we decrement the life of the parent.
					if (c.liveChildren == 0)
						decrementLife(c.parent, cached_subproofs);
				}
			}
			
			return evidence;
		}
		
		private static void decrementLife(QueueItem q, StatementMap cached_subproofs) {
			q.liveChildren--;
			if (q.liveChildren == 0) {
				Statement t = evaluate(q.rule.body[q.ind], q.env);
				cached_subproofs[t] = q.solutions;
				if (Debug && q.solutions == null) Console.Error.WriteLine("Euler: Died: " + q);
			}
		}
		
		private static bool unify(Resource s, Hashtable senv, Resource d, Hashtable denv, bool f) {
			if (s is Variable) {
				Resource sval = evaluate(s, senv);
				if (sval != null) return unify(sval, senv, d, denv, f);
				else return true;
			} else if (d is Variable) {
				Resource dval = evaluate(d, denv);
				if (dval != null) {
					return unify(s, senv, dval, denv, f);
				} else {
					if (f) denv[d] = evaluate(s, senv);
					return true;
				}
			} else if (s.Equals(d)) {
				return true;
			} else {
				return false;
			}
		}

		private static bool unify(Statement s, Hashtable senv, Statement d, Hashtable denv, bool f) {
			if (s == Statement.All) return false;
			if (!unify(s.Subject, senv, d.Subject, denv, f)) return false;
			if (!unify(s.Predicate, senv, d.Predicate, denv, f)) return false;
			if (!unify(s.Object, senv, d.Object, denv, f)) return false;
			return true;
		}
		
		private static Resource evaluate(Resource t, Hashtable env) {
			if (t is Variable) {
				Resource val = (Resource)env[t];
				if (val != null)
					return evaluate(val, env);
				else
					return null;
			}
			return t;
		}
		
		private static Statement evaluate(Statement t, Hashtable env) {
			Resource s = evaluate(t.Subject, env);
			Resource p = evaluate(t.Predicate, env);
			Resource o = evaluate(t.Object, env);
		
			// If we can't represent this statement because it requires
			// putting a literal in subject or predicate position,
			// return Statement.All (i.e. null S/P/O).
			if (s is Literal || p is Literal)
				return Statement.All;
				
			return new Statement((Entity)s, (Entity)p, o, Statement.DefaultMeta);
		}
		
		// The next few routines convert a set of axioms from a StatementSource
		// into a data structure of use for the algorithm, with Sequents and things.
		
		

		private static Hashtable RulesToCases(StatementSource rules) {
			Hashtable cases = new Hashtable();
			MemoryStore rules_store = new MemoryStore(rules);
			foreach (Statement p in rules_store) {
				if (p.Meta == Statement.DefaultMeta) {
					if (p.Predicate == entLOGIMPLIES && p.Object is Entity) {
						MemoryStore body = new MemoryStore();
						MemoryStore head = new MemoryStore();
					
						rules_store.Select(new Statement(null, null, null,  (Entity)p.Subject), new RemoveMeta(body));
						rules_store.Select(new Statement(null, null, null,  (Entity)p.Object), new RemoveMeta(head));
						
						// Any variables in the head not bound in the body represent existentially closed bnodes.
						// (Euler's OWL test case does this. Wish they had used bnodes instead of vars...)
						ResSet bodyvars = new ResSet();
						foreach (Statement b in body) {
							if (b.Subject is Variable) bodyvars.Add(b.Subject);
							if (b.Predicate is Variable) bodyvars.Add(b.Predicate);
							if (b.Object is Variable) bodyvars.Add(b.Object);
						}
						foreach (Entity v in head.GetEntities()) {
							if (v is Variable && !bodyvars.Contains(v))
								head.Replace(v, new BNode(((Variable)v).LocalName));
						}
						
						// Replace (...) lists in the body that are tied to the subjects
						// of user predicates with callArgs objects.
						Hashtable callArgs = new Hashtable();
						CollectCallArgs(body, callArgs);
						
						// Rules can't have more than one statement in their
						// consequent.  The best we can do is break up
						// the consequent into multiple rules.  (Since all head
						// variables are bound in body, it's equivalent...?)
						foreach (Statement h in head)
							AddSequent(cases, new Sequent(h, body.ToArray(), callArgs));
					} else {
						AddSequent(cases, new Sequent(p, new Statement[0], null));
					}
				}
			}
			
			return cases;
		}

		private class RemoveMeta : StatementSink {
			StatementSink sink;
			public RemoveMeta(StatementSink s) { sink = s; }
			public bool Add(Statement s) {
				s.Meta = Statement.DefaultMeta;
				return sink.Add(s);
			}
		}
		
		private static void CollectCallArgs(MemoryStore body, Hashtable callArgs) {
			foreach (Statement s in new MemoryStore(body)) { // make a copy since we remove statements from b within
				if (FindUserPredicate(s.Predicate) == null) continue;
				
				Entity subj = s.Subject;
				if (!(subj is BNode)) continue; // it would be nice to exclude variables, but we've already converted bnodes to variables
				
				BNode head = (BNode)subj;
				
				ArrayList argList = new ArrayList();
				
				while (true) {
					Resource[] objs = body.SelectObjects(subj, entRDFFIRST);
					if (objs.Length == 0) break; // if this is the first iteration, then we're just not looking at a list
												 // on later iterations, the list must not be well-formed, so we'll just
												 // stop reading it here
					
					argList.Add(objs[0]); // assume a properly formed list
					body.Remove(new Statement(subj, entRDFFIRST, null));
					
					Resource[] rests = body.SelectObjects(subj, entRDFREST);
					if (rests.Length == 0) break; // not well formed
					body.Remove(new Statement(subj, entRDFREST, null));
					if (rests[0] == entRDFNIL) break; // that's the end of the list
					if (!(rests[0] is Entity)) break; // also not well formed
					
					subj = (Entity)rests[0];
				}
				
				if (argList.Count > 0)
					callArgs[head] = argList.ToArray(typeof(Resource));
			}
		}
		
		private static void AddSequent(Hashtable cases, Sequent s) {
			object key = s.head.Predicate;
			if (key is Variable) key = "WILDCARD";
			ArrayList list = (ArrayList)cases[key];
			if (list == null) {
				list = new ArrayList();
				cases[key] = list;
			}
			list.Add(s);
		}

		// Lastly, we have special and built-in predicates.

		private static RdfRelation FindUserPredicate(Entity predicate) {
			if (predicate.Uri == null) return null;
			if (builtInRelations.ContainsKey(predicate.Uri))
				return (RdfRelation)builtInRelations[predicate.Uri];
			return null;
		}
	}
}
