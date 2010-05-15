using System;
using System.Collections;

using SemWeb.Util;

namespace SemWeb {
	public struct Statement :
#if DOTNET2
	IEquatable<Statement>, IComparable<Statement>
#else
	IComparable
#endif
	{
	
		public Entity Subject;
		public Entity Predicate;
		public Resource Object;
		public Entity Meta;
		
		public static Entity DefaultMeta = new BNode();
		
		public static Statement All = new Statement(null, null, null, null);
		
		public Statement(Entity subject, Entity predicate, Resource @object)
		: this(subject, predicate, @object, DefaultMeta) {
		}
		
		public Statement(Entity subject, Entity predicate, Resource @object, Entity meta) {
		  Subject = subject;
		  Predicate = predicate;
		  Object = @object;
		  Meta = meta;
		}
		
		public bool AnyNull {
			get {
				return Subject == null || Predicate == null || Object == null || Meta == null;
			}
		}
		
		public Statement Invert() {
			if (!(Object is Entity)) throw new ArgumentException("The object of the statement must be an entity.");
			return new Statement((Entity)Object, Predicate, Subject, Meta);
		}
		
		public bool Matches(Statement statement) {
			if (Subject != null && Subject != statement.Subject && statement.Subject != null) return false;
			if (Predicate != null && Predicate != statement.Predicate && statement.Predicate != null) return false;
			if (Object != null && Object != statement.Object && statement.Object != null) return false;
			if (Meta != null && Meta != statement.Meta && statement.Meta != null) return false;
			return true;
		}
		
		public override string ToString() {
			string ret = "";
			if (Subject != null) ret += Subject.ToString(); else ret += "?"; ret += " ";
			if (Predicate != null) ret += Predicate.ToString(); else ret += "?"; ret += " ";
			if (Object != null) ret += Object.ToString(); else ret += "?"; ret += " ";
			if (Meta != null && Meta != DefaultMeta) ret += "meta=" + Meta.ToString();
			return ret + ".";
		}
		
		public Statement Replace(Resource find, Resource replacement) {
			if (replacement is Literal) {
				if (find == Object)
					return new Statement(Subject, Predicate, replacement, Meta);
				return this;
			} else {
				Entity ent = (Entity)replacement;
				return new Statement(
					Subject == find ? ent : Subject,
					Predicate == find ? ent : Predicate,
					Object == find ? ent : Object,
					Meta == find ? ent : Meta
					);
			}
		}
		
		public Statement Replace(Hashtable resourceMap) {
			return new Statement(
				!resourceMap.ContainsKey(Subject) ? Subject : (Entity)resourceMap[Subject],
				!resourceMap.ContainsKey(Predicate) ? Predicate : (Entity)resourceMap[Predicate],
				!resourceMap.ContainsKey(Object) ? Object : (Resource)resourceMap[Object],
				!resourceMap.ContainsKey(Meta) ? Meta : (Entity)resourceMap[Meta]
				);
		}


		public override bool Equals(object other) {
			return (Statement)other == this;
		}
		
#if DOTNET2
		bool IEquatable<Statement>.Equals(Statement other) {
			return other == this;
		}
#endif

		public override int GetHashCode() {
			int ret = 0;
			if (Subject != null) ret = unchecked(ret + Subject.GetHashCode());
			if (Predicate != null) ret = unchecked(ret + Predicate.GetHashCode());
			if (Object != null) ret = unchecked(ret + Object.GetHashCode());
			if (Meta != null) ret = unchecked(ret + Meta.GetHashCode());
			return ret;
		}
		
		public static bool operator ==(Statement a, Statement b) {
			if ((a.Subject == null) != (b.Subject == null)) return false;
			if ((a.Predicate == null) != (b.Predicate == null)) return false;
			if ((a.Object == null) != (b.Object == null)) return false;
			if ((a.Meta == null) != (b.Meta == null)) return false;
			if (a.Subject != null && !a.Subject.Equals(b.Subject)) return false;
			if (a.Predicate != null && !a.Predicate.Equals(b.Predicate)) return false;
			if (a.Object != null && !a.Object.Equals(b.Object)) return false;
			if (a.Meta != null && !a.Meta.Equals(b.Meta)) return false;
			return true;
		}
		public static bool operator !=(Statement a, Statement b) {
			return !(a == b);
		}

#if !DOTNET2
		int IComparable.CompareTo(object other) {
			return CompareTo((Statement)other);
		}
#endif

		public int CompareTo(Statement s) {
			int x;
			x = cmp(Subject, s.Subject); if (x != 0) return x;
			x = cmp(Predicate, s.Predicate); if (x != 0) return x;
			x = cmp(Object, s.Object); if (x != 0) return x;
			x = cmp(Meta, s.Meta); if (x != 0) return x;
			return 0;
		}
		int cmp(Resource a, Resource b) {
			if (a == null && b == null) return 0;
			if (a == null) return -1;
			if (b == null) return 1;
			return ((IComparable)a).CompareTo(b);
		}
		
		internal Resource GetComponent(int index) {
			switch (index) {
				case 0: return Subject;
				case 1: return Predicate;
				case 2: return Object;
				case 3: return Meta;
			}
			throw new ArgumentException("index");
		}
		internal void SetComponent(int index, Resource r) {
			switch (index) {
				case 0: Subject = (Entity)r; break;
				case 1: Predicate = (Entity)r; break;
				case 2: Object = r; break;
				case 3: Meta = (Entity)r; break;
				default: throw new ArgumentException("index");
			}
		}
	}
	
	public struct SelectFilter : IEnumerable {
		public Entity[] Subjects;
		public Entity[] Predicates;
		public Resource[] Objects;
		public Entity[] Metas;
		public LiteralFilter[] LiteralFilters;
		public int Limit;
		
		public static SelectFilter All = new SelectFilter(null, null, null, null);
		
		public SelectFilter(Statement statement) {
			Subjects = null; Predicates = null; Objects = null; Metas = null; LiteralFilters = null; Limit = 0;
			if (statement.Subject != null) Subjects = new Entity[] { statement.Subject };
			if (statement.Predicate != null) Predicates = new Entity[] { statement.Predicate };
			if (statement.Object != null) Objects = new Resource[] { statement.Object };
			if (statement.Meta != null) Metas = new Entity[] { statement.Meta };
		}
		
		public SelectFilter(Entity[] subjects, Entity[] predicates, Resource[] objects, Entity[] metas) {
			Subjects = null; Predicates = null; Objects = null; Metas = null; LiteralFilters = null; Limit = 0;
			Subjects = subjects;
			Predicates = predicates;
			Objects = objects;
			Metas = metas;
		}
		
		internal Resource[] GetComponent(int index) {
			switch (index) {
				case 0: return Subjects;
				case 1: return Predicates;
				case 2: return Objects;
				case 3: return Metas;
			}
			throw new ArgumentException("index");
		}
		
		internal void SetComponent(int index, Resource[] res) {
			switch (index) {
				case 0: Subjects = (Entity[])res; break;
				case 1: Predicates = (Entity[])res; break;
				case 2: Objects = res; break;
				case 3: Metas = (Entity[])res; break;
				default: throw new ArgumentException("index");
			}
		}

		public override string ToString() {
			string ret =
				ToString(Subjects) + " " +
				ToString(Predicates) + " " +
				ToString(Objects);
			if (Metas == null || Metas.Length > 1 || Metas[0] != Statement.DefaultMeta)
				ret += " meta=" + ToString(Metas);
			return ret;
		}
		
		private string ToString(Resource[] res) {
			if (res == null) return "?";
			if (res.Length == 1) return res[0].ToString();
			System.Text.StringBuilder b = new System.Text.StringBuilder();
			b.Append("{ ");
			bool first = true;
			bool cutoff = false;
			foreach (Resource r in res) {
				if (!first) b.Append(", "); first = false;
				if (b.Length > 50) { b.Append("..."); cutoff = true; break; }
				b.Append(r.ToString());
			}
			b.Append(" }");
			if (cutoff) b.Insert(2, "(" + res.Length +") ");
			return b.ToString();
		}

		public override bool Equals(object other) {
			return this == (SelectFilter)other;
		}
		
		public override int GetHashCode() {
			int hc = 0;
			if (Subjects != null) hc ^= Subjects[0].GetHashCode();
			if (Predicates != null) hc ^= Predicates[0].GetHashCode();
			if (Objects != null) hc ^= Objects[0].GetHashCode();
			if (Metas != null) hc ^= Metas[0].GetHashCode();
			return hc;
		}
		
		public static bool operator ==(SelectFilter a, SelectFilter b) {
			return eq(a.Subjects, b.Subjects)
				&& eq(a.Predicates, b.Predicates)
				&& eq(a.Objects, b.Objects)
				&& eq(a.Metas, b.Metas)
				&& a.LiteralFilters == b.LiteralFilters
				&& a.Limit == b.Limit;
		}
		public static bool operator !=(SelectFilter a, SelectFilter b) {
			return !(a == b);
		}
		static bool eq(Resource[] a, Resource[] b) {
			if (a == b) return true;
			if (a == null || b == null) return false;
			if (a.Length != b.Length) return false;
			bool alleq = true;
			for (int i = 0; i < a.Length; i++)
				if (!a[i].Equals(b[i]))
					alleq = false;
			if (alleq) return true;
			ResSet xa = new ResSet(a);
			ResSet xb = new ResSet(b);
			xa.RetainAll(xb);
			return xa.Count == xb.Count;
		}
		
		public static SelectFilter[] FromGraph(Statement[] graph) {
			SelectFilter[] ret = new SelectFilter[graph.Length];
			for (int i = 0; i < ret.Length; i++)
				ret[i] = new SelectFilter(graph[i]);
			return ret;
		}
		
		public IEnumerator GetEnumerator() {
			return new StatementIterator(this);
		}
		
		class StatementIterator : IEnumerator {
			SelectFilter f;
			SemWeb.Util.Permutation p;
			int[] cur;
			public StatementIterator(SelectFilter filter) {
				f = filter;
				p = new SemWeb.Util.Permutation(new int[] {
					f.Subjects == null ? 1 : f.Subjects.Length,
					f.Predicates == null ? 1 : f.Predicates.Length,
					f.Objects == null ? 1 : f.Objects.Length,
					f.Metas == null ? 1 : f.Metas.Length,
					} );
			}
			
			public object Current {
				get {
					if (cur == null) throw new InvalidOperationException("Call MoveNext!");
					return new Statement(
						f.Subjects == null ? null : f.Subjects[cur[0]],
						f.Predicates == null ? null : f.Predicates[cur[1]],
						f.Objects == null ? null : f.Objects[cur[2]],
						f.Metas == null ? null : f.Metas[cur[3]]
						);
				}
			}
			
			public bool MoveNext() {
				cur = p.Next();
				return cur != null;
			}
			
			public void Reset() { cur = null; p.Reset(); } 
		}
	}
}
