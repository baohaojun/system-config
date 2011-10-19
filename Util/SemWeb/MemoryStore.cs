using System;
using System.Collections;

using SemWeb;
using SemWeb.Stores;
using SemWeb.Util;

namespace SemWeb {
	public class MemoryStore : Store, 
#if DOTNET2
	System.Collections.Generic.IEnumerable<Statement>
#else
	IEnumerable
#endif
	{
	
		internal StoreImpl impl;

		public MemoryStore()
			: this(new StoreImpl()) {
		}

		public MemoryStore(StatementSource source)
			: this() {
			Import(source);
		}
		
		public MemoryStore(Statement[] statements)
			: this(new StoreImpl(statements)) {
		}
		
		private MemoryStore(StoreImpl impl) {
			this.impl = impl;
			AddSource2(impl);
		}
		
		public override void AddSource(SelectableSource store) {
			throw new InvalidOperationException("AddSource is not valid on the MemoryStore.");
		}
		
		public override void AddSource(SelectableSource store, string uri) {
			throw new InvalidOperationException("AddSource is not valid on the MemoryStore.");
		}

		public Statement this[int index] {
			get {
				return impl[index];
			}
		}

		public Statement[] ToArray() {
			return impl.ToArray();
		}

		#if DOTNET2
		System.Collections.Generic.IEnumerator<Statement> System.Collections.Generic.IEnumerable<Statement>.GetEnumerator() {
			return ((System.Collections.Generic.IEnumerable<Statement>)impl).GetEnumerator();
		}
		#endif
		IEnumerator IEnumerable.GetEnumerator() {
			return ((IEnumerable)impl).GetEnumerator();
		}

		internal bool allowIndexing { set { impl.allowIndexing = value; } }
		internal bool checkForDuplicates { set { impl.checkForDuplicates = value; } }


	internal class StoreImpl : SelectableSource, StaticSource, ModifiableSource, 
#if DOTNET2
	System.Collections.Generic.IEnumerable<Statement>
#else
	IEnumerable
#endif
	
	{
		#if DOTNET2
		private class StatementList : System.Collections.Generic.List<Statement> {
			public StatementList() : base() { }
			public StatementList(Statement[] statements) : base(statements) { }
		}
		#endif

		StatementList statements;
		
		Hashtable statementsAboutSubject = new Hashtable();
		Hashtable statementsAboutObject = new Hashtable();
		
		bool isIndexed = false;
		internal bool allowIndexing = true;
		internal bool checkForDuplicates = false;
		bool distinct = true;
		
		string guid = null;
		Hashtable pbnodeToId = null;
		Hashtable pbnodeFromId = null;
		
		const string rdfli = NS.RDF + "_";
		
		public StoreImpl() {
			statements = new StatementList();
		}
		
		public StoreImpl(StatementSource source) : this() {
			Import(source);
		}
		
		public StoreImpl(Statement[] statements) {
			this.statements = new StatementList(statements);
		}

		public Statement[] ToArray() {
#if DOTNET2
			return statements.ToArray();
#else
			return (Statement[])statements.ToArray(typeof(Statement));
#endif
		}

		public bool Distinct { get { return distinct; } }
		
		public int StatementCount { get { return statements.Count; } }
		
		public Statement this[int index] {
			get {
				return statements[index];
			}
		}
		
#if DOTNET2
		System.Collections.Generic.IEnumerator<Statement> System.Collections.Generic.IEnumerable<Statement>.GetEnumerator() {
			return statements.GetEnumerator();
		}
#endif
		IEnumerator IEnumerable.GetEnumerator() {
			return statements.GetEnumerator();
		}
		
		public void Clear() {
			statements.Clear();
			statementsAboutSubject.Clear();
			statementsAboutObject.Clear();
			distinct = true;
		}
		
		private StatementList GetIndexArray(Hashtable from, Resource entity) {
			StatementList ret = (StatementList)from[entity];
			if (ret == null) {
				ret = new StatementList();
				from[entity] = ret;
			}
			return ret;
		}
		
		bool StatementSink.Add(Statement statement) {
			Add(statement);
			return true;
		}
		
		public void Add(Statement statement) {
			if (statement.AnyNull) throw new ArgumentNullException();
			if (checkForDuplicates && Contains(statement)) return;
			statements.Add(statement);
			if (isIndexed) {
				GetIndexArray(statementsAboutSubject, statement.Subject).Add(statement);
				GetIndexArray(statementsAboutObject, statement.Object).Add(statement);
			}
			if (!checkForDuplicates) distinct = false;
		}
		
		public void Import(StatementSource source) {
			bool newDistinct = checkForDuplicates || ((StatementCount==0) && source.Distinct);
			source.Select(this);
			distinct = newDistinct;
		}
		
		public void Remove(Statement statement) {
			if (statement.AnyNull) {
				for (int i = 0; i < statements.Count; i++) {
					Statement s = (Statement)statements[i];
					if (statement.Matches(s)) {
						statements.RemoveAt(i); i--;
						if (isIndexed) {
							GetIndexArray(statementsAboutSubject, s.Subject).Remove(s);
							GetIndexArray(statementsAboutObject, s.Object).Remove(s);
						}
					}
				}
			} else {
				statements.Remove(statement);
				if (isIndexed) {
					GetIndexArray(statementsAboutSubject, statement.Subject).Remove(statement);
					GetIndexArray(statementsAboutObject, statement.Object).Remove(statement);
				}
			}
		}
		
		public void RemoveAll(Statement[] statements) {
			foreach (Statement t in statements)
				Remove(t);
		}
		
		public Entity[] GetEntities() {
			Hashtable h = new Hashtable();
			foreach (Statement s in statements) {
				if (s.Subject != null) h[s.Subject] = h;
				if (s.Predicate != null) h[s.Predicate] = h;
				if (s.Object != null && s.Object is Entity) h[s.Object] = h;
				if (s.Meta != null && s.Meta != Statement.DefaultMeta) h[s.Meta] = h;
			}
			return (Entity[])new ArrayList(h.Keys).ToArray(typeof(Entity));
		}
		
		public Entity[] GetPredicates() {
			Hashtable h = new Hashtable();
			foreach (Statement s in statements)
				h[s.Predicate] = h;
			return (Entity[])new ArrayList(h.Keys).ToArray(typeof(Entity));
		}

		public Entity[] GetMetas() {
			Hashtable h = new Hashtable();
			foreach (Statement s in statements)
				h[s.Meta] = h;
			return (Entity[])new ArrayList(h.Keys).ToArray(typeof(Entity));
		}

		private void ShorterList(ref StatementList list1, StatementList list2) {
			if (list2.Count < list1.Count)
				list1 = list2;
		}
		
		public void Select(StatementSink result) {
			Select(Statement.All, result);
		}
		
		public void Select(Statement template, StatementSink result) {
			StatementList source = statements;
			
			// The first time select is called, turn indexing on for the store.
			// TODO: Perform this index in a background thread if there are a lot
			// of statements.
			if (!isIndexed && allowIndexing) {
				isIndexed = true;
				for (int i = 0; i < StatementCount; i++) {
					Statement statement = this[i];
					GetIndexArray(statementsAboutSubject, statement.Subject).Add(statement);
					GetIndexArray(statementsAboutObject, statement.Object).Add(statement);
				}
			}
			
			if (template.Subject != null) ShorterList(ref source, GetIndexArray(statementsAboutSubject, template.Subject));
			else if (template.Object != null) ShorterList(ref source, GetIndexArray(statementsAboutObject, template.Object));
			
			if (source == null) return;
			
			bool isRdfsMemberPredicate = (template.Predicate != null && template.Predicate.Uri != null
				&& template.Predicate.Uri == NS.RDFS + "member");
			if (isRdfsMemberPredicate)
				template.Predicate = null;
			
			for (int i = 0; i < source.Count; i++) {
				Statement statement = source[i];
				if (!template.Matches(statement))
					continue;
				
				if (isRdfsMemberPredicate && (statement.Predicate.Uri == null || !statement.Predicate.Uri.StartsWith(rdfli)))
					continue;
					
				if (!result.Add(statement)) return;
			}
		}

		public void Select(SelectFilter filter, StatementSink result) {
			ResSet
				s = filter.Subjects == null ? null : new ResSet(filter.Subjects),
				p = filter.Predicates == null ? null : new ResSet(filter.Predicates),
				o = filter.Objects == null ? null : new ResSet(filter.Objects),
				m = filter.Metas == null ? null : new ResSet(filter.Metas);
				
			foreach (Statement st in statements) {
				if (s != null && !s.Contains(st.Subject)) continue;
				if (p != null && !p.Contains(st.Predicate)) continue;
				if (o != null && !o.Contains(st.Object)) continue;
				if (m != null && !m.Contains(st.Meta)) continue;
				if (filter.LiteralFilters != null && !LiteralFilter.MatchesFilters(st.Object, filter.LiteralFilters, this)) continue;
				if (!result.Add(st)) return;
			}
		}
		
		public bool Contains(Resource r) {
			foreach (Statement s in statements) {
				if (s.Subject == r) return true;
				if (s.Predicate == r) return true;
				if (s.Object == r) return true;
				if (s.Meta == r) return true;
			}
			return false;
		}
		
		public bool Contains(Statement template) {
			return Store.DefaultContains(this, template);
		}

		public void Replace(Entity a, Entity b) {
			MemoryStore removals = new MemoryStore();
			MemoryStore additions = new MemoryStore();
			foreach (Statement statement in statements) {
				if ((statement.Subject != null && statement.Subject == a) || (statement.Predicate != null && statement.Predicate == a) || (statement.Object != null && statement.Object == a) || (statement.Meta != null && statement.Meta == a)) {
					removals.Add(statement);
					additions.Add(statement.Replace(a, b));
				}
			}
			RemoveAll(removals.ToArray());
			Import(additions);
		}
		
		public void Replace(Statement find, Statement replacement) {
			Remove(find);
			Add(replacement);
		}

		private string GetStoreGuid() {
			if (guid == null) guid = Guid.NewGuid().ToString("N");;
			return guid;
		}
		
		public string GetPersistentBNodeId(BNode node) {
			if (pbnodeToId == null) {
				pbnodeToId = new Hashtable();
				pbnodeFromId = new Hashtable();
			}
			if (pbnodeToId.ContainsKey(node)) return (string)pbnodeToId[node];
			string id = GetStoreGuid() + ":" + pbnodeToId.Count.ToString();
			pbnodeToId[node] = id;
			pbnodeFromId[id] = node;
			return id;
		}
		
		public BNode GetBNodeFromPersistentId(string persistentId) {
			if (pbnodeFromId == null) return null;
			return (BNode)pbnodeFromId[persistentId];
		}
	}

	}

}
