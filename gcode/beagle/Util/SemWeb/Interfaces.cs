using System;
using System.Collections;

using SemWeb.Util;

namespace SemWeb {
	public interface StatementSource {
		bool Distinct { get; }
		void Select(StatementSink sink);
	}
	
	public interface SelectableSource : StatementSource {
		bool Contains(Resource resource);
		bool Contains(Statement template);
		void Select(Statement template, StatementSink sink);
		void Select(SelectFilter filter, StatementSink sink);
	}

	public interface QueryableSource : SelectableSource {
		SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options);
		void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SemWeb.Query.QueryResultSink sink);
	}
	
	public interface StaticSource : SelectableSource {
		int StatementCount { get; }

		Entity[] GetEntities();
		Entity[] GetPredicates();
		Entity[] GetMetas();

		string GetPersistentBNodeId(BNode node);
		BNode GetBNodeFromPersistentId(string persistentId);
	}
	
	public interface StatementSink {
		bool Add(Statement statement);
	}

	public interface ModifiableSource : SelectableSource, StatementSink {
		void Clear();
		void Import(StatementSource source);
		void Remove(Statement template);
		void RemoveAll(Statement[] templates);
		void Replace(Entity find, Entity replacement);
		void Replace(Statement find, Statement replacement);
	}
	
	internal class StatementCounterSink : StatementSink {
		int counter = 0;
		
		public int StatementCount { get { return counter; } }
		
		public bool Add(Statement statement) {
			counter++;
			return true;
		}
	}

	internal class StatementExistsSink : StatementSink {
		bool exists = false;
		
		public bool Exists { get { return exists; } }
		
		public bool Add(Statement statement) {
			exists = true;
			return false;
		}
	}

	
}