/**
 * SQLStore.cs: An abstract implementation of an RDF triple store
 * using an SQL-based backend.  This class is extended by the
 * MySQLStore, SQLiteStore, and PostreSQLStore classes.
 *
 * The SQLStore creates three tables to store its data.  The tables
 * are organizes follows:
 *   table                columns
 * PREFIX_entites    id (int), value (case-sensitive string)
 * PREFIX_literals   id (int), value (case-sens. string), language (short case-insens. string),
                     datatype (case-sense. string), hash (28byte case-sense string)
 * PREFIX_statements subject (int), predicate (int), object (int), meta (int), objecttype (tiny int)
 *
 * Every resource (named node, bnode, and literal) is given a numeric ID.
 * Zero is reserved.  One is used for the bnode in the static field Statement.DefaultMeta.
 * The numbers for entities and literals are drawn from the same set of numbers,
 * so there cannot be an entity and a literal with the same ID.
 *
 * The subject, predicate, object, and meta columns in the _statements table
 * refer to the numeric IDs of resources.  objecttype is zero if the object
 * is an entity (named or blank), otherwise one if it is a literal.  Some databases
 * (i.e. MySQL) add a UNIQUE constraint over the subject, predicate, object,
 * and meta columns so that the database is guaranteed not to have duplicate
 * rows.  Not all databases will do this, though.
 *
 * All literals have a row in the _literals table.  The value, language, and
 * datatype columns have the obvious things.  Notably, the datatype column
 * is a string column, even though you might think of it as an entity that
 * should be in the entities table.  The hash column contains a SHA1 hash
 * over the three fields and is used to speed up look-ups for literals.  It
 * also is used for creating a UNIQUE index over the table.  Because literal
 * values can be arbitrarily long, creating a fixed-size hash is the only
 * way to reliably create a UNIQUE constraint over the table.  A literal
 * value is entered into this table at most once.  You can't have two rows
 * that have the exact same value, language, and datatype.
 *
 * The _entities table contains all *named* entities.  Basically it is just
 * a mapping between entity IDs in the _statements table and their URIs.
 * Importantly, bnodes are not represented in this table (it would be a
 * waste of space since they have nothing to map IDs to).  A UNIQUE constraint
 * is placed over the value column to ensure that a URI ends up in the table
 * at most once.  Because bnodes are not in this table, the only way to
 * get a list of them is to see what IDs are used in _statements that are
 * not in this table or in the _literals table.
 */

#if !SILVERLIGHT

using System;
using System.Collections;
using System.Collections.Specialized;
using System.Data;
using System.IO;
using System.Security.Cryptography;
using System.Text;

using SemWeb.Util;

namespace SemWeb.Stores {
	public abstract class SQLStore : QueryableSource, StaticSource, ModifiableSource, IDisposable {
		// Table initialization, etc.
		// --------------------------
	
		// This is a version number representing the current 'schema' implemented
		// by this class in case of future updates.
		int dbformat = 1;
	
		// 'table' is the prefix of the tables used by this store, i.e.
		// {table}_statements, {table}_literals, {table}_entities.  This is
		// set in the constructor.
		string table;
		
		// 'guid' is a GUID assigned to this store.  It is created the
		// first time the SQL table structure is made and is saved in
		// the info block of the literal with ID zero.
		string guid;
		
		// this flag tracks the first access to the backend, when it
		// creates tables and indexes if necessary
		bool firstUse = true;

		// Importing
		// ------------
		
		// The SQL store operates in two modes, isImporting == false and
		// isImporting == true.  The first mode is the usual mode, where
		// calls to Add are executed immediately.  The second mode, which
		// is activated by the Import() method, batches Add calls to make
		// insertions faster.  While importing, no public methods should be
		// called except by this class itself.
		bool isImporting = false;
		
		// Each time we need to add a resource, we need to find it an ID.
		// When importing, we track the next ID available in this field
		// and increment it as necessary.  When not importing, we do a
		// DB query to find the next available ID.
		int cachedNextId = -1;
		
		// These variables hold on to the IDs of literals and entities during
		// importing. They are periodically cleared.
		Hashtable entityCache = new Hashtable();
		Hashtable literalCache = new Hashtable();
		
		// This is a buffer of statements waiting to be processed.
		StatementList addStatementBuffer = null;
		
		// These track the performance of our buffer so we can adjust its size
		// on the fly to maximize performance.
		int importAddBufferSize = 200, importAddBufferRotation = 0;
		TimeSpan importAddBufferTime = TimeSpan.MinValue;
		
		// Other Flags
		// -----------
		
		// When adding a statement that has a bnode in it (not while importing),
		// we have to do a two-staged procedure.  This holds on to a list of
		// GUIDs that we've temporarily assigned to bnodes that are cleared
		// at the end of Add().
		ArrayList anonEntityHeldIds = new ArrayList();
		
		// Tracks whether any statements have been removed from the store by this
		// object.  When Close() is called, if true, the entities and literals
		// tables are cleaned up to remove unreferenced resoures.
		bool statementsRemoved = false;

		// Debugging flags from environment variables.
		static bool Debug = System.Environment.GetEnvironmentVariable("SEMWEB_DEBUG_SQL") != null;
		static bool DebugLogSpeed = System.Environment.GetEnvironmentVariable("SEMWEB_DEBUG_SQL_LOG_SPEED") != null;
		static string InitCommands = System.Environment.GetEnvironmentVariable("SEMWEB_SQL_INIT_COMMANDS");
		
		// This guy is reused in various calls to avoid allocating a new one of
		// these all the time.
		StringBuilder cmdBuffer = new StringBuilder();
		 
		// The quote character that surrounds strings in SQL statements.
		// Initialized in the constructor.
		char quote;
		
		// Ensure that calls to Select() and Query() are synchronized to make these methods thread-safe.
		object syncroot = new object();
		
		// Our SHA1 object which we use to create hashes of literal values.
		SHA1 sha = SHA1.Create();
		
		// This class is placed inside entities to cache their numeric IDs.
		private class ResourceKey {
			public int ResId;
			
			public ResourceKey(int id) { ResId = id; }
			
			public override int GetHashCode() { return ResId.GetHashCode(); }
			public override bool Equals(object other) { return (other is ResourceKey) && ((ResourceKey)other).ResId == ResId; }
		}
		
		// Some helpers.
		
		const string rdfs_member = NS.RDFS + "member";
		const string rdf_li = NS.RDF + "_";
		
		private static readonly string[] fourcols = new string[] { "subject", "predicate", "object", "meta" };
		private static readonly string[] predcol = new string[] { "predicate" };
		private static readonly string[] metacol = new string[] { "meta" };

		private string INSERT_INTO_LITERALS_VALUES { get { return "INSERT INTO " + table + "_literals VALUES "; } }
		private string INSERT_INTO_ENTITIES_VALUES { get { return "INSERT INTO " + table + "_entities VALUES "; } }
		private string INSERT_INTO_STATEMENTS_VALUES { get { return "INSERT " + (HasUniqueStatementsConstraint ? InsertIgnoreCommand : "") + " INTO " + table + "_statements VALUES "; } }
			

		// The constructor called by subclasses.
		protected SQLStore(string table) {
			this.table = table;
			
			quote = GetQuoteChar();
		}
		
		protected string TableName { get { return table; } }
		
		// The next few abstract and virtual methods allow implementors to control
		// what features the SQLStore takes advantage of and controls the SQL
		// language use.

		// See the API docs for more on these.
		protected abstract bool HasUniqueStatementsConstraint { get; } // may not return true unless INSERT (IGNORE COMMAND) is supported
		protected abstract string InsertIgnoreCommand { get; }
		protected abstract bool SupportsInsertCombined { get; }
		protected abstract bool SupportsSubquery { get; }
		protected virtual bool SupportsLimitClause { get { return true; } }
		protected virtual bool SupportsViews { get { return false; } }
		protected virtual int MaximumUriLength { get { return -1; } }
		
		protected abstract void CreateNullTest(string column, System.Text.StringBuilder command);
		protected abstract void CreateLikeTest(string column, string prefix, int method, System.Text.StringBuilder command);
			// method: 0 == startswith, 1 == contains, 2 == ends with
		
		protected virtual bool CreateEntityPrefixTest(string column, string prefix, System.Text.StringBuilder command) {
			command.Append('(');
			command.Append(column);
			command.Append(" IN (SELECT id from ");
			command.Append(TableName);
			command.Append("_entities WHERE ");
			CreateLikeTest("value", prefix, 0, command);
			command.Append("))");
			return true;
		}
		
		// If this is the first use, initialize the table and index structures.
		// CreateTable() will create tables if they don't already exist.
		// CreateIndexes() will only be run if this is a new database, so that
		// the user may customize the indexes after the table is first created
		// without SemWeb adding its own indexes the next time again.
		private void Init() {
			if (!firstUse) return;
			firstUse = false;
			
			CreateTable();
			if (CreateVersion()) // tests if this is a new table
				CreateIndexes();

			if (InitCommands != null)
				RunCommand(InitCommands);
		}
		
		// Creates the info block in the literal row with ID zero.  Returns true
		// if it created a new info block (i.e. this is a new database).
		private bool CreateVersion() {	
			string verdatastr = RunScalarString("SELECT value FROM " + table + "_literals WHERE id = 0");
			bool isNew = (verdatastr == null);
			
			NameValueCollection verdata = ParseVersionInfo(verdatastr);
			
			if (verdatastr != null && verdata["ver"] == null)
				throw new InvalidOperationException("The SQLStore adapter in this version of SemWeb cannot read databases created in previous versions.");
			
			verdata["ver"] = dbformat.ToString();
			
			if (verdata["guid"] == null) {
				guid = Guid.NewGuid().ToString("N");
				verdata["guid"] = guid;
			} else {
				guid = verdata["guid"];
			}
			
			string newverdata = SerializeVersionInfo(verdata);
			if (verdatastr == null)
				RunCommand("INSERT INTO " + table + "_literals (id, value) VALUES (0, " + Escape(newverdata, true) + ")");
			else if (verdatastr != newverdata)
				RunCommand("UPDATE " + table + "_literals SET value = " + Escape(newverdata, true) + " WHERE id = 0");
				
			return isNew;
		}
		
		NameValueCollection ParseVersionInfo(string verdata) {
			NameValueCollection nvc = new NameValueCollection();
			if (verdata == null) return nvc;
			foreach (string s in verdata.Split('\n')) {
				int c = s.IndexOf(':');
				if (c == -1) continue;
				nvc[s.Substring(0, c)] = s.Substring(c+1);
			}
			return nvc;
		}
		string SerializeVersionInfo(NameValueCollection verdata) {
			string ret = "";
			foreach (string k in verdata.Keys)
				ret += k + ":" + verdata[k] + "\n";
			return ret;
		}
		
		// Now we get to the Store implementation.
		
		// Why do we return true here?
		public bool Distinct { get { return true; } }
		
		public int StatementCount {
			get {
				Init();
				RunAddBuffer();
				return RunScalarInt("select count(subject) from " + table + "_statements", 0);
			}
		}
		
		public string GetStoreGuid() { return guid; }
		
		public string GetPersistentBNodeId(BNode node) {
			ResourceKey rk = (ResourceKey)GetResourceKey(node);
			if (rk == null) return null;
			return GetStoreGuid() + ":" + rk.ResId.ToString();
		}
		
		public BNode GetBNodeFromPersistentId(string persistentId) {
			try {
				int colon = persistentId.IndexOf(':');
				if (colon == -1) return null;
				if (GetStoreGuid() != persistentId.Substring(0, colon)) return null;
				int id = int.Parse(persistentId.Substring(colon+1));
				return (BNode)MakeEntity(id, null, null);
			} catch (Exception) {
				return null;
			}
		}
		
		// Returns the next ID available for a resource.  If we're importing,
		// use and increment the cached ID.  Otherwise, scan the tables for
		// the highest ID in use and use that plus one.  (We have to scan all
		// tables because bnode IDs are only in the statements table and
		// entities and literals may be orphaned so those are only in those tables.)
		private int NextId() {
			if (isImporting && cachedNextId != -1)
				return ++cachedNextId;
			
			RunAddBuffer();
			
			// The 0 id is not used.
			// The 1 id is reserved for Statement.DefaultMeta.
			int nextid = 2;
			
			CheckMax("select max(subject) from " + table + "_statements", ref nextid);
			CheckMax("select max(predicate) from " + table + "_statements", ref nextid);
			CheckMax("select max(object) from " + table + "_statements", ref nextid);
			CheckMax("select max(meta) from " + table + "_statements", ref nextid);
			CheckMax("select max(id) from " + table + "_literals", ref nextid);
			CheckMax("select max(id) from " + table + "_entities", ref nextid);
			
			cachedNextId = nextid;
			
			return nextid;
		}
		
		private void CheckMax(string command, ref int nextid) {
			int maxid = RunScalarInt(command, 0);
			if (maxid >= nextid) nextid = maxid + 1;
		}
		
		// Implements Store.Clear() by dropping the tables entirely.
		public void Clear() {
			// Drop the tables, if they exist.
			try { RunCommand("DROP TABLE " + table + "_statements;"); } catch (Exception) { }
			try { RunCommand("DROP TABLE " + table + "_literals;"); } catch (Exception) { }
			try { RunCommand("DROP TABLE " + table + "_entities;"); } catch (Exception) { }
			firstUse = true;
		
			Init();
			if (addStatementBuffer != null) addStatementBuffer.Clear();
			
			//RunCommand("DELETE FROM " + table + "_statements;");
			//RunCommand("DELETE FROM " + table + "_literals;");
			//RunCommand("DELETE FROM " + table + "_entities;");
		}
		
		// Computes a hash for a literal value to put in the hash column of the _literals table.
		private string GetLiteralHash(Literal literal) {
			byte[] data = System.Text.Encoding.Unicode.GetBytes(literal.ToString());
			byte[] hash = sha.ComputeHash(data);
			return Convert.ToBase64String(hash);
		}
		
		// Gets the ID of a literal in the database given an actual Literal object.
		// If create is false, return 0 if no such literal exists in the database.
		// Otherwise, create a row for the literal if none exists putting the
		// SQL insertion statement into the buffer argument.
		// If we're in isImporting mode, we expect that the literal's ID has already
		// been pre-fetched and put into literalCache (if the literal exists in the DB).
		private int GetLiteralId(Literal literal, bool create, StringBuilder buffer, bool insertCombined, ref bool firstInsert) {
			// Returns the literal ID associated with the literal.  If a literal
			// doesn't exist and create is true, a new literal is created,
			// otherwise 0 is returned.
			
			if (isImporting) {
				object ret = literalCache[literal];
				if (ret != null) return (int)ret;
			} else {
				StringBuilder b = cmdBuffer; cmdBuffer.Length = 0;
				b.Append("SELECT ");
				if (!SupportsLimitClause)
					b.Append("TOP 1 ");
				b.Append("id FROM ");
				b.Append(table);
				b.Append("_literals WHERE hash =");
				b.Append(quote);
				b.Append(GetLiteralHash(literal));
				b.Append(quote);
				if (SupportsLimitClause)
					b.Append(" LIMIT 1");
				b.Append(';');
				
				object id = RunScalar(b.ToString());
				if (id != null) return AsInt(id);
			}
				
			if (create) {
				int id = AddLiteral(literal, buffer, insertCombined, ref firstInsert);
				if (isImporting)
					literalCache[literal] = id;
				return id;
			}
			
			return 0;
		}
		
		// Creates the SQL command to add a literal to the _literals table.
		private int AddLiteral(Literal literal, StringBuilder buffer, bool insertCombined, ref bool firstInsert) {
			int id = NextId();
			
			StringBuilder b;
			if (buffer != null) {
				b = buffer;
			} else {
				b = cmdBuffer; cmdBuffer.Length = 0;
			}
			
			if (!insertCombined) {
				b.Append(INSERT_INTO_LITERALS_VALUES);
			} else {
				if (!firstInsert)
					b.Append(',');
				firstInsert = false;
			}
			b.Append('(');
			b.Append(id);
			b.Append(',');
			EscapedAppend(b, literal.Value);
			b.Append(',');
			if (literal.Language != null)
				EscapedAppend(b, literal.Language);
			else
				b.Append("NULL");
			b.Append(',');
			if (literal.DataType != null)
				EscapedAppend(b, literal.DataType);
			else
				b.Append("NULL");
			b.Append(',');
			b.Append(quote);
			b.Append(GetLiteralHash(literal));
			b.Append(quote);
			b.Append(')');
			if (!insertCombined)
				b.Append(';');
			
			if (buffer == null) {
				if (Debug) Console.Error.WriteLine(b.ToString());
				RunCommand(b.ToString());
			}
			
			return id;
		}

		// Gets the ID of an entity in the database given a URI.
		// If create is false, return 0 if no such entity exists in the database.
		// Otherwise, create a row for the entity if none exists putting the
		// SQL insertion statement into the entityInsertBuffer argument.
		// If we're in isImporting mode, we expect that the entity's ID has already
		// been pre-fetched and put into entityCache (if the entity exists in the DB).
		private int GetEntityId(string uri, bool create, StringBuilder entityInsertBuffer, bool insertCombined, bool checkIfExists, ref bool firstInsert) {
			// Returns the resource ID associated with the URI.  If a resource
			// doesn't exist and create is true, a new resource is created,
			// otherwise 0 is returned.
			
			int id;	
			
			if (isImporting) {
				object idobj = entityCache[uri];
				if (idobj == null && !create) return 0;
				if (idobj != null) return (int)idobj;
			} else if (checkIfExists) {
				StringBuilder cmd = cmdBuffer; cmdBuffer.Length = 0;
				cmd.Append("SELECT id FROM ");
				cmd.Append(table);
				cmd.Append("_entities WHERE value =");
				EscapedAppend(cmd, uri);
				id = RunScalarInt(cmd.ToString(), 0);
				if (id != 0 || !create) return id;
			}
			
			// If we got here, no such resource exists and create is true.
			
			if (MaximumUriLength != -1 && uri.Length > MaximumUriLength)
				throw new NotSupportedException("URI exceeds maximum length supported by data store.");

			id = NextId();
			
			StringBuilder b;
			if (entityInsertBuffer != null) {
				b = entityInsertBuffer;
			} else {
				b = cmdBuffer; cmdBuffer.Length = 0;
			}
			
			if (!insertCombined) {
				b.Append(INSERT_INTO_ENTITIES_VALUES);
			} else {
				if (!firstInsert)
					b.Append(',');
				firstInsert = false;
			}
			b.Append('(');
			b.Append(id);
			b.Append(',');
			EscapedAppend(b, uri);
			b.Append(')');
			if (!insertCombined)
				b.Append(';');
			
			if (entityInsertBuffer == null)
				RunCommand(b.ToString());
				
			// Add it to the URI map
					
			if (isImporting)
				entityCache[uri] = id;
			
			return id;
		}
		
		// Gets the ID of an entity in the database given a URI.
		// If create is false, return 0 if no such entity exists in the database.
		// Otherwise, create a row for the entity if none exists.
		private int GetResourceId(Resource resource, bool create) {
			bool firstLiteralInsert = true, firstEntityInsert = true;
			return GetResourceIdBuffer(resource, create, null, null, false, ref firstLiteralInsert, ref firstEntityInsert);
		}
		
		// Gets the ID of an entity or literal in the database given a Resource object.
		// If create is false, return 0 if no such entity exists in the database.
		// Otherwise, create a row for the resource if none exists putting the
		// SQL insertion statements into the literalInsertBuffer and entityInsertBuffer arguments.
		// If we're in isImporting mode, we expect that the resources's ID has already
		// been pre-fetched and put into one of the cache variables (if the resource exists in the DB).
		// If we're trying to get the ID for a bnode (i.e. we want to add it to the database),
		//   if we're isImporting, we know the next ID we can use -- increment and return that.
		//   otherwise we have to create a temporary row in the _entities table to hold onto
		//   the ID just until we add the statement, at which point the row can be removed.
		private int GetResourceIdBuffer(Resource resource, bool create, StringBuilder literalInsertBuffer, StringBuilder entityInsertBuffer, bool insertCombined, ref bool firstLiteralInsert, ref bool firstEntityInsert) {
			if (resource == null) return 0;
			
			if (resource is Literal) {
				Literal lit = (Literal)resource;
				return GetLiteralId(lit, create, literalInsertBuffer, insertCombined, ref firstLiteralInsert);
			}
			
			if (object.ReferenceEquals(resource, Statement.DefaultMeta))
				return 1;
			
			ResourceKey key = (ResourceKey)GetResourceKey(resource);
			if (key != null) return key.ResId;
			
			int id;
			
			if (resource.Uri != null) {
				id = GetEntityId(resource.Uri, create, entityInsertBuffer, insertCombined, true, ref firstEntityInsert);
			} else {
				// This anonymous node didn't come from the database
				// since it didn't have a resource key.  If !create,
				// then just return 0 to signal the resource doesn't exist.
				if (!create) return 0;

				if (isImporting) {
					// Can just increment the counter.
					id = NextId();
				} else {
					// We need to reserve an id for this resource so that
					// this function returns other ids for other anonymous
					// resources.  To do that, we'll insert a record
					// into the entities table with a GUID for the entity.
					// Inserting something into the table also gives us
					// concurrency, I hope.  Then, once the resource is
					// added to the statements table, this row can be
					// removed.
					string guid = "semweb-bnode-guid://taubz.for.net,2006/"
						+ Guid.NewGuid().ToString("N");
					id = GetEntityId(guid, create, entityInsertBuffer, insertCombined, false, ref firstEntityInsert);
					anonEntityHeldIds.Add(id);
				}
			}
				
			if (id != 0)
				SetResourceKey(resource, new ResourceKey(id));
			return id;
		}

		// Gets the type of the Resource, 0 for entities; 1 for literals.
		private int ObjectType(Resource r) {
			if (r is Literal) return 1;
			return 0;
		}
		
		// Creates an entity given its ID and its URI, and put it into
		// the cache argument if the argument is not null.
		private Entity MakeEntity(int resourceId, string uri, Hashtable cache) {
			if (resourceId == 0)
				return null;
			if (resourceId == 1)
				return Statement.DefaultMeta;
			
			ResourceKey rk = new ResourceKey(resourceId);
			
			if (cache != null && cache.ContainsKey(rk))
				return (Entity)cache[rk];
			
			Entity ent;
			if (uri != null) {
				ent = new Entity(uri);
			} else {
				ent = new BNode();
			}
			
			SetResourceKey(ent, rk);
			
			if (cache != null)
				cache[rk] = ent;
				
			return ent;
		}
		
		// Adds a statement to the store.
		// If we're isImoprting, buffer the statement, and if the buffer is full,
		// run the buffer.
		// Otherwise, add it immediately.
		bool StatementSink.Add(Statement statement) {
			Add(statement);
			return true;
		}
		public void Add(Statement statement) {
			if (statement.AnyNull) throw new ArgumentNullException();
			
			if (addStatementBuffer != null) {
				addStatementBuffer.Add(statement);
				RunAddBufferDynamic();
				return;
			}
			
			Init();
			
			int subj = GetResourceId(statement.Subject, true);
			int pred = GetResourceId(statement.Predicate, true);
			int objtype = ObjectType(statement.Object);
			int obj = GetResourceId(statement.Object, true);
			int meta = GetResourceId(statement.Meta, true);
			
			StringBuilder addBuffer = cmdBuffer; addBuffer.Length = 0;
			
			addBuffer.Append(INSERT_INTO_STATEMENTS_VALUES);
			addBuffer.Append('(');

			addBuffer.Append(subj);
			addBuffer.Append(',');
			addBuffer.Append(pred);
			addBuffer.Append(',');
			addBuffer.Append(objtype);
			addBuffer.Append(',');
			addBuffer.Append(obj);
			addBuffer.Append(',');
			addBuffer.Append(meta);
			addBuffer.Append("); ");
			
			RunCommand(addBuffer.ToString());
			
			// Remove any entries in the entities table
			// for anonymous nodes.
			if (anonEntityHeldIds.Count > 0) {
				addBuffer.Length = 0;
				addBuffer.Append("DELETE FROM ");
				addBuffer.Append(table);
				addBuffer.Append("_entities where id IN (");
				bool first = true;
				foreach (int id in anonEntityHeldIds) {
					if (!first) addBuffer.Append(','); first = false;
					addBuffer.Append(id);
				}
				addBuffer.Append(')');
				RunCommand(addBuffer.ToString());
				anonEntityHeldIds.Clear();
			}
		}
		
		private void RunAddBufferDynamic() {
			// This complicated code here adjusts the size of the add
			// buffer dynamically to maximize performance.
			int thresh = importAddBufferSize;
			if (importAddBufferRotation == 1) thresh += 100; // experiment with changing
			if (importAddBufferRotation == 2) thresh -= 100; // the buffer size
			
			if (addStatementBuffer.Count >= thresh) {
				DateTime start = DateTime.Now;
				RunAddBuffer();
				TimeSpan duration = DateTime.Now - start;
				
				if (DebugLogSpeed)
					Console.Error.WriteLine(thresh + "\t" + thresh/duration.TotalSeconds);
				
				// If there was an improvement in speed, per statement, on an 
				// experimental change in buffer size, keep the change.
				if (importAddBufferRotation != 0
					&& duration.TotalSeconds/thresh < importAddBufferTime.TotalSeconds/importAddBufferSize
					&& thresh >= 200 && thresh <= 10000)
					importAddBufferSize = thresh;
				importAddBufferTime = duration;
				importAddBufferRotation++;
				if (importAddBufferRotation == 3) importAddBufferRotation = 0;
			}
		}
		
		private void RunAddBuffer() {
			if (addStatementBuffer == null || addStatementBuffer.Count == 0) return;
			
			bool insertCombined = SupportsInsertCombined;
			
			Init();
			
			// Prevent recursion through NextId=>StatementCount
			StatementList statements = addStatementBuffer;
			addStatementBuffer = null;
			
			try {
				// Prefetch the IDs of all entities mentioned in statements.
				StringBuilder cmd = new StringBuilder();
				cmd.Append("SELECT id, value FROM ");
				cmd.Append(table);
				cmd.Append("_entities WHERE value IN (");
				Hashtable entseen = new Hashtable();
				bool hasEnts = false;
				for (int i = 0; i < statements.Count; i++) {
					Statement s = (Statement)statements[i];
					for (int j = 0; j < 4; j++) {
						Entity ent = s.GetComponent(j) as Entity;
						if (ent == null || ent.Uri == null) continue;
						if (entityCache.ContainsKey(ent.Uri)) continue;
						if (entseen.ContainsKey(ent.Uri)) continue;
						
						if (hasEnts)
							cmd.Append(" , ");
						EscapedAppend(cmd, ent.Uri);
						hasEnts = true;
						entseen[ent.Uri] = ent.Uri;
					}
				}
				if (hasEnts) {
					cmd.Append(");");
					if (Debug) Console.Error.WriteLine(cmd.ToString());
					using (IDataReader reader = RunReader(cmd.ToString())) {
						while (reader.Read()) {
							int id = reader.GetInt32(0);
							string uri = AsString(reader[1]);
							entityCache[uri] = id;
						}
					}
				}
				
				
				// Prefetch the IDs of all literals mentioned in statements.
				cmd.Length = 0;
				cmd.Append("SELECT id, hash FROM ");
				cmd.Append(table);
				cmd.Append("_literals WHERE hash IN (");
				bool hasLiterals = false;
				Hashtable litseen = new Hashtable();
				for (int i = 0; i < statements.Count; i++) {
					Statement s = (Statement)statements[i];
					Literal lit = s.Object as Literal;
					if (lit == null) continue;
					if (literalCache.ContainsKey(lit)) continue;
					
					string hash = GetLiteralHash(lit);
					if (litseen.ContainsKey(hash)) continue;
					
					if (hasLiterals)
						cmd.Append(" , ");
					cmd.Append(quote);
					cmd.Append(hash);
					cmd.Append(quote);
					hasLiterals = true;
					litseen[hash] = lit;
				}
				if (hasLiterals) {
					cmd.Append(");");
					using (IDataReader reader = RunReader(cmd.ToString())) {
						while (reader.Read()) {
							int id = reader.GetInt32(0);
							string hash = AsString(reader[1]);
							Literal lit = (Literal)litseen[hash];
							literalCache[lit] = id;
						}
					}
				}
				
				StringBuilder entityInsertions = new StringBuilder();
				StringBuilder literalInsertions = new StringBuilder();
				if (insertCombined) entityInsertions.Append(INSERT_INTO_ENTITIES_VALUES);
				if (insertCombined) literalInsertions.Append(INSERT_INTO_LITERALS_VALUES);
				int entityInsertionsInitialLength = entityInsertions.Length;
				int literalInsertionsInitialLength = literalInsertions.Length;
				bool firstLiteralInsert = true, firstEntityInsert = true; // only used if insertCombined is true
				
				cmd = new StringBuilder();
				if (insertCombined)
					cmd.Append(INSERT_INTO_STATEMENTS_VALUES);

				for (int i = 0; i < statements.Count; i++) {
					Statement statement = (Statement)statements[i];
				
					int subj = GetResourceIdBuffer(statement.Subject, true, literalInsertions, entityInsertions, insertCombined, ref firstLiteralInsert, ref firstEntityInsert);
					int pred = GetResourceIdBuffer(statement.Predicate, true,  literalInsertions, entityInsertions, insertCombined, ref firstLiteralInsert, ref firstEntityInsert);
					int objtype = ObjectType(statement.Object);
					int obj = GetResourceIdBuffer(statement.Object, true, literalInsertions, entityInsertions, insertCombined, ref firstLiteralInsert, ref firstEntityInsert);
					int meta = GetResourceIdBuffer(statement.Meta, true, literalInsertions, entityInsertions, insertCombined, ref firstLiteralInsert, ref firstEntityInsert);
					
					if (!insertCombined)
						cmd.Append(INSERT_INTO_STATEMENTS_VALUES);
					
					cmd.Append('(');
					cmd.Append(subj);
					cmd.Append(',');
					cmd.Append(pred);
					cmd.Append(',');
					cmd.Append(objtype);
					cmd.Append(',');
					cmd.Append(obj);
					cmd.Append(',');
					cmd.Append(meta);
					if (i == statements.Count-1 || !insertCombined)
						cmd.Append(");");
					else
						cmd.Append("),");
				}
				
				if (literalInsertions.Length > literalInsertionsInitialLength) {
					if (insertCombined)
						literalInsertions.Append(';');
					if (Debug) Console.Error.WriteLine(literalInsertions.ToString());
					RunCommand(literalInsertions.ToString());
				}
				
				if (entityInsertions.Length > entityInsertionsInitialLength) {
					if (insertCombined)
						entityInsertions.Append(';');
					if (Debug) Console.Error.WriteLine(entityInsertions.ToString());
					RunCommand(entityInsertions.ToString());
				}
				
				if (Debug) Console.Error.WriteLine(cmd.ToString());
				RunCommand(cmd.ToString());
			
			} finally {
				// Clear the array and reuse it.
				statements.Clear();
				addStatementBuffer = statements;
				entityCache.Clear();
				literalCache.Clear();
			}
		}
		
		public void Remove(Statement template) {
			Init();
			RunAddBuffer();

			System.Text.StringBuilder cmd = new System.Text.StringBuilder("DELETE FROM ");
			cmd.Append(table);
			cmd.Append("_statements ");
			if (!WhereClause(template, cmd)) return;
			cmd.Append(';');
			
			RunCommand(cmd.ToString());
			
			statementsRemoved = true;
		}
		
		public void RemoveAll(Statement[] templates) {
			// TODO: Optimize this.
			foreach (Statement t in templates)
				Remove(t);
		}
		
		public Entity[] GetEntities() {
			return GetAllEntities(fourcols);
		}
			
		public Entity[] GetPredicates() {
			return GetAllEntities(predcol);
		}
		
		public Entity[] GetMetas() {
			return GetAllEntities(metacol);
		}

		private Entity[] GetAllEntities(string[] cols) {
			RunAddBuffer();
			ArrayList ret = new ArrayList();
			Hashtable seen = new Hashtable();
			foreach (string col in cols) {
				using (IDataReader reader = RunReader("SELECT " + col + ", value FROM " + table + "_statements LEFT JOIN " + table + "_entities ON " + col + "=id " + (col == "object" ? " WHERE objecttype=0" : "") + " GROUP BY " + col + ";")) {
					while (reader.Read()) {
						int id = reader.GetInt32(0);
						if (id == 1 && col == "meta") continue; // don't return DefaultMeta in meta column.
						
						if (seen.ContainsKey(id)) continue;
						seen[id] = seen;
						
						string uri = AsString(reader[1]);
						ret.Add(MakeEntity(id, uri, null));
					}
				}
			}
			return (Entity[])ret.ToArray(typeof(Entity));;
		}
		
		private bool WhereItem(string col, Resource r, System.Text.StringBuilder cmd, bool and) {
			if (and) cmd.Append(" and ");
			
			if (r is MultiRes) {
				cmd.Append('(');
				cmd.Append(col);
				cmd.Append(" IN (");
				if (!AppendMultiRes((MultiRes)r, cmd)) return false;
				cmd.Append(" ))");
			} else {
				if (r.Uri != null && r.Uri == rdfs_member) {
					if (CreateEntityPrefixTest(col, rdf_li, cmd)) return true;
				}
				
				int id = GetResourceId(r, false);
				if (id == 0) return false;
				if (Debug) Console.Error.WriteLine("(" + id + " " + r + ")");
				cmd.Append('(');
				cmd.Append(col);
				cmd.Append('=');
				cmd.Append(id);
				cmd.Append(')');
			}
			
			return true;
		}
		
		private bool AppendMultiRes(MultiRes r, StringBuilder cmd) {
			bool first = true;
			for (int i = 0; i < r.items.Length; i++) {
				int id = GetResourceId(r.items[i], false);
				if (id == 0) continue;
				if (!first) cmd.Append(','); first = false;
				cmd.Append(id);
			}
			if (first) return false; // none are in the store
			return true;
		}
		
		private bool WhereClause(Statement template, System.Text.StringBuilder cmd) {
			bool ww;
			return WhereClause(template.Subject, template.Predicate, template.Object, template.Meta, cmd, out ww);
		}

		private bool WhereClause(Resource templateSubject, Resource templatePredicate, Resource templateObject, Resource templateMeta, System.Text.StringBuilder cmd, out bool wroteWhere) {
			if (templateSubject == null && templatePredicate == null && templateObject == null && templateMeta == null) {
				wroteWhere = false;
				return true;
			}
			
			wroteWhere = true;
			cmd.Append(" WHERE ");
			
			if (templateSubject != null)
				if (!WhereItem("subject", templateSubject, cmd, false)) return false;
			
			if (templatePredicate != null)
				if (!WhereItem("predicate", templatePredicate, cmd, templateSubject != null)) return false;
			
			if (templateObject != null)
				if (!WhereItem("object", templateObject, cmd, templateSubject != null || templatePredicate != null)) return false;
			
			if (templateMeta != null)
				if (!WhereItem("meta", templateMeta, cmd, templateSubject != null || templatePredicate != null || templateObject != null)) return false;
			
			return true;
		}
		
		private int AsInt(object r) {
			if (r is int) return (int)r;
			if (r is uint) return (int)(uint)r;
			if (r is long) return (int)(long)r;
			if (r is string) return int.Parse((string)r);
			throw new ArgumentException(r.ToString());
		}
		
		private string AsString(object r) {
			if (r == null)
				return null;
			else if (r is System.DBNull)
				return null;
			else if (r is string)
				return (string)r;
			else if (r is byte[])
				return System.Text.Encoding.UTF8.GetString((byte[])r);
			else
				throw new FormatException("SQL store returned a literal value as " + r.GetType());
		}
		
		private static void AppendComma(StringBuilder builder, string text, bool comma) {
			if (comma)
				builder.Append(',');
			builder.Append(text);
		}
		
		///////////////////////////
		// QUERYING THE DATABASE //
		///////////////////////////
		
		public bool Contains(Resource resource) {
			return GetResourceId(resource, false) != 0;
		}
		
		public bool Contains(Statement template) {
			return Store.DefaultContains(this, template);
		}

		internal struct SelectColumnFilter {
			public bool SubjectId, PredicateId, ObjectId, MetaId;
			public bool SubjectUri, PredicateUri, ObjectData, MetaUri;
		}
	
		private static void SelectFilterColumns(SelectColumnFilter filter, StringBuilder cmd) {
			bool f = true;
			
			if (filter.SubjectId) { cmd.Append("q.subject"); f = false; }
			if (filter.PredicateId) { AppendComma(cmd, "q.predicate", !f); f = false; }
			if (filter.ObjectId) { AppendComma(cmd, "q.object", !f); f = false; }
			if (filter.MetaId) { AppendComma(cmd, "q.meta", !f); f = false; }
			if (filter.SubjectUri) { AppendComma(cmd, "suri.value", !f); f = false; }
			if (filter.PredicateUri) { AppendComma(cmd, "puri.value", !f); f = false; }
			if (filter.ObjectData) { AppendComma(cmd, "q.objecttype, ouri.value, lit.value, lit.language, lit.datatype", !f); f = false; }
			if (filter.MetaUri) { AppendComma(cmd, "muri.value", !f); f = false; }
		}
		
		private void SelectFilterTables(SelectColumnFilter filter, StringBuilder cmd) {
			if (filter.SubjectUri) {
				cmd.Append(" LEFT JOIN ");
				cmd.Append(table);
				cmd.Append("_entities AS suri ON q.subject = suri.id");
			}
			if (filter.PredicateUri) {
				cmd.Append(" LEFT JOIN ");
				cmd.Append(table);
				cmd.Append("_entities AS puri ON q.predicate = puri.id");
			}
			if (filter.ObjectData) {
				cmd.Append(" LEFT JOIN ");
				cmd.Append(table);
				cmd.Append("_entities AS ouri ON q.object = ouri.id");

				cmd.Append(" LEFT JOIN ");
				cmd.Append(table);
				cmd.Append("_literals AS lit ON q.object=lit.id");
			}
			if (filter.MetaUri) {
				cmd.Append(" LEFT JOIN ");
				cmd.Append(table);
				cmd.Append("_entities AS muri ON q.meta = muri.id");
			}
		}
		
		public void Select(StatementSink result) {
			Select(Statement.All, result);
		}

		public void Select(SelectFilter filter, StatementSink result) {
			if (result == null) throw new ArgumentNullException();
			foreach (Entity[] s in SplitArray(filter.Subjects))
			foreach (Entity[] p in SplitArray(filter.Predicates))
			foreach (Resource[] o in SplitArray(filter.Objects))
			foreach (Entity[] m in SplitArray(filter.Metas))
			{
				Select(
					ToMultiRes(s),
					ToMultiRes(p),
					ToMultiRes(o),
					ToMultiRes(m),
					filter.LiteralFilters,
					result,
					filter.Limit); // hmm, repeated
			}
		}
		
		Resource[][] SplitArray(Resource[] e) {
			int lim = 1000;
			if (e == null || e.Length <= lim) {
				if (e is Entity[])
					return new Entity[][] { (Entity[])e };
				else
					return new Resource[][] { e };
			}
			int overflow = e.Length % lim;
			int n = (e.Length / lim) + ((overflow != 0) ? 1 : 0);
			Resource[][] ret;
			if (e is Entity[]) ret = new Entity[n][]; else ret = new Resource[n][];
			for (int i = 0; i < n; i++) {
				int c = lim;
				if (i == n-1 && overflow != 0) c = overflow;
				if (e is Entity[]) ret[i] = new Entity[c]; else ret[i] = new Resource[c];
				Array.Copy(e, i*lim, ret[i], 0, c);
			}
			return ret;
		}
		
		Resource ToMultiRes(Resource[] r) {
			if (r == null || r.Length == 0) return null;
			if (r.Length == 1) return r[0];
			return new MultiRes(r);
		}
		
		private class MultiRes : Resource {
			public MultiRes(Resource[] a) { items = a; }
			public Resource[] items;
			public override string Uri { get { return null; } }
			public bool ContainsLiterals() {
				foreach (Resource r in items)
					if (r is Literal)
						return true;
				return false;
			}
		}
		
		void CleanMultiRes(MultiRes res) {
			ArrayList newitems = new ArrayList();
			foreach (Resource r in res.items)
				if ((object)r == (object)Statement.DefaultMeta || GetResourceKey(r) != null)
					newitems.Add(r);
			res.items = (Resource[])newitems.ToArray(typeof(Resource));
		}
		
		void CacheMultiObjects(Hashtable entMap, Resource obj) {
			if (!(obj is MultiRes)) return;
			foreach (Resource r in ((MultiRes)obj).items)
				entMap[GetResourceId(r, false)] = r;
		}
		
		bool isOrContains(Resource r, string uri) {
			if (r == null) return false;
			if (r is MultiRes) {
				foreach (Resource rr in ((MultiRes)r).items)
					if (isOrContains(rr, uri))
						return true;
			} else {
				if (r.Uri != null && r.Uri == uri)
					return true;
			}
			return false;
		}
		
		void PrefetchResourceIds(IList resources) {
			Hashtable seen_e = new Hashtable();
			Hashtable seen_l = new Hashtable();
			Hashtable res_map = new Hashtable();
			
			int resStart = 0;
			while (resStart < resources.Count) {

			StringBuilder cmd_e = new StringBuilder();
			cmd_e.Append("SELECT id, value FROM ");
			cmd_e.Append(table);
			cmd_e.Append("_entities WHERE value IN (");
			bool hasEnts = false;

			StringBuilder cmd_l = new StringBuilder();
			cmd_l.Append("SELECT id, hash FROM ");
			cmd_l.Append(table);
			cmd_l.Append("_literals WHERE hash IN (");
			bool hasLiterals = false;

			int ctr = 0;
			while (resStart < resources.Count && ctr < 1000) {
				Resource r = (Resource)resources[resStart++];
				
				if ((object)r == (object)Statement.DefaultMeta || GetResourceKey(r) != null) // no need to prefetch
					continue;
					
				ctr++;
			
				if (r.Uri != null) {
					if (seen_e.ContainsKey(r.Uri)) {
						// We can only query for a URI once, but it might be that multiple objects
						// coming in have the same URI, so when we see a duplicate, associate the
						// duplicate with the first instance of the URI we saw.
						if ((object)r != (object)seen_e[r.Uri])
							res_map[r] = seen_e[r.Uri];
						continue;
					}
					if (hasEnts)
						cmd_e.Append(" , ");
					EscapedAppend(cmd_e, r.Uri);
					hasEnts = true;
					seen_e[r.Uri] = r;
				}

				Literal lit = r as Literal;
				if (lit != null) {
					string hash = GetLiteralHash(lit);
					if (seen_l.ContainsKey(hash)) {
						// We can only query for a literal value once, but it might be that multiple objects
						// coming in have the same value, so when we see a duplicate, associate the
						// duplicate with the first instance of the value we saw.
						if ((object)lit != (object)seen_l[hash])
							res_map[lit] = seen_l[hash];
						continue;
					}
					
					if (hasLiterals)
						cmd_l.Append(" , ");
					cmd_l.Append(quote);
					cmd_l.Append(hash);
					cmd_l.Append(quote);
					hasLiterals = true;
					seen_l[hash] = lit;
				}
			}
			if (hasEnts) {
				cmd_e.Append(");");
				if (Debug) Console.Error.WriteLine(cmd_e.ToString());
				using (IDataReader reader = RunReader(cmd_e.ToString())) {
					while (reader.Read()) {
						int id = reader.GetInt32(0);
						string uri = AsString(reader[1]);
						SetResourceKey((Entity)seen_e[uri], new ResourceKey(id));
					}
				}
			}
				
			if (hasLiterals) {
				cmd_l.Append(");");
				using (IDataReader reader = RunReader(cmd_l.ToString())) {
					while (reader.Read()) {
						int id = reader.GetInt32(0);
						string hash = AsString(reader[1]);
						SetResourceKey((Literal)seen_l[hash], new ResourceKey(id));
					}
				}
			}
			
			}
			
			foreach (Resource r in res_map.Keys) {
				SetResourceKey(r, GetResourceKey((Resource)res_map[r]));
			}
		}
		
		public void Select(Statement template, StatementSink result) {
			if (result == null) throw new ArgumentNullException();
			Select(template.Subject, template.Predicate, template.Object, template.Meta, null, result, 0);
		}

		private void Select(Resource templateSubject, Resource templatePredicate, Resource templateObject, Resource templateMeta, LiteralFilter[] litFilters, StatementSink result, int limit) {
			if (result == null) throw new ArgumentNullException();
	
			lock (syncroot) {
			
			Init();
			RunAddBuffer();
			
			// Don't select on columns that we already know from the template.
			// But grab the URIs and literal values for MultiRes selection.
			SelectColumnFilter columns = new SelectColumnFilter();
			columns.SubjectId = (templateSubject == null) || templateSubject is MultiRes;
			columns.PredicateId = (templatePredicate == null) || templatePredicate is MultiRes;
			columns.ObjectId = (templateObject == null) || templateObject is MultiRes;
			columns.MetaId = (templateMeta == null) || templateMeta is MultiRes;
			columns.SubjectUri = templateSubject == null;
			columns.PredicateUri = templatePredicate == null;
			columns.ObjectData = templateObject == null || (templateObject is MultiRes && ((MultiRes)templateObject).ContainsLiterals());
			columns.MetaUri = templateMeta == null;
			
			if (isOrContains(templatePredicate, rdfs_member)) {
				columns.PredicateId = true;
				columns.PredicateUri = true;
			}
			
			// Meta URIs tend to be repeated a lot, so we don't
			// want to ever select them from the database.
			// This preloads them, although it makes the first
			// select quite slow.
			/*if (templateMeta == null && SupportsSubquery) {
				LoadMetaEntities();
				columns.MetaUri = false;
			}*/
			
			// Have to select something
			bool fakeSubjectIdSelect = false;
			if (!columns.SubjectId && !columns.PredicateId && !columns.ObjectId && !columns.MetaId) {
				columns.SubjectId = true;
				fakeSubjectIdSelect = true;
			}
				
			// Pre-cache the IDs of resources in a MultiRes. TODO: Pool these into one array.
			foreach (Resource r in new Resource[] { templateSubject, templatePredicate, templateObject, templateMeta }) {
				MultiRes mr = r as MultiRes;
				if (mr == null) continue;
				PrefetchResourceIds(mr.items);
				CleanMultiRes(mr);
				if (mr.items.Length == 0) // no possible values
					return;
			}
				
			// SQLite has a problem with LEFT JOIN: When a condition is made on the
			// first table in the ON clause (q.objecttype=0/1), when it fails,
			// it excludes the row from the first table, whereas it should only
			// exclude the results of the join.
						
			System.Text.StringBuilder cmd = new System.Text.StringBuilder("SELECT ");
			if (!SupportsLimitClause && limit >= 1) {
				cmd.Append("TOP ");
				cmd.Append(limit);
				cmd.Append(' ');
			}
			if (!HasUniqueStatementsConstraint)
				cmd.Append("DISTINCT ");
			SelectFilterColumns(columns, cmd);
			cmd.Append(" FROM ");
			cmd.Append(table);
			cmd.Append("_statements AS q");
			SelectFilterTables(columns, cmd);
			cmd.Append(' ');
			
			bool wroteWhere;
			if (!WhereClause(templateSubject, templatePredicate, templateObject, templateMeta, cmd, out wroteWhere)) return;
			
			// Transform literal filters into SQL.
			if (litFilters != null) {
				foreach (LiteralFilter f in litFilters) {
					string s = FilterToSQL(f, "lit.value");
					if (s != null) {
						if (!wroteWhere) { cmd.Append(" WHERE "); wroteWhere = true; }
						else { cmd.Append(" AND "); }
						cmd.Append(' ');
						cmd.Append(s);
					}
				}
			}
			
			if (SupportsLimitClause && limit >= 1) {
				cmd.Append(" LIMIT ");
				cmd.Append(limit);
			}

			cmd.Append(';');
			
			if (Debug) {
				string cmd2 = cmd.ToString();
				//if (cmd2.Length > 80) cmd2 = cmd2.Substring(0, 80);
				Console.Error.WriteLine(cmd2);
			}
			
			Hashtable entMap = new Hashtable();
			
			// Be sure if a MultiRes is involved we hash the
			// ids of the entities so we can return them
			// without creating new ones.
			CacheMultiObjects(entMap, templateSubject);
			CacheMultiObjects(entMap, templatePredicate);
			CacheMultiObjects(entMap, templateObject);
			CacheMultiObjects(entMap, templateMeta);
			
			using (IDataReader reader = RunReader(cmd.ToString())) {
				while (reader.Read()) {
					int col = 0;
					int sid = -1, pid = -1, ot = -1, oid = -1, mid = -1;
					string suri = null, puri = null, ouri = null, muri = null;
					string lv = null, ll = null, ld = null;
					
					if (columns.SubjectId) { sid = reader.GetInt32(col++); }
					if (columns.PredicateId) { pid = reader.GetInt32(col++); }
					if (columns.ObjectId) { oid = reader.GetInt32(col++); }
					if (columns.MetaId) { mid = reader.GetInt32(col++); }
					
					if (columns.SubjectUri) { suri = AsString(reader[col++]); }
					if (columns.PredicateUri) { puri = AsString(reader[col++]); }
					if (columns.ObjectData) { ot = reader.GetInt32(col++); ouri = AsString(reader[col++]); lv = AsString(reader[col++]); ll = AsString(reader[col++]); ld = AsString(reader[col++]);}
					if (columns.MetaUri) { muri = AsString(reader[col++]); }
					
					Entity subject = GetSelectedEntity(sid, suri, templateSubject, columns.SubjectId && !fakeSubjectIdSelect, columns.SubjectUri, entMap);
					Entity predicate = GetSelectedEntity(pid, puri, templatePredicate, columns.PredicateId, columns.PredicateUri, entMap);
					Resource objec = GetSelectedResource(oid, ot, ouri, lv, ll, ld, templateObject, columns.ObjectId, columns.ObjectData, entMap);
					Entity meta = GetSelectedEntity(mid, muri, templateMeta, columns.MetaId, columns.MetaUri, templateMeta != null ? entMap : null);

					if (litFilters != null && !LiteralFilter.MatchesFilters(objec, litFilters, this))
						continue;
						
					bool ret = result.Add(new Statement(subject, predicate, objec, meta));
					if (!ret) break;
				}
			}
			
			} // lock
		}
		
		public SemWeb.Query.MetaQueryResult MetaQuery(Statement[] graph, SemWeb.Query.QueryOptions options) {
			return new SemWeb.Inference.SimpleEntailment().MetaQuery(graph, options, this);
		}
		
		public void Query(Statement[] graph, SemWeb.Query.QueryOptions options, SemWeb.Query.QueryResultSink sink) {
			if (graph.Length == 0) throw new ArgumentException("graph array must have at least one element");
			
			// This method translates the graph pattern into a single SQL statement. Each graph statement
			// corresponds to a new use of the _statements table in the FROM clause. For instance:
			//     ?a foaf:knows ?b . ?b foaf:name ?c .
			// translates to
			//     SELECT
			//       g0.subject, v0.value,
			//       g0.object, v1.value,
			//       g1.object, v2.value, v2lit.value, v2lit.language, v2lit.datatype
			//     FROM
			//       db_tables as g0 LEFT JOIN db_entities AS v0 ON g0.subject=v0.id LEFT JOIN db_entities AS v1 ON g0.object=v1.id,
			//       db_tables as g1 LEFT JOIN db_entities AS v2 ON g1.object=v2.id LEFT JOIN db_literals AS v2lit ON g1.object=v2lit.id
			//     WHERE
			//       g0.predicate = <the id of the foaf:knows entity> AND
			//       g1.predicate = <the id of the foaf:name entity> AND
			//       g0.object = g1.subject
			//
			// If any variable column is an *undistinguished* variable --- which is to say that the caller
			// says it is a variable, but is not concerned with its values --- then we want to apply
			// DISTINCT to the SELECT statement. This is because while in the normal case we may get
			// duplicates, we expect that to not occur more than the caller expects, but in the latter
			// case there will often be many duplicates. Consider the SPARQL query:
			//      SELECT DISTINCT ?p WHERE { ?s ?p ?o }
			// to get a list of predicates in the dataset, which corresponds to the graph query
			//      ?s ?p ?o
			// where only ?p is distinguished.
			// This normally translates to:
			//     SELECT
			//       g0.predicate, v0.value,
			//     FROM
			//       db_tables as g0 LEFT JOIN db_entities AS v0 ON g0.predicate=v0.id
			// which of course is going to return a result for every triple in the database.
			// So we add DISTINCT to beginning ("SELECT DISTINCT").
			// Unfortunately, MySQL performs the DISTINCT bit only after the LEFT JOINs (which makes sense normally).
			// That means that MySQL is repeatedly fetching the URI values of the predicates and checking
			// if a new unique row has been created, and this is very slow. What we want is to get the distinct
			// IDs of the predicates first, and then get their URIs.
			// I first tried implementing this with VIEWs, but it didn't always speed things up, and it was
			// difficult to manage the creation and deletion of VIEWs.
			// So instead, in this case, we do the query in two parts. First we get the IDs of the variables,
			// and then we get their URIs.
			
			options = options.Clone(); // because we modify the knownvalues array
			
			// Order the variables mentioned in the graph.
			Variable[] varOrder;
			ResSet distinguishedVars = null;
			bool useDistinct = false;
			{
				if (options.DistinguishedVariables != null)
					distinguishedVars = new ResSet(options.DistinguishedVariables);
				else
					distinguishedVars = new ResSet();
			
				Hashtable seenvars = new Hashtable();
				foreach (Statement filter in graph) {
					for (int i = 0; i < 4; i++) {
						Resource r = filter.GetComponent(i);
						if (r == null)
							throw new ArgumentException("The graph may not have any null components.  Use Variables instead.");

						if (r is Variable) {
							if (options.DistinguishedVariables != null) {
								if (!distinguishedVars.Contains(r)) {
									// If we are omitting a column from the results because it is
									// not distinguished, and it's not a meta column, then we'll
									// use DISTINCT.
									if (i != 3)
										useDistinct = true;
										
									// Don't put this into seenvars.
									continue;
								}
							} else {
								distinguishedVars.Add(r); // all variables are distinguished
							}
							
							seenvars[r] = r;
						}
					}
				}
				
				varOrder = new Variable[seenvars.Count];
				int ctr = 0;
				foreach (Variable v in seenvars.Keys)
					varOrder[ctr++] = v;
			}
			
			// Set the initial bindings to the result sink

			sink.Init(varOrder);
			
			Hashtable varLitFilters = new Hashtable();
			
			// Prefetch the IDs of all resources mentioned in the graph and in variable known values.
			// For Resources in the graph that are not in the store, the query immediately fails.
			{
				ArrayList graphResources = new ArrayList();
				foreach (Statement s in graph) {
					for (int i = 0; i < 4; i++) {
						Resource r = s.GetComponent(i);
						if (!(r is BNode)) // definitely exclude variables, but bnodes are useless too
							graphResources.Add(r);
					}
				}
				if (options.VariableKnownValues != null)
					foreach (ICollection values in options.VariableKnownValues.Values)
						graphResources.AddRange(values);

				PrefetchResourceIds(graphResources);
				
				// Check resources in graph and fail fast if any is not in the store.
				foreach (Statement s in graph) {
					for (int i = 0; i < 4; i++) {
						Resource r = s.GetComponent(i);
						if (r is Variable) continue;
						if ((object)r != (object)Statement.DefaultMeta && GetResourceKey(r) == null) {
							sink.AddComments("Resource " + r + " is not contained in the data model.");
							sink.Finished();
							return;
						}
					}
				}
				
				// Check variable known values and remove any values not in the store.
				// Don't do any fail-fasting here because there might be entries in this
				// dictionary that aren't even used in this query (yes, poor design).
				// We check later anyway.
				if (options.VariableKnownValues != null) {
					#if !DOTNET2
					foreach (Variable v in new ArrayList(options.VariableKnownValues.Keys)) {
					#else
					foreach (Variable v in new System.Collections.Generic.List<Variable>(options.VariableKnownValues.Keys)) {
					#endif
						#if !DOTNET2
						ArrayList newvalues = new ArrayList();
						#else
						System.Collections.Generic.List<Resource> newvalues = new System.Collections.Generic.List<Resource>();
						#endif
						
						foreach (Resource r in (ICollection)options.VariableKnownValues[v]) {
							if ((object)r == (object)Statement.DefaultMeta || GetResourceKey(r) != null)
								newvalues.Add(r);
						}

						options.VariableKnownValues[v] = newvalues;
					}
				}
			}
			
			// Helpers
			
			string[] colnames = { "subject", "predicate", "object", "meta" };
			
			// we initialize these things while locked, but use them after we release the lock
			ArrayList results = new ArrayList();
			Hashtable resourceCache = new Hashtable(); // map resource ID to Resource instances
						
			// Lock the store and make sure we are initialized and any pending add's have been committed. 

			lock (syncroot) {
			
			Init();
			RunAddBuffer();
			
			// Compile the SQL statement.

			Hashtable varRef = new Hashtable(); // the column name representing the variable, as in "g0.subject"
			Hashtable varRef2 = new Hashtable(); // the index of the variable, for accessing the entities and literals joined tables
			Hashtable varSelectedLiteral = new Hashtable(); // whether the variable is in a literal column and a LEFT JOIN for the literals table was used for it
			Hashtable varCouldBeLiteral = new Hashtable(); // whether the variable is only in literal columns
			Hashtable varSelectedEntity = new Hashtable(); // whether a LEFT JOIN for the entities table was used for a variable
			
			StringBuilder fromClause = new StringBuilder();
			StringBuilder whereClause = new StringBuilder();
			
			for (int f = 0; f < graph.Length; f++) {
				// For each filter, we select FROM the statements table with an
				// alias: q#, where # is the filter's index.
				
				if (f > 0) fromClause.Append(',');
				fromClause.Append(table);
				fromClause.Append("_statements AS g");
				fromClause.Append(f);
				
				// For each component of the filter...
				
				for (int i = 0; i < 4; i++) {
					// This has the name of the column corresponding to this variable (i.e. "g1.predicate").
					string myRef = "g" + f + "." + colnames[i];
					
					Variable v = graph[f].GetComponent(i) as Variable;
					if (v != null) {
						// If the component is a variable, then if this is
						// the first time we're seeing the variable, we don't
						// add any restrictions to the WHERE clause, but we
						// note the variable's "name" in the world of SQL
						// so we can refer back to it later and we add the
						// necessary FROM tables so we can get its URI and
						// literal value if it is a reported variable.
						// If this isn't the first time, then we add a WHERE restriction so
						// that the proper columns here and in a previous
						// filter are forced to have the same value.
					
						if (!varRef.ContainsKey(v)) {
							// This is the first time we are seeing this variable.
									
							// Record the column name for the variable (i.e. g0.subject).
							varRef[v] = myRef;
							
							// Record an index for the variable (i.e. 0, 1, 2, ...)
							int vIndex = varRef.Count;
							varRef2[v] = vIndex;
							
							varCouldBeLiteral[v] = (i == 2);
							
							// LEFT JOIN the entities table for this variable to get its URI
							// only if it is a distinguished variable and we are not using DISTINCT.
							varSelectedEntity[v] = false;
							if (!useDistinct && distinguishedVars.Contains(v)) {
								varSelectedEntity[v] = true; // Record that we are selecting the entities table for this variable.
								fromClause.Append(" LEFT JOIN ");
								fromClause.Append(table);
								fromClause.Append("_entities AS vent");
								fromClause.Append(vIndex);
								fromClause.Append(" ON ");
								fromClause.Append(myRef);
								fromClause.Append("=");
								fromClause.Append("vent" + vIndex + ".id ");
							}
									
							// LEFT JOIN the literals table for this variable:
							//    if it is in an object position
							//    to get its value, language, and datatype only if it is a distinguished variable and we are not using DISTINCT
							//    to apply a literal value filter (which will be done later)
							#if !DOTNET2
							bool hasLitFilter = (options.VariableLiteralFilters != null && options.VariableLiteralFilters[v] != null);
							#else
							bool hasLitFilter = (options.VariableLiteralFilters != null && options.VariableLiteralFilters.ContainsKey(v));
							#endif
							varSelectedLiteral[v] = false;
							if (i == 2 && ((!useDistinct && distinguishedVars.Contains(v)) || hasLitFilter)) {
								varSelectedLiteral[v] = true; // Record that we are selecting the literals table for this variable.
								fromClause.Append(" LEFT JOIN ");
								fromClause.Append(table);
								fromClause.Append("_literals AS vlit");
								fromClause.Append(vIndex);
								fromClause.Append(" ON ");
								fromClause.Append(myRef);
								fromClause.Append("=");
								fromClause.Append("vlit" + vIndex + ".id ");
							}
							
							// If this variable has known values, then we must restrict what values can appear using a WHERE clause.
							if (options.VariableKnownValues != null) {
								ICollection values = null;
								#if DOTNET2
								if (options.VariableKnownValues.ContainsKey(v))
								#endif
									values = (ICollection)options.VariableKnownValues[v];
								if (values != null) {
									if (values.Count == 0) {
										sink.Finished();
										return;
									}
									Resource r = ToMultiRes((Resource[])new ArrayList(values).ToArray(typeof(Resource)));
									if (!WhereItem(myRef, r, whereClause, whereClause.Length != 0)) {
										// We know at this point that the query cannot return any results.
										sink.Finished();
										return;
									}
								}
							}
							
						} else {
							// We've seen this variable before, so link up the column in this
							// statement to the corresponding column in a previous (or this) statement.
							if (whereClause.Length != 0) whereClause.Append(" AND ");
							whereClause.Append('(');
							whereClause.Append((string)varRef[v]);
							whereClause.Append('=');
							whereClause.Append(myRef);
							whereClause.Append(')');
							if (i != 2)
								varCouldBeLiteral[v] = false;
						}
					
					} else {
						// If this is not a variable, then it is a resource.
					
						// Append something into the WHERE clause to make sure this component gets
						// the right fixed value. If we cannot add the component to the WHERE clause
						// because the fixed value isn't even known in the data source, we can stop early.
						if (!WhereItem(myRef, graph[f].GetComponent(i), whereClause, whereClause.Length != 0)) {
							// We know at this point that the query cannot return any results.
							sink.Finished();
							return;
						}

					}
				}
			
			} // graph filter 0...n
			
			// Add literal filters to the WHERE clause

			foreach (Variable v in varOrder) {
				// Is there a literal value filter?
				if (options.VariableLiteralFilters == null) continue;
				#if !DOTNET2
				if (options.VariableLiteralFilters[v] == null) continue;
				#else
				if (!options.VariableLiteralFilters.ContainsKey(v)) continue;
				#endif
				
				// If this variable was not used in a literal column, then
				// we cannot filter its value. Really, it will never be a literal.
				if (!(bool)varSelectedLiteral[v]) continue;

				foreach (LiteralFilter filter in (ICollection)options.VariableLiteralFilters[v]) {
					string s = FilterToSQL(filter, "vlit" + (int)varRef2[v] + ".value");
					if (s == null) continue;

					if (whereClause.Length != 0) whereClause.Append(" AND ");
					whereClause.Append(s);
				}
			}

			// Put the parts of the SQL statement together

			StringBuilder cmd = new StringBuilder();
			
			cmd.Append("SELECT ");

			if (!SupportsLimitClause && options.Limit > 0) {
				cmd.Append("TOP ");
				cmd.Append(options.Limit);
				cmd.Append(' ');
			}
			
			if (useDistinct) cmd.Append("DISTINCT ");
			
			// Add all of the distinguished variables to the SELECT clause.
			bool firstvar = true;
			foreach (Variable v in varOrder) {
				if (!firstvar) cmd.Append(','); firstvar = false;
				
				cmd.Append((string)varRef[v]);
				
				if ((bool)varSelectedEntity[v]) {
					cmd.Append(", vent" + (int)varRef2[v] + ".value");
				}
				if ((bool)varSelectedLiteral[v]) {
					cmd.Append(", vlit" + (int)varRef2[v] + ".value");
					cmd.Append(", vlit" + (int)varRef2[v] + ".language");
					cmd.Append(", vlit" + (int)varRef2[v] + ".datatype");
				}
			}
			
			cmd.Append(" FROM ");
			cmd.Append(fromClause.ToString());
			
			if (whereClause.Length > 0)
				cmd.Append(" WHERE ");
			cmd.Append(whereClause.ToString());
			
			if (SupportsLimitClause && options.Limit > 0) {
				cmd.Append(" LIMIT ");
				cmd.Append(options.Limit);
			}
			
			cmd.Append(';');

			if (Debug) {
				string cmd2 = cmd.ToString();
				//if (cmd2.Length > 80) cmd2 = cmd2.Substring(0, 80);
				Console.Error.WriteLine(cmd2);
			}
			
			// Execute the query.
					
			// When we use DISTINCT and don't select URI and literal values at first,
			// we have to select them after. And since we can't maintain two IDataReaders
			// simultaneously, that means we have to pull the first set of results into
			// memory. It would be nice to not have to do that when we don't use DISTINCT,
			// but in practice it doesn't really matter since in SPARQL it's all sucked
			// into memory anyway.
					
			using (IDataReader reader = RunReader(cmd.ToString())) {
				while (reader.Read()) {
					QueryResultRowVariable[] row = new QueryResultRowVariable[varOrder.Length];
					results.Add(row);
					
					int col = 0;
					for (int i = 0; i < varOrder.Length; i++) {
						Variable v = varOrder[i];
						
						row[i].id = reader.GetInt32(col++);
						if ((bool)varSelectedEntity[v]) {
							row[i].uri = AsString(reader[col++]);
						}
						if ((bool)varSelectedLiteral[v]) {
							row[i].litvalue = AsString(reader[col++]);
							row[i].litlanguage = AsString(reader[col++]);
							row[i].litdatatype = AsString(reader[col++]);
						}
					}
				}
			}
		
			// For any distinguished variable that we did not select URIs or literal values for,
			// select that information now.
			
			for (int i = 0; i < varOrder.Length; i++) {
				Variable v = varOrder[i];
			
				if ((bool)varSelectedEntity[v] && (!(bool)varCouldBeLiteral[v] || (bool)varSelectedLiteral[v])) continue;
				
				// Get the list of resource IDs found for this variable.
				ArrayList rids = new ArrayList();
				foreach (QueryResultRowVariable[] row in results) {
					if (row[i].id <= 1) continue; // can't fetch for Statement.DefaultMeta
					if (resourceCache.ContainsKey(row[i].id)) continue; // we've already fetched it
					rids.Add(row[i].id); // probably no need to remove duplicates
				}
				
				if (rids.Count > 0) {
					// Fetch what we can for entities.
					if (!(bool)varSelectedEntity[v]) {
						StringBuilder cmd2 = new StringBuilder();
						cmd2.Append("SELECT id, value FROM ");
						cmd2.Append(table);
						cmd2.Append("_entities WHERE id IN (");
						bool first = true;
						foreach (int id in rids) {
							if (!first) cmd2.Append(','); first = false;
							cmd2.Append(id);
						}
						cmd2.Append(")");
						if (Debug) { Console.Error.WriteLine(cmd2.ToString()); }
						using (IDataReader reader = RunReader(cmd2.ToString())) {
							while (reader.Read()) {
								int id = reader.GetInt32(0);
								string uri = AsString(reader[1]);
								resourceCache[id] = MakeEntity(id, uri, null);
							}
						}
					}
					
					// Fetch what we can for literals.
					if ((bool)varCouldBeLiteral[v] && !(bool)varSelectedLiteral[v]) {
						StringBuilder cmd2 = new StringBuilder();
						cmd2.Append("SELECT id, value, language, datatype FROM ");
						cmd2.Append(table);
						cmd2.Append("_literals WHERE id IN (");
						bool first = true;
						foreach (int id in rids) {
							if (!first) cmd2.Append(','); first = false;
							cmd2.Append(id);
						}
						cmd2.Append(")");
						if (Debug) { Console.Error.WriteLine(cmd2.ToString()); }
						using (IDataReader reader = RunReader(cmd2.ToString())) {
							while (reader.Read()) {
								int id = reader.GetInt32(0);
								string value = AsString(reader[1]);
								string language = AsString(reader[2]);
								string datatype = AsString(reader[3]);
								Literal lit = new Literal(value, language, datatype);
								SetResourceKey(lit, new ResourceKey(id));
								resourceCache[id] = lit;
							}
						}
					}
					
					// Any ids not found so far are bnodes.
					foreach (int id in rids) {
						if (!resourceCache.ContainsKey(id)) {
							BNode b = new BNode();
							SetResourceKey(b, new ResourceKey(id));
							resourceCache[id] = b;
						}
					}
				}
			}
			
			} // lock
			
			// Now loop through the binding results.
			
			foreach (QueryResultRowVariable[] row in results) {
				bool match = true;
				Resource[] variableBindings = new Resource[varOrder.Length];
				
				for (int i = 0; i < varOrder.Length; i++) {
					int id = row[i].id;
					if (resourceCache.ContainsKey(id)) {
						variableBindings[i] = (Resource)resourceCache[id];
					} else {
						if (row[i].litvalue == null) {
							variableBindings[i] = MakeEntity(id, row[i].uri, null);
						} else {
							Literal lit = new Literal(row[i].litvalue, row[i].litlanguage, row[i].litdatatype);
							
							ArrayList litFilters = (ArrayList)varLitFilters[varOrder[i]];
							if (litFilters != null && !LiteralFilter.MatchesFilters(lit, (LiteralFilter[])litFilters.ToArray(typeof(LiteralFilter)), this)) {
								match = false;
								break;
							}
								
							SetResourceKey(lit, new ResourceKey(id));
							variableBindings[i] = lit;
						}
						
						// reuse this entity later
						resourceCache[id] = variableBindings[i];
					}
				}
				
				if (!match) continue;
				if (!sink.Add(new SemWeb.Query.VariableBindings(varOrder, variableBindings))) return;
			}
			
			sink.Finished();
		}
				
		private struct QueryResultRowVariable {
			public int id;
			public string uri;
			public string litvalue, litlanguage, litdatatype;
		}
		
		Entity GetSelectedEntity(int id, string uri, Resource given, bool idSelected, bool uriSelected, Hashtable entMap) {
			if (!idSelected) return (Entity)given;
			if (!uriSelected) {
				Entity ent = (Entity)entMap[id];
				if (ent != null)
					return ent; // had a URI so was precached, or was otherwise precached
				else // didn't have a URI
					return MakeEntity(id, null, entMap);
			}
			return MakeEntity(id, uri, entMap);
		}
		
		Resource GetSelectedResource(int id, int type, string uri, string lv, string ll, string ld, Resource given, bool idSelected, bool uriSelected, Hashtable entMap) {
			if (!idSelected) return (Resource)given;
			if (!uriSelected) return (Resource)entMap[id];
			if (type == 0) {
				return MakeEntity(id, uri, entMap);
			} else {
				Literal lit = new Literal(lv, ll, ld);
				SetResourceKey(lit, new ResourceKey(id));
				return lit;
			}
		}
		
		private string CreateLikeTest(string column, string match, int method) {
			StringBuilder s = new StringBuilder();
			CreateLikeTest(column, match, method, s);
			return s.ToString();
		}

		private string FilterToSQL(LiteralFilter filter, string col) {
			if (filter is SemWeb.Filters.StringCompareFilter) {
				SemWeb.Filters.StringCompareFilter f = (SemWeb.Filters.StringCompareFilter)filter;
				return col + FilterOpToSQL(f.Type) + Escape(f.Pattern, true);
			}
			if (filter is SemWeb.Filters.StringContainsFilter) {
				SemWeb.Filters.StringContainsFilter f = (SemWeb.Filters.StringContainsFilter)filter;
				return CreateLikeTest(col, f.Pattern, 1); // 1=contains
			}
			if (filter is SemWeb.Filters.StringStartsWithFilter) {
				SemWeb.Filters.StringStartsWithFilter f = (SemWeb.Filters.StringStartsWithFilter)filter;
				return CreateLikeTest(col, f.Pattern, 0); // 0=starts-with
			}
			if (filter is SemWeb.Filters.StringEndsWithFilter) {
				SemWeb.Filters.StringEndsWithFilter f = (SemWeb.Filters.StringEndsWithFilter)filter;
				return CreateLikeTest(col, f.Pattern, 2); // 2==ends-with
			}
			if (filter is SemWeb.Filters.NumericCompareFilter) {
				SemWeb.Filters.NumericCompareFilter f = (SemWeb.Filters.NumericCompareFilter)filter;
				return col + FilterOpToSQL(f.Type) + f.Number;
			}
			return null;
		}
		
		private string FilterOpToSQL(LiteralFilter.CompType op) {
			switch (op) {
			case LiteralFilter.CompType.LT: return " < ";
			case LiteralFilter.CompType.LE: return " <= ";
			case LiteralFilter.CompType.NE: return " <> ";
			case LiteralFilter.CompType.EQ: return " = ";
			case LiteralFilter.CompType.GT: return " > ";
			case LiteralFilter.CompType.GE: return " >= ";
			default: throw new ArgumentException(op.ToString());
			}			
		}
		
		private string Escape(string str, bool quotes) {
			if (str == null) return "NULL";
			StringBuilder b = new StringBuilder();
			EscapedAppend(b, str, quotes, false);
			return b.ToString();
		}
		
		protected void EscapedAppend(StringBuilder b, string str) {
			EscapedAppend(b, str, true, false);
		}

		protected virtual char GetQuoteChar() {
			return '\"';
		}
		protected virtual void EscapedAppend(StringBuilder b, string str, bool quotes, bool forLike) {
			if (quotes) b.Append(quote);
			for (int i = 0; i < str.Length; i++) {
				char c = str[i];
				switch (c) {
					case '\n': b.Append("\\n"); break;
					case '\\':
					case '\"':
					case '*':
					case '\'':
						b.Append('\\');
						b.Append(c);
						break;
					case '%':
					case '_':
						if (forLike)
							b.Append('\\');
						b.Append(c);
						break;
					default:
						b.Append(c);
						break;
				}
			}
			if (quotes) b.Append(quote);
		}
		
		/*internal static void Escape(StringBuilder b) {
			b.Replace("\\", "\\\\");
			b.Replace("\"", "\\\"");
			b.Replace("\n", "\\n");
			b.Replace("%", "\\%");
			b.Replace("*", "\\*");
		}*/

		public void Import(StatementSource source) {
			if (source == null) throw new ArgumentNullException();
			if (isImporting) throw new InvalidOperationException("Store is already importing.");
			
			Init();
			RunAddBuffer();
			
			cachedNextId = -1;
			NextId(); // get this before starting transaction because it relies on indexes which may be disabled
			
			addStatementBuffer = new StatementList();
			
			BeginTransaction();
			
			try {
				isImporting = true;
				source.Select(this);
			} finally {
				RunAddBuffer();
				EndTransaction();
				
				addStatementBuffer = null;
				isImporting = false;

				entityCache.Clear();
				literalCache.Clear();
			}
		}

		public void Replace(Entity a, Entity b) {
			Init();
			RunAddBuffer();
			int id = GetResourceId(b, true);
			
			foreach (string col in fourcols) {
				StringBuilder cmd = new StringBuilder();
				cmd.Append("UPDATE ");
				cmd.Append(table);
				cmd.Append("_statements SET ");
				cmd.Append(col);
				cmd.Append('=');
				cmd.Append(id);
				if (!WhereItem(col, a, cmd, false)) return;
				cmd.Append(';');
				RunCommand(cmd.ToString());
			}

		}
		
		public void Replace(Statement find, Statement replacement) {
			if (find.AnyNull) throw new ArgumentNullException("find");
			if (replacement.AnyNull) throw new ArgumentNullException("replacement");
			if (find == replacement) return;
			
			Init();
			RunAddBuffer();

			int subj = GetResourceId(replacement.Subject, true);
			int pred = GetResourceId(replacement.Predicate, true);
			int objtype = ObjectType(replacement.Object);
			int obj = GetResourceId(replacement.Object, true);
			int meta = GetResourceId(replacement.Meta, true);

			StringBuilder cmd = cmdBuffer; cmd.Length = 0;
			
			cmd.Append("UPDATE ");
			cmd.Append(table);
			cmd.Append("_statements SET subject=");
			cmd.Append(subj);
			cmd.Append(", predicate=");
			cmd.Append(pred);
			cmd.Append(", objecttype=");
			cmd.Append(objtype);
			cmd.Append(", object=");
			cmd.Append(obj);
			cmd.Append(", meta=");
			cmd.Append(meta);
			cmd.Append(' ');
			
			if (!WhereClause(find, cmd))
				return;
			
			RunCommand(cmd.ToString());
		}
		
		private object GetResourceKey(Resource resource) {
			return resource.GetResourceKey(this);
		}

		private void SetResourceKey(Resource resource, object value) {
			resource.SetResourceKey(this, value);
		}

		protected abstract void RunCommand(string sql);
		protected abstract object RunScalar(string sql);
		protected abstract IDataReader RunReader(string sql);
		
		private int RunScalarInt(string sql, int def) {
			object ret = RunScalar(sql);
			if (ret == null) return def;
			if (ret is int) return (int)ret;
			try {
				return int.Parse(ret.ToString());
			} catch (FormatException) {
				return def;
			}
		}
		
		private string RunScalarString(string sql) {
			object ret = RunScalar(sql);
			if (ret == null) return null;
			if (ret is string) return (string)ret;
			if (ret is byte[]) return System.Text.Encoding.UTF8.GetString((byte[])ret);
			throw new FormatException("SQL store returned a literal value as " + ret);
		}
		
		void IDisposable.Dispose() {
			Close();
		}
		
		public virtual void Close() {
			if (statementsRemoved) {
				RunCommand("DELETE FROM " + table + "_literals where (select count(*) from " + table + "_statements where object=id) = 0 and id > 0");
				RunCommand("DELETE FROM " + table + "_entities where (select count(*) from " + table + "_statements where subject=id) = 0 and (select count(*) from " + table + "_statements where predicate=id) = 0 and (select count(*) from " + table + "_statements where object=id) = 0 and (select count(*) from " + table + "_statements where meta=id) = 0 ;");
			}
		}

		protected virtual void CreateTable() {
			foreach (string cmd in GetCreateTableCommands(table)) {
				try {
					RunCommand(cmd);
				} catch (Exception e) {
					if (Debug && e.Message.IndexOf("already exists") == -1) Console.Error.WriteLine(e);
				}
			}
		}
		
		protected virtual void CreateIndexes() {
			foreach (string cmd in GetCreateIndexCommands(table)) {
				try {
					RunCommand(cmd);
				} catch (Exception e) {
					if (Debug) Console.Error.WriteLine(e);
				}
			}
		}
		
		protected virtual void BeginTransaction() { }
		protected virtual void EndTransaction() { }
		
		internal static string[] GetCreateTableCommands(string table) {
			return new string[] {
				"CREATE TABLE " + table + "_statements" +
				"(subject int UNSIGNED NOT NULL, predicate int UNSIGNED NOT NULL, objecttype int NOT NULL, object int UNSIGNED NOT NULL, meta int UNSIGNED NOT NULL);",
				
				"CREATE TABLE " + table + "_literals" +
				"(id INT NOT NULL, value BLOB NOT NULL, language TEXT, datatype TEXT, hash BINARY(28), PRIMARY KEY(id));",
				
				"CREATE TABLE " + table + "_entities" +
				"(id INT NOT NULL, value BLOB NOT NULL, PRIMARY KEY(id));"
				};
		}
		
		internal static string[] GetCreateIndexCommands(string table) {
			return new string[] {
				"CREATE UNIQUE INDEX subject_full_index ON " + table + "_statements(subject, predicate, object, meta, objecttype);",
				"CREATE INDEX predicate_index ON " + table + "_statements(predicate, object);",
				"CREATE INDEX object_index ON " + table + "_statements(object);",
				"CREATE INDEX meta_index ON " + table + "_statements(meta);",
			
				"CREATE UNIQUE INDEX literal_index ON " + table + "_literals(hash);",
				"CREATE INDEX literal_value_index ON " + table + "_literals(value(20));",
				"CREATE UNIQUE INDEX entity_index ON " + table + "_entities(value(255));"
				};
		}
	}
	
}

#endif
