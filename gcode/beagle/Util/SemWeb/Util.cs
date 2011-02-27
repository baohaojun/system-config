using System;
using System.Collections;

using SemWeb;

namespace SemWeb.Util {

	internal class ResSet : ICollection {
		Hashtable items = new Hashtable();
		ICollection keys;
		
		public ResSet() {
		}
		
		#if !DOTNET2
		public ResSet(ICollection items) {
		#else
		public ResSet(System.Collections.Generic.ICollection<Resource> items) {
		#endif
			AddRange(items);
		}

		#if DOTNET2
		// this is for some call in SQLStore; it seems to having a generics casting issue that I don't know if it's a mono bug or what...
		internal ResSet(System.Collections.Generic.ICollection<Variable> items) {
			if (items == null) return;
			foreach (Resource r in items)
				Add(r);
		}
		#endif

		private ResSet(Hashtable items) {
			this.items = items;
		}

		public void Add(Resource res) {
			items[res] = items;
			keys = null;
		}
		
		#if !DOTNET2
		public void AddRange(ICollection items) {
		#else
		public void AddRange<T>(System.Collections.Generic.ICollection<T> items) where T : Resource {
		#endif
			if (items == null) return;
			foreach (Resource r in items)
				Add(r);
		}
			
		public void Remove(Resource res) {
			items.Remove(res);
			keys = null;
		}
		
		public bool Contains(Resource res) {
			return items.ContainsKey(res);
		}
		
		public ICollection Items {
			get {
				if (keys == null)
					keys = items.Keys;
				return keys;
			}
		}
		
		public void AddRange(ResSet set) {
			if (set == null) return;
			foreach (Resource r in set.Items) {
				Add(r);
			}
		}
		
		public void Clear() {
			items.Clear();
			keys = null;
		}
		
		public ResSet Clone() {
			return new ResSet((Hashtable)items.Clone());
		}
		
		public int Count { get { return items.Count; } }
		
		public IEnumerator GetEnumerator() { return items.Keys.GetEnumerator(); }
		
		bool ICollection.IsSynchronized { get { return false; } }
		object ICollection.SyncRoot { get { return null; } }
		
		public void CopyTo(System.Array array, int index) {
			foreach (Resource r in this)
				array.SetValue(r, index++);
		}
		
		public Resource[] ToArray() {
			Resource[] ret = new Resource[Count];
			CopyTo(ret, 0);
			return ret;
		}
		
		public Entity[] ToEntityArray() {
			Entity[] ret = new Entity[Count];
			CopyTo(ret, 0);
			return ret;
		}
		
		public void RetainAll(ResSet set) {
			foreach (Resource r in new ArrayList(this))
				if (!set.Contains(r))
					Remove(r);
		}

		/*Hashtable Intersect(Hashtable x, Hashtable y) {
			Hashtable a, b;
			if (x.Count < y.Count) { a = x; b = y; }
			else { b = x; a = y; }
			Hashtable c = new Hashtable();
			foreach (Resource r in a)
				if (b.ContainsKey(r))
					c[r] = c;
			return c;
		}*/		
	}
	
	public class DistinctStatementsSink : StatementSink {
		StatementSink sink;
		StatementMap hash;
		bool resetMeta;
		public DistinctStatementsSink(StatementSink sink, bool resetMeta) {
			this.sink = sink;
			hash = new StatementMap();
			this.resetMeta = resetMeta;
		}
		public bool Add(Statement s) {
			if (resetMeta) s.Meta = Statement.DefaultMeta;
			if (hash.ContainsKey(s)) return true;
			hash[s] = hash;
			return sink.Add(s);
		}
	}
	

	public class StatementList : ICollection {
		private const int DefaultInitialCapacity = 0x10;
		private int _size;
		private Statement[] _items;

		public StatementList() {
			_items = new Statement[DefaultInitialCapacity];
		}		

		public StatementList(Statement[] statements) {
			_items = (Statement[])statements.Clone();
			_size = _items.Length;
		}		

		public Statement this[int index] { 
			get { return _items[index]; }
			set { _items[index] = value; } 
		}

		public int Count { get { return _size; } }

		private void EnsureCapacity(int count) { 
			if (count <= _items.Length) return; 
			int newLength;
			Statement[] newData;
			newLength = _items.Length << 1;
			if (newLength == 0)
				newLength = DefaultInitialCapacity;
			while (newLength < count) 
				newLength <<= 1;
			newData = new Statement[newLength];
			Array.Copy(_items, 0, newData, 0, _items.Length);
			_items = newData;
		}
		
		private void Shift(int index, int count) { 
			if (count > 0) { 
				if (_size + count > _items.Length) { 
					int newLength;
					Statement[] newData;
					newLength = (_items.Length > 0) ? _items.Length << 1 : 1;
					while (newLength < _size + count) 
						newLength <<= 1;
					newData = new Statement[newLength];
					Array.Copy(_items, 0, newData, 0, index);
					Array.Copy(_items, index, newData, index + count, _size - index);
					_items = newData;
				} else {
					Array.Copy(_items, index, _items, index + count, _size - index);
				}
			} else if (count < 0) {
				int x = index - count ;
				Array.Copy(_items, x, _items, index, _size - x);
			}
		}

		public int Add(Statement value) { 
			if (_items.Length <= _size /* same as _items.Length < _size + 1) */) 
				EnsureCapacity(_size + 1);
			_items[_size] = value;
			return _size++;
		}
		
		public void Remove(Statement s) {
			if (_size == 0) return;
			int index = Array.IndexOf(_items, s, 0, _size);
			if (index < 0) return;
			RemoveAt(index);
		}

		public virtual void Clear() { 
			Array.Clear(_items, 0, _size);
			_size = 0;
		}

		public virtual void RemoveAt(int index) { 
			if (index < 0 || index >= _size) 
				throw new ArgumentOutOfRangeException();
			Shift(index, -1);
			_size--;
		}

		public void Reverse() {
			for (int i = 0; i <= Count / 2; i++) {
				Statement t = this[i];
				this[i] = this[Count-i-1];
				this[Count-i-1] = t;
			}				
		}
		
		public Statement[] ToArray() {
			Statement[] ret = new Statement[_size];
			Array.Copy(_items, ret, _size);
			return ret;
		}
		
		internal Statement[] ToArray(Type t) {
			return ToArray();
		}
		
		public static implicit operator Statement[](StatementList list) {
			return list.ToArray();
		}
		
		public IEnumerator GetEnumerator() { return new Enumer(this); }
		
		public void CopyTo(Array dest, int start) {
			_items.CopyTo(dest, start);
		}
		
		public object SyncRoot { get { return null; } }
		
		public bool IsSynchronized { get { return false; } }
		
		public void Sort() {
			Array.Sort(_items, 0, _size);
		}
		
		class Enumer : IEnumerator {
			StatementList list;
			int index;
			public Enumer(StatementList list) { this.list = list; index = -1; }
			public void Reset() { index = -1; }
			public bool MoveNext() {
				if (index == list.Count - 1)
					return false;
				index++;
				return true;
			}
			public object Current { get { return list[index]; } }
		}
	}


	// This is based on Mono's Hashtable implementation:
	// Copyright (C) 2004-2005 Novell, Inc (http://www.novell.com)
	public class StatementMap {

		struct Slot {
			internal bool used, removed;
			internal Statement key;
			internal Object value;
			internal int hashMix;
		}

		const int CHAIN_MARKER  = ~Int32.MaxValue;
		private readonly static string xstr = "Hashtable.Enumerator: snapshot out of sync.";

		private int inUse;
		private int modificationCount;
		private float loadFactor;
		private Slot [] table;
		private int threshold;
	
		private StatementList hashKeys;
		private HashValues hashValues;

		private static readonly int [] primeTbl = {
			11,
			19,
			37,
			73,
			109,
			163,
			251,
			367,
			557,
			823,
			1237,
			1861,
			2777,
			4177,
			6247,
			9371,
			14057,
			21089,
			31627,
			47431,
			71143,
			106721,
			160073,
			240101,
			360163,
			540217,
			810343,
			1215497,
			1823231,
			2734867,
			4102283,
			6153409,
			9230113,
			13845163
		};

		public StatementMap () : this (0, 1.0f) {}

		public StatementMap (int capacity, float loadFactor) {
			if (capacity<0)
				throw new ArgumentOutOfRangeException ("capacity", "negative capacity");

			if (loadFactor < 0.1f || loadFactor > 1.0f || Single.IsNaN (loadFactor))
				throw new ArgumentOutOfRangeException ("loadFactor", "load factor");

			if (capacity == 0) ++capacity;
			this.loadFactor = 0.75f*loadFactor;
			double tableSize = capacity / this.loadFactor;

                        if (tableSize > Int32.MaxValue)
                                throw new ArgumentException ("Size is too big");

                        int size = (int) tableSize;
			size = ToPrime (size);
			this.SetTable (new Slot [size]);

			this.inUse = 0;
			this.modificationCount = 0;
		}

		public int Count {
			get {
				return inUse;
			}
		}

		public StatementList Keys {
			get {
				if (this.hashKeys == null) {
					this.hashKeys = new StatementList ();
					Enumerator e = new Enumerator(this);
					while (e.MoveNext())
						hashKeys.Add(e.Key);
				}
				return this.hashKeys;
			}
		}
		
		public IEnumerable Values {
			get {
				if (this.hashValues == null)
					this.hashValues = new HashValues (this);
				return this.hashValues;
			}
		}

		public Object this [Statement key] {
			get {
				Slot [] table = this.table;
				uint size = (uint) table.Length;
				int h = key.GetHashCode() & Int32.MaxValue;
				uint indx = (uint)h;
				uint step = (uint) ((h >> 5)+1) % (size-1)+1;
				
				for (uint i = size; i > 0; i--) {
					indx %= size;
					Slot entry = table [indx];
					if (!entry.used)
						break;
					
					Statement k = entry.key;
					
					if ((entry.hashMix & Int32.MaxValue) == h
					    && k == key) {
						return entry.value;
					}
	
					if ((entry.hashMix & CHAIN_MARKER) == 0)
						break;
	
					indx += step;
				}
				
				return null;
			}
			
			set {
				PutImpl (key, value, true);
			}
		}

		public virtual void Clear ()
		{
			for (int i = 0;i<table.Length;i++) {
				table [i].used = false;
				table [i].removed = false;
				table [i].key = Statement.All;
				table [i].value = null;
				table [i].hashMix = 0;
			}

			inUse = 0;
			modificationCount++;
		}

		public bool ContainsKey (Statement key)
		{
			return (Find (key) >= 0);
		}

		public virtual void Remove (Statement key)
		{
			int i = Find (key);
			if (i >= 0) {
				Slot [] table = this.table;
				int h = table [i].hashMix;
				h &= CHAIN_MARKER;
				table [i].hashMix = h;
				table [i].key = Statement.All;
				if (h != 0) table [i].removed = true;
				else table [i].used = false;
				table [i].value = null;
				--inUse;
				++modificationCount;
			}
		}

		private void AdjustThreshold ()
		{
			int size = table.Length;

			threshold = (int) (size*loadFactor);
			if (this.threshold >= size)
				threshold = size-1;
		}

		private void SetTable (Slot [] table)
		{
			if (table == null)
				throw new ArgumentNullException ("table");

			this.table = table;
			AdjustThreshold ();
		}

		private int Find (Statement key)
		{
			Slot [] table = this.table;
			uint size = (uint) table.Length;
			int h = key.GetHashCode() & Int32.MaxValue;
			uint indx = (uint)h;
			uint step = (uint) ((h >> 5)+1) % (size-1)+1;
			
			for (uint i = size; i > 0; i--) {
				indx %= size;
				Slot entry = table [indx];
				if (!entry.used)
					break;
				
				Statement k = entry.key;
				
				if ((entry.hashMix & Int32.MaxValue) == h
				    && k == key) {
					return (int) indx;
				}

				if ((entry.hashMix & CHAIN_MARKER) == 0)
					break;

				indx += step;
			}
			return -1;
		}


		private void Rehash ()
		{
			int oldSize = this.table.Length;

			// From the SDK docs:
			//   Hashtable is automatically increased
			//   to the smallest prime number that is larger
			//   than twice the current number of Hashtable buckets
			uint newSize = (uint)ToPrime ((oldSize<<1)|1);


			Slot [] newTable = new Slot [newSize];
			Slot [] table = this.table;

			for (int i = 0;i<oldSize;i++) {
				Slot s = table [i];
				if (s.used) {
					int h = s.hashMix & Int32.MaxValue;
					uint spot = (uint)h;
					uint step = ((uint) (h>>5)+1)% (newSize-1)+1;
					for (uint j = spot%newSize;;spot+= step, j = spot%newSize) {
						// No check for KeyMarker.Removed here,
						// because the table is just allocated.
						if (!newTable [j].used) {
							newTable [j].used = s.used;
							newTable [j].removed = s.removed;
							newTable [j].key = s.key;
							newTable [j].value = s.value;
							newTable [j].hashMix |= h;
							break;
						} else {
							newTable [j].hashMix |= CHAIN_MARKER;
						}
					}
				}
			}

			++this.modificationCount;

			this.SetTable (newTable);
		}


		private void PutImpl (Statement key, Object value, bool overwrite)
		{
			uint size = (uint)this.table.Length;
			if (this.inUse >= this.threshold) {
				this.Rehash ();
				size = (uint)this.table.Length;
			}

			int h = key.GetHashCode() & Int32.MaxValue;
			uint spot = (uint)h;
			uint step = (uint) ((spot>>5)+1)% (size-1)+1;
			Slot [] table = this.table;
			Slot entry;

			int freeIndx = -1;
			for (int i = 0; i < size; i++) {
				int indx = (int) (spot % size);
				entry = table [indx];

				if (freeIndx == -1
				    && entry.removed
				    && (entry.hashMix & CHAIN_MARKER) != 0)
					freeIndx = indx;

				if (!entry.used ||
				    (entry.removed
				     && (entry.hashMix & CHAIN_MARKER) == 0)) {

					if (freeIndx == -1)
						freeIndx = indx;
					break;
				}

				if ((entry.hashMix & Int32.MaxValue) == h && key == entry.key) {
					if (overwrite) {
						table [indx].value = value;
						++this.modificationCount;
					} else {
						// Handle Add ():
						// An entry with the same key already exists in the Hashtable.
						throw new ArgumentException (
								"Key duplication when adding: " + key);
					}
					return;
				}

				if (freeIndx == -1) {
					table [indx].hashMix |= CHAIN_MARKER;
				}

				spot+= step;

			}

			if (freeIndx!= -1) {
				table [freeIndx].used = true;
				table [freeIndx].removed = false;
				table [freeIndx].key = key;
				table [freeIndx].value = value;
				table [freeIndx].hashMix |= h;

				++this.inUse;
				++this.modificationCount;
			}

		}

		//
		// Private static methods
		//
		internal static bool TestPrime (int x)
		{
			if ((x & 1) != 0) {
				for (int n = 3; n< (int)Math.Sqrt (x); n += 2) {
					if ((x % n) == 0)
						return false;
				}
				return true;
			}
			// There is only one even prime - 2.
			return (x == 2);
		}

		internal static int CalcPrime (int x)
		{
			for (int i = (x & (~1))-1; i< Int32.MaxValue; i += 2) {
				if (TestPrime (i)) return i;
			}
			return x;
		}

		internal static int ToPrime (int x)
		{
			for (int i = 0; i < primeTbl.Length; i++) {
				if (x <= primeTbl [i])
					return primeTbl [i];
			}
			return CalcPrime (x);
		}

		private sealed class Enumerator : IEnumerator {

			private StatementMap host;
			private int stamp;
			private int pos;
			private int size;

			private bool hasCurrentKey;
			private Statement currentKey;
			private Object currentValue;

			public Enumerator (StatementMap host) {
				this.host = host;
				stamp = host.modificationCount;
				size = host.table.Length;
				Reset ();
			}

			private void FailFast ()
			{
				if (host.modificationCount != stamp) {
					throw new InvalidOperationException (xstr);
				}
			}

			public void Reset ()
			{
				FailFast ();

				pos = -1;
				hasCurrentKey = false;
				currentKey = Statement.All;
				currentValue = null;
			}

			public bool MoveNext ()
			{
				FailFast ();

				if (pos < size) {
					while (++pos < size) {
						Slot entry = host.table [pos];

						if (entry.used && !entry.removed) {
							currentKey = entry.key;
							currentValue = entry.value;
							return true;
						}
					}
				}

				hasCurrentKey = false;
				currentKey = Statement.All;
				currentValue = null;
				return false;
			}

			public DictionaryEntry Entry
			{
				get {
					if (!hasCurrentKey) throw new InvalidOperationException ();
					FailFast ();
					return new DictionaryEntry (currentKey, currentValue);
				}
			}

			public Statement Key {
				get {
					if (!hasCurrentKey) throw new InvalidOperationException ();
					FailFast ();
					return currentKey;
				}
			}
			
			public Object Value {
				get {
					if (!hasCurrentKey) throw new InvalidOperationException ();
					FailFast ();
					return currentValue;
				}
			}

			public Object Current {
				get {
					if (!hasCurrentKey) throw new InvalidOperationException ();
					return currentValue;
				}
			}
		}

		private class HashValues : IEnumerable {
			private StatementMap host;

			public HashValues (StatementMap host) {
				if (host == null)
					throw new ArgumentNullException ();

				this.host = host;
			}

			public virtual IEnumerator GetEnumerator ()
			{
				return new Enumerator (host);
			}
		}
	}

	internal class MultiMap {
		Hashtable items = new Hashtable();
		
		public MultiMap() {
		}
		
		public void Put(object key, object value) {
			object entry = items[key];
			if (entry == null) {
				items[key] = value;
			} else if (entry is ArrayList) {
				((ArrayList)entry).Add(value);
			} else {
				ArrayList list = new ArrayList();
				list.Add(entry);
				list.Add(value);
				items[key] = list;
			}
		}

		public void Clear() {
			items.Clear();
		}
		
		public IList Get(object key) {
			object ret = items[key];
			if (ret == null) return null;
			if (ret is ArrayList) return (ArrayList)ret;
			ArrayList list = new ArrayList();
			list.Add(ret);
			return list;
		}
		
		public IEnumerable Keys {
			get {
				return items.Keys;
			}
		}
	}
	
	internal class Permutation {
		int[] state;
		int[] max;
		public Permutation(int n) : this(n, 2) {
		}
		public Permutation(int n, int e) {
			state = new int[n];
			max = new int[n];
			for (int i = 0; i < n; i++)
				max[i] = e;
		}
		public Permutation(int[] choices) {
			state = new int[choices.Length];
			max = choices;
		}
		public int[] Next() {
			if (state == null) return null;
		
			int[] ret = (int[])state.Clone();
			
			state[0]++;
			for (int i = 0; i < max.Length; i++) { // use max.Length because state becomes null
				if (state[i] < max[i]) break;
				state[i] = 0;
				if (i == state.Length-1) // done the next time around
					state = null;
				else // carry
					state[i+1]++;
			}

			return ret;
		}
		public void Reset() {
			state = new int[state.Length];
		}
	}
}
