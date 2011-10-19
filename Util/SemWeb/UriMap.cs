using System;
using System.Collections;
using System.Collections.Specialized;

namespace SemWeb.Util {
	internal class UriMap : IDictionary {
		private static char[] splitchars = { '\\', '/', '#' };
	
		Node Root;
		int count;
		
		public UriMap() {
			Clear();
		}
		
		bool ICollection.IsSynchronized { get { return false; } }
		object ICollection.SyncRoot { get { return null; } }

		bool IDictionary.IsFixedSize { get { return false; } }
		bool IDictionary.IsReadOnly { get { return false; } }		
		
		object IDictionary.this[object key] {
			get {
				return this[(string)key];
			}
			set {
				this[(string)key] = value;
			}
		}
		
		void IDictionary.Add(object key, object value) {
			this[(string)key] = value;
		}
		
		bool IDictionary.Contains(object key) {
			return Contains((string)key);
		}

		ICollection IDictionary.Keys { get { throw new NotImplementedException(); } }
		ICollection IDictionary.Values { get { throw new NotImplementedException(); } }
		IDictionaryEnumerator IDictionary.GetEnumerator() { throw new NotImplementedException(); }
		
		void IDictionary.Remove(object key) {
			Remove((string)key);
		}
		
		void ICollection.CopyTo(Array dest, int index) { throw new NotImplementedException(); }

		IEnumerator IEnumerable.GetEnumerator() { throw new NotImplementedException(); }
					
		public object this[string uri] {
			get {
				Node node = FindNode(uri, false);
				if (node == null) return null;
				return node.Value;
			}
			set {
				Node node = FindNode(uri, true);
				node.Value = value;
			}
		}
		
		public void Clear() {
			Root = new Node();
			count = 0;
		}
		
		public bool Contains(string uri) {
			Node node = FindNode(uri, false);
			return node != null;
		}
		
		public int Count { get { return count; } }
		
		Node FindNode(string uri, bool create) {
			Node node = Root;
			int pos; string piece;			
			NextPieceStart(out pos);
			while ((piece = NextPiece(uri, ref pos)) != null) {
				bool created;
				node = node.FindChild(piece, create, out created);
				if (created) count++;
				if (node == null) return null;
			}
			
			return node;
		}
		
		public void Remove(string uri) {
			Node node = Root;
			
			int pos;			
			NextPieceStart(out pos);
			string piece = NextPiece(uri, ref pos);
			string next = NextPiece(uri, ref pos);
			while (piece != null) {
				if (next != null) {
					bool created;
					node = node.FindChild(piece, false, out created);
					if (node == null) return;
				} else {
					if (node.RemoveChild(piece))
						count--;
				}
				piece = next;
				next = NextPiece(uri, ref pos);
			}
		}
		
		void NextPieceStart(out int position) {
			position = 0;
		}
		
		string NextPiece(string uri, ref int position) {
			if (position == uri.Length) return null;
			
			// Get the next stopping position
			int look = position;
			if (position == 0 && uri.Length > 15) look = 15;
			int next = uri.IndexOfAny(splitchars, look);
			if (next == -1) {
				string r = uri.Substring(position);
				position = uri.Length;
				return r;
			}
			
			// Grab sequences of the same character in a row.
			while (next != uri.Length-1 && uri[next] == uri[next+1])
				next++;
				
			string ret = uri.Substring(position, next-position+1);
			position = next+1;
			
			return ret;
		}
	
		private class Node {
			public IDictionary Children;
			public object Value;
			
			public Node FindChild(string name, bool create, out bool created) {
				if (Children == null && !create) {
					created = false;
					return null;
				}
				
				if (Children == null)
					#if !SILVERLIGHT
					Children = new HybridDictionary();
					#else
					Children = new Hashtable();
					#endif
				
				Node ret = (Node)Children[name];
				if (ret != null || !create) {
					created = false;
					return ret;
				} else {
					created = true;
					ret = new Node();
					Children[name] = ret;
					return ret;
				}
			}
			
			public bool RemoveChild(string name) {
				if (Children != null && Children.Contains(name)) {
					Children.Remove(name);
					return true;
				}
				return false;
			}
			
		}
	}
}
