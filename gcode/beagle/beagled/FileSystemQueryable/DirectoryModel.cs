//
// DirectoryModel.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

using System;
using System.Collections;
using System.IO;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	public class DirectoryModel : IComparable {

		// The ExpireEvent is fired whenever a directory's name
		// changes.
		public delegate void ExpireHandler (string expired_path, Guid unique_id);
		public static ExpireHandler ExpireEvent;
		
		private object big_lock;

		// Exactly one of the following two must be non-null.
		// If parent != null, this is a subdirectory of another DirectoryModel.
		// If rooted_to != null, this is the root for a directory tree.
		private DirectoryModel parent;
		private string rooted_to;

		private string name;
		private Guid unique_id;

		private DirectoryState state;
		private object watch_handle;
		private DateTime last_crawl_time;
		private DateTime last_activity_time;
		
		private string cached_full_name;
		private int cached_depth;

		private Hashtable children = null;
		
		private DirectoryModel (FileAttributes attr)
		{
			// Always assume an unknown state
			this.state = DirectoryState.Unknown;

			if (attr != null) {
				this.unique_id = attr.UniqueId;
				
				// Since we don't use the Mtime on directories,
				// we can safely store the last crawl time in it.
				this.last_crawl_time = attr.LastWriteTime;
			}
			
			this.cached_full_name = null;
			this.cached_depth = -1;
		}

		///////////////////////////////////////////////////////////

		static public DirectoryModel NewRoot (object big_lock, string path, FileAttributes attr)
		{
			path = StringFu.SanitizePath (path);
			
			DirectoryModel root;
			root = new DirectoryModel (attr);

			root.big_lock = big_lock;
			root.rooted_to = FileSystem.GetDirectoryNameRootOk (path);
			root.name = Path.GetFileName (path);

			return root;
		}

		public bool IsRoot {
			get { return rooted_to != null; }
		}

		///////////////////////////////////////////////////////////

		public string Name {
			get { return name; }
			
			set { 
				lock (big_lock) {
					if (name == value)
						return;
					if (parent != null) {
						if (parent.children.Contains (value)) {
							string msg;
							msg = String.Format ("'{0}' already contains a child named '{1}'",
									     parent.FullName, value);
							throw new Exception (msg);
						}

						parent.children.Remove (name);
					}
					name = value;
					ExpireCached_Unlocked ();
					if (parent != null)
						parent.children [name] = this;
				}
			}
		}

		public string FullName {
			get {
				if (!IsAttached)
					return "_HUH_NO_WAY_ARE_YOU_KIDDING_ME_";
				lock (big_lock) {
					if (cached_full_name == null) {
						string directly_above;
						if (IsRoot)
							directly_above = rooted_to;
						else
							directly_above = parent.FullName;
						cached_full_name = Path.Combine (directly_above, name);
					}
					return cached_full_name;
				}
			}
		}

		public Guid UniqueId {
			get { return unique_id; }
			set {
				if (unique_id != Guid.Empty)
					throw new Exception ("Don't change the unique id!");
				unique_id = value;
			}
		}

		public int Depth {
			get {
				lock (big_lock) {
					if (cached_depth < 0) {
						if (IsRoot)
							cached_depth = 0;
						else
							cached_depth = 1 + parent.Depth;
					}
					return cached_depth;
				}
			}
		}

		public DirectoryState State {
			get { return state; }
			set { state = value; }
		}

		public bool NeedsCrawl {
			get {
				return state != DirectoryState.Clean
					&& state != DirectoryState.PossiblyClean
					&& state != DirectoryState.Uncrawlable;
			}
		}

		public object WatchHandle {
			get { return watch_handle; }
			set { watch_handle = value; }
		}

		public bool IsWatched {
			get { return watch_handle != null; }
		}

		public DirectoryModel Parent {
			get { return parent; }
		}

		public DateTime LastCrawlTime {
			get { return last_crawl_time; }
			set { last_crawl_time = value; }
		}

		public DateTime LastActivityTime {
			get { return last_activity_time; }
			set { last_activity_time = value; }
		}

		public bool IsAttached {
			get { return parent != null || rooted_to != null; }
		}

		///////////////////////////////////////////////////////////

		public void MarkAsClean ()
		{
			// If we aren't being watched, the "cleanest"
			// state we can be in is PossiblyClean.
			if (watch_handle == null)
				state = DirectoryState.PossiblyClean;
			else
				state = DirectoryState.Clean;

			if (FileSystemQueryable.Debug)
				Log.Debug ("Marked {0} as {1}", this, state);
		}

		public void MarkAsUncrawlable ()
		{
			state = DirectoryState.Uncrawlable;
			
			if (FileSystemQueryable.Debug)
				Log.Debug ("Marked {0} as {1}", this, state);
		}

		public void MarkAsUnknown ()
		{
			state = DirectoryState.Unknown;

			if (FileSystemQueryable.Debug)
				Log.Debug ("Reset {0} to {1}", this, state);
		}

		///////////////////////////////////////////////////////////

		private void ExpireCached_Unlocked ()
		{
			if (cached_full_name != null || cached_depth >= 0) {

				if (cached_full_name != null && ExpireEvent != null)
					ExpireEvent (cached_full_name, unique_id);
				
				cached_full_name = null;
				cached_depth = -1;

				if (children != null)
					foreach (DirectoryModel child in children.Values)
						child.ExpireCached_Unlocked ();
			}
		}

		///////////////////////////////////////////////////////////

		private void Detatch_Recursively_Unlocked ()
		{
			if (this.children != null)
				foreach (DirectoryModel child in new ArrayList (this.children.Values))
					child.Detatch_Recursively_Unlocked ();

			this.Detatch_Unlocked ();
		}

		private void Detatch_Unlocked ()
		{
			if (IsRoot) 
				rooted_to = null;

			if (parent != null)
				parent.children.Remove (name);
			
			big_lock = null;
			parent = null;
			ExpireCached_Unlocked ();
		}

		private void Attach_Unlocked (DirectoryModel child)
		{
			string msg;
			
			if (child.IsRoot) {
				msg = String.Format ("Can't attach root node '{0}' to '{1}'",
						     child.Name, this.FullName);
				throw new Exception (msg);
			}

			if (child.parent != null) {
				msg = String.Format ("Can't attach non-detatched node '{0}' to '{1}'",
						     child.Name, this.FullName);
				throw new Exception (msg);
			}
				
			if (children == null) {
				children = new Hashtable ();
			} else if (children.Contains (child.Name)) {
				msg = String.Format ("'{0}' already contains a child named '{1}'",
						     this.FullName, child.Name);
				throw new Exception (msg);
			}
			
			child.big_lock = this.big_lock;
			child.parent = this;
				
			this.children [child.name] = child;
		}
	
		///////////////////////////////////////////////////////////

		// Dealing with the children

		public bool HasChildWithName (string child_name)
		{
			lock (big_lock)
				return children != null && children.Contains (child_name);
		}

		public DirectoryModel GetChildByName (string child_name)
		{
			lock (big_lock) {
				if (children != null)
					return children [child_name] as DirectoryModel;
				return null;
			}
		}

		public int ChildCount {
			get {
				lock (big_lock)
					return children != null ? children.Count : 0;
			}
		}

		static private object [] empty_collection = new object [0];
		public ICollection Children {
			get {
				lock (big_lock) {
					if (children == null)
						return empty_collection;
					return children.Values;
				}
			}
		}

		public ICollection ChildrenCopy {
			get {
				lock (big_lock) {
					if (children == null)
						return empty_collection;
					return new ArrayList (children.Values);
				}
			}
		}

		public DirectoryModel AddChild (string child_name, FileAttributes attr)
		{
			lock (big_lock) {
				DirectoryModel child;
				child = new DirectoryModel (attr);

				child.name = child_name;
				Attach_Unlocked (child);

				return child;
			}
		}

		public void Remove ()
		{
			lock (big_lock)
				Detatch_Recursively_Unlocked ();
		}

		///////////////////////////////////////////////////////////

		// Moving stuff around

		public void MoveTo (DirectoryModel new_parent, string new_name)
		{
			lock (big_lock)
				Detatch_Unlocked ();

			// No need to lock anything here, since this node
			// is just floating out in space.
			if (new_name != null)
				this.name = new_name;

			lock (new_parent.big_lock)
				new_parent.Attach_Unlocked (this);
		}

		public void MoveTo (DirectoryModel new_parent)
		{
			MoveTo (new_parent, null);
		}

		///////////////////////////////////////////////////////////

		public DirectoryModel WalkTree (string path)
		{
			lock (big_lock) {
				
				path = StringFu.SanitizePath (path);

				if (IsRoot) {

					if (! Path.IsPathRooted (path)) {
						string msg;
						msg = String.Format ("Attempt to walk non-rooted path '{0}' on root node '{1}'",
								     path, FullName);
						throw new Exception (msg);
					}

					// If this is a root, we have to make sure
					// that the path actually describes a file below
					// this root.
					if (! path.StartsWith (FullName))
						return null;

					if (path == FullName)
						return this;

					// The root directory is special in that it's the only
					// path that contains a trailing slash.  It has to be
					// handled specially.  What a pain.
					string fn = FullName == "/" ? "" : FullName;
				
					// This is safe: Since we know that path != FullName but
					// that path.StartsWith (FullName), we can conclude that
					// path.Length > FullName.Length.
					if (path [fn.Length] != Path.DirectorySeparatorChar) 
						return null;

					// Drop the part of the path containing the root's path.
					path = path.Substring (fn.Length+1);

				} else if (Path.IsPathRooted (path)) {
					string msg;
					msg = String.Format ("Attempt to walk rooted path '{0}' on non-root node '{1}'",
							     path, FullName);
					throw new Exception (msg);
				}

				if (path.Length == 0)
					return this;
				
				// If we don't actually have any children, why bother?
				if (children == null)
					return null;
				
				int i;
				i = path.IndexOf (Path.DirectorySeparatorChar);
				
				string child_name, rest_of_path;
				if (i == -1) {
					// This is the last path component
					child_name = path;
					rest_of_path = null;
				} else {
					child_name = path.Substring (0, i);
					rest_of_path = path.Substring (i+1);
				}

				DirectoryModel dir;
				dir = children [child_name] as DirectoryModel;

				// No such child, so we give up.
				if (dir == null)
					return null;
				
				if (rest_of_path != null)
					dir = dir.WalkTree (rest_of_path);
				
				return dir;
			}
		}
		
		///////////////////////////////////////////////////////////

		public override string ToString ()
		{
			return FullName;
		}

		///////////////////////////////////////////////////////////
		
		// Our implementation of IComparable

		public int CompareTo_Unlocked (object obj)
		{
			DirectoryModel other = obj as DirectoryModel;
			if (other == null)
				return 1;
			
			int cmp;
			
			cmp = DateTime.Compare (this.last_activity_time,
						other.last_activity_time);
			if (cmp != 0)
				return cmp;
			
			cmp = this.state - other.state;
			if (cmp != 0)
				return cmp;

			// Check depth before last crawl time only if the
			// state is unknown or dirty.  At this point we
			// know that the states are equal, so it doesn't
			// matter which one we check.
			if (this.state > DirectoryState.PossiblyClean) {
				// We only care about depths up to 3.
				// Anything beyond that and we prefer
				// last crawl time first.
				int this_depth = Math.Min (this.Depth, 3);
				int other_depth = Math.Min (other.Depth, 3);

				cmp = other_depth - this_depth;
				if (cmp != 0)
					return cmp;
			}

			cmp = DateTime.Compare (other.last_crawl_time,
						this.last_crawl_time);
			if (cmp != 0)
				return cmp;

			// Use real depths as an almost last resort.
			cmp = other.Depth - this.Depth;
			if (cmp != 0)
				return cmp;

			return other.Name.CompareTo (this.Name);
		}
		
		public int CompareTo (object obj)
		{
			lock (big_lock)
				return CompareTo_Unlocked (obj);
		}
		
	}
}
