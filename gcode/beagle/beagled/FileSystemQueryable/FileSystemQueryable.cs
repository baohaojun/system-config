//
// FileSystemQueryable.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	[QueryableFlavor (Name="Files", Domain=QueryDomain.Local | QueryDomain.Neighborhood, RequireInotify=false)]
	public class FileSystemQueryable : LuceneQueryable {

		static internal bool Debug = Beagle.Util.Debug.Enabled ("FSQ");

		// History:
		// 1: Initially set to force a reindex due to NameIndex changes.
		// 2: Overhauled everything to use new lucene infrastructure.
		// 3: Switched to UTC for all times, changed the properties a bit.
		// 4: Changed the key of TextFilenamePropKey to beagle:Filename - it might be useful in clients.
		//    Make SplitFilenamePropKey unstored
		// 5: Keyword properies in the private namespace are no longer lower cased; this is required to
		//    offset the change in LuceneCommon.cs
		// 6: Store beagle:FileType property denoting type of file like document, source, music etc.
		// 7: Store filesize as fixme:filesize
		// 8: Force re-index since now files whose EA gets removed wont get stored multiple times.
		const int MINOR_VERSION = 8;

		private object big_lock = new object ();

		private IFileEventBackend event_backend;

		// This is the task that walks the tree structure
		private TreeCrawlTask tree_crawl_task;

		// This is the task that finds the next place that
		// needs to be crawled in the tree and spawns off
		// the appropriate IndexableGenerator.
		private FileCrawlTask file_crawl_task;

		// An IndexableGenerator that batches file removals and additions generated
		// from the event backends, instead of creating one task per
		// file/directory.
		private FileSystemEventsGenerator fs_event_generator;

		private ArrayList roots = new ArrayList ();
		private ArrayList roots_by_path = new ArrayList ();

		private UidManager uid_manager;

		// XMP Sidecar handler
		XmpSidecarStore xmp_handler;
		private FileNameFilter filter;
		
		//////////////////////////////////////////////////////////////////////////

		public FileSystemQueryable () : base ("FileSystemIndex", MINOR_VERSION)
		{
			// Set up our event backend
			if (Inotify.Enabled) {
                                Logger.Log.Debug ("Starting Inotify FSQ file event backend");
                                event_backend = new InotifyBackend ();
                        } else {
                                Logger.Log.Debug ("Creating null FSQ file event backend");
				event_backend = new NullFileEventBackend ();
                        }

			tree_crawl_task = new TreeCrawlTask (this, new TreeCrawlTask.Handler (AddDirectory));
			tree_crawl_task.Source = this;

			file_crawl_task = new FileCrawlTask (this);
			file_crawl_task.Source = this;

			fs_event_generator = new FileSystemEventsGenerator (this);

			uid_manager = new UidManager (FileAttributesStore, Driver);
			xmp_handler = new XmpSidecarStore (uid_manager, this);

			PreloadDirectoryNameInfo ();

			// Setup our file-name filter
			filter = new FileNameFilter (this);

			// Do the right thing when paths expire
			DirectoryModel.ExpireEvent +=
				new DirectoryModel.ExpireHandler (ExpireDirectoryPath);
		}


		override protected IFileAttributesStore BuildFileAttributesStore ()
		{
			// FIXME: This is incorrect, but needed for DISABLE_XATTR
			// ExtendedAttribute.Supported only looks at homedirectory
			// There should be a similar check for all mount points or roots
			if (ExtendedAttribute.Supported)
				return new FileAttributesStore_Mixed (IndexDirectory, IndexFingerprint);
                        else
                                return new FileAttributesStore_Sqlite (IndexDirectory, IndexFingerprint);
		}

		override protected LuceneQueryingDriver BuildLuceneQueryingDriver (string index_name,
										   int    minor_version,
										   bool   read_only_mode)
		{
			return new LuceneNameResolver (index_name, minor_version, read_only_mode);
		}

		public FileNameFilter Filter {
			get { return filter; }
		}

		//////////////////////////////////////////////////////////////////////////

		//
		// This is where we build our Indexables
		//

		public static void AddStandardPropertiesToIndexable (Indexable indexable,
								     string    name, 
								     Guid      parent_id,
								     bool      mutable)
		{
			foreach (Property std_prop in Property.StandardFileProperties (name, mutable))
				indexable.AddProperty (std_prop);

			indexable.HitType = "File";

			if (parent_id == Guid.Empty)
				return;
			
			string str = GuidFu.ToUriString (parent_id);
			// We use the uri here to recycle terms in the index,
			// since each directory's uri will already be indexed.
			Property prop = Property.NewUnsearched (Property.ParentDirUriPropKey, str);
			prop.IsMutable = mutable;
			indexable.AddProperty (prop);
		}

		public static void AddStandardPropertiesToIndexable (Indexable      indexable,
								     string         name,
								     DirectoryModel parent,
								     bool           mutable)
		{
			AddStandardPropertiesToIndexable (indexable,
							  name,
							  parent == null ? Guid.Empty : parent.UniqueId,
							  mutable);

			indexable.LocalState ["Parent"] = parent;
		}

		public Indexable DirectoryToIndexable (string         path,
						       Guid           id,
						       DirectoryModel parent)
		{
			Indexable indexable;
			indexable = new Indexable (IndexableType.Add, GuidFu.ToUri (id));
			indexable.Timestamp = Directory.GetLastWriteTimeUtc (path);

			// If the directory was deleted, we'll bail out.
			if (! FileSystem.ExistsByDateTime (indexable.Timestamp))
				return null;

			indexable.MimeType = "inode/directory";
			indexable.NoContent = true;
			indexable.DisplayUri = UriFu.PathToFileUri (path);
			indexable.AddProperty (Property.NewKeyword ("beagle:FileType", "directory"));

			string name;
			if (parent == null)
				name = path;
			else
				name = Path.GetFileName (path);
			AddStandardPropertiesToIndexable (indexable, name, parent, true);

			Property prop;
			prop = Property.NewBool (Property.IsDirectoryPropKey, true);
			prop.IsMutable = true; // we want this in the secondary index, for efficiency
			indexable.AddProperty (prop);

			indexable.LocalState ["Path"] = path;

			MergeExternalPendingIndexable (indexable);

			return indexable;
		}

		public Indexable FileToIndexable (string         path,
						  Guid           id,
						  DirectoryModel parent,
						  bool           crawling)
		{
			Indexable indexable;

			if (path.EndsWith (".xmp")) {
				indexable = xmp_handler.GetXmpQueryable (path, id, parent);
				// Since this method is called with an id,
				// someone could have registered the id before calling this method. Free the id.
				if (indexable == null) {
					Log.Debug ("Ignoring xmp file {0}", path);
					uid_manager.ForgetNewId (path);
				}

				return indexable;
			}

			indexable = new Indexable (IndexableType.Add, GuidFu.ToUri (id));
			indexable.Timestamp = File.GetLastWriteTimeUtc (path);

			// If the file was deleted, bail out.
			if (! FileSystem.ExistsByDateTime (indexable.Timestamp))
				return null;

			indexable.ContentUri = UriFu.PathToFileUri (path);
			indexable.DisplayUri = UriFu.PathToFileUri (path);
			indexable.FlushBufferCache = crawling;
			indexable.Filtering = Beagle.IndexableFiltering.Always;

			FileInfo fi = new FileInfo (path);
			if (fi == null)
				return null; // You never know when files could vanish
			indexable.AddProperty (Property.NewUnsearched ("fixme:filesize", fi.Length));

			AddStandardPropertiesToIndexable (indexable, Path.GetFileName (path), parent, true);

			indexable.LocalState ["Path"] = path;

			MergeExternalPendingIndexable (indexable);
			Indexable xmp_indexable = xmp_handler.MergeXmpData (ref indexable, path, id, parent, crawling);

			// In full generality, the xmp_handler can request an entirely new indexable to be scheduled.
			// So, we should do something with the returned xmp_indexable if it is not null.
			// Currently, skipping the logic since we know the internals of xmp_handler
			// FIXME: Handle non-null xmp_indexable

			return indexable;
		}

		private Indexable NewRenamingIndexable (string         name,
							Guid           id,
							DirectoryModel parent,
							string last_known_path)
		{
			// FIXME
			if (name.EndsWith (".xmp")) {
				Log.Warn ("Renaming of xmp files is not yet supported!");
				return null;
			}

			Indexable indexable;
			indexable = new Indexable (IndexableType.PropertyChange, GuidFu.ToUri (id));
			indexable.DisplayUri = UriFu.PathToFileUri (name);

			AddStandardPropertiesToIndexable (indexable, name, parent, true);

			indexable.LocalState ["Id"] = id;
			indexable.LocalState ["LastKnownPath"] = last_known_path;

			MergeExternalPendingIndexable (indexable);

			return indexable;
		}

		private Indexable FileRemoveIndexable (DirectoryModel dir,
						       string         name)
		{
			// FIXME
			if (name.EndsWith (".xmp")) {
				Log.Warn ("Deleting of xmp files is not yet supported!");
				return null;
			}

			Guid unique_id;
			unique_id = uid_manager.NameAndParentToId (name, dir);
			if (unique_id == Guid.Empty) {
				Log.Info ("Could not resolve unique id of '{0}' in '{1}' for removal -- it is probably already gone",
					  name, dir.FullName);
				return null;
			}

			string path = Path.Combine (dir.FullName, name);
			Uri uri = GuidFu.ToUri (unique_id);
			Indexable indexable;
			indexable = new Indexable (IndexableType.Remove, uri);
			indexable.DisplayUri = UriFu.PathToFileUri (path);
			indexable.LocalState ["RemovedUri"] = indexable.DisplayUri;

			// While adding, wait till the files are added to index for clearing cached_uid and writing attributes
			// For removal, do them first and then remove from index
			uid_manager.ForgetNewId (path);
			FileAttributesStore.Drop (path);
			// Do the same for the corresponding xmp file
			uid_manager.ForgetNewId (string.Concat (path, ".xmp"));
			FileAttributesStore.Drop (string.Concat (path, ".xmp"));

			return indexable;
		}
		
		//////////////////////////////////////////////////////////////////////////

		//
		// Mapping from directory ids to paths
		//

		private Hashtable dir_models_by_id = new Hashtable ();
		// LEAK in name_info_by_id, directories that are present in Lucene,
		// but not actually present in file system will still be there in name_info_by_id.
		// I think (1) after crawling is over, the remaining directories can be
		// used to remove deleted files and dirs from the index
		// and (2) then, the hashtable could be cleared
		private Hashtable name_info_by_id = new Hashtable ();

		// We fall back to using the name information in the index
		// until we've fully constructed our set of DirectoryModels.
		private void PreloadDirectoryNameInfo ()
		{
			ICollection all;
			all = uid_manager.GetAllDirectoryNameInfo ();
			foreach (LuceneNameResolver.NameInfo info in all)
				name_info_by_id [info.Id] = info;
		}

		// This only works for directories.
		private string UniqueIdToDirectoryName (Guid id)
		{
			DirectoryModel dir;
			dir = dir_models_by_id [id] as DirectoryModel;
			if (dir != null)
				return dir.FullName;

			LuceneNameResolver.NameInfo info;
			info = name_info_by_id [id] as LuceneNameResolver.NameInfo;
			if (info != null) {
				if (info.ParentId == Guid.Empty) // i.e. this is a root
					return info.Name;
				else {
					string parent_name;
					parent_name = UniqueIdToDirectoryName (info.ParentId);
					if (parent_name == null)
						return null;
					return Path.Combine (parent_name, info.Name);
				}
			}

			return null;
		}

		private void CacheDirectoryNameChange (Guid id, Guid new_parent_id, string new_name)
		{
			LuceneNameResolver.NameInfo info;
			info = name_info_by_id [id] as LuceneNameResolver.NameInfo;
			if (info != null) {
				info.ParentId = new_parent_id;
				info.Name = new_name;
			}
		}

		private string ToFullPath (string name, Guid parent_id)
		{
			// This is the correct behavior for roots.
			if (parent_id == Guid.Empty)
				return name;

			string parent_name;
			parent_name = UniqueIdToDirectoryName (parent_id);
			if (parent_name == null)
				return null;

			return Path.Combine (parent_name, name);
		}

		// This works for both files and directories.
		private string UniqueIdToFullPath (Guid id)
		{
			// First, check if it is a directory.
			string path;
			path = UniqueIdToDirectoryName (id);
			if (path != null)
				return path;

			// If not, try to pull name information out of the index.
			LuceneNameResolver.NameInfo info;
			info = uid_manager.GetNameInfoById (id);
			if (info == null)
				return null;
			return ToFullPath (info.Name, info.ParentId);
		}

		private string UniqueIdToFileName (Guid id)
		{
			LuceneNameResolver.NameInfo info;
			info = uid_manager.GetNameInfoById (id);
			if (info == null)
				return null;
			return info.Name;
		}

		//////////////////////////////////////////////////////////////////////////

		//
		// Directory-related methods
		//

		private Hashtable dir_models_by_path = new Hashtable ();

		private DirectoryModel GetDirectoryModelByPath (string path)
		{
			DirectoryModel dir;

			lock (dir_models_by_path) {
				dir = dir_models_by_path [path] as DirectoryModel;
				if (dir != null)
					return dir;
			}

			// Walk each root until we find the correct path
			foreach (DirectoryModel root in roots) {
				dir = root.WalkTree (path);
				if (dir != null) {
					lock (dir_models_by_path)
						dir_models_by_path [path] = dir;
					break;
				}
			}

			return dir;
		}

		private void ExpireDirectoryPath (string expired_path, Guid unique_id)
		{
			if (Debug) 
				Logger.Log.Debug ("Expired '{0}'", expired_path);

			lock (dir_models_by_path)
				dir_models_by_path.Remove (expired_path);

			FileAttributesStore.Drop (expired_path);
		}

		public void AddDirectory (DirectoryModel parent, string name)
		{
			// Ignore the stuff we want to ignore.
			if (filter.Ignore (parent, name, true))
				return;

			// FIXME: ! parent.HasChildWithName (name)
			if (parent != null && parent.HasChildWithName (name))
				return;

			string path;
			path = (parent == null) ? name : Path.Combine (parent.FullName, name);

			if (Debug)
				Logger.Log.Debug ("Adding directory '{0}'", path, name);

			if (! Directory.Exists (path)) {
				Logger.Log.Error ("Can't add directory: '{0}' does not exist", path);
				return;
			}

			FileAttributes attr;
			attr = FileAttributesStore.Read (path);

			// Note that we don't look at the mtime of a directory when
			// deciding whether or not to index it.
			bool needs_indexing = false;
			if (attr == null) {
				// If it has no attributes, it definitely needs
				// indexing.
				needs_indexing = true;
			} else {
				// Make sure that it still has the same name as before.
				// If not, we need to re-index it.
				// We can do this since we preloaded all of the name
				// info in the directory via PreloadDirectoryNameInfo.
				string last_known_name;
				last_known_name = UniqueIdToDirectoryName (attr.UniqueId);
				if (last_known_name != path) {
					Logger.Log.Debug ("'{0}' now seems to be called '{1}'", last_known_name, path);
					needs_indexing = true;
				}
			}
			
			// If we can't descend into this directory, we want to
			// index it but not build a DirectoryModel for it.
			// FIXME: We should do the right thing when a
			// directory's permissions change.
			bool is_walkable;
			is_walkable = DirectoryWalker.IsWalkable (path);
			if (! is_walkable)
				Logger.Log.Debug ("Can't walk '{0}'", path);
			
			if (needs_indexing)
				ScheduleDirectory (name, parent, attr, is_walkable);
			else if (is_walkable)
				RegisterDirectory (name, parent, attr);
		}

		public void AddRoot (string path)
		{
			path = StringFu.SanitizePath (path);
			Logger.Log.Debug ("Adding root: {0}", path);

			if (roots_by_path.Contains (path)) {
				Logger.Log.Error ("Trying to add an existing root: {0}", path);
				return;
			}

			// If we're adding a root, this is probably a
			// long-running indexing task.  Set IsIndexing.
			IsIndexing = true;

			// We need to have the path key in the roots hashtable
			// for the filtering to work as we'd like before the root 
			// is actually added.
			roots_by_path.Add (path);

			AddDirectory (null, path);
		}

		public void RemoveRoot (string path)
		{
			Logger.Log.Debug ("Removing root: {0}", path);

			if (! roots_by_path.Contains (path)) {
				Logger.Log.Error ("Trying to remove a non-existing root: {0}", path);
				return;
			}
				
			// Find our directory model for the root
			DirectoryModel dir;
			dir = GetDirectoryModelByPath (path);

			if (dir == null) {
				Logger.Log.Error ("Could not find directory-model for root: {0}", path);
				return;
			}

			// FIXME: Make sure we're emptying the crawler task of any sub-directories 
			// to the root we're removing. It's not a big deal since we do an Ignore-check
			// in there, but it would be nice.

			roots_by_path.Remove (path);
			roots.Remove (dir);

			// Clean out the root from our directory cache.
			RemoveDirectory (dir);
		}

		private void ScheduleDirectory (string         name,
						DirectoryModel parent,
						FileAttributes attr,
						bool           is_walkable)
		{
			string path;
			path = (parent == null) ? name : Path.Combine (parent.FullName, name);

			Guid id;
			id = (attr == null) ? Guid.NewGuid () : attr.UniqueId;

			DateTime last_crawl;
			last_crawl = (attr == null) ? DateTime.MinValue : attr.LastWriteTime;

			Indexable indexable;
			indexable = DirectoryToIndexable (path, id, parent);

			if (indexable != null) {
				indexable.LocalState ["Name"] = name;
				indexable.LocalState ["LastCrawl"] = last_crawl;
				indexable.LocalState ["IsWalkable"] = is_walkable;

				Scheduler.Task task;
				task = NewAddTask (indexable);
				task.Priority = Scheduler.Priority.Delayed;
				ThisScheduler.Add (task);
			}
		}

		private bool RegisterDirectory (string name, DirectoryModel parent, FileAttributes attr)
		{
			string path;
			path = (parent == null) ? name : Path.Combine (parent.FullName, name);

			if (Debug)
				Logger.Log.Debug ("Registered directory '{0}' ({1})", path, attr.UniqueId);

			DateTime mtime = Directory.GetLastWriteTimeUtc (path);

			if (! FileSystem.ExistsByDateTime (mtime)) {
				Log.Debug ("Directory '{0}' ({1}) appears to have gone away", path, attr.UniqueId);
				return false;
			}

			DirectoryModel dir;
			if (parent == null)
				dir = DirectoryModel.NewRoot (big_lock, path, attr);
			else
				dir = parent.AddChild (name, attr);

			if (mtime > attr.LastWriteTime) {
				dir.State = DirectoryState.Dirty;
				if (Debug)
					Logger.Log.Debug ("'{0}' is dirty", path);
			}

			if (Debug) {
				if (dir.IsRoot)
					Logger.Log.Debug ("Created model '{0}'", dir.FullName);
				else
					Logger.Log.Debug ("Created model '{0}' with parent '{1}'", dir.FullName, dir.Parent.FullName);
			}

			// Add any roots we create to the list of roots
			if (dir.IsRoot)
				roots.Add (dir);

			// Add the directory to our by-id hash, and remove any NameInfo
			// we might have cached about it.
			dir_models_by_id [dir.UniqueId] = dir;
			name_info_by_id.Remove (dir.UniqueId);

			// Start watching the directory.
			dir.WatchHandle = event_backend.CreateWatch (path);
			
			// Schedule this directory for crawling.
			if (tree_crawl_task.Add (dir))
				ThisScheduler.Add (tree_crawl_task);

			// Make sure that our file crawling task is active,
			// since presumably we now have something new to crawl.
			ActivateFileCrawling ();

			return true;
		}

		private void ForgetDirectoryRecursively (DirectoryModel dir)
		{
			foreach (DirectoryModel child in dir.Children)
				ForgetDirectoryRecursively (child);

			if (dir.WatchHandle != null) {
				event_backend.ForgetWatch (dir.WatchHandle);
				dir.WatchHandle = null;
			}
			dir_models_by_id.Remove (dir.UniqueId);
			// We rely on the expire event to remove it from dir_models_by_path
		}

		private void RemoveDirectory (DirectoryModel dir)
		{
			Uri uri;
			uri = GuidFu.ToUri (dir.UniqueId);

			Indexable indexable;
			indexable = new Indexable (IndexableType.Remove, uri);
			indexable.DisplayUri = UriFu.PathToFileUri (dir.FullName);

			// Remember a copy of our external Uri, so that we can
			// easily remap it in the PostRemoveHook.
			indexable.LocalState ["RemovedUri"] = indexable.DisplayUri;

			// Forget watches and internal references
			ForgetDirectoryRecursively (dir);
			
			// Calling Remove will expire the path names,
			// so name caches will be cleaned up accordingly.
			dir.Remove ();

			Scheduler.Task task;
			task = NewAddTask (indexable); // We *add* the indexable to *remove* the index item
			task.Priority = Scheduler.Priority.Immediate;
			ThisScheduler.Add (task);
		}

		public void RemoveDirectory (string path)
		{
			DirectoryModel dir = GetDirectoryModelByPath (path);
			if (dir != null)
				RemoveDirectory (dir);
		}

		private void MoveDirectory (DirectoryModel dir, 
					    DirectoryModel new_parent, // or null if we are just renaming
					    string new_name)
		{
			if (dir == null) {
				Logger.Log.Warn ("Couldn't find DirectoryModel for directory moving to '{0}' in '{1}', so it was hopefully never there.",
						 new_name, new_parent.FullName);
				AddDirectory (new_parent, new_name);
				return;
			}

			if (dir.IsRoot)
				throw new Exception ("Can't move root " + dir.FullName);

			// We'll need this later in order to generate the
			// right change notification.
			string old_path;
			old_path = dir.FullName;
			
			if (new_parent != null && new_parent != dir.Parent)
				dir.MoveTo (new_parent, new_name);
			else
				dir.Name = new_name;

			// Remember this by path
			lock (dir_models_by_path)
				dir_models_by_path [dir.FullName] = dir;

			CacheDirectoryNameChange (dir.UniqueId, dir.Parent.UniqueId, new_name);

			Indexable indexable;
			indexable = NewRenamingIndexable (new_name,
							  dir.UniqueId,
							  dir.Parent, // == new_parent
							  old_path);
			indexable.LocalState ["OurDirectoryModel"] = dir;

			Scheduler.Task task;
			task = NewAddTask (indexable);
			task.Priority = Scheduler.Priority.Immediate;
			// Danger Will Robinson!
			// We need to use BlockUntilNoCollision to get the correct notifications
			// in a mv a b; mv b c; mv c a situation.
			// FIXME: And now that type no longer exists!
			ThisScheduler.Add (task);
		}

		//////////////////////////////////////////////////////////////////////////

		//
		// This code controls the directory crawl order
		//

		private DirectoryModel StupidWalk (DirectoryModel prev_best, DirectoryModel contender)
		{
			if (contender.NeedsCrawl) {
				if (prev_best == null || prev_best.CompareTo (contender) < 0)
					prev_best = contender;
			}

			foreach (DirectoryModel child in contender.Children)
				prev_best = StupidWalk (prev_best, child);

			return prev_best;
		}

		public DirectoryModel GetNextDirectoryToCrawl ()
		{
			DirectoryModel next_dir = null;
			
			foreach (DirectoryModel root in roots)
				next_dir = StupidWalk (next_dir, root);

			return next_dir;
		}

		// This is called from the PostFlushHook of DirectoryIndexableGenerator i.e.
		// after PostAddHook() has Registered the directory
		public void DoneCrawlingOneDirectory (DirectoryModel dir)
		{
			if (! dir.IsAttached)
				return;

			FileAttributes attr;
			attr = FileAttributesStore.Read (dir.FullName);

			// We couldn't read our attribute back in for some
			// reason.  Complain loudly.
			if (attr == null) {
				Log.Error ("Unable to read attributes for recently crawled directory {0}", dir.FullName);
				dir.MarkAsClean ();
				return;
			}

			// We don't have to be super-careful about this since
			// we only use the FileAttributes mtime on a directory
			// to determine its initial state, not whether or not
			// its index record is up-to-date.
			attr.LastWriteTime = DateTime.UtcNow;

			// ...but we do use this to decide which order directories get
			// crawled in.
			dir.LastCrawlTime = DateTime.UtcNow;

			FileAttributesStore.Write (attr);
			dir.MarkAsClean ();
		}

		public void MarkDirectoryAsUncrawlable (DirectoryModel dir)
		{
			if (! dir.IsAttached)
				return;
			
			// If we managed to get set up a watch on this directory,
			// drop it.
			if (dir.WatchHandle != null) {
				event_backend.ForgetWatch (dir.WatchHandle);
				dir.WatchHandle = null;
			}

			dir.MarkAsUncrawlable ();
		}

		public void Recrawl (string path) 
		{
			// Try to find a directory model for the path specified
			// so that we can re-crawl it.
			DirectoryModel dir;
			dir = GetDirectoryModelByPath (path);

			bool path_is_registered = true;

			if (dir == null) {
				dir = GetDirectoryModelByPath (FileSystem.GetDirectoryNameRootOk (path));
				path_is_registered = false;

				if (dir == null) {
					Logger.Log.Debug ("Unable to get directory-model for path: {0}", path);
					return;
				}
			}
			
			Logger.Log.Debug ("Re-crawling {0}", dir.FullName);
			
			if (tree_crawl_task.Add (dir))
				ThisScheduler.Add (tree_crawl_task);
			
			if (path_is_registered)
				Recrawl_Recursive (dir, DirectoryState.PossiblyClean);

			ActivateFileCrawling ();
			ActivateDirectoryCrawling ();
		}

		public void RecrawlEverything ()
		{
			Logger.Log.Debug ("Re-crawling all directories");
			
			foreach (DirectoryModel root in roots)
				Recrawl_Recursive (root, DirectoryState.PossiblyClean);
			
			ActivateFileCrawling ();
			ActivateDirectoryCrawling ();
		}
		
		private void Recrawl_Recursive (DirectoryModel dir, DirectoryState state)
		{
			dir.State = state;
			tree_crawl_task.Add (dir);
			foreach (DirectoryModel sub_dir in dir.Children) 
				Recrawl_Recursive (sub_dir, state);
		}

		private void ActivateFileCrawling ()
		{
			if (! file_crawl_task.IsActive)
				ThisScheduler.Add (file_crawl_task);
		}

		private void ActivateDirectoryCrawling ()
		{
			if (! tree_crawl_task.IsActive)
				ThisScheduler.Add (tree_crawl_task);
		}
		
		//////////////////////////////////////////////////////////////////////////

		// Task generator for file system events
		// Single shared queue that everyone (should) add tasks to
		// Maintains the order in which the events happened
		private class FileSystemEventsGenerator : IIndexableGenerator {

			private FileSystemQueryable queryable;
			private Scheduler.Task self_task;

			private object queue_lock = new object ();

			class Event {
				internal string dir_name;
				internal string file_name;
				internal bool addition;

				public Event (string dir_name, string file_name, bool addition)
				{
					this.dir_name = dir_name;
					this.file_name = file_name;
					this.addition = addition;
				}
			}

			private Queue<Event> event_queue = new Queue<Event> ();

			public FileSystemEventsGenerator (FileSystemQueryable queryable)
			{
				this.queryable = queryable;
			}

			public void Add (string dir_name, string file_name, bool addition)
			{
				lock (queue_lock) {
					event_queue.Enqueue (new Event (dir_name, file_name, addition));

					if (self_task == null) {
						self_task = queryable.NewAddTask (this);
						self_task.Priority = Scheduler.Priority.Immediate;
						queryable.ThisScheduler.Add (self_task);
					}
				}
			}

			public Indexable GetNextIndexable ()
			{
				Event evt = null;

				lock (queue_lock)
					evt = event_queue.Dequeue ();

				// evt should not be null
				if (evt.addition)
					return GetNextAdditionIndexable (evt.dir_name, evt.file_name);
				else
					return GetNextRemovalIndexable (evt.dir_name, evt.file_name);
			}

			public Indexable GetNextAdditionIndexable (string dir_name, string file_name)
			{
				DirectoryModel dir = queryable.GetDirectoryModelByPath (dir_name);
				if (dir == null) {
					Log.Warn ("AdditionGenerator.GetNextIndexable failed: Couldn't find DirectoryModel for '{0}'", dir_name);
					return null;
				}

				Guid unique_id;
				unique_id = queryable.RegisterFile (dir, file_name);

				if (unique_id == Guid.Empty)
					return null;

				string path = Path.Combine (dir.FullName, file_name);

				Indexable indexable;
				indexable = queryable.FileToIndexable (path, unique_id, dir, false);

				return indexable;
			}

			public Indexable GetNextRemovalIndexable (string dir_name, string file_name)
			{
				// A null file name means that we're dealing with a directory
				bool is_directory = (file_name == null);
				DirectoryModel dir = queryable.GetDirectoryModelByPath (dir_name);
				if (dir == null) {
					Log.Warn ("RemovalGenerator.GetNextIndexable failed: Couldn't find DirectoryModel for '{0}'", dir_name);
					return null;
				}

				if (is_directory) {
					Indexable indexable;
					indexable = new Indexable (IndexableType.Remove, GuidFu.ToUri (dir.UniqueId));
					indexable.DisplayUri = UriFu.PathToFileUri (dir.FullName);
					indexable.LocalState ["RemovedUri"] = indexable.DisplayUri;

					// Set the watch handle to null.  Since the directory
					// has been deleted from the file system at this point,
					// the watch will have been removed for us.
					dir.WatchHandle = null;
					queryable.ForgetDirectoryRecursively (dir);

					dir.Remove ();

					return indexable;
				} else {
					Indexable indexable;
					indexable = queryable.FileRemoveIndexable (dir, file_name);

					return indexable;
				}
			}

			public bool HasNextIndexable ()
			{
				lock (queue_lock) {
					return (event_queue.Count > 0);
				}
			}

			public string StatusName {
				get { return String.Format ("Events from file system backend: left ", event_queue.Count); }
			}

			public void PostFlushHook ()
			{
				lock (queue_lock) {
					if (event_queue.Count > 0)
						self_task.Reschedule = true;
					else
						self_task = null;
				}
			}

			internal void DebugHook ()
			{
				lock (queue_lock)
					Log.Debug ("FSQ:FileSystemEventsGenerator Debughook: {0} more events to process, generator is {1}active",
						event_queue.Count,
						(self_task != null ? String.Empty : "in"));
			}
		}

		//////////////////////////////////////////////////////////////////////////

		//
		// File-related methods
		//

		private enum RequiredAction {
			None,
			Index,
			Rename,
			Forget
		}

		// Finds what to do with the file and if re-indexing is needed, what id to use
		// During crawling, this is the sole method that finds the right id for a file based on its status, so
		// it might make sense to make this thread safe so that asynchronos inotify events
		// cannot cause any race.
		private RequiredAction DetermineRequiredAction (DirectoryModel dir,
								string         name,
								out Guid       id,
								out string     last_known_path)
		{
			last_known_path = null;
			id = Guid.Empty;

			string path;
			path = Path.Combine (dir.FullName, name);

			FileAttributes attr;
			attr = FileAttributesStore.Read (path);

			if (Debug)
				Logger.Log.Debug ("*** What should we do with {0}?", path);

			if (filter.Ignore (dir, name, false)) {
				// If there are attributes on the file, we must have indexed
				// it previously.  Since we are ignoring it now, we should strip
				// any file attributes from it.
				if (attr != null) {
					if (Debug)
						Logger.Log.Debug ("*** Forget it: File is ignored but has attributes");
					return RequiredAction.Forget;
				}
				if (Debug)
					Logger.Log.Debug ("*** Do nothing: File is ignored");
				return RequiredAction.None;
			}

			// If someone touches a file just before GetCrawlingFileIndexable gets to it,
			// then the file has been already added to the indexer_request but attr is null.
			// Though a waste, the simplest thing to do is to just add another indexable.
			// The two indexables will be merged if the previous one has not been sent to
			// the index-helper yet. Such races are rare and it is not worth the effort
			// to try to handle it better.
			if (attr == null) {
				if (Debug)
					Logger.Log.Debug ("*** Index it: File has no attributes");
				id = uid_manager.ReadOrCreateNewId (dir, name);
				return RequiredAction.Index;
			}

			// If the id for this file is not yet registered, use the id stored in the attribute
			id = uid_manager.GetNewId (path);
			if (id == Guid.Empty) {
				id = attr.UniqueId;
			} else {
				// If id does not match the attribute id, then definitely this file needs to be
				// re-indexed.
				// There are two ways the id of this file could get stored in uid_manager
				// 1. There was a corresponding xmp file and while adding it, it was determined
				// that this file should have a new id.
				// 2. This file was touched just before GetCrawlingFileIndexable reached it. Again,
				// RegisterFile() decided this file should have a new id.
				if (id != attr.UniqueId) {
					if (Debug)
						Logger.Log.Debug ("*** Index it: File has a different id in attribute");
					return RequiredAction.Index;
				}
			}

			// So at this point, id = attr.UniqueId

			// If this was not indexed before, try again
			// If filtercache is dirty, then there might be a newer filter
			if (! attr.HasFilterInfo && FilterFactory.DirtyFilterCache)
				return RequiredAction.Index;

			// FIXME: This does not take in to account that we might have a better matching filter to use now
			// That, however, is kind of expensive to figure out since we'd have to do mime-sniffing and shit.
			if (attr.FilterName != null && attr.FilterVersion > 0) {
				int current_filter_version;
				current_filter_version = FilterFactory.GetFilterVersion (attr.FilterName);

				if (current_filter_version > attr.FilterVersion) {
					if (Debug)
						Logger.Log.Debug ("*** Index it: Newer filter version found for filter {0}", attr.FilterName);
					return RequiredAction.Index;
				}
			}

			Mono.Unix.Native.Stat stat;
			try {
				Mono.Unix.Native.Syscall.stat (path, out stat);
			} catch (Exception ex) {
				Logger.Log.Debug (ex, "Caught exception stat-ing {0}", path);
				return RequiredAction.None;
			}

			DateTime last_write_time, last_attr_time;
			last_write_time = DateTimeUtil.UnixToDateTimeUtc (stat.st_mtime);
			last_attr_time = DateTimeUtil.UnixToDateTimeUtc (stat.st_ctime);

			if (attr.LastWriteTime != last_write_time) {
				if (Debug)
					Logger.Log.Debug ("*** Index it: MTime has changed ({0} vs {1})",
						DateTimeUtil.ToString (attr.LastWriteTime),
						DateTimeUtil.ToString (last_write_time));
				
				// If the file has been copied, it will have the
				// original file's EAs.  Thus we have to check to
				// make sure that the unique id in the EAs actually
				// belongs to this file.  If not, replace it with a new one.
				// (Thus touching & then immediately renaming a file can
				// cause its unique id to change, which is less than
				// optimal but probably can't be helped.)
				last_known_path = UniqueIdToFullPath (id);
				if (path != last_known_path) {
					if (Debug)
						Logger.Log.Debug ("*** Name has also changed, assigning new unique id");
					id = uid_manager.CreateNewId (path);
				}
				
				return RequiredAction.Index;
			}

			// If the inode ctime is newer than the last time we last
			// set file attributes, we might have been moved.  We don't
			// strictly compare times due to the fact that although
			// setting xattrs changes the ctime, if we don't have write
			// access our metadata will be stored in sqlite, and the
			// ctime will be at some point in the past.
			if (attr.LastAttrTime < last_attr_time) {
				if (Debug)
					Logger.Log.Debug ("*** CTime is newer, checking last known path ({0} vs {1})",
						DateTimeUtil.ToString (attr.LastAttrTime),
						DateTimeUtil.ToString (last_attr_time));

				last_known_path = UniqueIdToFullPath (id);

				if (last_known_path == null) {
					if (Debug)
						Logger.Log.Debug ("*** Index it: CTime has changed, but can't determine last known path");
					return RequiredAction.Index;
				}

				// If the name has changed but the mtime
				// hasn't, the only logical conclusion is that
				// the file has been renamed.
				if (path != last_known_path) {
					if (Debug)
						Logger.Log.Debug ("*** Rename it: CTime and path has changed");
					return RequiredAction.Rename;
				}
			}
			
			// We don't have to do anything, which is always preferable.
			if (Debug)
				Logger.Log.Debug ("*** Do nothing");
			return RequiredAction.None;	
		}

		// Return an indexable that will do the right thing with a file
		// (or null, if the right thing is to do nothing)
		public Indexable GetCrawlingFileIndexable (DirectoryModel dir, string name)
		{
			string path;
			path = Path.Combine (dir.FullName, name);

			RequiredAction action;
			string last_known_path;
			Guid unique_id;
			action = DetermineRequiredAction (dir, name, out unique_id, out last_known_path);

			if (action == RequiredAction.None)
				return null;

			Indexable indexable = null;

			switch (action) {

			case RequiredAction.Index:
				indexable = FileToIndexable (path, unique_id, dir, true);
				break;

			case RequiredAction.Rename:
				indexable = NewRenamingIndexable (name, unique_id, dir,
								  last_known_path);
				break;

			case RequiredAction.Forget:
				FileAttributesStore.Drop (path);
				
				break;
			}

			return indexable;
		}

		// Returns Guid.Empty if already scheduled
		public Guid RegisterFile (DirectoryModel dir, string name)
		{
			string path;
			path = Path.Combine (dir.FullName, name);

			if (! File.Exists (path))
				return Guid.Empty;

			if (FileSystem.IsSpecialFile (path))
				return Guid.Empty;
			
			if (filter.Ignore (dir, name, false))
				return Guid.Empty;

			if (! dir.IsAttached)
				return Guid.Empty;

			// If path is already in the id cache, it means the file
			// is already scheduled to be indexed
			if (uid_manager.HasNewId (path)) {
				if (Debug)
					Log.Debug ("Cache contains {0}", path);
				return Guid.Empty;
			}

			// If this file already has extended attributes,
			// make sure that the name matches the file
			// that is in the index.  If not, it could be
			// a copy of an already-indexed file and should
			// be assigned a new unique id.
			// However, xmp files are never written to the index and so
			// need not be matched with the index.
			Guid unique_id = Guid.Empty;
			FileAttributes attr = null;

			// Guid for xmp file is only written to the attributes and not to the index.
			// However, if sqlite is used, creating a new guid will result in multiple id for same file.
			// Again if xattr is used, copying xmp files would create different files with same id.
			// So, everytime create a new guid for any xmp file and delete the old attribute.
			// I know, I know, its a horrible hack.
			if (name.EndsWith (".xmp")) {
				FileAttributesStore.Drop (path);
			} else {
				attr = FileAttributesStore.Read (path);
			}

			if (attr != null) {
				LuceneNameResolver.NameInfo info;
				info = uid_manager.GetNameInfoById (attr.UniqueId);
				if (info != null
				    && info.Name == name
				    && info.ParentId == dir.UniqueId)
					unique_id = attr.UniqueId;
			}

			if (unique_id == Guid.Empty)
				unique_id = Guid.NewGuid ();

			uid_manager.RegisterNewId (name, dir, unique_id);
			
			return unique_id;
		}

		public void MoveFile (DirectoryModel old_dir, string old_name,
				      DirectoryModel new_dir, string new_name)
		{
			bool old_ignore, new_ignore;
			old_ignore = filter.Ignore (old_dir, old_name, false);
			new_ignore = filter.Ignore (new_dir, new_name, false);

			if (old_ignore && new_ignore)
				return;

			// If our ignore-state is changing, synthesize the appropriate
			// action.

			if (old_ignore && ! new_ignore) {
				fs_event_generator.Add (new_dir.FullName, new_name, true);
				return;
			}

			if (! old_ignore && new_ignore) {
				fs_event_generator.Add (new_dir.FullName, new_name, false);
				return;
			}

			// We need to find the file's unique id.
			// We can't look at the extended attributes w/o making
			// assumptions about whether they follow around the
			// file (EAs) or the path (sqlite)...
			Guid unique_id;
			unique_id = uid_manager.NameAndParentToId (old_name, old_dir);
			if (unique_id == Guid.Empty) {
				// If we can't find the unique ID, we have to
				// assume that the original file never made it
				// into the index ---  thus we treat this as
				// an Add.
				fs_event_generator.Add (new_dir.FullName, new_name, true);
				return;
			}

			uid_manager.RegisterNewId (new_name, new_dir, unique_id);

			string old_path;
			old_path = Path.Combine (old_dir.FullName, old_name);

			uid_manager.ForgetNewId (old_path);

			// FIXME: I think we need to be more conservative when we seen
			// events in a directory that has not been fully scanned, just to
			// avoid races.  i.e. what if we are in the middle of crawling that
			// directory and haven't reached this file yet?  Then the rename
			// will fail.
			Indexable indexable;
			indexable = NewRenamingIndexable (new_name,
							  unique_id,
							  new_dir,
							  old_path);
			
			Scheduler.Task task;
			task = NewAddTask (indexable);
			task.Priority = Scheduler.Priority.Immediate;
			// Danger Will Robinson!
			// We need to use BlockUntilNoCollision to get the correct notifications
			// in a mv a b; mv b c; mv c a situation.
			// FIXME: And now AddType no longer exists
			ThisScheduler.Add (task);
		}

		//////////////////////////////////////////////////////////////////////////
		
		// Configuration stuff

		public IList Roots {
			get {
				return roots_by_path;
			}
		}
		
		private void LoadConfiguration () 
		{
			Config config = Conf.Get (Conf.Names.FilesQueryableConfig);

			if (config.GetOption (Conf.Names.IndexHomeDir, true))
				AddRoot (PathFinder.HomeDir);
			
			List<string[]> roots = config.GetListOptionValues (Conf.Names.Roots);
			if (roots != null)
				foreach (string[] root in roots)
					AddRoot (root [0]);
			
			Conf.Subscribe (Conf.Names.FilesQueryableConfig, OnConfigurationChanged);
		}
		
		private void OnConfigurationChanged (Config config)
		{
			if (config == null || config.Name != Conf.Names.FilesQueryableConfig)
				return;

			List<string[]> values = config.GetListOptionValues (Conf.Names.Roots);
			if (values == null)
				values = new List<string[]> (0);

			ArrayList roots_wanted = new ArrayList (values.Count);
			foreach (string[] root in values)
				roots_wanted.Add (root [0]);
			
			if (config.GetOption (Conf.Names.IndexHomeDir, true))
				roots_wanted.Add (PathFinder.HomeDir);

			IList roots_to_add, roots_to_remove;
			ArrayFu.IntersectListChanges (roots_wanted, Roots, out roots_to_add, out roots_to_remove);

			foreach (string root in roots_to_remove)
				RemoveRoot (root);

			foreach (string root in roots_to_add)
				AddRoot (root);
		}

		//////////////////////////////////////////////////////////////////////////

		public void UpdateIsIndexing (DirectoryModel next_dir)
		{
			// If IsIndexing is false, then the indexing had
			// finished previously and we don't really care about
			// this call anymore.  It can be reset to true if a new
			// root is added, however.
			if (this.IsIndexing == false)
				return;

			// If crawled_dir is not null, we're
			// still indexing.  If not, check our tree and file crawl tasks to
			// see if we're still working on the queue.
			if (next_dir != null)
				this.IsIndexing = (next_dir.State > DirectoryState.PossiblyClean);
			else
				this.IsIndexing = (file_crawl_task.IsActive || tree_crawl_task.IsActive);
		}

		//////////////////////////////////////////////////////////////////////////

		// FIXME: Depending on how big this thing gets, we might want
		// to use some external storage like an SQLite database for
		// speed and memory reasons.

		private Hashtable external_pending_indexables = UriFu.NewHashtable ();

		private void AddExternalPendingIndexable (Indexable indexable)
		{
			Log.Debug ("Delaying add of {0} until FSQ comes across it", indexable.Uri);

			external_pending_indexables [indexable.Uri] = indexable;
		}

		private void MergeExternalPendingIndexable (Indexable indexable)
		{
			Indexable other = (Indexable) external_pending_indexables [indexable.DisplayUri];

			if (other == null)
				return;

			external_pending_indexables.Remove (indexable.DisplayUri);

			Log.Debug ("Merging in properties of external indexable for {0} into {1}",
				   indexable.DisplayUri, indexable.Uri);

			indexable.Merge (other);
		}

		//////////////////////////////////////////////////////////////////////////

		//
		// Our magic LuceneQueryable hooks
		//

		override protected bool PreAddIndexableHook (Indexable indexable)
		{
			if (Debug)
				Log.Debug ("Asking whether it's ok to index ({2}) {0} [{1}]", indexable.Uri, indexable.DisplayUri, indexable.Type);

			// Internal URIs are always allowed.
			if (indexable.Uri.Scheme == GuidFu.UriScheme)
				return true;

			// File URIs we try to remap
			Uri internal_uri = ExternalToInternalUri (indexable.Uri);

			if (internal_uri != null) {
				// Some slightly odd logic here.  DisplayUri
				// will always equal Uri if DisplayUri isn't
				// explicitly set.  Since we're going to
				// reassign Uri to an internal URI, explicitly
				// set DisplayUri to Uri, but only if
				// DisplayUri wasn't previously set.  Got it? :)
				if (indexable.DisplayUri == indexable.Uri)
					indexable.DisplayUri = indexable.Uri;

				if (Debug)
					Log.Debug ("Mapped {0} -> {1}", indexable.Uri, internal_uri);
				indexable.Uri = internal_uri;

				return true;
			}

			// The file doesn't exist.  We need to set it aside in
			// case it appears later.
			AddExternalPendingIndexable (indexable);

			return false;
		}

		override protected Uri PostAddHook (Indexable indexable, IndexerAddedReceipt receipt)
		{
			// We don't have anything to do if we are dealing with a child indexable
			if (indexable.ParentUri != null)
				return indexable.DisplayUri;

			string xmpfile_path = (string) indexable.LocalState ["XmpFilePath"];
			if (xmpfile_path != null) {
				// Get the uid of the xmp file
				string xmp_id_string = (string) indexable.LocalState ["XmpGuid"];
				Guid xmp_id = GuidFu.FromShortString (xmp_id_string);

				FileAttributes xmp_attr;
				xmp_attr = FileAttributesStore.ReadOrCreate (xmpfile_path, xmp_id);
				xmp_attr.Path = xmpfile_path;
				// Potential race here, attr->LastWriteTime should really be the last write
				// time as seen when this indexable was added
				xmp_attr.LastWriteTime = File.GetLastWriteTimeUtc (xmpfile_path);

				// Add filter information, otherwise the xmp file will be indexed on each recrawl
				xmp_attr.FilterName = XmpFile.FilterName;
				xmp_attr.FilterVersion = XmpFile.FilterVersion;

				// Write file attributes for xmp file
				if (Debug)
					Log.Debug ("Writing attributes for xmp {0}({1})", xmpfile_path, xmp_id_string);

				FileAttributesStore.Write (xmp_attr);
				uid_manager.ForgetNewId (xmpfile_path);
			}

			if (indexable.Type == IndexableType.PropertyChange) {
				// If we were moved, remap to our *old* external Uri
				// to make notification work out properly.  Otherwise,
				// this is an in-place property change and we don't
				// need to do anything.

				Uri remapped_uri = indexable.DisplayUri;
				string last_known_path;
				last_known_path = (string) indexable.LocalState ["LastKnownPath"];

				if (last_known_path != null) {
					remapped_uri = UriFu.PathToFileUri (last_known_path);
					Logger.Log.Debug ("Last known path is {0}", last_known_path);

					// This rename is now in the index, so we no
					// longer need to keep track of the uid in memory.
					uid_manager.ForgetNewId (last_known_path);
				} else if (xmpfile_path != null) {
					// Get the correct uri for notifications
					string basefile_path = (string) indexable.LocalState ["BaseFilePath"];
					remapped_uri = UriFu.PathToFileUri (basefile_path);
				}

				return remapped_uri;
			}

			string path;
			path = (string) indexable.LocalState ["Path"];

			if (Debug)
				Log.Debug ("PostAddHook for {0} ({1})", indexable.Uri, path);

			uid_manager.ForgetNewId (path);

			DirectoryModel parent;
			parent = indexable.LocalState ["Parent"] as DirectoryModel;

			// The parent directory might have run away since we were indexed
			if (parent != null && ! parent.IsAttached)
				return indexable.DisplayUri;

			Guid unique_id;
			unique_id = GuidFu.FromUri (indexable.Uri);

			FileAttributes attr;
			attr = FileAttributesStore.ReadOrCreate (path, unique_id);

			attr.Path = path;
			attr.LastWriteTime = indexable.Timestamp;
			
			attr.FilterName = receipt.FilterName;
			attr.FilterVersion = receipt.FilterVersion;

			if (indexable.LocalState ["IsWalkable"] != null) {
				string name;
				name = (string) indexable.LocalState ["Name"];

				if (! RegisterDirectory (name, parent, attr))
					return indexable.DisplayUri;
			}

			FileAttributesStore.Write (attr);

			// Return the remapped Uri so that change notification will work properly
			return UriFu.PathToFileUri (path);
		}

		override protected Uri PostRemoveHook (Indexable indexable, int num_remove)
		{
			// FIXME: If nothing is removed, something went wrong! Handle the situation better.
			if (num_remove <= 0)
				return indexable.Uri;

			// Find the cached external Uri and remap the Uri in the receipt.
			// We have to do this to make change notification work.
			Uri external_uri;
			external_uri = indexable.LocalState ["RemovedUri"] as Uri;
			if (external_uri == null)
				throw new Exception ("No cached external Uri for " + indexable.Uri);

			// Return the remapped uri
			return external_uri;
		}

		private bool RemapUri (Hit hit)
		{
			// Store the hit's internal uri in a property
			Property prop;
			prop = Property.NewUnsearched ("beagle:InternalUri",
						       UriFu.UriToEscapedString (hit.Uri));
			hit.AddProperty (prop);

			// Now assemble the path by looking at the parent and name
			string name = null, path, is_child;
			is_child = hit [Property.IsChildPropKey];

			if (is_child == "true")
				name = hit ["parent:" + Property.ExactFilenamePropKey];
			else
				name = hit [Property.ExactFilenamePropKey];

			if (name == null) {
				// If we don't have the filename property, we have to do a lookup
				// based on the guid.  This happens with synthetic hits produced by
				// index listeners.
				Guid hit_id;
				hit_id = GuidFu.FromUri (hit.Uri);
				path = UniqueIdToFullPath (hit_id);
			} else if (hit [Property.IsDirectoryPropKey] == "true") {
				// For directories, it is better to get the path directly from directory model
				Guid hit_id;
				hit_id = GuidFu.FromUri (hit.Uri);
				path = UniqueIdToDirectoryName (hit_id);
			} else {
				string parent_id_uri;
				parent_id_uri = hit [Property.ParentDirUriPropKey];
				if (parent_id_uri == null)
					parent_id_uri = hit ["parent:" + Property.ParentDirUriPropKey];

				Guid parent_id = Guid.Empty;
				if (parent_id_uri != null)
					parent_id = GuidFu.FromUriString (parent_id_uri);
			
				path = ToFullPath (name, parent_id);
				if (path == null)
					Logger.Log.Debug ("Couldn't find path of file with name '{0}' and parent '{1}'",
							  name, GuidFu.ToShortString (parent_id));
			}

			if (Debug)
				Log.Debug ("Resolved {0} to {1}", hit.Uri, path);

			if (path != null) {
				hit.Uri = UriFu.PathToFileUri (path);
				return true;
			}

			return false;
		}

		// Hit filter: this handles our mapping from internal->external uris,
		// and checks to see if the file is still there.
		override protected bool HitFilter (Hit hit)
		{
			Uri old_uri = hit.Uri;
			if (Debug)
				Log.Debug ("HitFilter ({0})", old_uri);

			if (! RemapUri (hit))
				return false;

			string path;
			path = hit.Uri.LocalPath;

			bool is_directory;
			bool exists = false;

			is_directory = hit.MimeType == "inode/directory";

			if (hit.MimeType == null && hit.Uri.IsFile && Directory.Exists (path)) {
				is_directory = true;
				exists = true;
			}

			if (! exists) {
				if (is_directory)
					exists = Directory.Exists (path);
				else
					exists = File.Exists (path);
			}

			// If the file doesn't exist, we do not schedule a removal and
			// return false.  This is to avoid "losing" files if they are
			// in a directory that has been renamed but which we haven't
			// scanned yet... if we dropped them from the index, they would
			// never get re-indexed (or at least not until the next time they
			// were touched) since they would still be stamped with EAs
			// indicating they were up-to-date.  And that would be bad.
			// FIXME: It would be safe if we were in a known state, right?
			// i.e. every DirectoryModel is clean.
			if (! exists)
				return false;

			// Fetch the parent directory model from our cache to do clever 
			// filtering to determine if we're ignoring it or not.
			DirectoryModel parent;
			parent = GetDirectoryModelByPath (Path.GetDirectoryName (path));

			// If child indexable, attach the relative URI at the end
			// Relative URI starts with '#'
			string is_child = hit [Property.IsChildPropKey];
			string fragment = null;
			if (is_child == "true") {
				Uri uri = UriFu.PathToFileUri (path);
				hit.Uri = UriFu.AddFragment (uri, old_uri.Fragment, true);
				hit.ParentUri = UriFu.PathToFileUri (path);
			}

			// Check the ignore status of the hit
			if (filter.Ignore (parent, Path.GetFileName (fragment == null ? path : fragment), is_directory))
				return false;

			return true;
		}

		override public bool HasUri (Uri uri)
		{
			Uri internal_uri = ExternalToInternalUri (uri);

			if (internal_uri == null)
				return false;

			return base.HasUri (internal_uri);
		}

		protected override QueryPart QueryPartHook(QueryPart part)
		{
			if (part is QueryPart_Uri)
				return RemapUriQueryPart ((QueryPart_Uri) part);

			if (part is QueryPart_Property) {
				QueryPart_Property prop_part = (QueryPart_Property) part;
				if (prop_part.Key == "inuri") // special case
					return RemapInUriQueryPart (prop_part);
			}

			return part;
		}

		// Remap uri in querypart_uri
		private QueryPart_Uri RemapUriQueryPart (QueryPart_Uri part)
		{
			Uri new_uri = ExternalToInternalUri (part.Uri);
			Log.Debug ("Remapping QueryPart_Uri from {0} to {1}", part.Uri, new_uri);

			// Do the right thing if the uri does not exist
			// Remember QueryPart_Uri can occur inside QueryPart_Or
			if (new_uri == null)
				new_uri = new Uri ("no-match:///"); // Will never match

			QueryPart_Uri new_part = new QueryPart_Uri ();
			new_part.Uri = new_uri;
			new_part.Logic = part.Logic;

			return new_part;
		}

		private QueryPart RemapInUriQueryPart (QueryPart_Property part)
		{
			string query = part.Value;

			if (query.StartsWith ("/"))
				query = UriFu.PathToFileUriString (query); // Make an URI

			if (query.StartsWith ("file:///")) {
				QueryPart_Property prop_part = new QueryPart_Property ();
				prop_part.Logic = part.Logic;
				prop_part.Key = Property.ParentDirUriPropKey;
				prop_part.Type = PropertyType.Keyword;

				Uri uri = ExternalToInternalUri (UriFu.EscapedStringToUri (query));
				if (uri == null)
					prop_part.Value = "no-match:///"; // FIXME: Returning null should work here
				else
					// From LuceneCommon.cs:AddPropertyToDocument since ParentDirUriPropKey is a private property
					prop_part.Value = UriFu.UriToEscapedString (uri);

				Log.Debug ("Remapped inuri={0} to {1}={2}", query, Property.ParentDirUriPropKey, prop_part.Value);
				return prop_part;
			}

			QueryPart_Or parent_dirs = new QueryPart_Or ();
			parent_dirs.Logic = part.Logic;

			lock (big_lock) {
				// Absolute path was not given.
				// Traverse the directories to find directories with _EXACTLY_ this name
				foreach (LuceneNameResolver.NameInfo info in uid_manager.GetAllDirectoryNameInfo (query)) {
					QueryPart_Property prop_part = new QueryPart_Property ();
					prop_part.Logic = QueryPartLogic.Required;
					prop_part.Type = PropertyType.Keyword;
					prop_part.Key = Property.ParentDirUriPropKey;
					prop_part.Value = GuidFu.ToUriString (info.Id);

					parent_dirs.Add (prop_part);
				}
			}

			Log.Debug ("Found {0} matching dirs with containing '{1}' in name", parent_dirs.SubParts.Count, query);
			if (parent_dirs.SubParts.Count == 0) {
				// Add dummy query to match nothing
				QueryPart_Property prop_part = new QueryPart_Property ();
				prop_part.Logic = QueryPartLogic.Required;
				prop_part.Type = PropertyType.Keyword;
				prop_part.Key = Property.ParentDirUriPropKey;
				prop_part.Value = "no-match:///";

				parent_dirs.Add (prop_part);
			}

			return parent_dirs;
		}

		override public ISnippetReader GetSnippet (string [] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			// Uri remapping from a hit is easy: the internal uri
			// is stored in a property.
			Uri uri = UriFu.EscapedStringToUri (hit ["beagle:InternalUri"]);

			bool self_cache = true;
			TextReader reader = TextCache.UserCache.GetReader (uri, ref self_cache);

			if (self_cache)
				return SnippetFu.GetSnippetFromFile (query_terms, hit.Uri.LocalPath, full_text, ctx_length, snp_length);

			if (reader == null)
				return null;

			return SnippetFu.GetSnippet (query_terms, reader, full_text, ctx_length, snp_length);
		}

		override public void Start ()
		{
			base.Start ();
			
			event_backend.Start (this);

			LoadConfiguration ();

			Logger.Log.Debug ("Done starting FileSystemQueryable");
		}

		//////////////////////////////////////////////////////////////////////////

		// These are the methods that the IFileEventBackend implementations should
		// call in response to events.
		
		public void ReportEventInDirectory (string directory_name)
		{
			DirectoryModel dir;
			dir = GetDirectoryModelByPath (directory_name);

			// If something goes wrong, just fail silently.
			if (dir == null)
				return;

			// We only use this information to prioritize the order in which
			// we crawl directories --- so if this directory doesn't
			// actually need to be crawled, we can safely ignore it.
			if (! dir.NeedsCrawl)
				return;

			dir.LastActivityTime = DateTime.Now;
			
			if (Debug)
				Log.Debug ("Saw event in '{0}'", directory_name);
		}

		public void HandleAddEvent (string directory_name, string file_name, bool is_directory)
		{
			if (Debug) {
				Log.Debug ("*** Add '{0}' '{1}' {2}", directory_name, file_name,
					   is_directory ? "(dir)" : "(file)");
			}
			
			DirectoryModel dir;
			dir = GetDirectoryModelByPath (directory_name);
			if (dir == null) {
				Logger.Log.Warn ("HandleAddEvent failed: Couldn't find DirectoryModel for '{0}'", directory_name);
				return;
			}

			if (is_directory)
				AddDirectory (dir, file_name);
			else
				fs_event_generator.Add (dir.FullName, file_name, true);
		}

		public void HandleRemoveEvent (string directory_name, string file_name, bool is_directory)
		{
			if (Debug) {
				Log.Debug ("*** Remove '{0}' '{1}' {2}", directory_name, file_name,
					   is_directory ? "(dir)" : "(file)");
			}

			if (is_directory)
				fs_event_generator.Add (Path.Combine (directory_name, file_name), null, false);
			else
				fs_event_generator.Add (directory_name, file_name, false);
		}

		public void HandleMoveEvent (string old_directory_name, string old_file_name,
					     string new_directory_name, string new_file_name,
					     bool is_directory)
		{
			if (Debug) {
				Log.Debug ("*** Move '{0}' '{1}' -> '{2}' '{3}' {4}",
					   old_directory_name, old_file_name,
					   new_directory_name, new_file_name,
					   is_directory ? "(dir)" : "(file)");
			}

			if (is_directory) {
				DirectoryModel dir, new_parent;
				dir = GetDirectoryModelByPath (Path.Combine (old_directory_name, old_file_name));
				new_parent = GetDirectoryModelByPath (new_directory_name);
				MoveDirectory (dir, new_parent, new_file_name);
				return;
			} else {
				DirectoryModel old_dir, new_dir;
				old_dir = GetDirectoryModelByPath (old_directory_name);
				new_dir = GetDirectoryModelByPath (new_directory_name);
				MoveFile (old_dir, old_file_name, new_dir, new_file_name);
			}
		}

		public void HandleAttribEvent (string directory_name, string file_name, bool is_directory)
		{
			if (Debug) {
				Log.Debug ("*** Attrib '{0}' '{1}' {2}", directory_name, file_name,
					   is_directory ? "(dir)" : "(file)");
			}

			string path = Path.Combine (directory_name, file_name);

			if (is_directory) {
				DirectoryModel dir = GetDirectoryModelByPath (path);
				if (dir == null) {
					Log.Warn ("HandleAttribEvent failed: Couldn't find DirectoryModel for '{0}'", path);
					return;
				}

				if (dir.State == DirectoryState.Uncrawlable && DirectoryWalker.IsWalkable (path)) {
					Log.Debug ("Re-adding previously uncrawlable directory '{0}'", path);
					dir.MarkAsUnknown ();
					
					if (dir.WatchHandle == null)
						dir.WatchHandle = event_backend.CreateWatch (path);

					if (tree_crawl_task.Add (dir))
						ThisScheduler.Add (tree_crawl_task);

					ActivateFileCrawling ();
				} else if (! DirectoryWalker.IsWalkable (path)) {
					// This is a no-op, because the HitFilter will remove any
					// search results that the user can't see.  The upside to
					// this is that if the directory is ever made visible
					// again, we don't have to expensively recrawl everything.
					// The downside is that these entries are still stored in
					// the index.  Maybe we should eventually schedule them
					// for removal?
				}
			}
		}

		public void HandleOverflowEvent ()
		{
			Log.Warn ("Queue overflows suck");
		}

		//////////////////////////

		private Uri ExternalToInternalUri (Uri external_uri)
		{
			FileAttributes attr;
			attr = FileAttributesStore.Read (external_uri.LocalPath);

			if (attr == null)
				return null;

			Uri internal_uri = GuidFu.ToUri (attr.UniqueId);

			return internal_uri;
		}

		//////////////////////////

		internal override void DebugHook ()
		{
			Log.Debug ("FSQ DebugHook: {0} roots, {1} roots by path, {2} dir_models_by_id, {3} name_info_by_id, {4} dir_models_by_path",
				(roots != null ? roots.Count : -1),
				(roots_by_path != null ? roots_by_path.Count : -1),
				(dir_models_by_id != null ? dir_models_by_id.Count : -1),
				(name_info_by_id != null ? name_info_by_id.Count : -1),
				(dir_models_by_path != null ? dir_models_by_path.Count : -1));

			Log.Debug ("FSQ Debughook: {0} external_pending_indexables",
				(external_pending_indexables != null ? external_pending_indexables.Count : -1));

			lock (big_lock) {
				foreach (DirectoryModel root in roots) {
					int count = 1;
					ArrayList queue = new ArrayList ();
					queue.Add (root);
					DirectoryModel child;
					while (queue.Count > 0) {
						child = (DirectoryModel) queue [0];
						queue.RemoveAt (0);

						if (child.ChildCount == 0)
							continue;
						count += child.ChildCount;
						queue.AddRange (child.Children);
					}
					Console.WriteLine ("FSQ Debughook: {0} has {1} subdirs", root.FullName, count);
				}
			}

			tree_crawl_task.DebugHook ();
			file_crawl_task.DebugHook ();
			fs_event_generator.DebugHook ();
			uid_manager.DebugHook ();
		}
	}
}
	
