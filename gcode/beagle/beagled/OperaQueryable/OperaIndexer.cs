//
// OperaIndexer.cs: Indexeds cache directories and maintains the index
//
// Copyright (C) 2007 Kevin Kubasik <Kevin@Kubasik.net>
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.IO;
using System.Collections;

using Beagle;
using Beagle.Util;

namespace Beagle.Daemon.OperaQueryable {
	
	public class OperaIndexer {
		private FileAttributesStore attribute_store;
		private ArrayList cache_dirs;
		private OperaQueryable queryable;
		
		public OperaIndexer(OperaQueryable queryable, FileAttributesStore store, string root_dir)
		{
			this.attribute_store = store;
			this.queryable = queryable;
			this.cache_dirs = new ArrayList ();
			
			// Try to find all cache dirs
			foreach (string dir in DirectoryWalker.GetDirectories (root_dir)) {
				foreach (string file in DirectoryWalker.GetItems
					(dir, new DirectoryWalker.FileFilter (IsCacheFile))) {
					Inotify.Subscribe (dir, OnInotify, Inotify.EventType.MovedTo | Inotify.EventType.CloseWrite);
						cache_dirs.Add (dir);
				}
			}
		}
		
		private bool IsCacheFile (string path, string filename)
		{
			if (filename.StartsWith ("dcache") && Path.GetExtension (filename) == ".url")
				return true;
			
			return false;
		}
		
		public void Crawl ()
		{
			if (cache_dirs.Count < 1) {
				Logger.Log.Info ("No Opera history to index!");
				return;
			}
			
			foreach (string cache_dir in cache_dirs)
				IndexDirectory (cache_dir);
		}
		
		public void IndexDirectory (string dir)
		{
			if (dir == null)
				return;
			
			OperaIndexableGenerator generator = new OperaIndexableGenerator (this, dir);
			AddTask (generator, dir);
		}
		
		private void AddTask (IIndexableGenerator generator, string tag)
		{
			if (queryable.ThisScheduler.ContainsByTag (tag)) {
				Logger.Log.Info ("Not adding already running task: {0}", tag);
				return;
			}
			
			Scheduler.Task task = queryable.NewAddTask (generator);
			task.Tag = tag;
			queryable.ThisScheduler.Add (task);
		}
		
		private void OnInotify (Inotify.Watch watch,
						string path,
						string subitem,
						string srcpath,
						Inotify.EventType event_type)
		{
			if ((event_type & Inotify.EventType.MovedTo) != 0 && subitem == "dcache4.url") {
				IndexDirectory (path);
				return;
			}
		}
		
		public FileAttributesStore AttributeStore {
			get { return attribute_store; }
		}
	}
	
}
