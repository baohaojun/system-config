//
// TreeCrawlTask.cs
//
// Copyright (C) 2005 Novell, Inc.
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
using System.Collections;
using System.IO;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	public class TreeCrawlTask : Scheduler.Task {

		public delegate void Handler (DirectoryModel parent, string name);

		private object big_lock = new object ();
		private bool is_active = false;
		private FileSystemQueryable queryable;
		private Handler handler;
		private Queue to_be_crawled = new Queue ();

		public TreeCrawlTask (FileSystemQueryable queryable, Handler handler)
		{
			this.queryable = queryable;
			this.handler = handler;
			this.Tag = "Tree Crawler";
			this.Priority = Scheduler.Priority.Delayed;
		}

		public bool IsActive {
			get { lock (big_lock) return is_active; }
		}

		// Must be called from inside big_lock
		private void SetIsActive (bool is_active, DirectoryModel current_dir)
		{
			this.is_active = is_active;

			queryable.UpdateIsIndexing (current_dir);
		}

		// Returns 'true' if the queue was empty before adding
		// this item.
		public bool Add (DirectoryModel dir)
		{
			lock (big_lock) {
				bool was_empty;
				was_empty = (to_be_crawled.Count == 0);

				if (!was_empty && to_be_crawled.Contains (dir))
					return false;

				to_be_crawled.Enqueue (dir);
				Description = String.Format ("Pending directories: {0}", to_be_crawled.Count);
				return was_empty;
			}
		}

		override protected void DoTaskReal ()
		{
			DirectoryModel dir;

			lock (big_lock) {
				if (to_be_crawled.Count == 0) {
					DoneCrawling ();
					return;
				}
				dir = to_be_crawled.Dequeue () as DirectoryModel;

				if (FileSystemQueryable.Debug)
					Log.Debug ("Running tree crawl task");

				SetIsActive (true, dir);
			}
			
			LuceneQueryable queryable = (LuceneQueryable) Source;

			if (dir.IsAttached) {
				if (FileSystemQueryable.Debug)
					Logger.Log.Debug ("Scanning '{0}' for subdirectories", dir.FullName);

				try {
					foreach (string name in DirectoryWalker.GetDirectoryNames (dir.FullName)) {
						string path;
						path = Path.Combine (dir.FullName, name);
						if (!FileSystem.IsSpecialFile (path))
							handler (dir, name);
					}
				} catch (DirectoryNotFoundException ex) {
					Logger.Log.Debug ("Couldn't scan '{0}' for subdirectories", dir.FullName);
				}
			}

			lock (big_lock) {
				if (to_be_crawled.Count != 0)
					Reschedule = true;
				else
					DoneCrawling ();
			}
		}

		private void DoneCrawling ()
		{
			Log.Debug ("Done crawling directory tree!!!");
			SetIsActive (false, null);
		}

		internal void DebugHook ()
		{
			lock (big_lock)
				Log.Debug ("FSQ:TreeCrawlTask Debughook: {0} directories left to crawl, current {1}active", to_be_crawled.Count, is_active ? String.Empty : "in");
		}
	}
}
