
//
// MailCrawler.cs
//
// Copyright (C) 2004 Novell, Inc.
//
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

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Daemon.EvolutionMailQueryable {
	
	class MailCrawler {
		public delegate void ItemAddedHandler (FileInfo file, bool inotify_event);

		private static bool Debug = false;

		ArrayList roots = new ArrayList ();

		Hashtable last_write_time_cache = new Hashtable ();

		public ItemAddedHandler MboxAddedEvent;
		public ItemAddedHandler SummaryAddedEvent;
		
		public MailCrawler (params string[] paths)
		{
			foreach (string p in paths) {
				if (Directory.Exists (p))
					roots.Add (p);
			}
		}

		private bool FileIsInteresting (FileInfo file)
		{
			if (! file.Exists)
				return false;

			DateTime cached_time = new DateTime ();
			if (last_write_time_cache.Contains (file.FullName))
				cached_time = (DateTime) last_write_time_cache [file.FullName];
			
			last_write_time_cache [file.FullName] = file.LastWriteTimeUtc;
			
			return cached_time < file.LastWriteTimeUtc;
		}

		private void OnInotifyEvent (Inotify.Watch watch,
					     string path,
					     string subitem,
					     string srcpath,
					     Inotify.EventType type)
		{
			if (subitem == "")
				return;

			string full_path = Path.Combine (path, subitem);

			if ((type & Inotify.EventType.Create) != 0 && (type & Inotify.EventType.IsDirectory) != 0) {
				Watch (full_path);
				return;
			}

			if ((type & Inotify.EventType.Delete) != 0 && (type & Inotify.EventType.IsDirectory) != 0) {
				watch.Unsubscribe ();
				return;
			}

			if ((type & Inotify.EventType.MovedTo) != 0) {
				if (subitem == "summary") {
					// IMAP summary
					if (SummaryAddedEvent != null && File.Exists (full_path)) {
						Logger.Log.Info ("Reindexing updated IMAP summary: {0}", full_path);
						SummaryAddedEvent (new FileInfo (full_path), true);
					}
				} else if (Path.GetExtension (full_path) == ".ev-summary") {
					// mbox summary
					string mbox_file = Path.ChangeExtension (full_path, null);
					if (MboxAddedEvent != null && File.Exists (mbox_file)) {
						Logger.Log.Info ("Reindexing updated mbox: {0}", mbox_file);
						MboxAddedEvent (new FileInfo (mbox_file), true);
					}
				}
			}
		}

		private void Watch (string root)
		{
			Queue pending = new Queue ();

			pending.Enqueue (root);

			while (pending.Count > 0) {

				string dir = (string) pending.Dequeue ();

				foreach (string subdir in DirectoryWalker.GetDirectories (dir)) {
					if (Shutdown.ShutdownRequested)
						return;

					if (Inotify.Enabled) {
						Inotify.Subscribe (subdir, OnInotifyEvent,
								   Inotify.EventType.Create
								   | Inotify.EventType.Delete
								   | Inotify.EventType.MovedTo);
					}

					pending.Enqueue (subdir);
				}

				Stopwatch watch = new Stopwatch ();
				if (Debug)
					Logger.Log.Debug ("Starting watch on {0}", dir);
				watch.Start ();
				foreach (string path in DirectoryWalker.GetItems (dir, new DirectoryWalker.FileFilter (IsSummary))) {
					if (Shutdown.ShutdownRequested)
						return;

					FileInfo file = new FileInfo (path);

					if (file.Name == "summary") {
						if (SummaryAddedEvent != null && FileIsInteresting (file))
							SummaryAddedEvent (file, false);
					} else if (file.Extension == ".ev-summary") {
						string mbox_name = Path.Combine (file.DirectoryName,
										 Path.GetFileNameWithoutExtension (file.Name));
						FileInfo mbox_file = new FileInfo (mbox_name);
						if (MboxAddedEvent != null && FileIsInteresting (mbox_file))
							MboxAddedEvent (mbox_file, false);
					}
				}
				watch.Stop ();
				if (Debug)
					Logger.Log.Debug ("Crawled {0} in {1}", dir, watch);
			}
		}

		private bool IsSummary (string path, string name)
		{
			if (name == "summary")
				return true;

			if (Path.GetExtension (name) == ".ev-summary")
				return true;

			return false;
		}

		public void Crawl ()
		{
			foreach (string root in roots) {
				if (Shutdown.ShutdownRequested)
					return;

				Watch (root);
			}
		}
	}
}
