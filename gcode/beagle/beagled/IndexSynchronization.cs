//
// IndexSynchronization.cs
//
// Copyright (C) 2005-2006 Novell, Inc.
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
using System.IO;
using System.Collections;
using System.Threading;

using Beagle.Util;

namespace Beagle.Daemon {

	public class IndexSynchronization {
		
		// 1 hour synchronization period
		static private int sync_interval_in_minutes = 60;
		
		// Synchonization lock object
		static private object synchronization_lock = new object ();

		// Original index storage directory
		static private string remote_index_dir = Path.Combine (PathFinder.StorageDir, "Indexes");

		// Locally sync'd storage directory
		static private string local_index_dir = PathFinder.IndexDir;

		////////////////////////////////////////////////////////////////

		public enum SynchronizationTarget {
			Local,
			Remote
		}

		static public void Initialize ()
		{
			Logger.Log.Debug ("Initializing index synchronization");

			if (! Directory.Exists (remote_index_dir))
				Directory.CreateDirectory (remote_index_dir);

			Log.Debug ("Remote index storage dir {0} will be synchronized to temp local storage dir {1}", remote_index_dir, local_index_dir);

			// Initial index synchronization			
			Synchronize (SynchronizationTarget.Local);

			Scheduler.Task task;
			
			// Add the synchronization task to the scheduler
			task = Scheduler.TaskFromHook (new Scheduler.TaskHook (SynchronizeHook));
			task.Tag = "Synchronize Indexes";
			task.Priority = Scheduler.Priority.Delayed;
			task.TriggerTime = DateTime.Now.AddMinutes (sync_interval_in_minutes);
			task.Source = synchronization_lock;
			Scheduler.Global.Add (task);

			// Set up the shutdown synchronization task
			task = Scheduler.TaskFromHook (new Scheduler.TaskHook (ShutdownHook));
			task.Tag = "Synchronize Indexes on Shutdown";
			task.Priority = Scheduler.Priority.Shutdown;
			task.Source = synchronization_lock;
			Scheduler.Global.Add (task);
		}

		////////////////////////////////////////////////////////////////

		static public void Synchronize (SynchronizationTarget target)
		{
			Stopwatch watch = new Stopwatch ();
			watch.Start ();

			lock (synchronization_lock) {
				Logger.Log.Debug ("Synchronizing... (target={0})", target);

				DirectoryInfo source_directory, target_directory;
				source_directory = new DirectoryInfo ((target == SynchronizationTarget.Local) ? remote_index_dir : local_index_dir);
				target_directory = new DirectoryInfo ((target == SynchronizationTarget.Local) ? local_index_dir : remote_index_dir);
				
				if (SynchronizeDirectory (source_directory, target_directory))
					Logger.Log.Debug ("Synchronized successfully in {0}", watch);
			}
		}
		
		////////////////////////////////////////////////////////////////

		static private void SynchronizeHook (Scheduler.Task task)
                {
			try {
				Synchronize (SynchronizationTarget.Remote);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught exception while synchronizing");
			}

                        task.Reschedule = true;
                        task.TriggerTime = DateTime.Now.AddMinutes (sync_interval_in_minutes);
                }
		
		static private void ShutdownHook (Scheduler.Task task)
		{
			try {
				Synchronize (SynchronizationTarget.Remote);

				// FIXME: This may not be safe to do here
				Logger.Log.Debug ("Purging locally synchronized indexes");
				Directory.Delete (local_index_dir, true);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught exception while doing shutdown synchronization");
			}
		}

		////////////////////////////////////////////////////////////////
		
		// FIXME: Non-Getto synchronization
		static private bool SynchronizeDirectory (DirectoryInfo source_directory,
							  DirectoryInfo target_directory)
		{
			if (! source_directory.Exists)
				throw new Exception ("Synchronization error: Source directory does not exist");

			DirectoryInfo target_directory_temp, target_directory_trash;

			// Setup directories for the copy/move/move/delete procedure
			target_directory_temp = new DirectoryInfo (target_directory.FullName + ".tmp");
			target_directory_trash = new DirectoryInfo (target_directory.FullName + ".trash");

			if (target_directory_temp.Exists)
				target_directory_temp.Delete (true);
			target_directory_temp.Create ();

			try {
				// Copy the directory structure and files
				CopyDirectoryRecursively (source_directory, target_directory_temp);

				if (target_directory_trash.Exists)
					target_directory_trash.Delete (true);
				
				target_directory.MoveTo (target_directory_trash.FullName);
				target_directory_temp.MoveTo (target_directory.FullName);
				
				target_directory_trash.Delete (true);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught error while synchronizing directory");
				return false;
			}

			return true;
		}

		static private void CopyDirectoryRecursively (DirectoryInfo source_directory,
							      DirectoryInfo target_directory)
		{
			if (!target_directory.Exists)
				target_directory.Create ();

			foreach (FileInfo source_file in DirectoryWalker.GetFileInfos (source_directory)) {
				FileInfo target_file = new FileInfo (Path.Combine (target_directory.FullName, source_file.Name));
				
				// FIXME: Don't hard code filenames - Mono.Posix.StatMode.Regular
				if (source_file.Name.IndexOf ("socket") != -1 ||
				    source_file.Name.EndsWith ("-journal"))
					continue;
				
				File.Copy (source_file.FullName, target_file.FullName, true);
			}
			
			foreach (DirectoryInfo source_child_directory in DirectoryWalker.GetDirectoryInfos (source_directory)) {
				DirectoryInfo target_child_directory = new DirectoryInfo (Path.Combine (target_directory.FullName, source_child_directory.Name));
				
				CopyDirectoryRecursively (source_child_directory,
							  target_child_directory);
			}
		}
	}
}
