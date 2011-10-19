//
// FileNameFilter.cs
//
// Copyright (C) 2004, 2005 Novell, Inc.
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
using System.Text.RegularExpressions;
using System.IO;

using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	public class FileNameFilter {

		private FileSystemQueryable queryable;

		private static bool Debug = false;
		
		// User defined paths to exclude
		private ArrayList exclude_paths = new ArrayList ();
		
		// User defined exclude patterns
		private Regex exclude_regex = null;
		private ArrayList exclude_patterns = new ArrayList ();

		/////////////////////////////////////////////////////////////

		private void AddExcludePattern (string value)
		{
			if (String.IsNullOrEmpty (value))
				return;

			if (Debug)
				Logger.Log.Debug ("FileNameFilter: Adding ExcludePattern '{0}'", value);

			exclude_patterns.Add (value);
		}

		private void RemoveExcludePattern (string value)
		{
			if (String.IsNullOrEmpty (value))
				return;

			if (Debug)
				Logger.Log.Debug ("FileNameFilter: Removing ExcludePattern '{0}'", value);

			exclude_patterns.Remove (value);
		}

		private void AddExcludeDir (string value)
		{
			if (String.IsNullOrEmpty (value))
				return;

			if (Debug)
				Logger.Log.Debug ("FileNameFilter: Adding ExcludeDir '{0}'", value);

			exclude_paths.Add (value);
			queryable.RemoveDirectory (value);
		}

		private void RemoveExcludeDir (string value)
		{
			if (String.IsNullOrEmpty (value))
				return;

			if (Debug)
				Logger.Log.Debug ("FileNameFilter: Removing ExcludeDir '{0}'", value);

			exclude_paths.Remove (value);
			// Make sure we re-crawl the paths we used to ignored but
			// no longer do.
			queryable.Recrawl (value);
		}

		// Returns the full directory path for this value, after stripping end '/' and expanding env variable
		private static string GetExcludeDirectory (string value)
		{
			if (String.IsNullOrEmpty (value))
				return null;

			// Remove end '/'
			if (value != "/")
				value = value.TrimEnd ('/');

			value = StringFu.ExpandEnvVariables (value);
			if (String.IsNullOrEmpty (value))
				return null;

			value = Path.GetFullPath (value);
			if (Directory.Exists (value))
				return value;

			return null;
		}

		/////////////////////////////////////////////////////////////

		public FileNameFilter (FileSystemQueryable queryable)
		{
			this.queryable = queryable;

			LoadConfiguration ();
		}

		/////////////////////////////////////////////////////////////

		// Load data from configuration. Intersect deltas to the currently active excludes and
		// implement any changes upon notification.

		private void LoadConfiguration () 
		{
			Config config = Conf.Get (Conf.Names.FilesQueryableConfig);

			List<string[]> values = config.GetListOptionValues (Conf.Names.ExcludeSubdirectory);
			if (values != null) {
				foreach (string[] value in values)
					AddExcludeDir (GetExcludeDirectory (value [0]));
			}

			values = config.GetListOptionValues (Conf.Names.ExcludePattern);
			if (values != null)
				foreach (string[] exclude in values)
					// RemoveQuotes from beginning and end
					AddExcludePattern (exclude [0]);

			exclude_regex = StringFu.GetPatternRegex (exclude_patterns);

			Conf.WatchForUpdates ();
			Conf.Subscribe (Conf.Names.FilesQueryableConfig, OnConfigurationChanged);
		}

		private void OnConfigurationChanged (Config config)
		{
			if (config == null || config.Name != Conf.Names.FilesQueryableConfig)
				return;

			bool clear_fs_state = false;

			List<string[]> values = config.GetListOptionValues (Conf.Names.ExcludeSubdirectory);
			if (values != null) {
				ArrayList subdirs = new ArrayList (values.Count);
				foreach (string[] value in values) {
					string dir = GetExcludeDirectory (value [0]);
					if (! String.IsNullOrEmpty (dir))
						subdirs.Add (dir);
				}

				IList excludes_wanted = subdirs;
				IList excludes_to_add, excludes_to_remove;

				ArrayFu.IntersectListChanges (excludes_wanted, 
							      exclude_paths, 
						      	      out excludes_to_add, 
						      	      out excludes_to_remove);

				// Process any excludes we think we should remove
				foreach (string path in excludes_to_remove)
					RemoveExcludeDir (path);

				// Process any excludes we found to be new
				foreach (string path in excludes_to_add)
					AddExcludeDir (path);
			}

			values = config.GetListOptionValues (Conf.Names.ExcludePattern);
			if (values != null) {
				ArrayList patterns = new ArrayList (values.Count);
				foreach (string[] value in values)
					patterns.Add (value [0]);

				IList excludes_wanted = patterns;
				IList excludes_to_add, excludes_to_remove;

				ArrayFu.IntersectListChanges (excludes_wanted, 
							      exclude_patterns,
						      	      out excludes_to_add, 
						      	      out excludes_to_remove);

				// Process any excludes we think we should remove
				foreach (string pattern in excludes_to_remove) {
					clear_fs_state = true;
					RemoveExcludePattern (pattern);
				}

				// Process any excludes we found to be new
				foreach (string pattern in excludes_to_add)
					AddExcludePattern (pattern);

				exclude_regex = StringFu.GetPatternRegex (exclude_patterns);
			}

			// If an exclude pattern is removed, we need to recrawl everything
			// so that we can index those files which were previously ignored.
			if (clear_fs_state)
				queryable.RecrawlEverything ();
		}

		/////////////////////////////////////////////////////////////

		// Try to match any of our current excludes to determine if 
		// we should ignore a file/directory or not.

		public bool Ignore (DirectoryModel parent, string name, bool is_directory) 
		{
			if (Debug)
				Logger.Log.Debug ("*** Ignore Check (parent={0}, name={1}, is_directory={2})", (parent != null) ? parent.FullName : null, name, is_directory);

			// If parent is null, we have a root. But it might not be
			// active anymore so we need to check if it's still in the list.
			if (parent == null && queryable.Roots.Contains (name)) {
				if (Debug)
					Logger.Log.Debug ("*** Ignore Check Passed");
				return false;
			}
			
			string path;
			if (parent != null)
				path = Path.Combine (parent.FullName, name);
			else
				path = name;
			
			// Exclude paths
			foreach (string exclude in exclude_paths)
				if (path.StartsWith (exclude))
					return true;
			
			// Hardcode this pattern without relying on config
			if (name.StartsWith ("."))
				return true;

			// Exclude patterns
			if (exclude_regex != null && exclude_regex.IsMatch (name))
				return true;

			if (parent == null) {
				if (Debug)
					Logger.Log.Debug ("*** Parent is null (name={0}, is_directory={1}", name, is_directory);
				return false;
			}

			// This is kind of a hack, but if parent.Parent is null, we need to pass
			// the full path of the directory as second argument to Ignore to allow
			// us to do the root check.
			return Ignore (parent.Parent, (parent.Parent == null) ? parent.FullName : parent.Name, true);
		}
	}		
}
