//
// FileAttributesStore.cs
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

using Beagle.Util;

namespace Beagle.Daemon {

	public class FileAttributesStore : IDisposable {
		
		private static bool Debug = Beagle.Util.Debug.Enabled ("FileAttributesStore");

		private IFileAttributesStore ifas;

		public FileAttributesStore (IFileAttributesStore ifas)
		{
			this.ifas = ifas;
		}

		public void Dispose ()
		{
			ifas.Dispose ();
		}

		public FileAttributes Read (string path)
		{
			lock (ifas) {
				if (Debug)
					Log.Debug ("Reading attr for {0}", path);

				FileAttributes attr = ifas.Read (path);

				if (Debug)
					Log.Debug ("  attr is {0}null", attr != null ? "non-" : "");

				return attr;
			}
		}

		public FileAttributes ReadOrCreate (string path, Guid unique_id, out bool created)
		{
			lock (ifas) {
				created = false;

				if (Debug)
					Log.Debug ("Reading or creating attr for {0}", path);

				FileAttributes attr = ifas.Read (path);

				if (attr == null && unique_id == Guid.Empty)
					unique_id = Guid.NewGuid ();

				// If we pass in a Guid that doesn't match the one we found in the
				// the attributes, clobber the old attributes and the old unique Guid.
				if (attr == null
				    || (unique_id != Guid.Empty && unique_id != attr.UniqueId)) {
					// First drop the old attribute, if there is one.
					if (attr != null)
						ifas.Drop (path);

					// Now create the new attribute
					attr = new FileAttributes ();
					attr.UniqueId = unique_id;
					attr.Path = path;
					
					created = true;
				}

				if (Debug)
					Log.Debug ("  {0} attr for {1}", created ? "Created" : "Read existing", path);

				return attr;
			}
		}

		public FileAttributes ReadOrCreate (string path, Guid unique_id)
		{
			bool dummy;
			return ReadOrCreate (path, unique_id, out dummy);
		}

		public FileAttributes ReadOrCreate (string path)
		{
			return ReadOrCreate (path, Guid.Empty);
		}

		public bool Write (FileAttributes attr)
		{
			lock (ifas) {
				if (Debug)
					Log.Debug ("Writing attr for {0}", attr.Path);

				attr.LastAttrTime = DateTime.UtcNow;

				bool success = ifas.Write (attr);

				if (Debug)
					Log.Debug ("  write {0}", success ? "succeeded" : "FAILED");

				return success;
			}
		}

		public void Drop (string path)
		{
			lock (ifas) {
				if (Debug)
					Log.Debug ("Dropping attr for {0}", path);

				ifas.Drop (path);
			}
		}

		public void BeginTransaction ()
		{
			lock (ifas)
				ifas.BeginTransaction ();
		}

		public void CommitTransaction ()
		{
			lock (ifas)
				ifas.CommitTransaction ();
		}

		//////////////////////////////////////////////////////////

		public static bool IsUpToDate (string path, FileAttributes attr)
		{
			if (attr == null)
				return false;

			return (attr.LastWriteTime >= FileSystem.GetLastWriteTimeUtc (path));
		}

		// To be used if the last_write_time to use for comparison is not the
		// one obtained from path
		public static bool IsUpToDate (FileAttributes attr, DateTime last_write_time)
		{
			if (attr == null)
				return false;

			return (attr.LastWriteTime >= last_write_time);
		}

		public bool IsUpToDate (string path, Filter filter)
		{
			return IsUpToDate (path, filter, DateTime.MaxValue);
		}

		// To be used if the last_write_time to use for comparison is not the
		// one obtained from path
		public bool IsUpToDate (string path, Filter filter, DateTime last_write_time)
		{
			FileAttributes attr;

			attr = Read (path);

			// If there are no attributes set on the file, there is no
			// way that we can be up-to-date.
			// Also, if attribute has no filter information, try once
			// again.
			if (attr == null)
				return false;

			// Note that when filter is set to null, we ignore
			// any existing filter data.  That might not be the
			// expected behavior...
			if (filter != null) {

				if (! attr.HasFilterInfo)
					return false;

				if (attr.FilterName != filter.Name)
					return false;
				
				// FIXME: Obviously we want to reindex if
				// attr.FilterVersion < filter.Version.
				// But what if the filter we would use is older
				// than the one that was previously used?
				if (attr.FilterVersion != filter.Version)
					return false;
			} 

			if (last_write_time == DateTime.MaxValue)
				return (attr.LastWriteTime >= FileSystem.GetLastWriteTimeUtc (path));
			else
				return (attr.LastWriteTime >= last_write_time);
		}

		public bool IsUpToDateAndFiltered (string path)
		{
			return IsUpToDateAndFiltered (path, DateTime.MaxValue);
		}

		// To be used if the last_write_time to use for comparison is not the
		// one obtained from path
		public bool IsUpToDateAndFiltered (string path, DateTime last_write_time)
		{
			FileAttributes attr;

			attr = Read (path);

			// If there are no attributes set on the file, there is no
			// way that we can be up-to-date.
			if (attr == null)
				return false;

			if (! FilterFactory.DirtyFilterCache) {
				// If the filters are same as in last run,
				// since attr is not null, check the timestamps (bypass HasFilterInfo)
				if (last_write_time == DateTime.MaxValue)
					return (attr.LastWriteTime >= FileSystem.GetLastWriteTimeUtc (path));
				else
					return (attr.LastWriteTime >= last_write_time);
			}

			// If there is a new filter in the mean time
			// take previous filter information into consideration.
			if (! attr.HasFilterInfo)
				return false;

			int current_filter_version;
			current_filter_version = FilterFactory.GetFilterVersion (attr.FilterName);
			if (current_filter_version > attr.FilterVersion)
				return false;

			if (last_write_time == DateTime.MaxValue)
				return (attr.LastWriteTime >= FileSystem.GetLastWriteTimeUtc (path));
			else
				return (attr.LastWriteTime >= last_write_time);
		}

		public bool IsUpToDate (string path)
		{
			return IsUpToDate (path, DateTime.MaxValue);
		}

		// To be used if the last_write_time to use for comparison is not the
		// one obtained from path
		public bool IsUpToDate (string path, DateTime last_write_time)
		{
			return IsUpToDate (path, (Filter) null);
		}

		//////////////////////////////////////////////////////////

		// A convenience routine.
		public void AttachLastWriteTime (string path, DateTime mtime)
		{
			FileAttributes attr = ReadOrCreate (path);
			attr.LastWriteTime = mtime;
			if (! Write (attr))
				Logger.Log.Warn ("Couldn't store file attributes for {0}", path);
		}
	}
}
