//
// DirectoryIndexableGenerator.cs
//
// Copyright (C) 2004 Novell, Inc.
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

using Beagrep.Util;
using Beagrep.Daemon;

namespace Beagrep.Daemon.FileSystemQueryable {

	public class DirectoryIndexableGenerator : IIndexableGenerator {

		FileSystemQueryable queryable;
		DirectoryModel directory;
		IEnumerator files;
		bool done = false;

		public DirectoryIndexableGenerator (FileSystemQueryable queryable,
						    DirectoryModel      directory)
		{
			this.queryable = queryable;
			this.directory = directory;

			if (this.directory == null)
				done = true;
			else 
				files = DirectoryWalker.GetFileInfos (this.directory.FullName).GetEnumerator ();
		}

		public Indexable GetNextIndexable ()
		{
			if (done)
				return null;

			while (files.MoveNext ()) {
				FileInfo f = files.Current as FileInfo;
				Indexable indexable = null;
				try { 
					if (f.Exists && this.directory.IsAttached)
						indexable = queryable.GetCrawlingFileIndexable (directory, f.Name);
				} catch (Exception ex) {
					Logger.Log.Debug (ex, "Caught exception calling GetCrawlingFileIndexable on '{0}'", f.FullName);
				}
				if (indexable != null)
					return indexable;
			}

			done = true;
			return null;
		}

		public bool HasNextIndexable ()
		{
			return ! done;
		}

		public string StatusName {
			get { 
				if (this.directory == null)
					return "Indexing files inside the null directory?";
				return "Indexing files inside " + this.directory.FullName;
			}
		}

		public void PostFlushHook () 
		{
			// Do nothing
		}
	}
}
