//
// EmpathyCrawler.cs
//
// Copyright (C) 2007 Kevin Kubasik
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

namespace Beagle.Daemon.EmpathyQueryable {
	
	class EmpathyCrawler {

		string log_dir;

		Hashtable last_write_time_cache = new Hashtable ();

		ArrayList log_files = new ArrayList ();
		
		public EmpathyCrawler (string log_dir)
		{
			this.log_dir = log_dir;
		}

		private bool FileIsInteresting (FileInfo file)
		{
			DateTime cached_time = new DateTime ();
			if (last_write_time_cache.Contains (file.FullName))
				cached_time = (DateTime) last_write_time_cache [file.FullName];
			
			last_write_time_cache [file.FullName] = file.LastWriteTime;
			
			return cached_time < file.LastWriteTime;
		}

		public void Crawl ()
		{
			log_files.Clear ();

			Queue pending = new Queue ();

			pending.Enqueue (log_dir);

			while (pending.Count > 0) {

				string dir = (string) pending.Dequeue ();

				foreach (string subdir in DirectoryWalker.GetDirectories (dir))
					pending.Enqueue (subdir);

				foreach (FileInfo file in DirectoryWalker.GetFileInfos (dir)) {
					if (FileIsInteresting (file))
						log_files.Add (file);
				}
			}
		}

		public ICollection Logs {
			get { return log_files; } 
		}
	}
}
