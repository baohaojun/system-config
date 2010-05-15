//
// FileAttributes.cs
//
// Copyright (C) 2004 Novell, Inc.
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

using Beagle.Util;

namespace Beagle.Daemon {

	public class FileAttributes {
		
		private Guid     unique_id;
		private string   path;
		private DateTime last_mtime;

		private DateTime last_attrtime;

		private string   filter_name;
		private int      filter_version;
		
		public Guid UniqueId {
			get { return unique_id; }
			set { unique_id = value; }
		}

		public string Directory {
			get { return FileSystem.GetDirectoryNameRootOk (path); }
			set { path = System.IO.Path.Combine (value, Filename); }
		}

		public string Filename {
			get { return System.IO.Path.GetFileName (path); }
			set { path = System.IO.Path.Combine (Directory, value); }
		}

		public string Path {
			get { return path; }
			set { 
				path = value != null ? System.IO.Path.GetFullPath (value) : null;
				if (path != null && path != "/" && path.EndsWith ("/"))
					path = path.TrimEnd ('/');
			}
		}

		public DateTime LastWriteTime {
			get { return last_mtime; }
			set { last_mtime = value; }
		}

		// When the attributes were last written out.
		public DateTime LastAttrTime {
			get { return last_attrtime; }
			set { last_attrtime = value; }
		}

		public bool HasFilterInfo {
			get { return filter_name != null && filter_version >= 0; }
		}
		
		public string FilterName {
			get { return filter_name; }
			set { filter_name = value; }
		}

		public int FilterVersion {
			get { return filter_version; }
			set { filter_version = value; }
		}
	}
}
	
