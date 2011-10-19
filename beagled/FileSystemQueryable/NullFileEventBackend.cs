//
// NullFileEventBackend.cs
//
// Copyright (C) 2006 Novell, Inc.
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

using Beagle.Util;

namespace Beagle.Daemon.FileSystemQueryable {

	public class NullFileEventBackend : IFileEventBackend {

		static public bool Debug = false;

		public object CreateWatch (string path)
		{
			if (Debug)
				Logger.Log.Debug ("NullFileEventBackend: Asked to watch {0}", path);

			return null;
		}

		public bool ForgetWatch (object watch_object)
		{
			if (Debug)
				Logger.Log.Debug ("NullFileEventBackend: Asked to forget {0}", watch_object);

			return false;
		}

		public void Start (FileSystemQueryable queryable)
		{
		}
	}
}