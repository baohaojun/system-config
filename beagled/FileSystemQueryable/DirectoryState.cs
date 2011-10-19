//
// DirectoryState.cs
//
// Copyright (C) 2005 Novell, Inc.
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

namespace Beagle.Daemon.FileSystemQueryable {

	public enum DirectoryState {

		// Everything is A-OK: this directory is fully indexed and
		// is being watched.
		Clean         = 0, 

		// It was clean last time we checked.  This state only occurs
		// in unwatched directories, and is the 'best' state such a
		// directory can be in.
		PossiblyClean = 1,

		// Something might have changed, but we aren't sure.
		Unknown       = 2, 

		// Something definitely has changed.
		Dirty         = 3,

		// Something is wrong: probably funky permissions.
		// Never try to crawl a directory in this state.
		Uncrawlable   = 4
	}

}
