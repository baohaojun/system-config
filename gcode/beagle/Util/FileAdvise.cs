//
// FileAdvise.cs
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

// FIXME: This is not portable to Win32

using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;

using Beagle.Util;

namespace Beagle.Util {

	public class FileAdvise {

		// FIXME: On 64-bit architectures, we need to use "long" not "int" here for
		// "offset" and "len"
		[DllImport ("libc", SetLastError=true)]
		static extern int posix_fadvise (int fd, int offset, int len, int advice);

		// The following are from /usr/include/linux/fadvise.h and will not change
		private const int AdviseNormal = 0;	// POSIX_FADV_NORMAL
		private const int AdviseRandom = 1;	// POSIX_FADV_RANDOM
		private const int AdviseSequential = 2;	// POSIX_FADV_SEQUENTIAL
		private const int AdviseWillNeed = 3;	// POSIX_FADV_WILLNEED
		private const int AdviseDontNeed = 4;	// POSIX_FADV_DONTNEED
		private const int AdviseNoReUse = 5;	// POSIX_FADV_NOREUSE

		static private int GiveAdvice (FileStream file, int advice)
		{
			int fd = file.Handle.ToInt32 ();
			return posix_fadvise (fd, 0, 0, advice);
		}

		static public void FlushCache (FileStream file)
		{
			GiveAdvice (file, AdviseDontNeed);
		}

		static public void PreLoad (FileStream file)
		{
			GiveAdvice (file, AdviseWillNeed);
		}

		static public void IncreaseReadAhead (FileStream file)
		{
			GiveAdvice (file, AdviseSequential);
		}

		static public void DisableReadAhead (FileStream file)
		{
			GiveAdvice (file, AdviseRandom);
		}

		static public void NormalReadAhead (FileStream file)
		{
			GiveAdvice (file, AdviseNormal);
		}

		static public void WillAccessOnlyOnce (FileStream file)
		{
			GiveAdvice (file, AdviseNoReUse);
		}

		static public void TestAdvise ()
		{
			try {
				FileStream file = new FileStream ("/etc/fstab",
								  System.IO.FileMode.Open,
								  FileAccess.Read);
				int ret = GiveAdvice (file, AdviseNormal);
				if (ret != 0)
					Log.Error ("FileAdvise failed: {0}", Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError()));
				file.Close ();
			} catch { }
		}
	}
}
