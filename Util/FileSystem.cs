//
// FileSystem.cs
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
using System.IO;

namespace Beagrep.Util {
	
	public class FileSystem {

		// Value returned by GetLastWriteTimeUtc() for files which
		// don't exist.  Allows us to do non-racy existence checks if
		// we're getting the last write time
		static private DateTime nonexistent_file_datetime = new DateTime (1601, 1, 1, 0, 0, 0, DateTimeKind.Utc);

		static public bool Exists (string path)
		{
			return File.Exists (path) || Directory.Exists (path);
		}

		static public DateTime GetLastWriteTimeUtc (string path)
		{
			if (path == null)
				throw new ArgumentNullException ("path");

			if (File.Exists (path))
				return File.GetLastWriteTimeUtc (path);
			else if (Directory.Exists (path))
				return Directory.GetLastWriteTimeUtc (path);
			else
				return new DateTime (1601, 1, 1, 0, 0, 0, DateTimeKind.Utc);
		}

		// Assumes input is UTC
		static public bool ExistsByDateTime (DateTime date)
		{
			if (date != nonexistent_file_datetime)
				return true;
			else
				return false;
		}

		static public FileSystemInfo New (string path)
		{
			if (Directory.Exists (path))
				return new DirectoryInfo (path);
			return new FileInfo (path);
		}

		// I guess this is as good a place for this as any.
		static public bool IsSymLink (string path)
		{
			Mono.Unix.Native.Stat stat;
			Mono.Unix.Native.Syscall.lstat (path, out stat);
			return (stat.st_mode & Mono.Unix.Native.FilePermissions.S_IFLNK) == Mono.Unix.Native.FilePermissions.S_IFLNK;
		}

		static public string Readlink (string path) {
			System.Text.StringBuilder sb = new System.Text.StringBuilder();
			int n = Mono.Unix.Native.Syscall.readlink(path, sb);
			if (n >= 0) {
				return sb.ToString(0, n);
			}
			return "";
		}

		static public bool IsSpecialFile (string path)
		{
			Mono.Unix.Native.Stat stat;
			Mono.Unix.Native.Syscall.lstat (path, out stat);

			Mono.Unix.Native.FilePermissions type = (stat.st_mode & Mono.Unix.Native.FilePermissions.S_IFMT);

			if (type == Mono.Unix.Native.FilePermissions.S_IFLNK
			    || type == Mono.Unix.Native.FilePermissions.S_IFCHR
			    || type == Mono.Unix.Native.FilePermissions.S_IFBLK
			    || type == Mono.Unix.Native.FilePermissions.S_IFIFO
			    || type == Mono.Unix.Native.FilePermissions.S_IFSOCK)
				return true;

			return false;
		}

		public static bool IsWritable (string path)
		{
			Mono.Unix.Native.Stat stat;
			Mono.Unix.Native.Syscall.lstat (path, out stat);

			Mono.Unix.Native.FilePermissions type = (stat.st_mode & Mono.Unix.Native.FilePermissions.S_IFMT);

			if (type == Mono.Unix.Native.FilePermissions.S_IWUSR
			    || type == Mono.Unix.Native.FilePermissions.S_IWGRP
			    || type == Mono.Unix.Native.FilePermissions.S_IWOTH)
				return true;

			return false;
		}

		// Special version of this function which handles the root directory.
		static public string GetDirectoryNameRootOk (string path)
		{
			// System.IO.Path.GetDirectoryName ("/") returns null.
			// Handle it specially.
			if (path == "/")
				return path;

			return System.IO.Path.GetDirectoryName (path);
		}

		// Based on Path.GetTempFileName() from Mono
                public static string GetTempFileName (string extension)
                {
                        FileStream f = null;
                        string path;
                        Random rnd;
                        int num = 0;

			if (! String.IsNullOrEmpty (extension) && extension [0] != '.')
				extension = "." + extension;

                        rnd = new Random ();
                        do {
                                num = rnd.Next ();
                                num++;
                                path = Path.Combine (Path.GetTempPath(), "tmp" + num.ToString("x") + extension);

                                try {
                                        f = new FileStream (path, FileMode.CreateNew);
                                } catch { }
                        } while (f == null);
                        
                        f.Close();
                        return path;
                }

		// Windows (and hence .Net File.Delete) requires write
		// permission on a file to delete it. This is different from
		// the POSIX behaviour and works against our readonly
		// tmp files.
		public static void PosixDelete (string path)
		{
			int ret = Mono.Unix.Native.Syscall.unlink (path);
		    	if (ret == -1)
				throw new System.IO.IOException (String.Format (
					    "Delete failed for {0}: {1}",
					    path,
					    Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError ())));
		}
	}

	public class NoSpaceException : Exception {
	}

}
