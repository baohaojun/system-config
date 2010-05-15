/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

using System;
using Mono.Unix.Native;

namespace Lucene.Net.Store
{
	
	/// <summary> Implements {@link LockFactory} using {@link File#createNewFile()}.  This is
	/// currently the default LockFactory used for {@link FSDirectory} if no
	/// LockFactory instance is otherwise provided.
	/// 
	/// Note that there are known problems with this locking implementation on NFS.
	/// 
	/// </summary>
	/// <seealso cref="LockFactory">
	/// </seealso>
	
	public class SimpleFSLockFactory : LockFactory
	{
		
		/// <summary> Directory specified by <code>Lucene.Net.lockDir</code>
		/// system property.  If that is not set, then <code>java.io.tmpdir</code>
		/// system property is used.
		/// </summary>
		
		private System.IO.FileInfo lockDir;
		
		/// <summary> Instantiate using the provided directory (as a File instance).</summary>
		/// <param name="lockDir">where lock files should be created.
		/// </param>
		public SimpleFSLockFactory(System.IO.FileInfo lockDir)
		{
			Init(lockDir);
		}
		
		/// <summary> Instantiate using the provided directory name (String).</summary>
		/// <param name="lockDirName">where lock files should be created.
		/// </param>
		public SimpleFSLockFactory(System.String lockDirName)
		{
			lockDir = new System.IO.FileInfo(lockDirName);
			Init(lockDir);
		}

        /// <summary>
        /// </summary>
        internal SimpleFSLockFactory()
        {
            lockDir = null;
        }

        /// <summary>
        /// Set the lock directory.  This is package-private and is
        /// only used externally by FSDirectory when creating this
        /// LockFactory via the System property
        /// org.apache.lucene.store.FSDirectoryLockFactoryClass.
        /// </summary>
        /// <param name="lockDir"></param>
        internal void SetLockDir(System.IO.FileInfo lockDir)
        {
            this.lockDir = lockDir;
            if (lockDir != null)
            {
                // Ensure that lockDir exists and is a directory.
                if (!lockDir.Exists)
                {
                    if (System.IO.Directory.CreateDirectory(lockDir.FullName) == null)
                        throw new System.IO.IOException("Cannot create directory: " + lockDir.FullName);
                }
                else if (!new System.IO.DirectoryInfo(lockDir.FullName).Exists)
                {
                    throw new System.IO.IOException("Found regular file where directory expected: " + lockDir.FullName);
                }
            }
        }
		
		protected internal virtual void  Init(System.IO.FileInfo lockDir)
		{
			this.lockDir = lockDir;
		}
		
		public override Lock MakeLock(System.String lockName)
		{
			if (lockPrefix != null)
			{
				lockName = lockPrefix + "-" + lockName;
			}
			return new SimpleFSLock(lockDir, lockName);
		}
		
		public override void  ClearLock(System.String lockName)
		{
			bool tmpBool;
			if (System.IO.File.Exists(lockDir.FullName))
				tmpBool = true;
			else
				tmpBool = System.IO.Directory.Exists(lockDir.FullName);
			if (tmpBool)
			{
				if (lockPrefix != null)
				{
					lockName = lockPrefix + "-" + lockName;
				}
				string lockFile = System.IO.Path.Combine(lockDir.FullName, lockName);
				bool tmpBool2;
				if (System.IO.File.Exists(lockFile))
					tmpBool2 = true;
				else
					tmpBool2 = System.IO.Directory.Exists(lockFile);
				bool tmpBool3;
				if (System.IO.File.Exists(lockFile))
				{
					System.IO.File.Delete(lockFile);
					tmpBool3 = true;
				}
				else if (System.IO.Directory.Exists(lockFile))
				{
					System.IO.Directory.Delete(lockFile);
					tmpBool3 = true;
				}
				else
					tmpBool3 = false;
				if (tmpBool2 && !tmpBool3)
				{
					throw new System.IO.IOException("Cannot delete " + lockFile);
				}
			}
		}
	}
	
	
	class SimpleFSLock : Lock
	{
		
		internal System.IO.FileInfo lockFile;
		internal System.IO.FileInfo lockDir;
		
		public SimpleFSLock(System.IO.FileInfo lockDir, System.String lockFileName)
		{
			this.lockDir = lockDir;
			lockFile = new System.IO.FileInfo(System.IO.Path.Combine(lockDir.FullName, lockFileName));
		}
		
		public override bool Obtain()
		{
			
			// Ensure that lockDir exists and is a directory:
			bool tmpBool;
			if (System.IO.File.Exists(lockDir.FullName))
				tmpBool = true;
			else
				tmpBool = System.IO.Directory.Exists(lockDir.FullName);
			if (!tmpBool)
			{
                try
                {
                    System.IO.Directory.CreateDirectory(lockDir.FullName);
                }
                catch
                {
                    throw new System.IO.IOException("Cannot create directory: " + lockDir.FullName);
                }
			}
			else
			{
                try
                {
                    System.IO.Directory.Exists(lockDir.FullName);
                }
                catch
                {
                    throw new System.IO.IOException("Found regular file where directory expected: " + lockDir.FullName);
                }
			}

                try
                {
		    int fd = Mono.Unix.Native.Syscall.open (
				lockFile.FullName,
				Mono.Unix.Native.OpenFlags.O_RDWR | 
				Mono.Unix.Native.OpenFlags.O_CREAT |
				Mono.Unix.Native.OpenFlags.O_EXCL,
				Mono.Unix.Native.FilePermissions.S_IRUSR);
		    if (fd == -1) {
			    Mono.Unix.Native.Errno error = Mono.Unix.Native.Stdlib.GetLastError ();
			    if (error == Mono.Unix.Native.Errno.ENOSPC)
				    throw new Beagle.Util.NoSpaceException ();
			    else
				    throw new System.IO.IOException ("Could not create lock file: "
					    + Mono.Unix.Native.Stdlib.strerror (error));
		    }

		    // This code replaces the commented-out code below because
		    // it ends up being much faster.  The reason for this is
		    // that closing a UnixStream causes Syscall.fsync() to be
		    // called, and that apparently is extremely slow!
		    //
		    // Time(ms) Count   P/call(ms) Method name
		    // 1563.926      68   22.999   Mono.Unix.Native.Syscall::fsync(int)
		    //
		    // Since the lock file is written out very often, this time
		    // adds up and noticably slows down indexing.
		    IntPtr ptr = IntPtr.Zero;
		    long ret;

		    try {
			    string s = System.Diagnostics.Process.GetCurrentProcess ().Id.ToString () + "\n";
			    ptr = Mono.Unix.UnixMarshal.StringToHeap (s);

			    do {
				    ret = Mono.Unix.Native.Syscall.write (fd, ptr, (ulong) s.Length);
			    } while (Mono.Unix.UnixMarshal.ShouldRetrySyscall ((int) ret));
			    if ((int)ret == -1) {
				    Mono.Unix.Native.Errno error = Mono.Unix.Native.Stdlib.GetLastError ();
				    if (error == Mono.Unix.Native.Errno.ENOSPC)
					    throw new Beagle.Util.NoSpaceException ();
				    else
					    Mono.Unix.UnixMarshal.ThrowExceptionForError (error);
			    }
		    } finally {
			    Mono.Unix.UnixMarshal.FreeHeap (ptr);

			    do {
				    ret = Mono.Unix.Native.Syscall.close (fd);
			    } while (Mono.Unix.UnixMarshal.ShouldRetrySyscall ((int) ret));
			    Mono.Unix.UnixMarshal.ThrowExceptionForLastErrorIf ((int) ret);
		    }

		    //System.IO.StreamWriter w = new System.IO.StreamWriter (new Mono.Unix.UnixStream (fd, true));
		    //w.WriteLine (System.Diagnostics.Process.GetCurrentProcess ().Id);
		    //w.Close ();
                    return true;
                }
		catch (Beagle.Util.NoSpaceException e)
		{
			throw e;
		}
                catch (Exception e)
                {
		    Log ("Exception in CreateNew for file:" + lockFile.FullName + ":" + e);
                    return false;
                }
		}
		
		public override void  Release()
		{
			Beagle.Util.FileSystem.PosixDelete (lockFile.FullName);

			if (System.IO.File.Exists(lockFile.FullName)) {
				Beagle.Util.Logger.Log.Warn ("Release didnt delete lockfile {0}.", lockFile.FullName);
			}
		}
		
		public override bool IsLocked()
		{
			bool tmpBool;
			if (System.IO.File.Exists(lockFile.FullName))
				tmpBool = true;
			else
				tmpBool = System.IO.Directory.Exists(lockFile.FullName);
			return tmpBool;
		}
		
		public override System.String ToString()
		{
			return "SimpleFSLock@" + lockFile.FullName;
		}
		
		static public Beagle.Util.Logger Logger = null;
		//static public Beagle.Util.Logger Logger = Beagle.Util.Logger.Log;
		static public void Log (string format, params object[] args)
		{
			if (Logger != null)
				Logger.Debug (format, args);
		}

		static public void Log (Exception e)
		{
			if (Logger != null)
				Logger.Debug (e);
		}
		
	}
}
