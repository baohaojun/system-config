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

namespace Lucene.Net.Store
{
	
	/// <summary>An interprocess mutex lock.
	/// <p>Typical use might look like:<pre>
	/// new Lock.With(directory.makeLock("my.lock")) {
	/// public Object doBody() {
	/// <i>... code to execute while locked ...</i>
	/// }
	/// }.run();
	/// </pre>
	/// 
	/// </summary>
	/// <author>  Doug Cutting
	/// </author>
	/// <version>  $Id: Lock.java 472959 2006-11-09 16:21:50Z yonik $
	/// </version>
	/// <seealso cref="Directory#MakeLock(String)">
	/// </seealso>
	public abstract class Lock
	{
		public static long LOCK_POLL_INTERVAL = 1000;
		
		/// <summary>Attempts to obtain exclusive access and immediately return
		/// upon success or failure.
		/// </summary>
		/// <returns> true iff exclusive access is obtained
		/// </returns>
		public abstract bool Obtain();
		
		/// <summary> If a lock obtain called, this failureReason may be set
		/// with the "root cause" Exception as to why the lock was
		/// not obtained.
		/// </summary>
		protected internal System.Exception failureReason;
		
		/// <summary>Attempts to obtain an exclusive lock within amount
		/// of time given. Currently polls once per second until
		/// lockWaitTimeout is passed.
		/// </summary>
		/// <param name="lockWaitTimeout">length of time to wait in ms
		/// </param>
		/// <returns> true if lock was obtained
		/// </returns>
		/// <throws>  IOException if lock wait times out or obtain() throws an IOException </throws>
		public virtual bool Obtain(long lockWaitTimeout)
		{
			failureReason = null;
			int maxSleepCount = (int) (lockWaitTimeout / LOCK_POLL_INTERVAL);
			int sleepCount = 0;
			maxSleepCount = System.Math.Min (maxSleepCount, 1);
			SimpleFSLock.Log ("Lock.Obtain timeout={0} maxsleepcount={1}", lockWaitTimeout, maxSleepCount);
			bool locked = Obtain();
			while (!locked)
			{
				if (sleepCount++ == maxSleepCount)
				{
					// Try and be a little more verbose on failure
					string lockpath = this.ToString ();
					System.Text.StringBuilder ex = new System.Text.StringBuilder();
					ex.Append ("Lock obtain timed out: ");
					ex.Append (lockpath);
					if (System.IO.File.Exists (lockpath)) {
						System.IO.FileStream fs = System.IO.File.Open (lockpath, System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read);
						System.IO.StreamReader sr = new System.IO.StreamReader (fs);
						string pid = sr.ReadToEnd ().Trim ();
						sr.Close ();
						fs.Close ();
						ex.Append (" -- pid ").Append (pid);

						if (pid == String.Empty)
							ex.Append ("(empty)");
						else if (System.IO.Directory.Exists ("/proc/" + pid))
							ex.Append (" -- process exists");
						else
							ex.Append (" -- process does not exist, stale lockfile?");
					} else {
						ex.Append (" -- lock file doesn't exist!?");
					}
					throw new System.IO.IOException(ex.ToString ());
				}
				System.Threading.Thread.Sleep((int) LOCK_POLL_INTERVAL);
				locked = Obtain();
			}
			return locked;
		}
		
		/// <summary>Releases exclusive access. </summary>
		public abstract void  Release();
		
		/// <summary>Returns true if the resource is currently locked.  Note that one must
		/// still call {@link #Obtain()} before using the resource. 
		/// </summary>
		public abstract bool IsLocked();
		
		
		/// <summary>Utility class for executing code with exclusive access. </summary>
		public abstract class With
		{
			private Lock lock_Renamed;
			private long lockWaitTimeout;
			
			
			/// <summary>Constructs an executor that will grab the named lock. </summary>
			public With(Lock lock_Renamed, long lockWaitTimeout)
			{
				this.lock_Renamed = lock_Renamed;
				this.lockWaitTimeout = lockWaitTimeout;
			}
			
			/// <summary>Code to execute with exclusive access. </summary>
			protected internal abstract System.Object DoBody();
			
			/// <summary>Calls {@link #doBody} while <i>lock</i> is obtained.  Blocks if lock
			/// cannot be obtained immediately.  Retries to obtain lock once per second
			/// until it is obtained, or until it has tried ten times. Lock is released when
			/// {@link #doBody} exits. 
			/// </summary>
			public virtual System.Object Run()
			{
				bool locked = false;
				try
				{
					locked = lock_Renamed.Obtain(lockWaitTimeout);
					return DoBody();
				}
				finally
				{
					if (locked)
						lock_Renamed.Release();
				}
			}
		}
	}
}
