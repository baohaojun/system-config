//
// ExceptionHandlingThread.cs
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

using System;
using System.Collections;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Threading;

namespace Beagle.Util {

	public class ExceptionHandlingThread {

		private static ArrayList live_threads = new ArrayList ();
		private Thread thread;
		private ThreadStart method;

		private ExceptionHandlingThread (ThreadStart method)
		{
			if (method == null)
				throw new ArgumentNullException ("method");

			this.method = method;
		}

		private void ThreadStarted ()
		{
			Process proc = Process.GetCurrentProcess ();

			this.thread.Name = String.Format ("EHT {0:00000} [{1:00000} {2}] {3}:{4}", wrap_gettid (), proc.Id, proc.ProcessName,
							  method.Target == null ? method.Method.DeclaringType.ToString () : method.Target.ToString (), method.Method.Name);

			//Log.Debug ("Starting thread {0}", this.thread.Name);

			try {
				this.method ();
			} catch (ThreadAbortException e) {
				Logger.Log.Debug ("Thread aborted: {0}\n{1}\n", this.thread.Name, e.StackTrace);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Exception caught in {2} while executing {0}:{1}",
						 this.method.Target, this.method.Method, this.thread.Name);
			}

			lock (live_threads)
				live_threads.Remove (this.thread);

			// Reset variables to help the GC
			this.method = null;
			string thread_name = this.thread.Name;
			this.thread = null;

			//Log.Debug ("Finished thread {0}", thread_name);
		}

		public static Thread Start (ThreadStart method)
		{
			ExceptionHandlingThread eht = new ExceptionHandlingThread (method);

			eht.thread = new Thread (new ThreadStart (eht.ThreadStarted));

			lock (live_threads)
				live_threads.Add (eht.thread);

			eht.thread.Start ();

			return eht.thread;
		}

		public static void SpewLiveThreads ()
		{
			bool have_live_thread = false;

			lock (live_threads) {
				foreach (Thread t in live_threads) {
					Logger.Log.Debug ("Live ExceptionHandlingThread: {0}", t.Name);
					have_live_thread = true;
				}
			}

			if (! have_live_thread)
				Logger.Log.Debug ("No live ExceptionHandlingThreads!");
		}

		public static void AbortThreads ()
		{
			ArrayList cancel_threads = null;

			// Copy the list to avoid recursively locking
			lock (live_threads)
				cancel_threads = (ArrayList) live_threads.Clone ();

			foreach (Thread t in cancel_threads) {
				Logger.Log.Debug ("Aborting thread: {0}", t.Name);
				t.Abort ();
			}
		}

		// Only safe to call from the main thread!
		public static void JoinAllThreads ()
		{
			ArrayList join_threads = null;

			// We have to copy the live_threads array to avoid
			// recursively locking.  We iterate the process to
			// ensure that we join all the threads, including ones
			// that are created after JoinAllThreads() is called.
			while (live_threads.Count > 0) {
				// Copy the list to avoid recursively locking
				lock (live_threads)
					join_threads = (ArrayList) live_threads.Clone ();

				int count = 0;
				foreach (Thread t in join_threads) {
					++count;

					if (! t.IsAlive) {
						Log.Debug ("Skipping over finished thread {0} of {1}: {2}", count, join_threads.Count, t.Name);
						// If a thread from live_threads is not alive, that is a sign of a trouble
						// Anyway, remove the thread from live_threads, since this thread is not anymore live
						// This does not generally happen and should not happen,
						// but rarely happens and in one case caused a continuous looping when somehow a thread remained in live_threads even though not alive
						lock (live_threads)
							live_threads.Remove (t);
					} else {
						Log.Debug ("Joining thread {0} of {1}: {2}", count, join_threads.Count, t.Name);
						t.Join ();
					}
				}
			}
		}

		[DllImport ("libbeagleglue")]
		static extern uint wrap_gettid ();
	}
}
