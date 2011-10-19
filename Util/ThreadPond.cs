//
// ThreadPond.cs
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
using System.Threading;

namespace Beagle.Util {

	public class ThreadPond {

		private object queue_lock = new object ();
		private Queue queue = new Queue ();
		private Thread [] threads;
		private bool running;

		public ThreadPond (int N)
		{
			queue_lock = new object ();

			queue = new Queue ();

			ThreadStart worker;
			worker = new ThreadStart (Worker);

			threads = new Thread [N];
			for (int i = 0; i < N; ++i)
				threads [i] = new Thread (worker);

			running = false;
		}

		public void Add (ThreadStart start)
		{
			lock (queue_lock) {
				queue.Enqueue (start);
				Monitor.Pulse (queue_lock);
			}
		}

		public void Start ()
		{
			running = true;
			Log.Debug ("Starting {0} threads in pond", threads.Length);
			for (int i = 0; i < threads.Length; ++i)
				threads [i].Start ();
		}

		public void Stop ()
		{
			running = false;

			// We need to pulse the lock once per thread, so that they
			// will all have an opportunity to wake up and terminate.
			for (int i = 0; i < threads.Length; ++i)
				lock (queue_lock)
					Monitor.Pulse (queue_lock);
		}

		private void Worker ()
		{
			while (running) {
				
				ThreadStart start = null;

				lock (queue_lock) {
					if (! running)
						return;
					if (queue.Count == 0) {
						Monitor.Wait (queue_lock);
						continue;
					}
					start = queue.Dequeue () as ThreadStart;
				}

				if (start == null)
					continue;

				try {
					start ();
				} catch (Exception ex) {
					Logger.Log.Warn ("Caught exception in ThreadPond worker");
					Logger.Log.Warn (ex);
				}
			}
		}

	}

}
