//
// Shutdown.cs
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
using System.Threading;
using System.Collections;
using System.Diagnostics;
using Beagle.Util;
using Mono.Unix;

namespace Beagle.Daemon {

	public class Shutdown {

		static public bool Debug = Beagle.Util.Debug.Enabled ("Shutdown");

		static object shutdownLock = new object ();
		static Hashtable workers = new Hashtable ();
		static Hashtable workers_names = new Hashtable ();
		static bool shutdownRequested = false;
		static bool shutdownStarted = false;
		static bool mainloop_finished = false;

		public delegate void ShutdownHandler ();
		public static event ShutdownHandler ShutdownEvent;

		public static bool WorkerStart (object o, string name)
		{
			lock (shutdownLock) {
				if (shutdownRequested) {
					return false;
				}
				int refcount = 0;
				if (workers.Contains (o))
					refcount = (int)workers[o];
				++refcount;
				workers[o] = refcount;
				workers_names[o] = name;

				if (Debug)
					Logger.Log.Debug ("worker added: name={0} refcount={1}", name, refcount);
			}
			return true;
		}

		public static bool WorkerStart (object o)
		{
			return WorkerStart (o, o.ToString ());
		}

		public static void WorkerFinished (object o)
		{
			lock (shutdownLock) {
				if (!workers.Contains (o)) {
					Logger.Log.Warn ("extra WorkerFinished called for {0}", o);
					return;
				}

				int refcount = (int)workers[o];
				--refcount;
				if (refcount == 0) {
					if (Debug)
						Logger.Log.Debug ("worker removed: name={0}", workers_names[o]);
					workers.Remove (o);
					workers_names.Remove (o);
				} else {
					if (Debug)
						Logger.Log.Debug ("worker finished: name={0} refcount={1}", workers_names[o], refcount);
					workers[o] = refcount;
				}

				Monitor.Pulse (shutdownLock);
			}
		}

		static public bool ShutdownRequested {
			get { return shutdownRequested; }
			set {
				lock (shutdownLock)
					shutdownRequested = value;
			}
		}

		private static GLib.MainLoop main_loop = null;

		public static void RegisterMainLoop (GLib.MainLoop loop)
		{
			main_loop = loop;
		}

		public static void BeginShutdown ()
		{
			lock (shutdownLock) {
				shutdownRequested = true;

				// There should be only *ONE* call to BeginShutdown
				// e.g. in IndexHelper, signalhandler and daemonmonitor thread
				// both can call BeginShutdown. Prevent the deadlock.
				if (shutdownStarted)
					return;
				shutdownStarted = true;
			}

			Log.Always ("Shutdown requested");

			if (ShutdownEvent != null) {
				try {
					ShutdownEvent ();
				} catch (Exception ex) {
					Logger.Log.Warn (ex, "Caught unhandled exception during shutdown event");
				}
			}

			int count = 0;
			
			lock (shutdownLock) { 
				while (workers.Count > 0) {
					++count;
					Logger.Log.Debug ("({0}) Waiting for {1} worker{2}...",
							  count,
							  workers.Count,
							  workers.Count > 1 ? "s" : "");					
					foreach (object o in workers.Keys) 
						Logger.Log.Debug ("waiting for {0}", workers_names[o]);
					Monitor.Wait (shutdownLock);
				}
			}

			Logger.Log.Info ("All workers have finished.  Exiting main loop.");
			main_loop.Quit ();

#if MONO_1_9
			// So (re)sending a SIGTERM or SIGINT after mainloop is over will kill the process.
			// Is it good or bad ? (some of the exception handling thread might still be running)
			// I think its good; it sort of emulates the 5sec forceful killing feature of < 0.3.4 
			lock (shutdownLock)
				mainloop_finished = true;

			// Stop the signal handler thread by sending a signal
			Mono.Unix.Native.Syscall.kill (Process.GetCurrentProcess ().Id, Mono.Unix.Native.Signum.SIGINT);
#endif
		}

#if MONO_1_9
		static bool MainloopFinished {
			get {
				lock (shutdownLock)
					return mainloop_finished;
			}
		}

		public delegate void SignalHandler (int signal);

		public static void SetupSignalHandlers (SignalHandler signal_handler)
		{
			UnixSignal[] signals = new UnixSignal [] {
				new UnixSignal (Mono.Unix.Native.Signum.SIGINT),
				new UnixSignal (Mono.Unix.Native.Signum.SIGTERM),
				new UnixSignal (Mono.Unix.Native.Signum.SIGUSR1),
				new UnixSignal (Mono.Unix.Native.Signum.SIGUSR2)
			};

			// Ignore SIGPIPE
			// FIXME: Shouldn't this be done in every thread ?
			Mono.Unix.Native.Stdlib.SetSignalAction (Mono.Unix.Native.Signum.SIGPIPE, Mono.Unix.Native.SignalAction.Ignore);

			// Work around a mono feature/bug
			// https://bugzilla.novell.com/show_bug.cgi?id=381928
			// When beagle crashes, mono will try to print a stack trace and then call abort()
			// The abort somehow calls back into beagle and causes a deadlock
			if (Environment.GetEnvironmentVariable ("BEAGLE_MONO_DEBUG_FLAG_IS_SET") == null)
				Mono.Unix.Native.Stdlib.SetSignalAction (Mono.Unix.Native.Signum.SIGABRT, Mono.Unix.Native.SignalAction.Default);

			Thread signal_thread = new Thread (delegate () {
				Log.Debug ("Starting signal handler thread");
				int signal_handler_timeout = -1;
				while (! MainloopFinished) {
					int index = UnixSignal.WaitAny (signals, signal_handler_timeout);

					if (index > 3)
						continue;
					Mono.Unix.Native.Signum signal = signals [index].Signum;

					// Set shutdown flag to true so that other threads can stop initializing
					if (signal == Mono.Unix.Native.Signum.SIGINT ||
					    signal == Mono.Unix.Native.Signum.SIGTERM) {
						ShutdownRequested = true;
						signal_handler_timeout = 200; // 200 ms
					}

					if (signal_handler == null)
						continue;

					// Do all signal handling work in the main loop and not in the signal handler.
					GLib.Idle.Add (new GLib.IdleHandler (delegate () { signal_handler ((int) signal); return false; }));
				}
				Log.Debug ("Exiting signal handler thread");
			});

			signal_thread.Start ();
		}
#endif
	}
}
