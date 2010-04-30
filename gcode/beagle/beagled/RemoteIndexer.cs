//
// RemoteIndexer.cs
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
using System.Diagnostics;
using System.IO;
using System.Threading;
using Mono.Unix;

using Beagle;
using Beagle.Util;
using Stopwatch = Beagle.Util.Stopwatch;

namespace Beagle.Daemon {

	public class RemoteIndexer : IIndexer {

		static string helper_path;
		static int helper_pid = -1;

		string remote_index_name;
		int remote_index_minor_version = 0;
		int last_item_count = -1;

		static RemoteIndexer ()
		{
			string bihp = Environment.GetEnvironmentVariable ("_BEAGLED_INDEX_HELPER_PATH");
			if (bihp == null)
				throw new Exception ("_BEAGLED_INDEX_HELPER_PATH not set!");
			
			helper_path = Path.GetFullPath (Path.Combine (bihp, "beagled-index-helper"));
			if (! File.Exists (helper_path))
				throw new Exception ("Could not find " + helper_path);
			Logger.Log.Debug ("Found index helper at {0}", helper_path);
		}

		static public IIndexer NewRemoteIndexer (string name, int minor_version)
		{
			return new RemoteIndexer (name, minor_version);
		}

		public RemoteIndexer (string name, int minor_version)
		{
			this.remote_index_name = name;
			this.remote_index_minor_version = minor_version;
		}

		public IndexerReceipt [] Flush (IndexerRequest request)
		{
			// If there isn't actually any work to do, just return
			// an empty array.
			if (request.IsEmpty)
				return new IndexerReceipt [0];

			// Iterate through the items in the IndexerRequest to
			// store the streams before passing them to the helper.
			foreach (Indexable indexable in request.Indexables) {
				if (indexable.Type == IndexableType.Add)
					indexable.StoreStream ();
			}

			RemoteIndexerRequest remote_request;
			remote_request = new RemoteIndexerRequest ();
			remote_request.RemoteIndexName = this.remote_index_name;
			remote_request.RemoteIndexMinorVersion = this.remote_index_minor_version;
			remote_request.Request = request;
			
			RemoteIndexerResponse response;
			response = SendRequest (remote_request);

			if (response == null) {
				Logger.Log.Error ("Something terrible happened --- Flush failed");
				request.Cleanup ();
				return null;
			}
			
			last_item_count = response.ItemCount;

			return response.Receipts;
		}

		public int GetItemCount ()
		{
			if (last_item_count == -1) {
				// Send an empty indexing request to cause the last item count to be
				// initialized.
				RemoteIndexerRequest request;
				request = new RemoteIndexerRequest ();

				RemoteIndexerResponse response;
				response = SendRequest (request);
				if (response != null)
					last_item_count = response.ItemCount;
				else
					Logger.Log.Error ("Something terrible happened --- GetItemCount failed");
			}

			return last_item_count;
		}

		/////////////////////////////////////////////////////////

		private RemoteIndexerResponse SendRequest (RemoteIndexerRequest request)
		{
			RemoteIndexerResponse response = null;
			int exception_count = 0;
			bool start_helper_by_hand = false;

			if (Environment.GetEnvironmentVariable ("BEAGLE_RUN_HELPER_BY_HAND") != null)
				start_helper_by_hand = true;

			request.RemoteIndexName = remote_index_name;
			request.RemoteIndexMinorVersion = remote_index_minor_version;
			
			while (response == null
			       && exception_count < 5
				&& ! Shutdown.ShutdownRequested) {

				bool need_helper = false;

				//Logger.Log.Debug ("Sending request!");
				try {
					response = request.Send () as RemoteIndexerResponse;
					//Logger.Log.Debug ("Done sending request");
				} catch (ResponseMessageException ex) {
					Logger.Log.Debug ("Caught ResponseMessageException: {0}", ex.Message);

					if (ex.InnerException is System.Net.Sockets.SocketException) {
						Logger.Log.Debug ("InnerException is SocketException -- we probably need to launch a helper");
						need_helper = true;
					} else if (ex.InnerException is IOException) {
						Logger.Log.Debug ("InnerException is IOException -- we probably need to launch a helper");
						need_helper = true;
					} else {
						Logger.Log.Debug (ex, "Unexpected exception from IndexHelper. Giving up sending this request.");
						return null;
					}
				}

				// If we caught an exception...
				if (response == null) {
					if (! start_helper_by_hand || ! need_helper)
						++exception_count;

					if (start_helper_by_hand) {
						// Sleep briefly before trying again.
						Thread.Sleep (1000);
					} else {
						// Try to activate the helper.
						LaunchHelper ();
					}
				}
			}

			if (response == null && exception_count >= 5)
				Logger.Log.Error ("Exception limit exceeded trying to activate a helper.  Giving up on indexing!");

			return response;
		}

		/////////////////////////////////////////////////////////

		static bool CheckHelper ()
		{
			string storage_dir = PathFinder.GetRemoteStorageDir (false);

			if (storage_dir == null)
				return false;

			// FIXME: We shouldn't need to know the path to the helper socket.
			string socket_name = Path.Combine (storage_dir, "socket-helper");

			if (! File.Exists (socket_name))
				return false;

			// Open, and then immediately close, a connection to the helper's socket.
			try {
				UnixClient test_client;
				test_client = new UnixClient (socket_name);
				test_client.Close ();
				return true;
			} catch (Exception ex) {
				return false;
			}
		}

		static object helper_lock = new object ();
		
		static void LaunchHelper ()
		{
			// If we are in the process of shutting down, return immediately.
			if (Shutdown.ShutdownRequested)
				return;

			lock (helper_lock) {

				// If a helper appears to be running, return immediately.
				if (CheckHelper ())
					return;
				
				Logger.Log.Debug ("Launching helper process");

				SafeProcess p = new SafeProcess ();
				string[] args = new string [3];
				args [0] = helper_path;

				if (BeagleDaemon.DisableTextCache)
					args [1] = "--disable-text-cache";
				else
					args [1] = String.Empty;

				if (Log.Level == LogLevel.Debug)
					args [2] = "--debug";
				else
					args [2] = String.Empty;

				p.Arguments = args;
				p.RedirectStandardOutput = false;
				p.RedirectStandardError = false;
				p.Start ();

				Logger.Log.Debug ("IndexHelper PID is {0}", p.Id);

				// Poll the helper's socket.  Wait up to a minute
				// (500 ms * 120 times) for the helper to be ready
				// to handle requests.
				Stopwatch watch = new Stopwatch ();
				watch.Start ();
				int poll_count = 0;
				bool found_helper;
				do {
					Thread.Sleep (500);
					++poll_count;
					found_helper = CheckHelper ();
				} while (poll_count < 120 
					 && ! found_helper
					 && ! Shutdown.ShutdownRequested);
				watch.Stop ();
				
				if (! found_helper)
					throw new Exception (String.Format ("Couldn't launch helper process {0}", p.Id));

				Logger.Log.Debug ("Found IndexHelper ({0}) in {1}", p.Id, watch);
				helper_pid = p.Id;
			}
		}

		static public void SignalRemoteIndexer (Mono.Unix.Native.Signum signal)
		{
			// No helper running right now
			if (helper_pid == -1 || ! CheckHelper ()) {
				helper_pid = -1;
				return;
			}

			Log.Debug ("Forwarding signal {0} ({1}) to index helper (pid {2})", (int) signal, signal, helper_pid);
			Mono.Unix.Native.Syscall.kill (helper_pid, signal);
		}
	}
}
