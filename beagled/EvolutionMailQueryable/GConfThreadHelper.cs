//
// GConfThreadHelper.cs
//
// Copyright (C) 2004 Novell, Inc.
//
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
using GConf;
using GLib;

namespace Beagle.Daemon.EvolutionMailQueryable {

	public class GConfThreadHelper {
		private static object lock_obj = new object ();
		private static GConf.Client gconf_client = null;

		private string path;
		private object data;
		private bool finished;
		private Exception ex;

		private GConfThreadHelper (string path)
		{			
			this.path = path;
			this.finished = false;
		}

		private bool GConfReady ()
		{
			lock (lock_obj) {
				if (gconf_client == null)
					gconf_client = new GConf.Client ();

				try {
					this.data = gconf_client.Get (this.path);
				} catch (Exception ex) {
					this.ex = ex;
				}

				this.finished = true;
				Monitor.Pulse (lock_obj);
			}

			return false;
		}

		static private TimeSpan one_second = new TimeSpan (10000000);

		public static object Get (string path)
		{
			lock (lock_obj) {
				GConfThreadHelper helper = new GConfThreadHelper (path);

				GLib.Idle.Add (new GLib.IdleHandler (helper.GConfReady));

				while (! helper.finished
				       && ! Shutdown.ShutdownRequested)
					Monitor.Wait (lock_obj, one_second);

				if (helper.ex != null)
					throw helper.ex;

				return helper.data;
			}
		}
	}
}
