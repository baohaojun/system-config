//
// Galago.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using Galago;

namespace Beagle.Util {

	public static class GalagoTools {

		public enum Status {
			Available = 0,
			Offline = 1,
			Away = 2,
			Idle = 3,
			NoStatus = -1,
		};

		private static bool init;

		static GalagoTools ()
		{
			// Init only once. I see no reason to init and uninit during every function invocation.
			try {
				init = Galago.Global.Init ("beagle-galago-presence");
			} catch (DllNotFoundException) {
				// FIXME: Catch Debian Galago packaging bug.
				// Catching runtime exceptions is not right. Do here but monitor the distro bug.
				init = false;
				Console.Error.WriteLine ("Galago init failed. No buddy information will be provided.");
			}
		}

		public static Status GetPresence (string service_id, string username)
		{
			if (! init)
				return Status.NoStatus;

			Galago.Service service = Galago.Global.GetService (service_id, Galago.Origin.Remote, true);
			if (service == null)
				return Status.NoStatus;

			Galago.Account account = service.GetAccount (username, true);

			if (account == null)
				return Status.NoStatus;

			Galago.Presence presence = account.Presence;

			if (presence == null)
				return Status.NoStatus;

			Status user_status = Status.NoStatus;
			StatusType active_status;
			if (presence.IsIdle) {
				user_status = Status.Idle; 
			// FIXME: We should try to find a way to display the actual away message (if relivent)
			}
			else {
				active_status = presence.ActiveStatus.Primitive;
				switch (active_status) {
					case StatusType.Away : 
						user_status = Status.Away;
						break;
					case StatusType.Offline :
						user_status = Status.Offline;
						break;
					case StatusType.Available:
						user_status = Status.Available;
						break;
				}
			}

			return user_status;
		}

		public static string GetIdleTime (string service_id, string username)
		{
			if (! init)
				return null;

			Galago.Service service = Galago.Global.GetService (service_id, Galago.Origin.Remote, true);
			if (service == null)
				return null;

			Galago.Account account = service.GetAccount (username, true);

			if (account == null)
				return null;
	
			Galago.Presence presence = account.Presence;

			if (presence == null)
				return null;
			
			string str =  StringFu.DurationToPrettyString  ( DateTime.Now, presence.IdleStartTime);
			
			return str;
		}
	}
}
