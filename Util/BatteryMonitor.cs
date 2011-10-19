//
//  BatteryMonitor.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>
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
using System.Collections;
using System.Collections.Generic;

using Hal;

namespace Beagle.Util {

	public static class BatteryMonitor {

		private static Device adapter = null;
		private static bool prev_on_battery = false;

		public static void Init ()
		{
			try {
				// Init System DBus
				NDesk.DBus.BusG.Init (NDesk.DBus.Bus.System);
			} catch (Exception e) {
				// Lack of specific exception
				Log.Error (e, "Failed to access dbus session bus. Battery monitoring will be disabled.");
				return;
			}

			try {
				Manager manager = new Manager (new Context ());

				foreach (Device device in manager.FindDeviceByCapability ("ac_adapter")) {
					Log.Debug ("Found HAL device AC adapter for battery monitoring.");
					
					device.PropertyModified += OnPropertyModified;
					adapter = device;
					
					prev_on_battery = ! device.GetPropertyBoolean ("ac_adapter.present");
					
					break;
				}
			} catch (Exception e) {
				Log.Error (e, "Failed to acquire a HAL device for battery monitoring");
			}
		}

		private static void OnPropertyModified (int num_changes, PropertyModification[] props)
		{
			foreach (PropertyModification p in props) {
				if (p.Key != "ac_adapter.present")
					continue;

				CheckStatus ();
				return;
			}
		}

		private static void CheckStatus ()
		{
			bool on_ac = adapter.GetPropertyBoolean ("ac_adapter.present");
			bool index_on_battery = Conf.Daemon.GetOption (Conf.Names.IndexOnBattery, false);

			if (prev_on_battery && (on_ac || index_on_battery)) {
				if (on_ac) {
					Log.Info ("Detected a switch from battery to AC power. Restarting scheduler.");
				}

				Scheduler.Global.Start ();
				prev_on_battery = false;
			} else if (! prev_on_battery && ! on_ac && ! index_on_battery) {
				Log.Info ("Detected a switch from AC power to battery.  Stopping scheduler.");
				Scheduler.Global.Stop ();
				prev_on_battery = true;
			}
		}

		public static bool UsingAC {
			get {
				if (adapter == null)
					return true;

				return adapter.GetPropertyBoolean ("ac_adapter.present");
			}
		}
	}
}
