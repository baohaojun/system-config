//
//  SystemPowerManagement.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class SystemPowerManagement : ISystemPowerManagement {

		private const string root_path = "/org/freedesktop/Hal/devices/computer";

		private ISystemPowerManagement power_management;

		public SystemPowerManagement (Context context)
			: this (context, new ObjectPath (root_path))
		{
		}

		public SystemPowerManagement (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public SystemPowerManagement (Context context, ObjectPath opath)
		{
			this.power_management = context.GetObject<ISystemPowerManagement> (opath);
		}

		public int Suspend (int num_secs_to_wakeup)
		{
			return power_management.Suspend (num_secs_to_wakeup);
		}

		public int Hibernate ()
		{
			return power_management.Hibernate ();
		}

		public int SuspendHybrid (int num_secs_to_wakeup)
		{
			return power_management.SuspendHybrid (num_secs_to_wakeup);
		}

		public int Shutdown ()
		{
			return power_management.Shutdown ();
		}

		public int Reboot ()
		{
			return power_management.Reboot ();
		}

		public int SetPowerSave (bool should_save_power)
		{
			return power_management.SetPowerSave (should_save_power);
		}
	}
}