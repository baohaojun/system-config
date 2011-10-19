//
//  ISystemPowerManagement.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	[Interface ("org.freedesktop.Hal.Device.SystemPowerManagement")]
	internal interface ISystemPowerManagement {
		int Suspend (int num_secs_to_wakeup);
		int Hibernate ();
		int SuspendHybrid (int num_secs_to_wakeup);
		int Shutdown ();
		int Reboot ();
		int SetPowerSave (bool should_save_power);
	}
}