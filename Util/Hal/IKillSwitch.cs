//
//  IKillSwitch.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	[Interface ("org.freedesktop.Hal.Device.KillSwitch")]
	internal interface IKillSwitch {
		int GetPower ();
		void SetPower (bool power);
	}
}