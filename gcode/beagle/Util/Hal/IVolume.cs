//
//  IVolume.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	[Interface ("org.freedesktop.Hal.Device.Volume")]
	internal interface IVolume {
		int Mount (string mount_point, string fstype, string[] options);
		int Unmount (string[] options);
		int Eject (string[] options);
	}
}