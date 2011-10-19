//
//  IStorage.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	[Interface ("org.freedesktop.Hal.Device.Storage")]
	internal interface IStorage {
		int Eject (string[] options);
		int CloseTray (string[] options);
	}
}