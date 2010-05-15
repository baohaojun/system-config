//
//  IAccessControl.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public delegate void ACLEvent (uint unix_user_id);

	[Interface ("org.freedesktop.Hal.Device.AccessControl")]
	internal interface IAccessControl {
		event ACLEvent ACLAdded;
		event ACLEvent ACLRemoved;
	}
}