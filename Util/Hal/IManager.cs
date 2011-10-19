//
//  IManager.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	internal delegate void DeviceAddedEvent (ObjectPath opath);
	internal delegate void DeviceRemovedEvent (ObjectPath opath);
	internal delegate void NewCapabilityEvent (ObjectPath opath, string cap);
	internal delegate void GlobalInterfaceLockEvent (string name, string owner, int holders);

	[Interface ("org.freedesktop.Hal.Manager")]
	internal interface IManager {

		ObjectPath[] GetAllDevices ();
		bool DeviceExists (ObjectPath device);
		ObjectPath[] FindDeviceStringMatch (string key, string value);
		ObjectPath[] FindDeviceByCapability (string capability);
		ObjectPath NewDevice ();
		void Remove (ObjectPath tmp_udi);
		void CommitToGdl (ObjectPath tmp_udi, ObjectPath udi);
		void AcquireGlobalInterfaceLock (string interface_name, bool exclusive);
		void ReleaseGlobalInterfaceLock (string interface_name);

		event DeviceAddedEvent DeviceAdded;
		event DeviceRemovedEvent DeviceRemoved;
		event NewCapabilityEvent NewCapability;
		event GlobalInterfaceLockEvent GlobalInterfaceLockAcquired;
		event GlobalInterfaceLockEvent GlobalInterfaceLockReleased;
	}
}