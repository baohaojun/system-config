//
//  Manager.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;
using org.freedesktop.DBus;

namespace Hal {

	public delegate void DeviceDelegate (Device device);
	public delegate void CapabilityDelegate (Device device, string capability);
	public delegate void GlobalInterfaceLockDelegate (string name, string owner, int holders);

	public class Manager {

		private Context context = null;
		private IManager manager = null;

		public event DeviceDelegate DeviceAdded;
		public event DeviceDelegate DeviceRemoved;
		public event CapabilityDelegate NewCapability;
		public event GlobalInterfaceLockDelegate GlobalInterfaceLockAcquired;
		public event GlobalInterfaceLockDelegate GlobalInterfaceLockReleased;
		
		public Manager (Context context)
		{
			this.context = context;
			this.manager = context.GetObject<IManager> (new ObjectPath ("/org/freedesktop/Hal/Manager"));

			manager.DeviceAdded += OnDeviceAdded;
			manager.DeviceRemoved += OnDeviceRemoved;
			manager.NewCapability += OnNewCapability;
			manager.GlobalInterfaceLockAcquired += OnGlobalInterfaceLockAcquired;
			manager.GlobalInterfaceLockReleased += OnGlobalInterfaceLockReleased;
		}
		
		public ICollection<Device> GetAllDevices ()
		{
			List<Device> devices = new List<Device> ();

			foreach (ObjectPath opath in manager.GetAllDevices ()) {
				Device device = new Device (context, opath);
				devices.Add (device);
			}

			return devices;
		}

		public bool DeviceExists (Device device)
		{
			return manager.DeviceExists (device.ObjectPath);
		}

		public ICollection<Device> FindDeviceStringMatch (string key, string value)
		{
			List<Device> devices = new List<Device> ();

			foreach (ObjectPath opath in manager.FindDeviceStringMatch (key, value)) {
				Device device = new Device (context, opath);
				devices.Add (device);
			}

			return devices;
		}

		public ICollection<Device> FindDeviceByCapability (string capability)
		{
			List<Device> devices = new List<Device> ();

			foreach (ObjectPath opath in manager.FindDeviceByCapability (capability)) {
				Device device = new Device (context, opath);
				devices.Add (device);
			}

			return devices;
		}

		public Device NewDevice ()
		{
			ObjectPath opath = manager.NewDevice ();
			Device device = new Device (context, opath);

			return device;
		}

		public void Remove (Device udi)
		{
			manager.Remove (udi.ObjectPath);
		}
		
		public void CommitToGdl (Device tmp_udi, Device udi)
		{
			manager.CommitToGdl (tmp_udi.ObjectPath, udi.ObjectPath);
		}
		
		public void AcquireGlobalInterfaceLock (string interface_name, bool exclusive)
		{
			manager.AcquireGlobalInterfaceLock (interface_name, exclusive);
		}
		
		public void ReleaseGlobalInterfaceLock (string interface_name)
		{
			manager.ReleaseGlobalInterfaceLock (interface_name);
		}

		private void OnDeviceAdded (ObjectPath opath)
		{
			if (DeviceAdded != null)
				DeviceAdded (new Device (context, opath));
		}

		private void OnDeviceRemoved (ObjectPath opath)
		{
			if (DeviceRemoved != null)
				DeviceRemoved (new Device (context, opath));
		}

		private void OnNewCapability (ObjectPath opath, string capability)
		{
			if (NewCapability != null)
				NewCapability (new Device (context, opath), capability);
		}

		private void OnGlobalInterfaceLockAcquired (string name, string owner, int holders)
		{
			if (GlobalInterfaceLockAcquired != null)
				GlobalInterfaceLockAcquired (name, owner, holders);
		}

		private void OnGlobalInterfaceLockReleased (string name, string owner, int holders)
		{
			if (GlobalInterfaceLockReleased != null)
				GlobalInterfaceLockReleased (name, owner, holders);
		}
	}
}