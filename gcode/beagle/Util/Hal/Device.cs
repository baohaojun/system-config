//
//  Device.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class Device {

		private Context context = null;
		private ObjectPath object_path;
		private IDevice device = null;

		public event PropertyModifiedEvent PropertyModified; 
		public event ConditionEvent Condition;
		public event InterfaceLockEvent InterfaceLockAcquired;
		public event InterfaceLockEvent InterfaceLockReleased;

		public Device (Context context, ObjectPath object_path)
		{
			this.context = context;
			this.object_path = object_path;
			this.device = context.GetObject<IDevice> (object_path);

			device.PropertyModified += OnPropertyModified;
			device.Condition += OnCondition;
			device.InterfaceLockAcquired += OnInterfaceLockAcquired;
			device.InterfaceLockReleased += OnInterfaceLockReleased;
		}

		public object GetProperty (string key)
		{
			return device.GetProperty (key);
		}

		public string GetPropertyString (string key)
		{
			return device.GetPropertyString (key);
		}

		public string[] GetPropertyStringList (string key)
		{
			return device.GetPropertyStringList (key);
		}

		public int GetPropertyInteger (string key)
		{
			return device.GetPropertyInteger (key);
		}

		public ulong GetPropertyUInt64 (string key)
		{
			return device.GetPropertyUInt64 (key);
		}

		public bool GetPropertyBoolean (string key)
		{
			return device.GetPropertyBoolean (key);
		}

		public double GetPropertyDouble (string key)
		{
			return device.GetPropertyDouble (key);
		}
		
		public void SetProperty (string key, object value)
		{
			device.SetProperty (key, value);
		}

		public void  SetPropertyString (string key, string value)
		{
			device.SetPropertyString (key, value);
		}

		public void SetPropertyStringList (string key, string[] value)
		{
			device.SetPropertyStringList (key, value);
		}

		public void SetPropertyInteger (string key, int value)
		{
			device.SetPropertyInteger (key, value);
		}

		public void SetPropertyUInt64 (string key, ulong value)
		{
			device.SetPropertyUInt64 (key, value);
		}

		public void SetPropertyBoolean	(string key, bool value)
		{
			device.SetPropertyBoolean (key, value);
		}

		public void SetPropertyDouble (string key, double value)
		{
			device.SetPropertyDouble (key, value);
		}

		public void RemoveProperty (string key)
		{
			device.RemoveProperty (key);
		}

		public int GetPropertyType (string key)
		{
			return device.GetPropertyType (key);
		}

		public bool PropertyExists (string key)
		{
			return device.PropertyExists (key);
		}

		public void AddCapability (string capability)
		{
			device.AddCapability (capability);
		}

		public bool QueryCapability (string capability)
		{
			return device.QueryCapability (capability);
		}

		public bool Lock (string reason)
		{
			return device.Lock (reason);
		}

		public bool Unlock ()
		{
			return device.Unlock ();
		}

		public void AcquireInterfaceLock (string interface_name, bool exclusive)
		{
			device.AcquireInterfaceLock (interface_name, exclusive);
		}

		public void ReleaseInterfaceLock (string interface_name)
		{
			device.ReleaseInterfaceLock (interface_name);
		}

		public bool IsCallerLockedOut (string interface_name, string caller_unique_name)
		{
			return device.IsCallerLockedOut (interface_name, caller_unique_name);
		}

		public string IsCallerPrivileged (string privilege, string caller_unique_name)
		{
			return device.IsCallerPrivileged (privilege, caller_unique_name);
		}

		public bool IsLockedByOthers (string interface_name)
		{
			return device.IsLockedByOthers (interface_name);
		}

		public void StringListAppend (string key, string value)
		{
			device.StringListAppend (key, value);
		}

		public void StringListPrepend (string key, string value)
		{
			device.StringListPrepend (key, value);
		}

		public void StringListRemove (string key, string value)
		{
			device.StringListRemove (key, value);
		}

		public bool EmitCondition (string name, string details)
		{
			return device.EmitCondition (name, details);
		}

		public bool Rescan ()
		{
			return device.Rescan ();
		}

		public bool Reprobe ()
		{
			return device.Reprobe ();
		}

		public bool ClaimInterface (string name, string introspection_xml)
		{
			return device.ClaimInterface (name, introspection_xml);
		}

		public bool AddonIsReady ()
		{
			return device.AddonIsReady ();
		}

		private void OnPropertyModified (int num, PropertyModification[] mods)
		{
			if (PropertyModified != null)
				PropertyModified (num, mods);
		}

		private void OnCondition (string name, string details)
		{
			if (Condition != null)
				Condition (name, details);
		}

		private void OnInterfaceLockAcquired (string lock_name, string lock_owner, int num_holders)
		{
			if (InterfaceLockAcquired != null)
				InterfaceLockAcquired (lock_name, lock_owner, num_holders);
		}

		private void OnInterfaceLockReleased (string lock_name, string lock_owner, int num_holders)
		{
			if (InterfaceLockReleased != null)
				InterfaceLockReleased (lock_name, lock_owner, num_holders);
		}

		public ObjectPath ObjectPath {
			get { return object_path; }
		}

		public Device Parent {
			get {
				if (String.IsNullOrEmpty (this ["info.parent"]))
					return null;

				ObjectPath opath = new ObjectPath (this ["info.parent"]);
				return new Device (context, opath);
			}
		}

		public string this [string key] {
			get { return GetPropertyString (key); }
			set { SetPropertyString (key, value); }
		}
	}
}