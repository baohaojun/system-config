//
//  IDevice.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public delegate void PropertyModifiedEvent (int num, PropertyModification[] props);
	public delegate void ConditionEvent (string name, string details);
	public delegate void InterfaceLockEvent (string lock_name, string lock_owner, int num_holders);

	[Interface ("org.freedesktop.Hal.Device")]
	internal interface IDevice {

		object GetProperty (string key);
		string GetPropertyString (string key);
		string[] GetPropertyStringList (string key);
		int GetPropertyInteger (string key);
		ulong GetPropertyUInt64 (string key);
		bool GetPropertyBoolean	(string key);
		double GetPropertyDouble (string key);

		void SetProperty (string key, object value);
		void SetPropertyString (string key, string value);
		void SetPropertyStringList (string key, string[] value);
		void SetPropertyInteger (string key, int value);
		void SetPropertyUInt64 (string key, ulong value);
		void SetPropertyBoolean	(string key, bool value);
		void SetPropertyDouble (string key, double value);

		void RemoveProperty (string key);
		int GetPropertyType (string key);
		bool PropertyExists (string key);
		void AddCapability (string capability);
		bool QueryCapability (string capability);
		bool Lock (string reason);
		bool Unlock ();
		void AcquireInterfaceLock (string interface_name, bool exclusive);
		void ReleaseInterfaceLock (string interface_name);
		bool IsCallerLockedOut (string interface_name, string caller_unique_name);
		string IsCallerPrivileged (string privilege, string caller_unique_name);		
		bool IsLockedByOthers (string interface_name);
		void StringListAppend (string key, string value);
		void StringListPrepend (string key, string value);
		void StringListRemove (string key, string value);
		bool EmitCondition (string name, string details);
		bool Rescan ();
		bool Reprobe ();
		bool ClaimInterface (string name, string introspection_xml);
		bool AddonIsReady ();

		event PropertyModifiedEvent PropertyModified;
		event ConditionEvent Condition;
		event InterfaceLockEvent InterfaceLockAcquired;
		event InterfaceLockEvent InterfaceLockReleased;
	}
}