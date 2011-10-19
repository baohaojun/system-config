//
//  AccessControl.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class AccessControl : IAccessControl {

		private IAccessControl access_control = null;

		public event ACLEvent ACLAdded;
		public event ACLEvent ACLRemoved;

		public AccessControl (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public AccessControl (Context context, ObjectPath opath)
		{
			this.access_control = context.GetObject<IAccessControl> (opath);
			
			access_control.ACLAdded += OnACLAdded;
			access_control.ACLRemoved += OnACLRemoved;
		}

		private void OnACLAdded (uint uid)
		{
			if (ACLAdded != null)
				ACLAdded (uid);
		}

		private void OnACLRemoved (uint uid)
		{
			if (ACLRemoved != null)
				ACLRemoved (uid);
		}
	}
}
