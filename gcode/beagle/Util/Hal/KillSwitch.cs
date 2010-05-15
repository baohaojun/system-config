//
//  KillSwitch.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class KillSwitch : IKillSwitch {

		private IKillSwitch killswitch = null;

		public KillSwitch (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public KillSwitch (Context context, ObjectPath opath)
		{
			this.killswitch = context.GetObject<IKillSwitch> (opath);
		}

		public int GetPower ()
		{
			return killswitch.GetPower ();
		}
		
		public void SetPower (bool power)
		{
			killswitch.SetPower (power);
		}
	}
}
