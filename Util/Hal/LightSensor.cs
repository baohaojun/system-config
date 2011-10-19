//
//  LightSensor.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class LightSensor : ILightSensor {

		private ILightSensor light_sensor = null;

		public LightSensor (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public LightSensor (Context context, ObjectPath opath)
		{
			this.light_sensor = context.GetObject<ILightSensor> (opath);
		}

		public int[] GetBrightness ()
		{
			return light_sensor.GetBrightness ();
		}
	}
}
