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

	public class Storage : IStorage {

		private IStorage storage = null;

		public Storage (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public Storage (Context context, ObjectPath opath)
		{
			this.storage = context.GetObject<IStorage> (opath);
		}

		public int Eject (string[] options)
		{
			return storage.Eject (options);
		}

		public int CloseTray (string[] options)
		{
			return storage.CloseTray (options);
		}
	}
}
