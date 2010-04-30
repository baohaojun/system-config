//
//  Volume.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class Volume : IVolume {

		private IVolume volume = null;

		public Volume (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public Volume (Context context, ObjectPath opath)
		{
			this.volume = context.GetObject<IVolume> (opath);
		}

		public int Mount (string mount_point, string fstype, string[] options)
		{
			return volume.Mount (mount_point, fstype, options);
		}

		public int Unmount (string[] options)
		{
			return volume.Unmount (options);
		}

		public int Eject (string[] options)
		{
			return volume.Eject (options);
		}
	}
}
