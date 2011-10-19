//
//  Context.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class Context {

		private Bus bus = null;

		public Context ()
		{
			this.bus = Bus.System;
		}

		public T GetObject<T> (string bus_name, ObjectPath opath)
		{
			return bus.GetObject<T> (bus_name, opath);
		}

		public T GetObject<T> (ObjectPath opath)
		{
			return GetObject<T> ("org.freedesktop.Hal", opath);
		}
	}
}
