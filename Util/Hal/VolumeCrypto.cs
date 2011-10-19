//
//  VolumeCrypto.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class VolumeCrypto : IVolumeCrypto {

		private IVolumeCrypto crypto = null;

		public VolumeCrypto (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public VolumeCrypto (Context context, ObjectPath opath)
		{
			this.crypto = context.GetObject<IVolumeCrypto> (opath);
		}

		public int Setup (string passphrase)
		{
			return crypto.Setup (passphrase);
		}
		
		public int Teardown ()
		{
			return crypto.Teardown ();
		}
	}
}
