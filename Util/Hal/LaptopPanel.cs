//
//  LaptopPanel.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class LaptopPanel : ILaptopPanel {

		private ILaptopPanel laptop_panel = null;

		public LaptopPanel (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public LaptopPanel (Context context, ObjectPath opath)
		{
			this.laptop_panel = context.GetObject<ILaptopPanel> (opath);
		}

		public int GetBrightness ()
		{
			return laptop_panel.GetBrightness ();
		}

		public void SetBrightness (int brightness)
		{
			laptop_panel.SetBrightness (brightness);
		}
	}
}