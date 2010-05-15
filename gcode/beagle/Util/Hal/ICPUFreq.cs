//
//  ICPUFreq.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	[Interface ("org.freedesktop.Hal.Device.CPUFreq")]
	internal interface ICPUFreq {
		ObjectPath[] GetCPUFreqAvailableGovernors ();
		ObjectPath GetCPUFreqGovernor ();
		void SetCPUFreqGovernor (ObjectPath governor);
		void SetCPUFreqPerformance (int i);
		int GetCPUFreqPerformance ();
		void SetCPUFreqConsiderNice (bool consider_niced_processes);
		bool GetCPUFreqConsiderNice ();
	}
}