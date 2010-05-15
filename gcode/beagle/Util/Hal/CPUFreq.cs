//
//  CPUFreq.cs
//
//  Copyright (c) 2007 Lukas Lipka, <lukaslipka@gmail.com>.
//

using System;
using System.Collections;
using System.Collections.Generic;

using NDesk.DBus;

namespace Hal {

	public class CPUFreq {
		
		private const string root_path = "/org/freedesktop/Hal/devices/computer";

		private ICPUFreq cpu_freq = null;

		public CPUFreq (Context context)
			: this (context, new ObjectPath (root_path))
		{
		}

		public CPUFreq (Context context, Device device)
			: this (context, device.ObjectPath)
		{
		}

		public CPUFreq (Context context, ObjectPath opath)
		{
			this.cpu_freq = context.GetObject<ICPUFreq> (opath);
		}

		public ObjectPath[] GetCPUFreqAvailableGovernors ()
		{
			return cpu_freq.GetCPUFreqAvailableGovernors ();
		}

		public ObjectPath GetCPUFreqGovernor ()
		{
			return cpu_freq.GetCPUFreqGovernor ();
		}

		public void SetCPUFreqGovernor (ObjectPath governor)
		{
			cpu_freq.SetCPUFreqGovernor (governor);
		}

		public void SetCPUFreqPerformance (int i)
		{
			cpu_freq.SetCPUFreqPerformance (i);
		}

		public int GetCPUFreqPerformance ()
		{
			return cpu_freq.GetCPUFreqPerformance ();
		}

		public void SetCPUFreqConsiderNice (bool consider_niced_processes)
		{
			cpu_freq.SetCPUFreqConsiderNice (consider_niced_processes);
		}

		public bool GetCPUFreqConsiderNice ()
		{
			return cpu_freq.GetCPUFreqConsiderNice ();
		}
	}
}