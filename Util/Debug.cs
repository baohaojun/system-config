//
// Debug.cs
// Control which classes want to print debug information based on
// user supplied environment variable
//
// Copyright (C) 2008 Debajyoti Bera <dbera.web@gmail.com>
//

//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//

using System;
using System.Collections.Generic;

namespace Beagle.Util {
	public static class Debug {
		static Dictionary<string, bool> debug_table = new Dictionary<string, bool> ();
	
		static Debug ()
		{
			SetupDebugTable ();
		}
	
		static void SetupDebugTable ()
		{
			// User can pass env var "BEAGLE_DEBUG=classname1,classname2,classname3"
			string debug_str = Environment.GetEnvironmentVariable ("BEAGLE_DEBUG");
			if (String.IsNullOrEmpty (debug_str))
				return;
	
			// split the value of the variable by ','
			// no need to be smart and check for ",," type of empty var
			string[] vars = debug_str.Split (',');
			foreach (string var in vars)
				debug_table.Add (var, true);
		}
	
		public static bool Enabled (string name)
		{
			return debug_table.ContainsKey (name);
		}
	}
}
