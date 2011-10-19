//
// Shutdown.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Reflection;

using Beagle;
using Beagle.Util;

[assembly: AssemblyTitle ("beagle-shutdown")]
[assembly: AssemblyDescription ("Tool for shutting down the Beagle daemon")]

public class Shutdown {

	public static int Main (string[] args)
	{
		if (Array.IndexOf (args, "--help") > -1) {
			VersionFu.PrintHeader ();
			return 0;
		}

		if (Array.IndexOf (args, "--version") > -1) {
			VersionFu.PrintVersion ();
			return 0;
		}

		ShutdownRequest request = new ShutdownRequest ();

		try {
			request.Send ();
		} catch {
			Console.WriteLine ("ERROR: The Beagle daemon does not appear to be running");
			return 1;
		}

		return 0;
	}
}
