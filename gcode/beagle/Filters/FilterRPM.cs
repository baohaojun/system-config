//
// FilterRPM.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Text;
using System.Collections;
using System.Diagnostics;
using System.Xml;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {
	public class FilterRPM : FilterPackage {
		public FilterRPM ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-rpm"));
		}

		private const string property_queryformat = "%{NAME}\n<>\n" +
			"%{VERSION}\n<>\n" +
			"%{SUMMARY}\n<>\n" +
			"%{GROUP}\n<>\n" +
			"%{LICENSE}\n<>\n" +
			"%{PACKAGER}\n<>\n" +
			"%{URL}\n<>\n" +
			"%{SIZE}\n<>\n";

		private const string text_queryformat = "%{DESCRIPTION}\n" +
			"[%{OLDFILENAMES} ]\n" +
			"[%{BASENAMES} ]\n";

		protected override bool PullPackageProperties ()
		{
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "rpm", "-qp", "--queryformat", property_queryformat, FileInfo.FullName };
			pc.RedirectStandardOutput = true;
			pc.UseLangC = true;

			// Let rpm run for 15 seconds for properties, max.
			pc.CpuLimit = 15;
			
			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				return false;
			}

			StreamReader pout = new StreamReader (pc.StandardOutput);

			// Order is dependent on the queryformat string
			PackageName = ReadString (pout);
			PackageVersion = ReadString (pout);
			Summary = ReadString (pout);
			Category = ReadString (pout);
			License = ReadString (pout);
			Packager = ReadString (pout);
			Homepage = ReadString (pout);
			string size = ReadString (pout);
			Size = Convert.ToInt64 (size);

			pout.Close ();
			pc.Close ();

			return true;
		}

		StringBuilder sb = new StringBuilder ();
		private string ReadString (StreamReader reader)
		{
			sb.Length = 0;
			string s = reader.ReadLine ();
			//Log.Debug ("Read : [{0}]", s);

			while (s != null && s != "<>") {
				if (s != "(none)") {
					if (sb.Length != 0)
						sb.Append ("\n");
					sb.Append (s);
				}
				s = reader.ReadLine ();
				//Log.Debug ("Read : [{0}]", s);
			}

			if (sb.Length == 0)
				return null;

			return sb.ToString ();
		}

		protected override void DoPull ()
		{
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "rpm", "-qp", "--queryformat", text_queryformat, FileInfo.FullName };
			pc.RedirectStandardOutput = true;

			// Let rpm run for 90 seconds for text, max.
			pc.CpuLimit = 90;
			
			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				Error ();
				return;
			}

			StreamReader pout = new StreamReader (pc.StandardOutput);

			string s;
			while ((s = pout.ReadLine ()) != null) {
				if (s == "(none)")
					continue;
				AppendWord (s);
			}

			pout.Close ();
			pc.Close ();

			Finished ();
		}

	}
}
