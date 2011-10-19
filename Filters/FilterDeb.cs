//
// FilterDeb.cs
//
// Copyright (C) 2006 Kevin Kubasik <kevin@kubasik.net>
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

using System;
using System.IO;
using System.Diagnostics;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {

	public class FilterDeb : Beagle.Filters.FilterPackage {

		public FilterDeb ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-deb"));
		}
		
		protected override bool PullPackageProperties ()
		{
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "dpkg-deb", "-I", FileInfo.FullName};
			pc.RedirectStandardOutput = true;
			pc.UseLangC = true;
			
			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				return false;
			}
			
			StreamReader pout = new StreamReader (pc.StandardOutput);
			
			string str = null;
			string[] tokens = null;
			char[] splits = { ',', '|'};
			string[] list = null;

			while ((str = pout.ReadLine ()) != null) {
				tokens = str.Split (':');
				if (tokens.Length <= 1) 
					continue;

				switch (tokens[0].Trim ()) {
					
				case "Package":
					PackageName = tokens [1];
					break;
					
				case "Maintainer":
					Packager = tokens [1];
					break;
					
				case "Version":
					PackageVersion = tokens [1];
					break;
					
				case "Section":
					Category = tokens [1];
					break;
					
				case "Architecture":
					AddProperty (Beagle.Property.NewUnsearched ("fixme:arch", tokens [1]));
					break;
					
				case "Depends":
					list = tokens [1].Split (splits);
					foreach (string s in list)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:depends", s));
					break;
					
				case "Recommends":
					list = tokens [1].Split (splits);
					foreach (string s in list)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:recommend", s));
					break;

				case "Conflicts":
					list = tokens [1].Split (splits);
					foreach (string s in list)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:conflict", s));
					break;
					
				case "Replaces":
					list = tokens [1].Split (splits);
					foreach (string s in list)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:replaces", s));
					break;
					
				case "Provides":
					list = tokens [1].Split (splits);
					foreach (string s in list)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:provides", s));
					break;
					
				case "Installed-Size":
					// Installed-Size is given in number of KBs
					AddProperty (Beagle.Property.NewUnsearched ("fixme:size", tokens [1] + "000"));
					break;
					
				case "Description":
					AppendText (tokens [1]);
					AppendStructuralBreak ();
					while ((str = pout.ReadLine ()) != null) {
						if(str.Trim () == ".")
							AppendStructuralBreak ();
						else
							AppendText (str);
					}
					break;
				}
			}

			pout.Close ();
			pc.Close ();
			return true;
		}
	}
}
