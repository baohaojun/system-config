//
// FilterDesktop.cs
//
// Copyright (C) 2005 Novell, Inc.
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
using System.Collections;
using System.IO;
using System.Text;
using System.Diagnostics;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {

	public class FilterDesktop : Filter {

		protected StreamReader reader;

		public FilterDesktop ()
		{
			// 1: Added Categories field
			// 2: Added Type field
			SetVersion (2);
			SetFileType ("application");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-desktop"));
		}

		override protected void DoOpen (FileInfo file) 
		{
			try {
				reader = new StreamReader (file.FullName);
			} catch (Exception e) {
				Logger.Log.Error (e, "Could not open '{0}':", file.FullName);
				Error ();
				return;
			}
		}
		
		override protected void DoPullProperties ()
		{
			if (reader == null) {
				Error ();
				return;
			}
			
			string line;

			// FIXME: Use GKeyFile to parse .desktop files (which are like .ini files)
			// Find the [Desktop Entry] group
			while ((line = reader.ReadLine ()) != null)
				if (line == "[Desktop Entry]") break;

			if (line == null) {
				Logger.Log.Error ("Could not filter: No group [Desktop Entry]");
				Error ();
				return;
			}

			// desktop files must have a name
			bool have_name = false;
			
			while ((line = reader.ReadLine ()) != null)  {
				string [] sline = line.Split ('=');
				if (sline.Length != 2)
					continue;
				
				if (sline [0].Equals ("Icon") || sline [0].Equals ("Exec")) {
					AddProperty (Property.NewUnsearched ("fixme:" + sline [0], sline [1]));
				} else if (sline [0].StartsWith ("Name")) {
					if (sline [0] == "Name")
						have_name = true;
					AddProperty (Property.New ("fixme:" + sline [0], sline [1]));
				} else if (sline [0].StartsWith ("Comment")) {
					AddProperty (Property.New ("fixme:" + sline [0], sline [1]));
				} else if (sline [0].StartsWith ("Categories")) {
					string [] categories =  sline [1].Split (';');
					foreach (string c in categories)
						AddProperty (Property.New ("fixme:" + sline [0], c));
				} else if (sline [0].StartsWith ("Type")) {
					AddProperty (Property.NewKeyword ("fixme:" + sline [0], sline [1]));
				} else if (sline [0].StartsWith ("Keywords") || sline [0].StartsWith ("X-KDE-Keywords")) {
					AddProperty (Property.New ("fixme:" + sline [0], sline [1]));
				}
			}
			
			reader.Close ();

			if (have_name) {
				Finished ();
			} else {
				Logger.Log.Error ("Could not filter: No name in desktop entry");
				Error ();
			}
		}
	}
}
