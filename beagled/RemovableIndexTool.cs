//
// RemovableIndexTool.cs
// Tool to load and unload removable indexes
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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
using System.Reflection;
using System.Runtime.InteropServices;
using Beagle;
using Beagle.Util;

// Assembly information
[assembly: AssemblyTitle ("beagle-removable-index")]
[assembly: AssemblyDescription ("Command-line interface for managing Beagle indexes for removable media")]

public class RemovableIndexTool {
	private static void PrintUsage ()
	{
		VersionFu.PrintHeader ();

		string usage =
			"Usage: beagle-removable-index --indexdir INDEXDIR --mount DIR\n" +
			"       beagle-removable-index --indexdir INDEXDIR --unmount DIR\n" +
			"Options:\n" +
			"  --indexdir DIR\tSpecify the path to the index directory.\n" +
			"  --mount DIR \tTell beagled to load a removable index from the given directory.\n" +
			"  --umount DIR\tTell beagled to unload a removable index from the given directory.\n" +
			"               \tGive full path to the directory in the above options.\n\n" +
			"  --help\t\tPrint this usage message.\n" +
			"  --version\t\tPrint version information.\n";

		Console.WriteLine (usage);
	}

	[DllImport("libgobject-2.0.so.0")]
	static extern void g_type_init ();

	public static void Main (string[] args) 
	{
		// Initialize GObject type system
		g_type_init ();

		string index_dir = null;
		string target_dir = null;
		bool mount = false;

		int i = 0;
		while (i < args.Length) {
			
			string arg = args [i];
			++i;
			string next_arg = i < args.Length ? args [i] : null;

			switch (arg) {
			case "-h":
			case "--help":
				PrintUsage ();
				Environment.Exit (0);
				break;

			case "--indexdir":
				if (next_arg != null)
					index_dir = next_arg;
				++ i;
				break;

			case "--mount":
				if (next_arg != null) {
					target_dir = next_arg;
					mount = true;
				}
				++ i;
				break;

			case "--unmount":
				if (next_arg != null) {
					target_dir = next_arg;
					mount = false;
				}
				++ i;
				break;

			case "--version":
				VersionFu.PrintVersion ();
				Environment.Exit (0);
				break;

			default:
				PrintUsage ();
				Environment.Exit (1);
				break;

			}
		}

		if (target_dir == null || index_dir == null) {
			PrintUsage ();
			Environment.Exit (1);
		}

		if (mount)
			MountRemovableIndex (index_dir, target_dir);
		else
			UnmountRemovableIndex (index_dir, target_dir);
	}

	private static void MountRemovableIndex (string index_dir, string mnt_dir)
	{
		Console.WriteLine ("Loading removable index from '{0}' for '{1}'", index_dir, mnt_dir);

		RemovableIndexRequest req = new RemovableIndexRequest ();
		req.Mount = true;
		req.IndexDir = Path.IsPathRooted (index_dir) ? index_dir : Path.GetFullPath (index_dir);
		req.MountDir = Path.IsPathRooted (mnt_dir) ? mnt_dir : Path.GetFullPath (mnt_dir);

		ResponseMessage resp;
		
		try {
			resp = req.Send ();
		} catch (ResponseMessageException ex) {
			Log.Error ("Error in loading index: {0}", ex.ToString ());
			return;
		}

		RemovableIndexResponse res = (RemovableIndexResponse) resp;
		Console.WriteLine ("Successfully mounted '{0}'@{1} from {2}", res.Source, mnt_dir, index_dir);
	}

	private static void UnmountRemovableIndex (string index_dir, string mnt_dir)
	{
		Console.WriteLine ("Unloading removable index from '{0}' for '{1}'", index_dir, mnt_dir);

		RemovableIndexRequest req = new RemovableIndexRequest ();
		req.Mount = false;
		req.IndexDir = Path.IsPathRooted (index_dir) ? index_dir : Path.GetFullPath (index_dir);
		req.MountDir = Path.IsPathRooted (mnt_dir) ? mnt_dir : Path.GetFullPath (mnt_dir);

		ResponseMessage resp;
		try {
			resp = req.Send ();
		} catch (ResponseMessageException ex) {
			Log.Error ("Error in unloading index: {0}", ex.ToString ());
			return;
		}

		RemovableIndexResponse res = (RemovableIndexResponse) resp;
		Console.WriteLine ("Successfully unloaded '{0}'@{1} from {2}", res.Source, mnt_dir, index_dir);
	}
}
