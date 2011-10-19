//
// Info.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Text;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

using Beagle;
using Beagle.Daemon;
using Beagle.Util;

// Assembly information
[assembly: AssemblyTitle ("beagle-info")]
[assembly: AssemblyDescription ("Statistics from the Beagle daemon")]

public class InfoTool {

	public static void PrintUsageAndExit () 
	{
		VersionFu.PrintHeader ();

		string usage =
			"Usage: beagle-info <OPTIONS>\n\n" +
			"Options:\n" +
			"  --daemon-version\t\tPrint the version of the running daemon.\n" +
			"  --status\t\t\tDisplay status of the running daemon.\n" +
			"  --index-info\t\t\tDisplay statistics of the Beagle indexes.\n" +
			"  --is-indexing\t\t\tDisplay whether the indexer is currently active.\n" +
			"  --all-info\t\t\tAll of the above information.\n" +
			"  --list-backends\t\tList the currently available backends.\n" +
			"  --list-filters\t\tList the currently available filters.\n" +
			"  --list-static-indexes\t\tList the available static indexes.\n" +
			"  --help\t\t\tPrint this usage message.\n" +
			"  --version\t\t\tPrint version information.\n";

		Console.WriteLine (usage);

		System.Environment.Exit (0);
	}

	static int Main (string[] args)
	{
		if (args.Length == 0 || Array.IndexOf (args, "--help") > -1)
			PrintUsageAndExit ();

		if (Array.IndexOf (args, "--version") > -1) {
			VersionFu.PrintVersion ();
			return 0;
		}

		if (Array.IndexOf (args, "--list-filters") > -1)
			PrintFilterInformation ();
		else if (Array.IndexOf (args, "--list-backends") > -1)
			PrintBackendInformation ();
		else if (Array.IndexOf (args, "--list-static-indexes") > -1)
			PrintStaticIndexInformation ();
		else
			return PrintDaemonInformation (args);

		return 0;
	}
	
	private static int PrintDaemonInformation (string[] args)
	{
		DaemonInformationRequest request = new DaemonInformationRequest ();
		DaemonInformationResponse response;

		bool get_version = false;
		bool get_sched_info = false;
		bool get_index_status = false;
		bool get_is_indexing = false;

		get_version = (Array.IndexOf (args, "--daemon-version") > -1);
		get_sched_info = (Array.IndexOf (args, "--status") > -1);
		get_index_status = (Array.IndexOf (args, "--index-info") > -1);
		get_is_indexing = (Array.IndexOf (args, "--is-indexing") > -1);

		if (Array.IndexOf (args, "--all-info") > -1)
			get_version = get_sched_info = get_index_status = get_is_indexing = true;

		try {
			response = (DaemonInformationResponse) request.Send ();
		} catch (Beagle.ResponseMessageException) {
			Console.WriteLine ("Could not connect to the daemon.");
			return 1;
		}

		if (get_version)
			Console.WriteLine ("Daemon version: {0}", response.Version);

		if (get_sched_info)
			Console.Write (response.HumanReadableStatus);

		if (get_index_status) {
			Console.WriteLine ("Index information:");
			Console.WriteLine (response.IndexInformation);
		}

		if (get_is_indexing)
			Console.WriteLine ("Daemon indexing: {0}", response.IsIndexing);

		return 0;
	}

	private static void PrintFilterInformation ()
	{
		ReflectionFu.ScanEnvironmentForAssemblies ("BEAGLE_FILTER_PATH", PathFinder.FilterDir, PrintFilterDetails);
	}

	static void PrintFilterDetails (Assembly assembly)
	{
		StringBuilder sb = new StringBuilder ();
		foreach (Type t in ReflectionFu.GetTypesFromAssemblyAttribute (assembly, typeof (FilterTypesAttribute))) {
			Filter filter = null;

			try {
				filter = (Filter) Activator.CreateInstance (t);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught exception while instantiating {0}", t);
			}

			if (filter == null)
				continue;

			string name;

			if (t.FullName.StartsWith (t.Namespace))
				name = t.FullName.Substring (t.Namespace.Length + 1);
			else
				name = t.FullName;

			sb.Length = 0;
			sb.Append (name + " - Version " + filter.Version + " (" + assembly.Location + ")\n");
			bool has_filter = false;

			foreach (FilterFlavor flavor in filter.SupportedFlavors) {
				sb.Append ("  - ");
				sb.Append (flavor);
				sb.Append ("\n");
				has_filter = true;
			}

			if (has_filter)
				Console.WriteLine (sb.ToString ());
		}
	}

	private static void PrintBackendInformation ()
	{
		ArrayList assemblies = ReflectionFu.ScanEnvironmentForAssemblies ("BEAGLE_BACKEND_PATH", PathFinder.BackendDir);

		// Add BeagleDaemonLib if it hasn't already been added.
		bool found_daemon_lib = false;
		foreach (Assembly assembly in assemblies) {
			if (assembly.GetName ().Name == "BeagleDaemonLib") {
				found_daemon_lib = true;
				break;
			}
		}

		if (!found_daemon_lib) {
			try {
				assemblies.Add (Assembly.LoadFrom (Path.Combine (PathFinder.PkgLibDir, "BeagleDaemonLib.dll")));
			} catch (FileNotFoundException) {
				Console.WriteLine ("WARNING: Could not find backend list.");
				Environment.Exit (1);
			}
		}

		foreach (Assembly assembly in assemblies) {
			foreach (Type type in ReflectionFu.GetTypesFromAssemblyAttribute (assembly, typeof (IQueryableTypesAttribute))) {
				foreach (Beagle.Daemon.QueryableFlavor flavor in ReflectionFu.ScanTypeForAttribute (type, typeof (Beagle.Daemon.QueryableFlavor))) {
					Console.WriteLine ("{0,-20} (" + assembly.Location + ")", flavor.Name);
				}
			}
		}
	}

	private static void PrintStaticIndexInformation ()
	{
		try {
			foreach (DirectoryInfo index_dir in new DirectoryInfo (PathFinder.SystemIndexesDir).GetDirectories ())
				Console.WriteLine ("[System index] " + index_dir.Name + " (" + index_dir.FullName + ")");
		} catch (DirectoryNotFoundException) { }

		Config config = Conf.Load (Conf.Names.DaemonConfig);
		if (config == null)
			return;

		List<string[]> values = config.GetListOptionValues (Conf.Names.StaticQueryables);
		if (values == null)
			return;

		foreach (string[] index_path in values)
			Console.WriteLine ("[User index]   " + index_path [0]);
	}
	
}
		
		
