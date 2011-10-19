//
// Config.cs
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
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

using Beagle;
using Beagle.Daemon;
using Beagle.Util;

// Assembly information
[assembly: AssemblyTitle ("beagle-config")]
[assembly: AssemblyDescription ("Command-line interface to the Beagle config file")]

public static class ConfigTool {

	private static void PrintUsageAndExit ()
	{
		VersionFu.PrintHeader ();

		string usage =
			"Usage: \n" + 
			"     beagle-config [OPTIONS]\n" +
			" * to list sections:\n" +
			"     beagle-config --list-sections\n" +
			" * to list options in a section:\n" +
			"     beagle-config SECTION\n" +
			" * to view current values of an option:\n" +
			"     beagle-config SECTION SECTIONOPTION\n" +
			" * to change values of a boolean or a string options:\n" +
			"     beagle-config SECTION SECTIONOPTION VALUE\n" +
			" * to add values to a list option:\n" +
			"     beagle-config SECTION SECTIONOPTION PARAMS\n" +
			" * to remove a value from a list option:\n" +
			"     beagle-config SECTION SECTIONOPTION - PARAMS\n\n" +
			"Options:\n" +
			"  --list-sections\t\tList the available sections.\n" +
			"  --beagled-reload-config\tAsk the beagle daemon to reload\n" +
			"                         \tthe configuration file.\n" +
			"  --list-backends\t\tList the available backends.\n" +
			"\n" +
			"  Xml based operations:\n" +
			"  The following two methods can be used by non-C# programs to\n" +
			"  read and set config options.\n" +
			"  --write-xml SECTION\t\tPrint the section in xml format.\n" +
			"                     \t\tSuitable for parsing by other programs.\n" +
			"  --read-xml SECTION\t\tReads the xml of a section from stdin.\n" +
			"                    \t\tSuitable for setting options by other programs.\n" +
			"  In case of any error, the following error codes are returned:\n" +
			"     1 : Bad section name\n" +
			"     2 : Invalid xml\n" +
			"     3 : Xml does not correspond to the section name\n" +
			"\n" +
			"  --help\t\t\tPrint this usage message.\n" +
			"  --version\t\t\tPrint version information.\n\n";

		Console.WriteLine (usage);

		System.Environment.Exit (0);
	}

	private static void ListBackends ()
	{
		ArrayList backends = new ArrayList ();

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
				foreach (Beagle.Daemon.QueryableFlavor flavor in ReflectionFu.ScanTypeForAttribute (type, typeof (Beagle.Daemon.QueryableFlavor)))
					backends.Add (flavor.Name);
			}
		}
		
		if ( Directory.Exists (PathFinder.SystemIndexesDir)) {
			foreach (DirectoryInfo index_dir in new DirectoryInfo (PathFinder.SystemIndexesDir).GetDirectories ())
				backends.Add (index_dir.Name);
		}

		bool found_any = false;

		Console.WriteLine ("Allowed backends:");

		Config config = Conf.Load ("daemon");
		Option opt;
		ArrayList denied_backends = new ArrayList ();

		if (config != null && (opt = config ["DeniedBackends"]) != null) {
			List<string[]> denied_backends_list = config.GetListOptionValues (opt.Name);
			if (denied_backends_list != null) {
				foreach (string[] val in denied_backends_list)
					denied_backends.Add (val [0]);
			}
		}

		foreach (string name in backends) {
			if (denied_backends.Contains (name))
				continue;
			Console.WriteLine (" - {0}", name);
			found_any = true;
		}

		if (! found_any)
			Console.WriteLine (" (none)");
		
		Console.WriteLine ();

		found_any = false;

		Console.WriteLine ("Denied backends:");
		foreach (string name in denied_backends) {
			Console.WriteLine (" - {0}", name);
			found_any = true;
		}

		if (! found_any)
			Console.WriteLine (" (none)");
	}

	public static void Main (string [] args)
	{
		if (args.Length == 0)
			PrintUsageAndExit ();

		for (int i = 0; i < args.Length; i ++) {
			switch (args [i]) {
			case "--list-sections":
				ListSections ();
				return;

			case "--list-backends":
				ListBackends ();
				return;

			case "--reload":
			case "--beagled-reload-config":
				ReloadConfig ();
				return;

			case "--write-xml":
				if (args.Length == i + 1)
					PrintUsageAndExit ();
				PrintSectionXml (args [i + 1]);
				return;

			case "--read-xml":
				if (args.Length == i + 1)
					PrintUsageAndExit ();
				ReadSectionXml (args [i + 1]);
				return;

			case "--help":
			case "--usage":
				PrintUsageAndExit ();
				return;

			case "--version":
				VersionFu.PrintVersion ();
				Environment.Exit (0);
				break;

			default:
				break;
			}
		}

		if (args [0].StartsWith ("--"))
			PrintUsageAndExit ();

		Config config = Conf.Load (args [0]);

		if (config == null) {
			Console.WriteLine ("No section found: " + args [0]);
			return;
		}

		if (args.Length >= 2) {
			try {
				if (HandleArgs (config, args))
					Conf.Save (config);
			} catch (ArgumentException e) {
				Console.WriteLine ("** Error: " + e.Message);
			}

			return;
		}
			
		foreach (Option option in config.Options.Values)
			ShowOption (config, option);
	}

	private static void ListSections ()
	{
		string global_dir = Path.Combine (Path.Combine (ExternalStringsHack.SysConfDir, "beagle"), "config-files");
		string local_dir = Path.Combine (PathFinder.StorageDir, "config");

		string[] global_configs;
		try {
			global_configs = Directory.GetFiles (global_dir, "*.xml");
		} catch (DirectoryNotFoundException) {
			global_configs = new string[0];
		}

		string[] local_configs;
		try {
			local_configs = Directory.GetFiles (local_dir, "*.xml");
		} catch (DirectoryNotFoundException) {
			local_configs = new string [0];
		}

		for (int i = 0; i < global_configs.Length; ++ i)
			global_configs [i] = Path.GetFileNameWithoutExtension (global_configs [i]);

		for (int i = 0; i < local_configs.Length; ++ i)
			local_configs [i] = Path.GetFileNameWithoutExtension (local_configs [i]);

		Console.WriteLine ("Available sections:");
		foreach (string file in global_configs)
			Console.WriteLine (" - {0}", file);

		foreach (string file in local_configs)
			if (Array.IndexOf (global_configs, file) == -1)
				Console.WriteLine (" - {0}", file);

	}

	private static void ReloadConfig ()
	{
		try {
			ReloadConfigRequest request = new ReloadConfigRequest ();
			request.Send ();
			Console.WriteLine ("ReloadConfig request was sent successfully.");
			System.Environment.Exit (0);
		} catch (Exception e) {
			Console.Error.WriteLine ("ERROR: Could not send ReloadConfig request: {0}", e.Message);
			System.Environment.Exit (-1);
		}
	}

	const int ERROR_CONFIG_NO_SECTION = 1; // Bad section name
	const int ERROR_CONFIG_XML = 2; // Invalid xml
	const int ERROR_CONFIG_BAD = 3; // Xml does not correspond to the right config
	const int ERROR_CONFIG_LOAD = 4; // Error in loading config
	const int ERROR_CONFIG_SAVE = 5; // Error in saving config

	private static void PrintSectionXml (string section)
	{
		Log.Level = LogLevel.Always; // shhhh... silence

		Config config = null;
		try {
			config = Conf.Load (section);
		} catch {
			System.Environment.Exit (ERROR_CONFIG_LOAD);
		}

		if (config == null) {
			System.Environment.Exit (ERROR_CONFIG_NO_SECTION);
		}

		try {
			Conf.WriteSectionXml (config, Console.Out);
		} catch {
			System.Environment.Exit (ERROR_CONFIG_XML);
		}

		System.Environment.Exit (0);
	}

	private static void ReadSectionXml (string section)
	{
		Log.Level = LogLevel.Always; // shhhh... silence

		Config config = null;
		try {
			config = Conf.Load (section);
		} catch {
			System.Environment.Exit (ERROR_CONFIG_LOAD);
		}

		if (config == null) {
			System.Environment.Exit (ERROR_CONFIG_NO_SECTION);
		}

		try {
			if (! Conf.ReadSectionXml (config, Console.In))
				System.Environment.Exit (ERROR_CONFIG_BAD);
		} catch (System.Xml.XmlException) {
			System.Environment.Exit (ERROR_CONFIG_XML);
		} catch (Exception) {
			System.Environment.Exit (ERROR_CONFIG_BAD);
		}

		//Console.WriteLine ("Successfully read config for {0}", section);
		//Conf.WriteSectionXml (config, Console.Out);
		//Console.WriteLine ();

		try {
			Conf.Save (config);
		} catch {
			System.Environment.Exit (ERROR_CONFIG_SAVE);
		}

		System.Environment.Exit (0);
	}

	private static bool HandleArgs (Config config, string[] args)
	{
		Option option = (Option) config.Options [args [1]];
		if (option == null) {
			Console.WriteLine ("Error: No option {0}", args [1]);
			return false;
		}

		if (args.Length == 2) {
			ShowOption (config, option);
			return false;
		}

		return SetOption (config, option, args);
	}

	private static void ShowOption (Config config, Option option)
	{
		if (option.Type == OptionType.Bool) {
			Console.WriteLine ("  - {0}={2} ({1})",
					    option.Name,
					    option.Description,
					    config.GetOption (option.Name, true));

		} else if (option.Type == OptionType.String) {
			Console.WriteLine ("  - {0}={2} ({1})",
					    option.Name,
					    option.Description,
					    config.GetOption (option.Name, String.Empty));

		} else if (option.Type == OptionType.List) {
			Console.WriteLine ("  - {0} : ({1})", option.Name, option.Description);

			Console.Write ("    Parameters:");
			string[] param_names = config.GetListOptionParams (option.Name);

			for (int j = 0; j < param_names.Length; ++j)
				Console.Write (" [{0}] ", param_names [j]);
			Console.WriteLine ();

			List<string[]> items = config.GetListOptionValues (option.Name);
			if (items == null)
				return;

			Console.WriteLine ("    Values:");
			foreach (string[] item in items) {
				DisplayListItem (param_names, item);
			}
		}
	}

	private static void DisplayListItem (string[] param_names, string[] item)
	{
		Console.Write ("\t- ");
		for (int j = 0; j < param_names.Length; ++j)
			Console.Write ("[{0}]", item [j]);
		Console.WriteLine ();
	}

	private static bool SetOption (Config config, Option option, string[] args)
	{
		if (option.Type == OptionType.Bool) {
			if (args.Length != 3) {
				Console.WriteLine ("Error: Require {0} boolean argument(s)", 1);
				return false;
			}

			Console.WriteLine ("Changed:");
			ShowOption (config, option);
			config.SetOption (option.Name, Convert.ToBoolean (args [2]));
			Console.WriteLine ("  to:");
			ShowOption (config, option);

			return true;

		} else if (option.Type == OptionType.String) {
			if (args.Length != 3) {
				Console.WriteLine ("Error: Require {0} string argument(s)", 1);
				return false;
			}

			Console.WriteLine ("Changed:");
			ShowOption (config, option);
			config.SetOption (option.Name, args [2]);
			Console.WriteLine ("  to:");
			ShowOption (config, option);

			return true;

		} else if (option.Type == OptionType.List) {
			return SetListOption (config, option, args);
		}

		return false;
	}

	private static bool SetListOption (Config config, Option option, string[] args)
	{
		if (args [2] == "-")
			return RemoveListOption (config, option, args);

		string[] new_args = new string[args.Length - 2];
		Array.Copy (args, 2, new_args, 0, args.Length - 2);

		return config.AddListOptionValue (option.Name, new_args);
	}

	private static bool RemoveListOption (Config config, Option option, string[] args)
	{
		string[] new_args = new string[args.Length - 3];
		Array.Copy (args, 3, new_args, 0, args.Length - 3);

		bool to_save = false;

		if (config.RemoveListOptionValue (option.Name, new_args)) {
			Console.WriteLine ("Removing:");
			to_save = true;
			DisplayListItem (config.GetListOptionParams (option.Name), new_args);
		} else {
			Console.WriteLine ("No such option exists: " + String.Join (" ", new_args));
		}

		return to_save;
	}
}
