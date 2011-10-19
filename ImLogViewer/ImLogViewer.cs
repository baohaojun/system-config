//
// ImLogViewer.cs
//
// Lukas Lipka <lukaslipka@gmail.com>
// Raphael  Slinckx <rslinckx@gmail.com>
//
// Copyright (C) 2005 Novell, Inc.
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
using System.Reflection;

using Mono.Unix;
using Beagle.Util;

[assembly: AssemblyTitle ("beagle-imlogviewer")]
[assembly: AssemblyDescription ("Excellent IM history viewer for multiple clients")]

namespace ImLogViewer {

	public class ImLogViewer {
		
		private static string highlight;
		private static string search;
		private static string path;
		private static string client;

		public static void Main (string[] args)
		{
			// I18N
			Catalog.Init ("beagle", Beagle.Util.ExternalStringsHack.LocaleDir);

			SystemInformation.SetProcessName ("beagle-imlogviewer");

			ParseArgs (args);

			ImClient imclient;
			try {
				imclient = (ImClient) Enum.Parse (typeof (ImClient), client, true);
			} catch (Exception) {
				Console.WriteLine ("ERROR: '{0}' is not a valid client name.", client);
				Environment.Exit (3);
				return;
			}

			new ImLogWindow (imclient, path, search, highlight);
		}

		private static void PrintUsageAndExit ()
		{
			VersionFu.PrintHeader ();

			Console.WriteLine ("USAGE: beagle-imlogviewer --client <CLIENT> [OPTIONS] <log file or directory>\n" +
					   "Options:\n" +
					   "  --client\t\t\tClient that the log belongs to (e.g. Pidgin).\n" +					   
					   "  --highlight-search\t\tWords to highlight in the buffer.\n" +
					   "  --search\t\t\tSearch query to filter hits on.\n" +
					   "  --help\t\t\tPrint this usage message.\n" +
					   "  --version\t\t\tPrint version information.\n");

			Environment.Exit (0);
		}

		private static void ParseArgs (string [] args)
		{
			if (args.Length < 1)
				PrintUsageAndExit ();
			
			for (int i = 0; i < args.Length; i++) {
				switch (args [i]) {
				case "-h":
				case "--help":
					PrintUsageAndExit ();
					break;

				case "--version":
					VersionFu.PrintVersion ();
					Environment.Exit (0);
					break;

				case "--highlight-search":
					highlight = args [i + 1];
					i++;
					break;

				case "--search":
					search = args [i + 1];
					i++;
					break;

				case "--client":
					client = args [i + 1];
					i++;
					break;

				default:
					if (args [i].StartsWith ("--")) {
						Console.WriteLine ("WARN: Invalid option {0}", args [i]);
					} else {
						path = args [i];
					}
					break;
				}
			}

			if (path == null) {
				Console.WriteLine ("ERROR: Please specify a valid log path or log directory.");
				Environment.Exit (1);
			}

			if (client == null) {
				Console.WriteLine ("ERROR: Please specify a valid client name.");
				Environment.Exit (2);
			}
		}
	}
}
