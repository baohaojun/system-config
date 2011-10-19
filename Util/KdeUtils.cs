//
// KdeUtils.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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
using System.Diagnostics;
using System.IO;
using System.Text;

namespace Beagle.Util {
	public static class KdeUtils {

		private static string [] icon_sizes = { "128x128", "64x64", "48x48", "32x32", "22x22", "16x16" };
		private static string [] kde_locations = { ExternalStringsHack.KdePrefix, Environment.GetEnvironmentVariable ("KDEDIR"), "/opt/kde3", "/usr" };
		public static string [] KdeLocations {
			get { return kde_locations; }
		}

		// Finds an icon by its name and returns its absolute path, or null if not found.
		public static string LookupIcon (string icon_name) {
			foreach (string kde_dir in KdeLocations) {
				if (kde_dir == null || kde_dir == String.Empty || !Directory.Exists (kde_dir))
					continue;

				string kde_share = Path.Combine (kde_dir, "share");
				string icon_prefix = Path.Combine (kde_share, "icons");
				string icon_theme_hicolor = Path.Combine (icon_prefix, "hicolor");
				string [] icon_themes = { null, null };

				if (! Directory.Exists (icon_theme_hicolor))
					icon_theme_hicolor = null;

				// FIXME: We should probably support svg icons at some point
				if (! icon_name.EndsWith(".png"))
					icon_name = icon_name + ".png";

				// We try up to 2 icon themes: we first try the theme pointed at by the
				// "default.kde" link, and then we try the trusted default "hicolor" theme.
				// We handle the situations if either (or both) of these aren't present, or
				// if default.kde == hicolor.

				StringBuilder icon_theme_default_sb = new StringBuilder ();
				Mono.Unix.Native.Syscall.readlink (Path.Combine (icon_prefix, "default.kde"), icon_theme_default_sb);
				string icon_theme_default = icon_theme_default_sb.ToString ();
				if (icon_theme_default != null) {
					if (! icon_theme_default.StartsWith ("/"))
						icon_theme_default = Path.Combine (icon_prefix, icon_theme_default);

					if (! Directory.Exists (icon_theme_default) || icon_theme_default == icon_theme_hicolor)
						icon_theme_default = null;
				}

				int i = 0;
				if (icon_theme_default != null)
					icon_themes [i++] = icon_theme_default;
				if (icon_theme_hicolor != null)
					icon_themes [i++] = icon_theme_hicolor;
				if (i == 0)
					continue;

				// Loop through all detected themes
				foreach (string theme in icon_themes) {
					if (theme == null)
						continue;

					// Try the preset icon sizes
					foreach (string size in icon_sizes) {
						string icon_base = Path.Combine (theme, size);
						if (! Directory.Exists (icon_base))
							continue;

						foreach (string icon_subdir in Directory.GetDirectories (icon_base)) {
							string icon_dir = Path.Combine (icon_base, icon_subdir);

							// Check for icon existance
							string icon_path = Path.Combine (icon_dir, icon_name);
							if (File.Exists (icon_path))
								return icon_path;
						}
					}
				}
				// Only search the first valid path that we find
				break; 
			}
			return null;
		}

		private static string kde3_user_dir = Path.Combine (PathFinder.HomeDir, ".kde");
		public static string KDE3UserDir {
			get { return kde3_user_dir; }
		}

		private static string kde4_user_dir = String.Empty; // default initial value
		public static string KDE4UserDir {
			get {
				if (kde4_user_dir != String.Empty)
					return kde4_user_dir;

				// first check if the KDE_SESSION_VERSION is set and is equal to 4
				string env = Environment.GetEnvironmentVariable ("KDE_SESSION_VERSION");
				// if the environment variable is set, ~/.kde4 is the kde4 directory; always.
				if (env == "4")
					kde4_user_dir = Path.Combine (PathFinder.HomeDir, ".kde4");
				else
					kde4_user_dir = null;

				return kde4_user_dir;
			}
		}

		public static string KDEUserDir {
			get {
				if (KDE4UserDir != null)
					return KDE4UserDir;

				// If not sure running kde4, check if the .kde4 directory was created while beagle was running
				string kde4_dir = Path.Combine (PathFinder.HomeDir, ".kde4");
				if (Directory.Exists (kde4_dir)) {
					kde4_user_dir = kde4_dir; // if the kde4 directory is found, cache is for future
					return kde4_user_dir;
				}

				// else use the kde3 directory
				return KDE3UserDir;
			}
		}

		public static string ReadPasswordKDEWallet (string folder, string username)
		{
			if (String.IsNullOrEmpty (folder) || String.IsNullOrEmpty (username))
				throw new ArgumentException ("folder, username", "cannot be empty");

			// Get name of the local wallet
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string[] { "dcop", "kded", "kwalletd", "localWallet" };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string localWallet = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				localWallet = pout.ReadLine ();
			pc.Close ();

			if (String.IsNullOrEmpty (localWallet) || localWallet == "-1")
				throw new ArgumentException ("kwalletd", "Unable to reach local KDE wallet");

			// Open local wallet
			pc = new SafeProcess ();
			pc.Arguments = new string[] {"dcop", "kded", "kwalletd", "open", localWallet, "K" };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string wallet_id = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				wallet_id = pout.ReadLine ();
			pc.Close ();

			if (String.IsNullOrEmpty (wallet_id) || wallet_id == "-1")
				throw new ArgumentException ("kwalletd", "Unable to open local KDE wallet");

			// Read password from the given folder and for the given username
			pc = new SafeProcess ();
			pc.Arguments = new string[] {"dcop", "kded", "kwalletd", "readPassword", wallet_id, folder, username };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string password = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				password = pout.ReadLine ();
			pc.Close ();

			if (String.IsNullOrEmpty (password))
				throw new ArgumentException ("kwalletd", "Unable to read password.");

			return password;
		}

		public static void StorePasswordKDEWallet (string folder, string username, string password)
		{
			if (String.IsNullOrEmpty (folder) || String.IsNullOrEmpty (username) || String.IsNullOrEmpty (password))
				throw new ArgumentException ("folder, username or password", "cannot be empty");

			// Get name of the local wallet
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string[] { "dcop", "kded", "kwalletd", "localWallet" };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string localWallet = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				localWallet = pout.ReadLine ();
			pc.Close ();

			if (String.IsNullOrEmpty (localWallet) || localWallet == "-1")
				throw new ArgumentException ("kwalletd", "Local KDE Wallet is not found. Please run kwalletmanager to enable KDE Wallet.");

			// Open local wallet
			pc = new SafeProcess ();
			pc.Arguments = new string[] {"dcop", "kded", "kwalletd", "open", localWallet, "K" };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string wallet_id = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				wallet_id = pout.ReadLine ();
			pc.Close ();

			if (String.IsNullOrEmpty (wallet_id) || wallet_id == "-1")
				throw new ArgumentException ("kwalletd", "Unable to open local KDE wallet");

			// Write given password for the given folder and username
			pc = new SafeProcess ();
			pc.Arguments = new string[] {"dcop", "kded", "kwalletd", "writePassword", wallet_id, folder, username, password };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			pc.Start ();
			string ret = null;
			using (StreamReader pout = new StreamReader (pc.StandardOutput))
				ret = pout.ReadLine ();
			pc.Close ();

			if (ret != "0")
				throw new ArgumentException ("kwalletd", "Unable to save password.");
		}

	}
}

