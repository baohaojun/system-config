//
// FilterEbuild.cs
//
// Copyright (C) 2006 Pat Double <pat@patdouble.com>
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
using System.Text.RegularExpressions;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterEbuild : FilterPackage {
		static Regex metadata_pattern = new Regex ("\\s*(?<key>([A-Z_]+))\\s*=\\s*\"(?<value>(.*))\"\\s*", RegexOptions.Compiled);
		static Regex einfo_pattern = new Regex ("\\s*(einfo|ewarn)\\s+\"(?<message>(.*))\"\\s*", RegexOptions.Compiled);
		static Regex package_pattern = new Regex ("(?<name>([^0-9]+))-(?<version>(.+)).ebuild", RegexOptions.Compiled);

		public FilterEbuild () 
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromExtension (".ebuild"));
		}

		override protected void DoOpen (FileInfo file)
		{
			Match match = package_pattern.Match (file.Name);

			PackageName = match.Groups ["name"].ToString();
			PackageVersion = match.Groups ["version"].ToString();

			if (PackageName.Length == 0 && PackageVersion.Length == 0)
				return;

			// get download file size
			FileInfo digest = new FileInfo (file.Directory.FullName + "/files/digest-" + PackageName + "-" + PackageVersion);
			if (digest.Exists) {
				long download_size = 0;
				StreamReader digest_reader = new StreamReader (new FileStream (digest.FullName, FileMode.Open, FileAccess.Read, FileShare.Read));
				string digest_line = null;
				while ((digest_line = digest_reader.ReadLine ()) != null) {
					string[] digest_parts = digest_line.Split (' ');
					if (digest_parts.Length < 4)
						continue;
					if (digest_parts[0].Equals ("MD5"))
						download_size += Int64.Parse (digest_parts[3]);
				}
				Size = download_size;
				digest_reader.Close ();
			}

			// get installation information
			if (! file.FullName.StartsWith ("/var/db/pkg/"))
				return;
			FileInfo contents = new FileInfo (Path.Combine (file.Directory.FullName, "CONTENTS"));
			if (!contents.Exists)
				return;
			AddProperty (Beagle.Property.NewDate ("fixme:install_time", contents.LastWriteTime));

			// find installed objects
			long file_count = 0;
			long dir_count = 0;
			long byte_count = 0;
			StreamReader contents_reader = new StreamReader (new FileStream (contents.FullName, FileMode.Open, FileAccess.Read, FileShare.Read));
			string contents_line = null;
			while ((contents_line = contents_reader.ReadLine ()) != null) {
				if (contents_line.StartsWith ("dir"))
					dir_count ++;
				else if (!contents_line.StartsWith ("obj"))
					continue;

				file_count ++;
				string[] contents_parts = contents_line.Split (' ');
				if (contents_parts.Length < 2)
					continue;
				FileInfo desktop_file = null;
				try {
					FileInfo installed = new FileInfo (contents_parts[1]);
					if (installed.Exists)
					{
						byte_count += installed.Length;
						if (contents_parts[1].EndsWith (".desktop"))
							desktop_file = installed;
					}
				}
				catch (IOException) {
					// Mostly likely caused by permission error
				}

				if (desktop_file == null)
					continue;
				// verify this is a desktop file
				using (StreamReader desktop_reader = new StreamReader (new FileStream (desktop_file.FullName, FileMode.Open, FileAccess.Read, FileShare.Read))) {
					string desktop_line = null;
					bool desktop_valid = false;
					while ((desktop_line = desktop_reader.ReadLine ()) != null) {
						if (desktop_line.Trim ().Length > 0) {
							desktop_valid = desktop_line.Equals ("[Desktop Entry]");
							break;
						}
					}
				
					// add property
					if (desktop_valid)
						AddProperty (Beagle.Property.NewUnsearched ("fixme:desktop_file", desktop_file.FullName));
				}
			}

			contents_reader.Close ();

			AddProperty (Beagle.Property.NewUnsearched ("fixme:contents_byte_count", byte_count));
			AddProperty (Beagle.Property.NewUnsearched ("fixme:contents_file_count", file_count));
			AddProperty (Beagle.Property.NewUnsearched ("fixme:contents_dir_count", dir_count));
		}

		override protected bool PullPackageProperties () 
		{
			string str = null;
			while ((str = TextReader.ReadLine ()) != null) {
				// Skip comments
				if (str.StartsWith ("#"))
					continue;

				// Handle line continuation
				string str2 = null;
				while (str.Trim ().EndsWith ("\\") && ((str2 = TextReader.ReadLine ()) != null) ) {
					str = str.Trim ();
					if (str.Length == 1)
						str = str2;
					else
						str = str.Substring (0, str.Length - 1) + " " + str2.Trim ();
				}

				if (str.Length == 0)
					continue;

				// check for meta data
				MatchCollection matches;
				matches = metadata_pattern.Matches (str);

				if (matches.Count > 0) {
					foreach (Match the_match in matches) {
						String key = the_match.Groups ["key"].ToString ();
						String val = the_match.Groups ["value"].ToString ();
						if (key.Equals ("DESCRIPTION"))
							Summary = val; // Ebuild descriptions are short - use them as summary.
						else if (key.Equals ("LICENSE"))
							License = val;
						else if (key.Equals ("HOMEPAGE"))
							Homepage = val;
					}
				} else {
					// check for einfo/ewarn
					matches = einfo_pattern.Matches (str);
					if (matches.Count > 0) {
						foreach (Match the_match in matches)
							AppendText (the_match.Groups ["message"].ToString ());
					}
				}
			}
			return true;
		}
	}
}
