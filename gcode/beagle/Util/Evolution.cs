//
// Evolution.cs
//
// Copyright (C) 2005 Novell, Inc.
//
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

using Mono.Unix;

namespace Beagle.Util {
	// FIXME: IMAP4
	public class Evolution
	{
		public static ICollection Accounts
		{
			get {
				ArrayList accounts = new ArrayList ();
				string mail_dir = Path.Combine (Environment.GetEnvironmentVariable("HOME"), Path.Combine (".evolution", "mail"));

				if (Directory.Exists (Path.Combine (mail_dir, "local"))) {
					MailAccount local_account = new MailAccount (Catalog.GetString ("On This Computer"), Path.Combine (mail_dir, "local"));
					local_account.AddChildren (GetLocalChildren (local_account));
					accounts.Add (local_account);
				}

				if (!Directory.Exists (Path.Combine (mail_dir, "imap"))) {
					return accounts;
				}
				DirectoryInfo imap_accounts_dir = new DirectoryInfo (Path.Combine (mail_dir, "imap"));
				foreach (DirectoryInfo imap_account_dir in imap_accounts_dir.GetDirectories ()) {
					MailAccount imap_account = new MailAccount (imap_account_dir.Name, imap_account_dir.FullName);
					imap_account.AddChildren (GetImapChildren (imap_account));
					accounts.Add (imap_account);
				}
				
				return accounts;
			}
		}

		private static ICollection GetLocalChildren (MailFolder folder) {
			ArrayList children = new ArrayList ();
			DirectoryInfo folder_dir;

			folder_dir = new DirectoryInfo (folder.Path);

			if (!folder_dir.Exists)
				folder_dir = new DirectoryInfo (folder_dir.FullName + ".sbd");

			if (!folder_dir.Exists)
				return children;
			
			foreach (FileInfo child_summary_file in folder_dir.GetFiles ("*.cmeta")) {
				MailFolder child_folder = new MailFolder (child_summary_file.Name.Substring (0,child_summary_file.Name.Length-6),
									  child_summary_file.FullName.Substring (0, child_summary_file.FullName.Length-6));
				child_folder.AddChildren (GetLocalChildren (child_folder));
				children.Add (child_folder);
			}

			return children;
		}

		private static ICollection GetImapChildren (MailFolder folder) {
			ArrayList children = new ArrayList ();
			DirectoryInfo folder_dir;

			folder_dir = new DirectoryInfo (Path.Combine(folder.Path,"folders"));

			if (!folder_dir.Exists)
				folder_dir = new DirectoryInfo (Path.Combine(folder.Path,"subfolders"));
			
			if (!folder_dir.Exists)
				return children;
			
			foreach (DirectoryInfo child_folder_dir in folder_dir.GetDirectories ()) {
				MailFolder child_folder = new MailFolder (child_folder_dir.Name, child_folder_dir.FullName);
				child_folder.AddChildren (GetImapChildren (child_folder));
				children.Add (child_folder);
			}

			return children;
		}
	}
	
	public class MailAccount : MailFolder
	{
		public MailAccount (string name, string path) : base (name, path) { }
	}
	
	public class MailFolder
	{
		string name, path;
		public string Name {
			get { return name; }
			set { name = value; }
		}
		
		public string Path {
			get { return path; }
			set { path = value; }
		}

		public MailFolder (string name, string path) {
			this.name = name;
			this.path = path;
		}

		ArrayList children = new ArrayList ();
		
		public void AddChild (MailFolder child)
		{
			this.children.Add (child);
		}

		public void AddChildren (ICollection children)
		{
			this.children.AddRange (children);
		}
		
		public ICollection Children
		{
			get {
				return children;
			}
		}

		public override string ToString () 
		{
			return GetNameForPath (path);
		}

		private static string[] crap = { "/imap/", "/local/", "/imap4/", "/folders", "/subfolder", ".sbd"};

		public static string GetNameForPath (string folder_path)
		{
			if (folder_path.StartsWith (System.IO.Path.Combine (Environment.GetEnvironmentVariable ("HOME"),".evolution/mail")))
				folder_path = folder_path.Substring (System.IO.Path.Combine (Environment.GetEnvironmentVariable ("HOME"),".evolution/mail").Length);

			if (folder_path.StartsWith ("/local/"))
				folder_path = String.Format ("{0}/{1}", Catalog.GetString ("On This Computer"), folder_path);

			foreach (string shit in crap)
				folder_path = folder_path.Replace (shit, "");

			return folder_path;
		}
	}
}
