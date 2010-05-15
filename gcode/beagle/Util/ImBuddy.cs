//
// ImBuddy.cs
//
// Copyright (C) 2004 Matthew Jones <mattharrison sbcglobal net>
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
using System.Collections;
using System.Xml;
using System.IO;

namespace Beagle.Util {

	public class ImBuddy {

		public string protocol = null;
		public string owner_account_name = null;
		public string buddy_account_name = null;
		public string alias = null;
		public string buddy_icon_location = null;
		public string buddy_icon_checksum = null;

		public ImBuddy (string protocol, string owner, string buddy, string alias, string location, string checksum)
		{
			this.protocol = protocol;
			this.owner_account_name = owner;
			this.buddy_account_name = buddy;
			this.alias = alias;
			this.buddy_icon_location = location;
			this.buddy_icon_checksum = checksum;
		}

		public string Protocol {
			get { return protocol; }
		}

		public string OwnerAccountName {
			get { return owner_account_name; }
		}

		public string BuddyAccountName {
			get { return buddy_account_name; }
		}

		public string Alias {
			get { return alias; }
			set { alias = value; }
		}

		public string BuddyIconLocation {
			get { return buddy_icon_location; }
			set { buddy_icon_location = value; }
		}

		public string BuddyIconChecksum {
			get { return buddy_icon_checksum; }
			set { buddy_icon_checksum = value; }
		}
	}
	
	///////////////////////////////////////////////////////////////////////////////

	public abstract class ImBuddyListReader {

		public Hashtable BuddyList = null;

		abstract public void Read ();

		protected static string Format (string name)
		{
			return name.ToLower ().Replace (" ", "");
		}

		public virtual ImBuddy Search (string buddy)
		{
			if (BuddyList == null)
				return null;

			return (ImBuddy)BuddyList[Format (buddy)];
		}
	}

	///////////////////////////////////////////////////////////////////////////////
	
	public class PidginBuddyListReader : ImBuddyListReader {

		private string icons_dir = null;
		private string buddy_list_path = null;
		private DateTime buddy_list_last_write;
		private uint timeout_id;

		public PidginBuddyListReader ()
		{
			string buddy_list_dir = Path.Combine (PathFinder.HomeDir, ".gaim");

			if (Directory.Exists (Path.Combine (PathFinder.HomeDir, ".purple")))
				buddy_list_dir = Path.Combine (PathFinder.HomeDir, ".purple");

			this.icons_dir = Path.Combine (buddy_list_dir, "icons");
			this.buddy_list_path = Path.Combine (buddy_list_dir, "blist.xml");
			
			if (File.Exists (buddy_list_path))
				Read ();
			
			// Poll the file once every minute
			timeout_id = GLib.Timeout.Add (60000, new GLib.TimeoutHandler (ReadTimeoutHandler));
		}

		public PidginBuddyListReader (string buddy_list_dir)
		{
			this.buddy_list_path = Path.Combine (buddy_list_dir, "blist.xml");
			
			if (File.Exists (buddy_list_path))
				Read ();

			// Poll the file once every minute
			timeout_id = GLib.Timeout.Add (60000, new GLib.TimeoutHandler (ReadTimeoutHandler));
		}

		~PidginBuddyListReader ()
		{
			if (timeout_id > 0)
				GLib.Source.Remove (timeout_id);
		}

		private bool ReadTimeoutHandler ()
		{
			if (File.Exists (buddy_list_path))
				Read ();

			return true;
		}

		public override void Read ()
		{
			// If the file hasn't changed, don't do anything.
			DateTime last_write = File.GetLastWriteTime (buddy_list_path);

			if (last_write == buddy_list_last_write)
				return;

			buddy_list_last_write = last_write;

			BuddyList = new Hashtable ();

			try {
				XmlDocument accounts = new XmlDocument ();
				accounts.Load (buddy_list_path);
				
				XmlNodeList contacts = accounts.SelectNodes ("//contact");
				
				foreach (XmlNode contact in contacts) {
					string group_alias = null;
					
					foreach (XmlAttribute attr in contact.Attributes) {
						if (attr.Name == "alias")
							group_alias = attr.Value;
					}
					
					if (!String.IsNullOrEmpty (group_alias)) {
						foreach (XmlNode buddy in contact.ChildNodes)
							AddBuddy (buddy, group_alias);
					}
				}
				
				foreach (XmlNode buddy in accounts.SelectNodes ("//contact[not(@name)]/buddy"))
					AddBuddy (buddy);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught exception while trying to parse Gaim contact list:");
			}
		}

		private void AddBuddy (XmlNode node, string group_alias)
		{
			string protocol = null;
			string owner = null;
			string buddy = null;
			string alias = null;
			string iconlocation = null;
			string iconchecksum = null;

			foreach (XmlAttribute attr in node.Attributes) {
				switch (attr.Name) {
				case "account":
					owner = attr.Value;
					break;
					
				case "proto":
					protocol = attr.Value;
					break;
				}
			}
		
			foreach (XmlNode attr in node.ChildNodes) {
				switch (attr.LocalName) {
				case "name":
					buddy = attr.InnerText;
					break;
					
				case "alias":
					alias = attr.InnerText;
					break;
					
				case "setting":
					foreach (XmlAttribute subattr in attr.Attributes) {
						if (subattr.Name == "name" && subattr.Value == "buddy_icon") {
							iconlocation = Path.Combine (icons_dir, attr.InnerText);
						} else if (subattr.Name == "name" && subattr.Value == "icon_checksum") {
							iconchecksum = attr.InnerText;
						}
					}
					break;
				}
			}

			if (!String.IsNullOrEmpty (group_alias))
				alias = group_alias;

			if (BuddyList.ContainsKey (Format (buddy))) {
				ImBuddy old = Search (buddy);

				if (String.IsNullOrEmpty (old.Alias) && !String.IsNullOrEmpty (alias))
					old.Alias = alias;

				if (String.IsNullOrEmpty (old.BuddyIconLocation) && !String.IsNullOrEmpty (iconlocation)) {
					old.BuddyIconLocation = iconlocation;
					old.BuddyIconChecksum = iconchecksum;
				}
			} else {
				ImBuddy im_buddy = new ImBuddy (protocol, owner, Format (buddy), alias, iconlocation, iconchecksum);
				BuddyList.Add (Format (buddy), im_buddy);
			}
		}

		private void AddBuddy (XmlNode buddy) 
		{
			AddBuddy (buddy, null);
		}
	}

	/////////////////////////////////////////////////////////////

	public class KopeteBuddyListReader : ImBuddyListReader {

		private string buddy_list_path = null;
		private string buddy_list_dir = null;
		private DateTime buddy_list_last_write;
		private uint timeout_id;

		public KopeteBuddyListReader ()
		{
			buddy_list_dir = Path.Combine (PathFinder.HomeDir, ".kde/share/apps/kopete");
			buddy_list_path = Path.Combine (buddy_list_dir, "contactlist.xml");
			
			if (File.Exists (buddy_list_path))
				Read ();

			// Poll the file once every minute
			timeout_id = GLib.Timeout.Add (60000, new GLib.TimeoutHandler (ReadTimeoutHandler));
		}

		~KopeteBuddyListReader ()
		{
			if (timeout_id > 0)
				GLib.Source.Remove (timeout_id);
		}

		private bool ReadTimeoutHandler ()
		{
			if (File.Exists (buddy_list_path))
				Read ();

			return true;
		}

		public override void Read ()
		{
			// Ignore empty files
			FileInfo fi = new FileInfo (buddy_list_path);

			if (fi.Length == 0)
				return;

			// If the file hasn't changed, don't do anything.
			DateTime last_write = File.GetLastWriteTime (buddy_list_path);

			if (last_write == buddy_list_last_write)
				return;

			buddy_list_last_write = last_write;

			BuddyList = new Hashtable ();

			try {
				XmlDocument accounts = new XmlDocument ();
				accounts.Load (buddy_list_path);
				
				// Find all xml contact nodes in the contact list
				foreach (XmlNode contact in accounts.SelectNodes ("//meta-contact"))
					AddContact (contact);
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Ignoring malformed Kopete contact list:");
			}
		}

		private void AddContact (XmlNode contact) 
		{
			string protocol = null;
			string owner = null;
			string buddy = null;
			string alias = null;

			// For each and every meta-contact, there can be multiple 
			// buddy information entries if we have a contact added on
			// multiple protocols. Loop through them.

 			foreach (XmlNode plugin_node in contact.SelectNodes ("plugin-data")) {
				// Determine the protocol
				XmlAttribute plugin_id_attr = plugin_node.Attributes ["plugin-id"];
				protocol = plugin_id_attr.Value.Substring (0, plugin_id_attr.Value.Length-8).ToLower ();

				// Fetch all the buddy properties
				foreach (XmlNode plugin_data_node in plugin_node.SelectNodes ("plugin-data-field")) { 
					switch (plugin_data_node.Attributes ["key"].Value) {
					case "contactId":
						buddy = plugin_data_node.InnerText;
						break;
					case "accountId":
						owner = plugin_data_node.InnerText;
						break;
					case "displayName":
						alias = plugin_data_node.InnerText;
						break;
					}
				}
				
				string buddy_name = Format (buddy);

				// Replace any earlier buddies with the same screenname
				// FIXME: Not safe since we can have the same screenname on different accounts.
				if (BuddyList.ContainsKey (buddy_name))
					BuddyList.Remove (buddy_name);
				
				ImBuddy im_buddy = new ImBuddy (protocol, owner, buddy_name, alias, null, null);

				BuddyList.Add (buddy_name, im_buddy);
			}
		}
	}
}
