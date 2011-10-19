//
// Settings.cs
//
// Copyright (C) 2005-2007 Novell, Inc.
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
using System.Threading;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;

using Mono.Unix;
using Mono.Unix.Native;

using Gtk;
using Gdk;
using Glade;

using Beagle;
using Beagle.Util;

public class SettingsDialog 
{
	public static void Main (string[] args)
	{
		try {
			SettingsDialog settings = new SettingsDialog (args);
			settings.Run ();
		} catch (Exception e) {
			Log.Error (e, "Error while running beagle-settings");
			HigMessageDialog dialog = new HigMessageDialog (null,
					DialogFlags.Modal,
					MessageType.Error, 
					ButtonsType.Close, 
					Catalog.GetString ("An error occurred"), 
					e.Message);
			dialog.Run ();
			Environment.Exit (1);
		}
	}

	////////////////////////////////////////////////////////////////
	// Widgets

	[Widget] Gtk.Window settings_dialog;
	[Widget] Notebook notebook;

	[Widget] VBox administration_frame;

	[Widget] CheckButton allow_root_toggle;
	[Widget] CheckButton autostart_toggle;
	[Widget] CheckButton battery_toggle;
	[Widget] CheckButton screensaver_toggle;
	[Widget] CheckButton auto_search_toggle;
	[Widget] Label shortcut_label;
	[Widget] Button shortcut_button;
	string binding;

	[Widget] CheckButton index_home_toggle;
	[Widget] Button remove_include_button;
	[Widget] Button remove_exclude_button;
		
	[Widget] Button display_up_button;
	[Widget] Button display_down_button;

	[Widget] ScrolledWindow include_sw;
	[Widget] ScrolledWindow exclude_sw;

	private IncludeView include_view;
	private ExcludeView exclude_view;

	////////////////////////////////////////////////////////////////
	// Backends

	[Widget] ScrolledWindow backends_sw;

	private BackendView backend_view;
	
	////////////////////////////////////////////////////////////////
	// Zeroconf

	[Widget] VBox networking_box;
        [Widget] CheckButton allow_webinterface_toggle;
        [Widget] CheckButton allow_global_access_toggle;
        [Widget] Alignment networking_settings_box;

        [Widget] ScrolledWindow networking_sw;

        [Widget] Alignment networking_password_box;

        [Widget] CheckButton require_password_toggle;

        [Widget] Entry index_name_entry;
        [Widget] Entry password_entry;

        [Widget] Button add_host_button;
        
        private NetworkingView networking_view;
        [Widget] Button remove_host_button;        

	////////////////////////////////////////////////////////////////
	// Initialize       

	public SettingsDialog (string[] args)
	{
		Application.Init ();

		Catalog.Init ("beagle", ExternalStringsHack.LocaleDir);

		Glade.XML glade = new Glade.XML (null, "settings.glade", "settings_dialog", "beagle");
		glade.Autoconnect (this);

		settings_dialog.Icon = IconTheme.Default.LoadIcon ("system-search", 16, IconLookupFlags.NoSvg);
		administration_frame.Visible = (Environment.UserName == "root");

		include_view = new IncludeView ();
		include_view.Selection.Changed += new EventHandler (OnIncludeSelected);
		include_view.Show ();
		include_sw.Child = include_view;

		exclude_view = new ExcludeView ();
		exclude_view.Selection.Changed += new EventHandler (OnExcludeSelected);
		exclude_view.Show ();
		exclude_sw.Child = exclude_view;

		backend_view = new BackendView ();
		backend_view.Show ();
		backends_sw.Child = backend_view;

		networking_view = new NetworkingView ();
		networking_view.Selection.Changed += new EventHandler (OnHostSelected);
		networking_view.Show ();
                networking_sw.Child = networking_view;
		networking_box.Show ();

		networking_settings_box.Visible = true;

		//FIXME Password feature is not yet implemented
		networking_password_box.Sensitive = false;
		require_password_toggle.Sensitive = false;

		// Keybinding button

		shortcut_button.Clicked += new EventHandler (OnKeybindingClicked);

		LoadConfiguration ();

		Conf.Subscribe (Conf.Names.FilesQueryableConfig, new Conf.ConfigUpdateHandler (OnConfigurationChanged));
		Conf.Subscribe (Conf.Names.BeagleSearchConfig, new Conf.ConfigUpdateHandler (OnConfigurationChanged));
		Conf.Subscribe (Conf.Names.DaemonConfig, new Conf.ConfigUpdateHandler (OnConfigurationChanged));
		Conf.Subscribe (Conf.Names.NetworkingConfig, new Conf.ConfigUpdateHandler (OnConfigurationChanged));

		ParseArgs (args);
	}

	private void ParseArgs (string[] args)
	{
		if (args.Length < 1)
			return;
		
		foreach (string s in args) {
			switch (s) {
			case "--searching":
				notebook.Page = 0;
				return;
			case "--indexing":
				notebook.Page = 1;
				return;
			case "--backends":
				notebook.Page = 2;
				return;
			case "--networking":
				notebook.Page = 3;
				return;
			}
		}
	}

	public void Run ()
	{
		Application.Run ();
	}

	////////////////////////////////////////////////////////////////
	// Configuration

	private void LoadConfiguration ()
	{
		Config fsq_config = Conf.Get (Conf.Names.FilesQueryableConfig);
		Config daemon_config = Conf.Get (Conf.Names.DaemonConfig);
		Config bs_config = Conf.Get (Conf.Names.BeagleSearchConfig);

		allow_root_toggle.Active = daemon_config.GetOption (Conf.Names.AllowRoot, false);
		auto_search_toggle.Active = bs_config.GetOption (Conf.Names.BeagleSearchAutoSearch, true);
		battery_toggle.Active = daemon_config.GetOption (Conf.Names.IndexOnBattery, false);
		screensaver_toggle.Active = daemon_config.GetOption (Conf.Names.IndexFasterOnScreensaver, true);

		autostart_toggle.Active = IsAutostartEnabled ();

		binding = bs_config.GetOption ("KeyBinding", null);
		if (String.IsNullOrEmpty (binding)) {
			// Move old preference value to new
			bool binding_ctrl = bs_config.GetOption (Conf.Names.KeyBinding_Ctrl, false);
			bool binding_alt = bs_config.GetOption (Conf.Names.KeyBinding_Alt, false);
			string binding_key = bs_config.GetOption (Conf.Names.KeyBinding_Key, "F12");
			KeyBinding show_binding = new KeyBinding (binding_key, binding_ctrl, binding_alt);

			binding = show_binding.ToString ();
		}

		shortcut_label.Text = String.Format (Catalog.GetString ("Display the search window by pressing {0}"), binding);

		if (fsq_config.GetOption (Conf.Names.IndexHomeDir, true))
			index_home_toggle.Active = true;

		List<string[]> values = fsq_config.GetListOptionValues (Conf.Names.Roots);
		if (values != null)
			foreach (string[] root in values)
				include_view.AddPath (root [0]);

		values = fsq_config.GetListOptionValues (Conf.Names.ExcludeSubdirectory);
		if (values != null)
			foreach (string[] subdir in values)
				exclude_view.AddItem (new ExcludeItem (ExcludeType.Path, subdir [0]));

		values = fsq_config.GetListOptionValues (Conf.Names.ExcludePattern);
		if (values != null)
			foreach (string[] pattern in values)
				exclude_view.AddItem (new ExcludeItem (ExcludeType.Pattern, pattern [0]));

		values = daemon_config.GetListOptionValues (Conf.Names.ExcludeMailfolder);
		if (values != null)
			foreach (string[] mailfolder in values)
				exclude_view.AddItem (new ExcludeItem (ExcludeType.MailFolder, mailfolder [0]));

		Config networking_config = Conf.Get (Conf.Names.NetworkingConfig);
                allow_webinterface_toggle.Active = networking_config.GetOption ("WebInterface", false);
                allow_global_access_toggle.Active = networking_config.GetOption (Conf.Names.ServiceEnabled, false);

		List<string[]> services = networking_config.GetListOptionValues (Conf.Names.NetworkServices);
		if (services != null) {
			foreach (string[] svc in services) {
				NetworkService s = new NetworkService ();
				s.Name = svc [0];
				s.UriString = svc [1];
				s.IsProtected = false;
				s.Cookie = null;
				networking_view.AddNode (s);
			}
		}

                require_password_toggle.Active = networking_config.GetOption (Conf.Names.PasswordRequired, true);
                index_name_entry.Text = networking_config.GetOption (Conf.Names.ServiceName, String.Empty);
                string password = networking_config.GetOption (Conf.Names.ServicePassword, String.Empty);
		password = password.PadRight (12);
                password_entry.Text = password.Substring (0, 12);

		values = daemon_config.GetListOptionValues (Conf.Names.DeniedBackends);
		if (values != null)
			foreach (string[] backend in values)
				backend_view.Set (backend[0], false);
	}

	private void SaveConfiguration ()
	{
		Config fsq_config = Conf.Get (Conf.Names.FilesQueryableConfig);
		Config daemon_config = Conf.Get (Conf.Names.DaemonConfig);
		Config bs_config = Conf.Get (Conf.Names.BeagleSearchConfig);

		daemon_config.SetOption (Conf.Names.AllowRoot, allow_root_toggle.Active);
		daemon_config.SetOption (Conf.Names.IndexOnBattery,battery_toggle.Active);
		daemon_config.SetOption (Conf.Names.IndexFasterOnScreensaver, screensaver_toggle.Active);

		bs_config.SetOption (Conf.Names.BeagleSearchAutoSearch,auto_search_toggle.Active);
		bs_config.SetOption ("KeyBinding", binding);

		fsq_config.SetOption (Conf.Names.IndexHomeDir, index_home_toggle.Active);

		List<string[]> roots = new List<string[]> (include_view.Includes.Count);
		foreach (string root in include_view.Includes) {
			roots.Add (new string[1] {root});
		}
		fsq_config.SetListOptionValues (Conf.Names.Roots, roots);

		List<string[]> excludes_path = new List<string[]> ();
		List<string[]> excludes_pattern = new List<string[]> ();
		List<string[]> excludes_mailfolder = new List<string[]> ();

		foreach (ExcludeItem exclude in exclude_view.Excludes) {
			if (exclude.Type == ExcludeType.Path)
				excludes_path.Add (new string[1] {exclude.Value});
			else if (exclude.Type == ExcludeType.Pattern)
				excludes_pattern.Add (new string[1] {exclude.Value});
			else if (exclude.Type == ExcludeType.MailFolder)
				excludes_mailfolder.Add (new string[1] {exclude.Value});
		}

		if (excludes_path.Count > 0)
			fsq_config.SetListOptionValues (Conf.Names.ExcludeSubdirectory, excludes_path);

		if (excludes_pattern.Count > 0)
			fsq_config.SetListOptionValues (Conf.Names.ExcludePattern, excludes_pattern);

		if (excludes_mailfolder.Count > 0)
			daemon_config.SetListOptionValues (Conf.Names.ExcludeMailfolder, excludes_mailfolder);


		Config networking_config = Conf.Get (Conf.Names.NetworkingConfig);

		networking_config.SetOption ("WebInterface", allow_webinterface_toggle.Active);
		networking_config.SetOption (Conf.Names.ServiceEnabled, allow_global_access_toggle.Active);
		networking_config.SetOption (Conf.Names.ServiceName, index_name_entry.Text);
		networking_config.SetOption (Conf.Names.PasswordRequired, require_password_toggle.Active);
		networking_config.SetOption (Conf.Names.ServicePassword, Password.Encode (password_entry.Text));

		List<string[]> svcs = new List<string[]> (networking_view.Nodes.Count);
		foreach (NetworkService svc in networking_view.Nodes) {
			svcs.Add (new string [4] {svc.Name, svc.UriString, Convert.ToString (svc.IsProtected), svc.Cookie});
		}
		networking_config.SetListOptionValues (Conf.Names.NetworkServices, svcs);

		Conf.Save (networking_config);

		List<string[]> denied_backends = new List<string[]> ();

		foreach (string backend in backend_view.Denied)
			denied_backends.Add (new string[] { backend });
		daemon_config.SetListOptionValues (Conf.Names.DeniedBackends, denied_backends);

		Conf.Save (fsq_config);
		Conf.Save (bs_config);
		Conf.Save (daemon_config);
	}

	private void OnConfigurationChanged (Config config)
	{
		HigMessageDialog dialog = new HigMessageDialog (settings_dialog,
								DialogFlags.Modal,
								MessageType.Question,
								ButtonsType.YesNo,
								Catalog.GetString ("Reload configuration"),
								Catalog.GetString ("The configuration file has been modified by another application. " + 
										   "Do you wish to discard the currently displayed values and reload configuration from disk?"));

		ResponseType response = (ResponseType) dialog.Run ();

		if (response == ResponseType.Yes)
			LoadConfiguration ();

		dialog.Destroy ();
	}

	////////////////////////////////////////////////////////////////
	// Autostart

	private string system_autostart_dir = Path.Combine (Path.Combine (ExternalStringsHack.SysConfDir, "xdg"), "autostart");
	private string local_autostart_dir = Path.Combine (Path.Combine (Environment.GetEnvironmentVariable ("HOME"), ".config"), "autostart");

	private bool IsAutostartEnabled ()
	{
		// FIXME: We need to do better than this.

		string local_beagled = Path.Combine (local_autostart_dir, "beagled-autostart.desktop");
		string system_beagled = Path.Combine (system_autostart_dir, "beagled-autostart.desktop");

		if (File.Exists (local_beagled)) {
			StreamReader reader = new StreamReader (local_beagled);

			try {
				string l;
				while ((l = reader.ReadLine ()) != null) {
					if (String.Compare (l, "X-GNOME-Autostart-enabled=false", true) == 0)
						return false;
				}

				return true;
			} finally {
				reader.Close ();
			}
		} else if (File.Exists (system_beagled)) {
			StreamReader reader = new StreamReader (system_beagled);

			try {
				string l;
				while ((l = reader.ReadLine ()) != null) {
					if (String.Compare (l, "X-GNOME-Autostart-enabled=false", true) == 0)
						return false;
				}

				return true;
			} finally {
				reader.Close ();
			}
		} else
			return false;
	}

	private void SetAutostart (bool enabled)
	{
		if (! Directory.Exists (local_autostart_dir)) {
			Directory.CreateDirectory (local_autostart_dir);
			Syscall.chmod (local_autostart_dir, (FilePermissions) 448); // 448 == 0700
		}

		string beagled_file = Path.Combine (local_autostart_dir, "beagled-autostart.desktop");
		string beagle_search_file = Path.Combine (local_autostart_dir, "beagle-search-autostart.desktop");

		Assembly assembly = Assembly.GetExecutingAssembly ();

		StreamReader reader = new StreamReader (assembly.GetManifestResourceStream ("beagled-autostart.desktop"));
		StreamWriter writer = new StreamWriter (beagled_file);

		string l;
		while ((l = reader.ReadLine ()) != null)
			writer.WriteLine (l);
		reader.Close ();

		if (! enabled) {
			writer.WriteLine ("# Setting Hidden=true unintuitively disables autostart on KDE and");
			writer.WriteLine ("# GNOME >= 2.19.2, but it breaks disabling of autostart in older GNOME.");
			writer.WriteLine ("Hidden=true");
			writer.WriteLine ("X-GNOME-Autostart-enabled=false");
		}

		writer.Close ();

		reader = new StreamReader (assembly.GetManifestResourceStream ("beagle-search-autostart.desktop"));
		writer = new StreamWriter (beagle_search_file);

		while ((l = reader.ReadLine ()) != null)
			writer.WriteLine (l);
		reader.Close ();

		if (! enabled) {
			writer.WriteLine ("# Setting Hidden=true unintuitively disables autostart on KDE and");
			writer.WriteLine ("# GNOME >= 2.19.2, but it breaks disabling of autostart in older GNOME.");
			writer.WriteLine ("Hidden=true");
			writer.WriteLine ("X-GNOME-Autostart-enabled=false");
		}

		writer.Close ();
	}

	////////////////////////////////////////////////////////////////
	// Eventhandlers

	private void OnAutostartToggled (object o, EventArgs args)
	{
		SetAutostart (((Gtk.ToggleButton) o).Active);
	}

	private void OnDialogResponse (object o, ResponseArgs args)
	{
		switch (args.ResponseId) {
		case ResponseType.Help:
			Gnome.Url.Show ("http://beagle-project.org/Configuring");
			break;
		case ResponseType.Ok:
			SaveConfiguration ();
			Application.Quit ();
			break;
		default:
			Application.Quit ();
			break;
		}
	}

	private void OnDisplaySelected (object o, EventArgs args)
	{
		display_up_button.Sensitive = true;
		display_down_button.Sensitive = true;
	}

	private void OnAddIncludeClicked (object o, EventArgs args)
	{
		CompatFileChooserDialog fs_dialog = new CompatFileChooserDialog (Catalog.GetString ("Select Path"), 
										 settings_dialog, 
										 CompatFileChooserDialog.Action.SelectFolder);
		fs_dialog.SelectMultiple = false;

		ResponseType fs_response = (ResponseType) fs_dialog.Run ();
		string new_include = fs_dialog.Filename;
		fs_dialog.Destroy ();
		
		if (fs_response == ResponseType.Ok) {
			string error_message = "";
			bool throw_error = false;
			ArrayList obsolete_includes = new ArrayList ();

			// Check and see if the current data collides with the new path in any way
			// FIXME: Do this with System.IO.Path or something
			foreach (string old_include in include_view.Includes) {
				if (new_include == old_include) {
					throw_error = true;
					error_message = Catalog.GetString ("The selected path is already selected for indexing and wasn't added.");
				} else if (new_include.StartsWith (old_include + System.IO.Path.PathSeparator)) {
					throw_error = true;
					error_message = Catalog.GetString ("The selected path wasn't added. The list contains items that supersedes it and the data is already being indexed.");
				} else if (old_include.StartsWith (new_include + System.IO.Path.PathSeparator)) {
					obsolete_includes.Add (old_include);
				}
			}

			if (throw_error) {
				HigMessageDialog.RunHigMessageDialog (settings_dialog,
								      DialogFlags.Modal,
								      MessageType.Warning,
								      ButtonsType.Ok,
								      Catalog.GetString ("Path not added"),
								      error_message);
			} else {
				// Confirm the removal of obsolete includes
				if (obsolete_includes.Count != 0) {
					HigMessageDialog dialog = new HigMessageDialog (settings_dialog,
											DialogFlags.Modal,
											MessageType.Question,
											ButtonsType.YesNo,
											Catalog.GetString ("Remove obsolete paths"),
											Catalog.GetString ("Adding this path will obsolete some of the existing include paths. " + 
													   "This will result in the removal of the old obsolete paths. Do you still wish to add it?"));
					
					ResponseType confirm_response = (ResponseType) dialog.Run ();
					
					if (confirm_response != ResponseType.Yes)
						return;

					foreach (string obsolete_include in obsolete_includes)
						include_view.RemovePath (obsolete_include);

					dialog.Destroy ();
				}

				include_view.AddPath (new_include);
			}
		}
	}

	private void OnRemoveIncludeClicked (object o, EventArgs args)
	{
		// Confirm removal
		HigMessageDialog dialog  = new HigMessageDialog (settings_dialog,
								 DialogFlags.Modal,
								 MessageType.Question,
								 ButtonsType.YesNo,
								 Catalog.GetString ("Remove path"),
								 Catalog.GetString ("Are you sure you wish to remove this path from the list of directories to be included for indexing?"));
		ResponseType response = (ResponseType) dialog.Run ();
		dialog.Destroy ();

		if (response != ResponseType.Yes)
			return;

		include_view.RemoveSelectedPath ();
		remove_include_button.Sensitive = false;
	}

	private void OnIncludeSelected (object o, EventArgs args)
	{
		remove_include_button.Sensitive = true;
	}

	private void OnAddExcludeClicked (object o, EventArgs args)
	{
		AddExcludeDialog dialog = new AddExcludeDialog (settings_dialog);
		dialog.ExcludeItemAddedEvent += new ExcludeItemAddedHandler (OnExcludeItemAdded);

	}

	private void OnRemoveExcludeClicked (object o, EventArgs args)
	{
		HigMessageDialog dialog = new HigMessageDialog (settings_dialog,
								DialogFlags.Modal,
								MessageType.Question,
								ButtonsType.YesNo,
								Catalog.GetString ("Remove item"),
								Catalog.GetString ("Are you sure you wish to remove this item from the list of data to be excluded from indexing?"));

		ResponseType response = (ResponseType) dialog.Run ();
		dialog.Destroy ();

		if (response != ResponseType.Yes)
			return;

		exclude_view.RemoveSelectedItem ();
		remove_exclude_button.Sensitive = false;
	}

	private void OnExcludeSelected (object o, EventArgs args)
	{
		remove_exclude_button.Sensitive = true;
	}

	private void OnExcludeItemAdded (ExcludeItem exclude_item)
	{
		exclude_view.AddItem (exclude_item);
	}

	private void OnAddHostClicked (object o, EventArgs args) 	 
	{
                string error_message = null;
                bool throw_error = false;

                AddHostDialog dialog = new AddHostDialog (settings_dialog);
                ResponseType resp = (ResponseType) dialog.Run ();
                
                if (resp != ResponseType.Ok) {
                        dialog.Destroy ();
                        return;
                }

		ICollection<NetworkService> new_nodes = dialog.GetSelectedHosts ();
		
		foreach (NetworkService s in new_nodes)
			Console.WriteLine (s.Name);

                dialog.Destroy ();
                
                // Check if the new entry matches an existing netbeagle entry
                foreach (NetworkService old_node in networking_view.Nodes) {
                        foreach (NetworkService node in new_nodes) {
                                if (node == old_node) {
                                        throw_error = true;
                                        error_message = Catalog.GetString ("Remote host already present in the list.");
                                } 
                        }
                }
                
                if (throw_error) {
                        HigMessageDialog.RunHigMessageDialog (settings_dialog,
                                                              DialogFlags.Modal,
                                                              MessageType.Warning,
                                                              ButtonsType.Ok,
                                                              Catalog.GetString ("Remote beagle host not added"),
                                                              error_message);
                } else {
                        foreach (NetworkService node in new_nodes)
                                networking_view.AddNode (node);
                }
	}
	  	 
	private void OnRemoveHostClicked (object o, EventArgs args) 	 
	{
		// Confirm removal 	 
		HigMessageDialog dialog  = new HigMessageDialog (settings_dialog, 	 
								 DialogFlags.Modal, 	 
								 MessageType.Question, 	 
								 ButtonsType.YesNo, 	 
								 Catalog.GetString ("Remove host"), 	 
								 Catalog.GetString ("Are you sure you wish to remove this host from the list?")); 	 
	  	 
		ResponseType response = (ResponseType) dialog.Run ();
		dialog.Destroy ();
	  	 
		if (response != ResponseType.Yes) 	 
			return; 	 
		
		networking_view.RemoveSelectedNode (); 	 
		remove_host_button.Sensitive = false;
	} 	 
	  	 
	private void OnHostSelected (object o, EventArgs args) 	 
	{
		remove_host_button.Sensitive = true;
	}
	
	private void OnGlobalAccessToggled (object o, EventArgs args)
	{
		networking_settings_box.Sensitive = allow_global_access_toggle.Active;
	}
	
	private void OnRequirePasswordToggled (object o, EventArgs args)
	{
		//FIXME Passwords for remote beagled is not yet implemented
		//networking_password_box.Sensitive = require_password_toggle.Active;
	}

	private void OnKeybindingClicked (object o, EventArgs args)
	{
		try {
			string new_binding = GetBindingFromKeygrabber ();
			if (! String.IsNullOrEmpty (new_binding))
				binding = new_binding;
		} catch (Exception e) {
			Console.WriteLine ("Could not run python keygrabber: {0}", e.Message);
			Console.WriteLine ("Showing old keybinding widget");
			binding = GetBindingFromUserInput ();
		}

		shortcut_label.Text = String.Format (Catalog.GetString ("Display the search window by pressing {0}"), binding);
	}

	private static string GetBindingFromKeygrabber ()
	{
		SafeProcess pc = new SafeProcess ();

		string keygrabber_file = Path.Combine (ExternalStringsHack.PkgLibDir, "keygrabber.py");
		if (! File.Exists (keygrabber_file))
			throw new Exception ("keygrabber.py not found");

		pc.Arguments = new string[] {"python", keygrabber_file, ExternalStringsHack.LocaleDir};
		pc.RedirectStandardError = false;
		pc.RedirectStandardOutput = true;

		pc.Start ();
		string output = null;
		using (StreamReader pout = new StreamReader (pc.StandardOutput))
			output = pout.ReadLine ();
		pc.Close ();

		// We can't check the return value of the program.
		// Instead we this to figure out if the program worked correctly.
		if (output == null)
			throw new ApplicationException ();

		Console.WriteLine ("New binding from keygrabber '{0}'", output);
		return output;
	}

	private string GetBindingFromUserInput ()
	{
		return UserShortcutDialog.GetUserShortcut (settings_dialog, binding);
	}

	////////////////////////////////////////////////////////////////
	// IncludeView 

	class IncludeView : TreeView 
	{
		private ListStore store;

		private ArrayList includes = new ArrayList ();

		public ArrayList Includes {
			get { return includes; }
		}

		private enum TargetType {
			Uri,
		};

		private static TargetEntry [] target_table = new TargetEntry [] {
			new TargetEntry ("STRING", 0, (uint) TargetType.Uri ),
			new TargetEntry ("text/plain", 0, (uint) TargetType.Uri),
		};

		public IncludeView ()
		{
			store = new ListStore (typeof (string));

			this.Model = store;

			AppendColumn (Catalog.GetString ("Name"), new CellRendererText (), "text", 0);

			// Enable drag and drop folders from nautilus
			Gtk.Drag.DestSet (this, DestDefaults.All, target_table, DragAction.Copy | DragAction.Move);
			DragDataReceived += new DragDataReceivedHandler (HandleData);
		}

		public void AddPath (string path)
		{
			includes.Add (path);
			store.AppendValues (path);
		} 

		public void RemovePath (string path)
		{
			find_path = path;
			found_iter = TreeIter.Zero;

			this.Model.Foreach (new TreeModelForeachFunc (ForeachFindPath));

			store.Remove (ref found_iter);
			includes.Remove (path);
		}

		private string find_path;
		private TreeIter found_iter;

		private bool ForeachFindPath (TreeModel model, TreePath path, TreeIter iter)
		{
			if ((string) model.GetValue (iter, 0) == find_path) {
				found_iter = iter;
				return true;
			}

			return false;
		}

		public void RemoveSelectedPath ()
		{
			TreeModel model;
			TreeIter iter;

			if (!this.Selection.GetSelected(out model, out iter)) {
				return;
			}
			string path = (string)model.GetValue(iter, 0);

			store.Remove (ref iter);
			includes.Remove (path);
		}

	        // Handle drag and drop data. Enables users to drag a folder that he wishes 
		// to add for indexing from Nautilus.
		// FIXME: Pass checks as in OnAddIncludeButtonClicked
		private void HandleData (object o, DragDataReceivedArgs args) {
			Uri uri;
			if (args.SelectionData.Length >=0 && args.SelectionData.Format == 8) {
				uri = new Uri (args.SelectionData.Text.Trim ());
				AddPath (uri.LocalPath);
				Gtk.Drag.Finish (args.Context, true, false, args.Time);
			}
			Gtk.Drag.Finish (args.Context, false, false, args.Time);
		}
	}

	////////////////////////////////////////////////////////////////
	// Exclude view

	class ExcludeView : TreeView 
	{
		ArrayList excludes = new ArrayList ();

		public ArrayList Excludes {
			get { return excludes; }
		}
			
		public ExcludeView ()
		{
			this.Model =  new ListStore (typeof (ExcludeItem));

			CellRendererText renderer_text = new CellRendererText ();

			TreeViewColumn type_column = new TreeViewColumn ();
			type_column.Title = Catalog.GetString ("Type");
			type_column.PackStart (renderer_text, false);
			type_column.SetCellDataFunc (renderer_text, new TreeCellDataFunc (TypeCellDataFunc));
			AppendColumn (type_column);

			TreeViewColumn name_column = new TreeViewColumn ();
			name_column.Title = Catalog.GetString ("Name");
			name_column.PackStart (renderer_text, false);
			name_column.SetCellDataFunc (renderer_text, new TreeCellDataFunc (NameCellDataFunc));
			AppendColumn (name_column);
		}

		public void RemoveSelectedItem ()
		{
			TreeModel model;
			TreeIter iter;

			if (!this.Selection.GetSelected(out model, out iter)) {
				return;
			}
			ExcludeItem exclude_item = (ExcludeItem) model.GetValue(iter, 0);

		        ((ListStore)this.Model).Remove (ref iter);
			excludes.Remove (exclude_item);			
		}

		public void AddItem (ExcludeItem exclude_item)
		{
			excludes.Add (exclude_item);
			((ListStore)this.Model).AppendValues (exclude_item);
		}

		private void NameCellDataFunc (TreeViewColumn column,
					       CellRenderer renderer,
					       TreeModel model,
					       TreeIter iter)
		{
			ExcludeItem exclude_item = (ExcludeItem) model.GetValue (iter, 0);
			if (exclude_item.Type == ExcludeType.MailFolder)
				((CellRendererText)renderer).Text = MailFolder.GetNameForPath (exclude_item.Value);
			else
				((CellRendererText)renderer).Text = exclude_item.Value;
		}

		private void TypeCellDataFunc (TreeViewColumn column,
						CellRenderer renderer,
						TreeModel model,
						TreeIter iter)
		{			
			ExcludeItem exclude_item = (ExcludeItem) model.GetValue (iter, 0);

			switch (exclude_item.Type) {
			case ExcludeType.Path:
				((CellRendererText)renderer).Text = Catalog.GetString ("Path:");
				break;
			case ExcludeType.Pattern:
				((CellRendererText)renderer).Text = Catalog.GetString ("Pattern:");
				break;
			case ExcludeType.MailFolder:
				((CellRendererText)renderer).Text = Catalog.GetString ("Mail folder:");
				break;
			}
		}
	}

	////////////////////////////////////////////////////////////////
	// NetworkingView 

	class NetworkingView : TreeView 	 
	{ 	 
		private ListStore store; 	 
		private ArrayList nodes = new ArrayList (); 	 
	  	
		public ArrayList Nodes { 	 
			get { return nodes; } 	 
		}

		public bool HasSelection {
			get { return (Selection.CountSelectedRows () > 0); }
		}
	  	 
		public NetworkingView ()
		{ 	 
			store = new ListStore (typeof (NetworkService));
			this.Model = store;

			TreeViewColumn column = new TreeViewColumn ();
			column.Title = Catalog.GetString ("Name");
			CellRendererText renderer = new CellRendererText ();
			column.PackStart (renderer, true);
			column.SetCellDataFunc (renderer, new TreeCellDataFunc (NameCellFunc));
			AppendColumn (column);

                        column = new TreeViewColumn ();
                        column.Title = Catalog.GetString ("Address");
                        renderer = new CellRendererText ();
                        column.PackStart (renderer, true);
                        column.SetCellDataFunc (renderer, new TreeCellDataFunc (AddressCellFunc));
                        AppendColumn (column);
		
		}

                public void NameCellFunc (TreeViewColumn col, CellRenderer cell, TreeModel model, TreeIter iter) 
                {
                        CellRendererText renderer = (CellRendererText) cell;
                        NetworkService s = (NetworkService) model.GetValue (iter, 0);
                        renderer.Markup = s.Name;
                }
                
                public void AddressCellFunc (TreeViewColumn col, CellRenderer cell, TreeModel model, TreeIter iter)
                {
                        CellRendererText renderer = (CellRendererText) cell;
                        NetworkService s = (NetworkService) model.GetValue (iter, 0);
                        renderer.Markup = String.Format ("{0}:{1}", s.GetUri ().Host, s.GetUri ().Port);
                }
	  	 
		public void AddNode (NetworkService service) 	 
		{ 	 
			nodes.Add (service); 	 
			store.AppendValues (service); 	 
		} 	 
		

		private NetworkService find_node; 	 
		private TreeIter found_iter; 	 

		public void RemoveNode (NetworkService service) 	 
		{ 	 
			find_node = service; 	 
			found_iter = TreeIter.Zero; 	 
			
			this.Model.Foreach (new TreeModelForeachFunc (ForeachFindNode)); 	 
			
			store.Remove (ref found_iter); 	 
			nodes.Remove (service); 	 
		} 	 
	  	
		private bool ForeachFindNode (TreeModel model, TreePath path, TreeIter iter) 	 
		{ 	 
			if ((NetworkService) model.GetValue (iter, 0) == find_node) { 	 
				found_iter = iter; 	 
				return true; 	 
			} 	 
			
			return false; 	 
		} 	 
	  	
		public void RemoveSelectedNode () 	 
		{ 	 
			TreeModel model; 	 
			TreeIter iter; 	 
			
			if (!this.Selection.GetSelected(out model, out iter)) { 	 
				return; 	 
			}

			NetworkService node = (NetworkService)model.GetValue(iter, 0); 	 
			
			store.Remove (ref iter); 	 
			nodes.Remove (node); 	 
		} 	 
	  	
	}

	////////////////////////////////////////////////////////////////
	// PublicfolderView 

	class PublicfolderView : TreeView 
	{
		private ListStore store;

		private ArrayList publicFolders = new ArrayList ();

		public ArrayList Publicfolders {
			get { return publicFolders; }
		}

		private enum TargetType {
			Uri,
		};

		private static TargetEntry [] target_table = new TargetEntry [] {
			new TargetEntry ("STRING", 0, (uint) TargetType.Uri ),
			new TargetEntry ("text/plain", 0, (uint) TargetType.Uri),
		};

		public PublicfolderView ()
		{
			store = new ListStore (typeof (string));

			this.Model = store;

			AppendColumn (Catalog.GetString ("Name"), new CellRendererText (), "text", 0);

			// Enable drag and drop folders from nautilus
			Gtk.Drag.DestSet (this, DestDefaults.All, target_table, DragAction.Copy | DragAction.Move);
			DragDataReceived += new DragDataReceivedHandler (HandleData);
		}

		public void AddPath (string path)
		{
			publicFolders.Add (path);
			store.AppendValues (path);
		} 

		public void RemovePath (string path)
		{
			find_path = path;
			found_iter = TreeIter.Zero;

			this.Model.Foreach (new TreeModelForeachFunc (ForeachFindPath));

			store.Remove (ref found_iter);
			publicFolders.Remove (path);
		}

		private string find_path;
		private TreeIter found_iter;

		private bool ForeachFindPath (TreeModel model, TreePath path, TreeIter iter)
		{
			if ((string) model.GetValue (iter, 0) == find_path) {
				found_iter = iter;
				return true;
			}

			return false;
		}

		public void RemoveSelectedPath ()
		{
			TreeModel model;
			TreeIter iter;

			if (!this.Selection.GetSelected(out model, out iter)) {
				return;
			}
			string path = (string)model.GetValue(iter, 0);

			store.Remove (ref iter);
			publicFolders.Remove (path);
		}

		// Handle drag and drop data. Enables users to drag a folder that he wishes 
		// to add for indexing from Nautilus.
		// FIXME: Pass checks as in OnAddIncludeButtonClicked
		private void HandleData (object o, DragDataReceivedArgs args) {
			Uri uri;
			if (args.SelectionData.Length >=0 && args.SelectionData.Format == 8) {
				uri = new Uri (args.SelectionData.Text.Trim ());
				AddPath (uri.LocalPath);
				Gtk.Drag.Finish (args.Context, true, false, args.Time);
			}
			Gtk.Drag.Finish (args.Context, false, false, args.Time);
		}
	}

	////////////////////////////////////////////////////////////////
	// Mail folder dialog

	class MailFolderDialog 
	{
		Gtk.Window parent;
		FolderView folder_view;

		[Widget] Dialog mail_folder_dialog;
		[Widget] ScrolledWindow folder_sw;

		public event ExcludeItemAddedHandler ExcludeItemAddedEvent;

		public MailFolderDialog (Gtk.Window parent)
		{
			this.parent = parent;

			Glade.XML glade = new Glade.XML (null, "settings.glade", "mail_folder_dialog", "beagle");
			glade.Autoconnect (this);

			folder_view = new FolderView ();
			folder_view.Show ();
			folder_sw.Child = folder_view;
		}

		private void OnDialogResponse (object o, ResponseArgs args)
		{
			if (args.ResponseId == ResponseType.Cancel) {
				mail_folder_dialog.Destroy ();
				return;
			}

			ExcludeItem exclude_item;
			object obj = folder_view.GetCurrentItem ();

			if (obj is MailAccount) {

			} else if (obj is MailFolder) {
				MailFolder folder = (MailFolder) obj;
				exclude_item = new ExcludeItem (ExcludeType.MailFolder, folder.Path);

				if (ExcludeItemAddedEvent != null)
					ExcludeItemAddedEvent (exclude_item);

				mail_folder_dialog.Destroy ();
			}
		}

		class FolderView : TreeView
		{
			TreeStore store;
			Gdk.Pixbuf folder_icon;

			public FolderView ()
			{
				store = new TreeStore (typeof (MailFolder));
				this.Model = store;

				folder_icon = this.RenderIcon (Stock.Open, IconSize.Menu, "");

				HeadersVisible = false;

				TreeViewColumn column = new TreeViewColumn ();
				CellRendererPixbuf renderer_icon = new CellRendererPixbuf ();
				column.PackStart (renderer_icon, false);
				column.SetCellDataFunc (renderer_icon, new TreeCellDataFunc (IconCellDataFunc));

				CellRendererText renderer_text = new CellRendererText ();
				column.PackStart (renderer_text, false);
				column.SetCellDataFunc (renderer_text, new TreeCellDataFunc (NameCellDataFunc));
				AppendColumn (column);

				foreach (MailAccount account in Beagle.Util.Evolution.Accounts) {
					TreeIter iter = store.AppendValues (account);

					foreach (MailFolder folder in account.Children) {
						Add (iter, folder);
					}
				}
			}

			private void Add (TreeIter parent, MailFolder folder)
			{
				TreeIter current = store.AppendValues (parent, folder);
				
				foreach (MailFolder child in folder.Children)
					Add (current, child);
			}

			private void IconCellDataFunc (TreeViewColumn column,
						       CellRenderer renderer,
						       TreeModel model,
						       TreeIter iter)
			{
				object obj = model.GetValue (iter, 0);
				((CellRendererPixbuf)renderer).Pixbuf = (obj is MailAccount) ? null : folder_icon;
			}

			private void NameCellDataFunc (TreeViewColumn column,
						       CellRenderer renderer,
						       TreeModel model,
						       TreeIter iter)
			{
				object obj = model.GetValue (iter, 0);

				if (obj is MailAccount) {
					MailAccount account = obj as MailAccount;
					((CellRendererText)renderer).Markup = String.Format ("<b>{0}</b>",account.Name);
				} else {
					MailFolder folder = obj as MailFolder;
					((CellRendererText)renderer).Text = folder.Name;
				}
			}
			
			public MailFolder GetCurrentItem ()
			{
				TreeModel model;
				TreeIter iter;
				
				if (!this.Selection.GetSelected (out model, out iter)) {
					return null;
				}

				return (MailFolder) model.GetValue (iter, 0);
			}
		}
	}

	////////////////////////////////////////////////////////////////
	// Exclude dialog

	private delegate void ExcludeItemAddedHandler (ExcludeItem item);

	class AddExcludeDialog 
	{
		Gtk.Window parent;

		[Widget] Dialog add_exclude_dialog;
		[Widget] Entry value_entry;
		[Widget] Label value_name_label;
		[Widget] Button browse_button;

		[Widget] RadioButton type_path_radio;
		[Widget] RadioButton type_pattern_radio;
		[Widget] RadioButton type_mailfolder_radio;

		public event ExcludeItemAddedHandler ExcludeItemAddedEvent;

		private string value;

		public string Value {
			get { 
				if (Type == ExcludeType.MailFolder)
					return value;
				else 
					return value_entry.Text; 
			}
		}

		public ExcludeType Type {
			get {
				if (type_path_radio.Active) 
					return ExcludeType.Path;
				else if (type_pattern_radio.Active) 
					return ExcludeType.Pattern;
				else
					return ExcludeType.MailFolder;
			}
		}

		public AddExcludeDialog (Gtk.Window parent)
		{
			this.parent = parent;

			Glade.XML glade = new Glade.XML (null, "settings.glade", "add_exclude_dialog", "beagle");
			glade.Autoconnect (this);
		}

		private void OnBrowseButtonClicked (object o, EventArgs args)
		{
			switch (Type) {
			case ExcludeType.Path:
				CompatFileChooserDialog fs_dialog = new CompatFileChooserDialog (Catalog.GetString ("Select Folder"), 
												 add_exclude_dialog, 
												 CompatFileChooserDialog.Action.SelectFolder);
				fs_dialog.SelectMultiple = false;
				
				ResponseType response = (ResponseType) fs_dialog.Run ();

				if (response == ResponseType.Ok)
					value_entry.Text = fs_dialog.Filename;

				fs_dialog.Destroy ();
				break;
			case ExcludeType.MailFolder:
				MailFolderDialog mf_dialog = new MailFolderDialog (add_exclude_dialog);
				mf_dialog.ExcludeItemAddedEvent += new ExcludeItemAddedHandler (OnExcludeItemAdded);
				break;
			}
		}

		private void OnRadioGroupChanged (object o, EventArgs args)
		{
			value_entry.Text = "";

			switch (Type) {
			case ExcludeType.Path:
				browse_button.Sensitive = true;
				value_name_label.TextWithMnemonic = Catalog.GetString ("P_ath:");
				value_entry.IsEditable = true;
				break;
			case ExcludeType.MailFolder:
				browse_button.Sensitive = true;
				value_name_label.TextWithMnemonic = Catalog.GetString ("M_ail folder:");
				value_entry.IsEditable = false;
				break;
			case ExcludeType.Pattern:
				browse_button.Sensitive = false;
				value_name_label.TextWithMnemonic = Catalog.GetString ("P_attern:");
				value_entry.IsEditable = true;
				break;
			}
		}
		
		private void OnExcludeItemAdded (ExcludeItem item)
		{
			value = item.Value; 
			value_entry.Text = MailFolder.GetNameForPath (item.Value);
		}

		private void OnDialogResponse (object o, ResponseArgs args)
		{
			if (((ResponseType)args.ResponseId) == ResponseType.Ok) {
				ExcludeItem exclude_item = new ExcludeItem (Type, Value);
				
				switch (Type) {
				case ExcludeType.Path:
					if (!Directory.Exists (Value)) {
						HigMessageDialog.RunHigMessageDialog(add_exclude_dialog,
										     DialogFlags.Modal,
										     MessageType.Error,
										     ButtonsType.Ok,
										     Catalog.GetString ("Error adding path"),
										     Catalog.GetString ("The specified path could not be found and therefore it could not be added to the list of resources excluded for indexing."));
						return;
					}
					break;
				}
				
				if (ExcludeItemAddedEvent != null)
					ExcludeItemAddedEvent (exclude_item);
			}
			add_exclude_dialog.Destroy ();
		}
		
		public void Destroy ()
		{
			add_exclude_dialog.Destroy ();
		}
	}

        ////////////////////////////////////////////////////////////////
        // PasswordDialog

        class PasswordDialog : Dialog
        {
                [Widget] private Dialog password_dialog;
                [Widget] private Label title_label;
                [Widget] private Entry password_entry;
		
                public string Password {
                        get { return password_entry.Text; }
                }
		
                public PasswordDialog (string name) : base (IntPtr.Zero)
                {
                        Glade.XML gxml = new Glade.XML (null, "settings.glade", "password_dialog", "beagle");
                        gxml.Autoconnect (this);

                        Raw = password_dialog.Handle;

                        Title = String.Format (Catalog.GetString ("Connecting to {0}"), name);
                        Present ();
                }
        }

	////////////////////////////////////////////////////////////////
	// ShortcutInput Dialog
	// Simple dialog that is shown in case the python keygrabber script
	// cannot be run.
	// This deliberately does not try to cover all cases. We assume
	// everyone has python installed.

	public class UserShortcutDialog : Dialog
	{
		CheckButton ctrl_button, alt_button;
		Entry entry;
		string binding;

		public UserShortcutDialog (Gtk.Window parent, string binding_string) : base (null, parent, DialogFlags.DestroyWithParent)
		{
			Title = "KeyGrabber";
			Modal = true;
			HasSeparator = false;

			AddButton ("Ok", ResponseType.Ok);
			AddButton ("Cancel", ResponseType.Cancel);

			binding = binding_string;

			ctrl_button = new CheckButton ("Ctrl");
			int i = binding_string.IndexOf ("<ctrl>", StringComparison.InvariantCultureIgnoreCase);
			if (i != -1) {
				ctrl_button.Active = true;
				binding_string = binding_string.Remove (i, 6);
			}

			alt_button = new CheckButton ("Alt");
			i = binding_string.IndexOf ("<alt>", StringComparison.InvariantCultureIgnoreCase);
			if (i != -1) {
				alt_button.Active = true;
				binding_string = binding_string.Remove (i, 5);
			}

			entry = new Entry ();
			entry.Text = binding_string;

			HBox box = new HBox (false, 0);
			box.PackEnd (ctrl_button, true, false, 0);
			ctrl_button.Show ();
			box.PackEnd (alt_button, true, false, 0);
			alt_button.Show ();
			box.PackEnd (entry, true, false, 0);
			entry.Show ();

			VBox.PackStart (box, true, false, 0);
			box.Show ();
		}

		public static string GetUserShortcut (Gtk.Window parent, string binding)
		{
			UserShortcutDialog dialog = new UserShortcutDialog (parent, binding);
			dialog.Response += new ResponseHandler (OnResponse);
			dialog.Run ();
			dialog.Destroy ();

			Console.WriteLine ("new binding = '{0}'", dialog.binding);
			return dialog.binding;
		}

		private static void OnResponse (object obj, ResponseArgs args)
		{
			UserShortcutDialog dialog = (UserShortcutDialog) obj;
			if (args.ResponseId != ResponseType.Ok)
				return;

			bool ctrl = dialog.ctrl_button.Active;
			bool alt = dialog.alt_button.Active;
			string key = dialog.entry.Text;
			dialog.binding = (new KeyBinding (key, ctrl, alt)).ToString ();
		}
	}

        ////////////////////////////////////////////////////////////////
        // AddHostDialog
        
        public class AddHostDialog : Dialog
        {
                const int COL_NAME = 0;
                const int COL_PIXBUF = 1;
                const int COL_AUTH = 2;
                const int COL_HOST = 3;
                const int COL_PORT = 4;
                
                [Glade.Widget] private Dialog add_host_dialog;
                [Glade.Widget] private RadioButton mdns_radio_button;
                [Glade.Widget] private RadioButton static_radio_button;
                [Glade.Widget] private Alignment static_section;
                [Glade.Widget] private Entry name_entry;
                [Glade.Widget] private Entry address_entry;
                [Glade.Widget] private Entry password_entry;
                [Glade.Widget] private SpinButton port_spin_button;
                [Glade.Widget] private IconView icon_view;
                
#if ENABLE_AVAHI
                private AvahiBrowser browser;
#endif  

                private ListStore store;
                private Gdk.Pixbuf unlocked_icon;
                private Gdk.Pixbuf locked_icon;
                
                public ICollection<NetworkService> GetSelectedHosts ()
		{
			List<NetworkService> services = new List<NetworkService> ();
                        
			if (this.mdns_radio_button.Active == false) {
				Uri uri = new Uri (String.Format ("http://{0}:{1}", address_entry.Text, port_spin_button.ValueAsInt));
				bool pw_required = (password_entry.Text.Length > 0) ? true : false;
				string name = null;
				
				if (name_entry.Text.Length > 0)
					name = name_entry.Text;
				else
					name = "Unnamed";

				services.Add (new NetworkService (name, uri, pw_required, "X"));
			} else {
#if ENABLE_AVAHI
				TreeIter iter;

				foreach (TreePath path in icon_view.SelectedItems) {
					if (store.GetIter (out iter, path) == false)
						continue;
					
					// FIXME: Searching by name is not the most correct thing to do
					string name = (string) store.GetValue (iter, COL_NAME);
					Console.WriteLine ("+ " + name);

					NetworkService s = (NetworkService) browser.GetServiceByName (name);
					
					if (s != null)
						services.Add (s);
				}
#endif
			}
			
			return services;
		}
                
                public AddHostDialog (Gtk.Window parent) : base (null, parent, DialogFlags.DestroyWithParent)
                {
                        Glade.XML gxml = new Glade.XML (null, "settings.glade", "add_host_dialog", null);
                        gxml.Autoconnect (this);
                        Raw = add_host_dialog.Handle;
                        
                        this.TransientFor = parent;

                        mdns_radio_button.Toggled += new EventHandler (OnRadioButtonToggled);
                        
                        // load the image to use for each node
                        unlocked_icon = Gtk.IconTheme.Default.LoadIcon ("gnome-fs-network", 
                                                                        48, (IconLookupFlags) 0);
                        locked_icon = Gtk.IconTheme.Default.LoadIcon ("gtk-dialog-authentication", 
                                                                      48, (IconLookupFlags) 0);
                        
                        CreateStore ();
                        icon_view.Model = store;
                        icon_view.TextColumn = COL_NAME;
                        icon_view.PixbufColumn = COL_PIXBUF;
                        icon_view.ColumnSpacing = 24;
                        icon_view.ItemActivated += new ItemActivatedHandler (OnItemActivated);
                        icon_view.GrabFocus ();

                        this.ShowAll ();

#if ENABLE_AVAHI
                        try {
                                browser = new AvahiBrowser ();
                                browser.HostFound += new AvahiEventHandler (OnHostFound);
                                browser.HostRemoved += new AvahiEventHandler (OnHostRemoved);
                                browser.Start ();
                        } catch (Exception e) {
                                //Console.Error.WriteLine ("Avahi Daemon must be unavailable. Hiding MDns stuff.");
                                static_radio_button.Toggle ();                                
                                icon_view.Visible = false;
                                mdns_radio_button.Visible = false;
                                static_radio_button.Visible = false;
                        }
#else
                                icon_view.Visible = false;
                                mdns_radio_button.Visible = false;
#endif  

			// FIXME Password for remote host is not yet implemented
			password_entry.Sensitive = false;
                }
                
                private void CreateStore ()
                {
                        store = new ListStore (typeof (string),     // Name
                                               typeof (Gdk.Pixbuf), // Icon
                                               typeof (bool),       // Requires authenticatio
                                               typeof (string),     // hostname
                                               typeof (int));       // port
                        
                        store.DefaultSortFunc = delegate (TreeModel model, TreeIter a, TreeIter b) {
                                string a_name = (string) model.GetValue (a, COL_NAME);
                                string b_name = (string) model.GetValue (b, COL_NAME);
                                
                                return String.Compare (a_name, b_name);
                        };
                        
                        store.SetSortColumnId (COL_NAME, SortType.Ascending);
                }

#if ENABLE_AVAHI
                private void OnHostFound (object sender, AvahiEventArgs args)
                {
                        store.AppendValues (args.Name, 
                                            (args.Service.IsProtected == true) ? locked_icon : unlocked_icon,
                                            args.Service.IsProtected,
                                            args.Address.Host,
                                            args.Address.Port);

                        icon_view.QueueDraw ();
                }

                private void OnHostRemoved (object sender, AvahiEventArgs args)
                {
                        find_node = args.Address.Host;
                       found_iter = TreeIter.Zero;
                        
                       store.Foreach (new TreeModelForeachFunc (ForeachFindNode));
                       store.Remove (ref found_iter);
                }
#endif

               private string find_node;
               private TreeIter found_iter;

               private bool ForeachFindNode (TreeModel model, TreePath path, TreeIter iter)
               {
                       if ((string) model.GetValue (iter, COL_HOST) == find_node) {
                               found_iter = iter;
                               return true;
                       }

                       return false;
               }
                
                private void OnItemActivated (object sender, ItemActivatedArgs args)
                {
                        TreeIter iter;
                        store.GetIter (out iter, args.Path);

                        try {
                                string host = (string) store.GetValue (iter, COL_HOST);
                                int port = (int) store.GetValue (iter, COL_PORT);

                                Console.WriteLine ("Host activated: {0}", 
                                                   String.Format ("{0}:{1}", host.Trim (), port));
                                
                                // The user has double clicked on the icon, so they probably expect
                                // the same behavior as selecting this icon and clicking "Add".
                                Respond (ResponseType.Ok);
                        } catch (Exception e) {
                                Console.Error.WriteLine ("Exception: {0}", e.Message);
                        }
                }
                
                private void OnRadioButtonToggled (object sender, EventArgs args)
                {
                        if (mdns_radio_button.Active) {
                                icon_view.Sensitive = true;
                                static_section.Sensitive = false;
                        } else {
                                icon_view.Sensitive = false;
                                static_section.Sensitive = true;
                        }
                }

                public override void Destroy ()
                {
#if ENABLE_AVAHI
                        if (browser != null) {
                                browser.Dispose ();
                                browser.HostFound -= OnHostFound;
                                browser.HostRemoved -= OnHostRemoved;
                                browser = null;
                        }
#endif 
                        base.Destroy ();
                }
   
	}

	////////////////////////////////////////////////////////////////
	// BackendView

	class BackendView : TreeView 
	{
		private ListStore store = null;

		private string[] backends = new string[] {
			"EvolutionMail",
			"EvolutionDataServer",
			"Thunderbird",
			"Akregator",
			"Blam",
			"Files",
			//"IndexingService", // Leave this enabled, it is vital for the Firefox plugin
			"KMail",
			"KNotes",
			"KOrganizer",
			"KAddressBook",
			"KonqBookmark",
			"KonquerorHistory",
			"Konversation",
			"Kopete",
			"Labyrinth",
			"Liferea",
			"NautilusMetadata",
			"NetworkServices", // This should be configurable in the network tab
			"Opera",
			"Pidgin",
			"Tomboy",
			/* System-wide indexes */
			"applications",
			"documentation",
			"manpages",
			"Locate",
			"monodoc",
			"windows"
		};

		private string[] descriptions = new string[] {
			Catalog.GetString ("Evolution's mail."),
			Catalog.GetString ("Evolution's address book, memos and tasks."),
			Catalog.GetString ("Thunderbird's email."),
			Catalog.GetString ("RSS feeds from Akregator."),
			Catalog.GetString ("RSS feeds from Blam."),
			Catalog.GetString ("Files and folders on the local file system."),
			Catalog.GetString ("Mail messages from KMail."),
			Catalog.GetString ("Notes from KNotes."),
			Catalog.GetString ("Agenda from KOrganizer."),
			Catalog.GetString ("Contacts from KAddressBook."),
			Catalog.GetString ("Konqueror's bookmarks."),
			Catalog.GetString ("Konqueror's history."),
			Catalog.GetString ("IMs and chats from Konversation."),
			Catalog.GetString ("IMs and chats from Kopete"),
			Catalog.GetString ("Mind-maps from Labyrinth."),
			Catalog.GetString ("RSS feeds from Liferea."),
			Catalog.GetString ("Nautilus' metadata (emblems, notes, etc.)"),
			Catalog.GetString ("Search other search services in the network (EXPERIMENTAL)"),
			Catalog.GetString ("Opera's bookmarks and browsing history."),
			Catalog.GetString ("IMs and chats from Pidgin."),
			Catalog.GetString ("Notes from Tomboy."),
			/* System-wide indexes */
			Catalog.GetString ("(System) Applications"),
			Catalog.GetString ("(System) Help files"),
			Catalog.GetString ("(System) Manual pages"),
			Catalog.GetString ("(System) Supplementary search results using 'locate' command"),
			Catalog.GetString ("(System) Mono documentation"),
			Catalog.GetString ("(System) Files from Windows' partition")
		};

		public BackendView ()
		{
			this.store = new ListStore (typeof (bool), typeof (string), typeof (string));

			CellRendererToggle toggle = new CellRendererToggle ();
			toggle.Activatable = true;
			toggle.Toggled += OnToggled;

			base.Model = store;
			base.AppendColumn (null, toggle, "active", 0);
			base.AppendColumn (Catalog.GetString ("Name"), new CellRendererText (), "text", 1);
			base.AppendColumn (Catalog.GetString ("Description"), new CellRendererText (), "markup", 2);

			for (int i = 0; i < backends.Length; i++)
				store.AppendValues (true, backends [i], "<i>" + descriptions [i] + "</i>");
		}

		public void Set (string backend, bool enabled)
		{
			TreeIter iter;

			if (! store.GetIterFirst (out iter))
				return;
			
			do {
				string name = (string) store.GetValue (iter, 1);

				if (name == backend)
					store.SetValue (iter, 0, enabled);
			} while (store.IterNext (ref iter));
		}

		private void OnToggled (object o, ToggledArgs args)
		{
			TreeIter iter;

			if (! store.GetIter (out iter, new TreePath (args.Path)))
				return;

			store.SetValue (iter, 0, ! (bool) store.GetValue (iter, 0));
		}

		private List<string> GetDenied ()
		{
			TreeIter iter;
			List<string> denied = new List<string> ();

			if (! store.GetIterFirst (out iter))
				return null;
			
			do {
				bool enabled = (bool) store.GetValue (iter, 0);
				
				if (! enabled)
					denied.Add ((string) store.GetValue (iter, 1));
			} while (store.IterNext (ref iter));

			return denied;
		}

		public List<string> Denied {
			get { return GetDenied (); }
		}
	}
}
