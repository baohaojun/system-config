//
// Search.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Collections;
using System.Diagnostics;

using NDesk.DBus;
using Mono.Unix;

using Beagle.Util;
using Beagle.Search.Tray;

namespace Beagle.Search {

	public class Search : ISearch {

		// The reference count is only valid when
		// we don't run in icon mode.

		private int ref_count = 0;

		private bool icon_enabled = false;
		private bool docs_enabled = false;

		private SearchWindow icon_window = null;
		private TrayIcon tray = null;
		private XKeybinder keybinder = null;

		public Search (bool icon_enabled, bool docs_enabled)
		{
			this.icon_enabled = icon_enabled;
			this.docs_enabled = docs_enabled;

			if (icon_enabled) {
				icon_window = new SearchWindow (this);
				icon_window.QueryEvent += OnQueryEvent;

				tray = new TrayIcon ();
				tray.Clicked += OnTrayActivated;
				tray.Search += OnTraySearch;
				tray.Quit += OnTrayQuit;

				Config config = Conf.Get (Conf.Names.BeagleSearchConfig);
				keybinder = new XKeybinder ();
				SetKeyBindings (config);

				Inotify.Start ();
				Conf.WatchForUpdates();
				Conf.Subscribe (Conf.Names.BeagleSearchConfig, OnConfigurationChanged);
			}
		}

		public void Query (string query_text)
		{
			if (icon_enabled) {
				if (!String.IsNullOrEmpty (query_text))
					icon_window.Search (query_text);

				icon_window.ShowAll ();
			} else {
				SearchWindow window = new SearchWindow (this);
				window.DeleteEvent += OnWindowDeleteEvent;
				
				if (!String.IsNullOrEmpty (query_text))
					window.Search (query_text);

				window.ShowAll ();
				ref_count++;
			}
		}

		private void SetKeyBindings (Config config)
		{
			string tip_text = Catalog.GetString ("Desktop Search");
			string binding = config.GetOption ("KeyBinding", null);
			Console.WriteLine ("new binding {0}", binding);

			if (String.IsNullOrEmpty (binding)) {
				// Move old preference value to new
				bool binding_ctrl = config.GetOption (Conf.Names.KeyBinding_Ctrl, false);
				bool binding_alt = config.GetOption (Conf.Names.KeyBinding_Alt, false);
				string binding_key = config.GetOption (Conf.Names.KeyBinding_Key, "F12");
				KeyBinding show_binding = new KeyBinding (binding_key, binding_ctrl, binding_alt);

				binding = show_binding.ToString ();
			}

			if (!String.IsNullOrEmpty (binding)) {
				tip_text += String.Format (" ({0})", binding);
				keybinder.UnbindAll ();
				keybinder.Bind (binding, OnTrayActivated);
			}

			tray.TooltipText = tip_text;
		}

		private void OnWindowDeleteEvent (object o, Gtk.DeleteEventArgs args)
		{
			Quit ();
		}

		private void OnTrayQuit (object o, EventArgs args)
		{
			Quit ();
		}

		private void Quit ()
		{
			Console.WriteLine (ref_count);
			if (--ref_count >= 1)
				return;

			Inotify.Stop ();
			Gtk.Application.Quit ();
		}

		private void OnTrayActivated (object o, EventArgs args)
		{
			if (! icon_window.Visible) {
				icon_window.ShowAll ();
				icon_window.Present ();
				icon_window.GrabEntryFocus ();
			} else {
				icon_window.Hide ();
			}
		}

		private void OnTraySearch (string query)
		{
			if (!icon_window.Visible)
				icon_window.ShowAll ();

			icon_window.Search (query);
		}

		private void OnQueryEvent (string query)
		{
			// Update the list of searches in the tray
			// icon menu.

			tray.AddSearch (query);
		}

		private void OnConfigurationChanged (Config config)
		{
			SetKeyBindings (config);
		}

		public bool IconEnabled {
			get { return icon_enabled; }
		}

		public bool DocsEnabled {
			get { return docs_enabled; }
		}
	}
}
