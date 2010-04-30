//
//  TrayIcon.cs
//
//  Copyright (C) 2005 Novell, Inc.
//

using System;
using System.Collections;
using System.Runtime.InteropServices;
using Mono.Unix;

using Gtk;
using Gdk;

namespace Beagle.Search.Tray
{
	public delegate void SearchDelegate (string query);

	public class TrayIcon
	{
		private EventBox eventbox;
		private Tooltips tips;
		private NotificationArea notification_area;
		private Menu popup;
		private ArrayList recent_searches;
		private Hashtable menu_to_query_map;
				
		public event EventHandler Clicked;
		public SearchDelegate Search;
		public event EventHandler Quit;

		public TrayIcon ()
		{
			notification_area = new NotificationArea (Catalog.GetString ("Desktop Search"));

			eventbox = new EventBox ();
			eventbox.ButtonPressEvent += OnClick;

			Gdk.Pixbuf pixbuf = WidgetFu.LoadThemeIcon ("system-search", 24);
			eventbox.Add (new Gtk.Image (pixbuf));
			
			notification_area.Add (eventbox);
			notification_area.ShowAll ();

			recent_searches = new ArrayList ();

			popup = MakeMenu (eventbox);
		}

		public string TooltipText {
			set {
				if (tips == null) {
					tips = new Gtk.Tooltips ();
					tips.Enable ();
				}

				tips.SetTip (eventbox, value, null);
			}
		}

		private Gtk.Menu MakeMenu (Gtk.Widget parent) 
		{
			Gtk.Menu menu = new Gtk.Menu ();
			menu.AttachToWidget (parent, new Gtk.MenuDetachFunc (DetachWidget));
			
			Gtk.ImageMenuItem item;
						
			// Quick Search menu items
			if (recent_searches.Count == 0) {
				item = new Gtk.ImageMenuItem (Catalog.GetString ("No Recent Searches"));
				item.Sensitive = false;
				menu.Append (item);
				menu_to_query_map = null;
			} else {
				item = new Gtk.ImageMenuItem (Catalog.GetString ("Recent Searches"));
				item.Sensitive = false;
				item.Image = new Gtk.Image (Stock.Find, IconSize.Menu);
				menu.Append (item);

				menu_to_query_map = new Hashtable ();

				foreach (string s in recent_searches) {
					// Replace all occurences of '_' with "__"
					// so that underscores don't become mnemonics.
					string query = s.Replace ("_", "__");

					item = new Gtk.ImageMenuItem (query);
					item.Activated += new EventHandler (OnSearch);
					menu.Append (item);
					menu_to_query_map [item] = s;
				}
			}			

			if (recent_searches.Count > 0) {
				item = new Gtk.ImageMenuItem (Catalog.GetString ("Clear"));
				item.Image = new Gtk.Image (Gtk.Stock.Clear, Gtk.IconSize.Menu);
				item.Activated += new EventHandler (OnClear);
				menu.Append (item);
			}

			menu.Append (new Gtk.SeparatorMenuItem ());			
		
			item = new Gtk.ImageMenuItem (Catalog.GetString ("Quit"));
			item.Image = new Gtk.Image (Gtk.Stock.Quit, Gtk.IconSize.Menu);
			item.Activated += new EventHandler (OnQuit);
			menu.Append (item);
			
			menu.ShowAll ();
			return menu;
		}

		public void AddSearch (string query)
		{
			if (! recent_searches.Contains (query)) {
				recent_searches.Add (query);
				popup = MakeMenu (eventbox);
			}
		}

		private void OnSearch (object sender, EventArgs args)
		{
			if (Search != null)
				Search ((string) menu_to_query_map [sender]);
		}

		private void OnQuit (object sender, EventArgs args)
		{
			if (Quit != null)
				Quit (sender, args);
		}

		private void OnClear (object sender, EventArgs args) 
		{			
			recent_searches.Clear ();
			popup = MakeMenu (eventbox);
		}

		private void DetachWidget (Gtk.Widget attach_widget, Gtk.Menu menu)
		{
		}
        
		public static void GetMenuPosition (Gtk.Menu menu, out int  x, out int  y, out bool push_in)
		{
			Gtk.Requisition menu_req = menu.SizeRequest ();
			menu.AttachWidget.GdkWindow.GetOrigin (out x, out y);

			if (y + menu_req.Height >= menu.AttachWidget.Screen.Height)
				y -= menu_req.Height;
			else
				y += menu.AttachWidget.Allocation.Height;

			push_in = true;
		}

		static void DeactivateMenu (object sender, EventArgs args) 
		{
			Gtk.Menu menu = (Gtk.Menu) sender;
			menu.Popdown ();
		}

		// Place the menu underneath an arbitrary parent widget.  The
		// parent widget must be set using menu.AttachToWidget before
		// calling this
		public static void PopupMenu (Gtk.Menu menu, Gdk.EventButton ev)
		{
			menu.Deactivated += DeactivateMenu;
			menu.Popup (null, null, new Gtk.MenuPositionFunc (GetMenuPosition), 
				    (ev == null) ? 0 : ev.Button, 
				    (ev == null) ? Gtk.Global.CurrentEventTime : ev.Time);
		}

		private void OnClick (object o, ButtonPressEventArgs args)
		{
			if (args.Event.Type != EventType.ButtonPress)
				return;

			switch (args.Event.Button) {
			case 1:
				if (Clicked != null)
					Clicked (o, args);
				break;
			case 3:
				PopupMenu (popup, args.Event);
				break;
			}

			args.RetVal = false;
		}
	}
}
