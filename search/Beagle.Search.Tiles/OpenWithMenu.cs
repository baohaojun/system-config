using System;
using System.Collections;
using System.Runtime.InteropServices;
using Gtk;
using Gdk;
using Gnome.Vfs;

namespace Beagle.Search.Tiles {

	public class OpenWithMenu : Gtk.Menu {

		public delegate void OpenWithHandler (MimeApplication app);
		public event OpenWithHandler ApplicationActivated;

		private ArrayList list = null;

		private bool show_icons = true;
		public bool ShowIcons {
			get { return show_icons; }
			set { show_icons = value; }
		}

		static OpenWithMenu ()
		{
			Gnome.Vfs.Vfs.Initialize ();
		}

		public OpenWithMenu (string mime)
		{
			list = GetApplications (mime);

			if (list == null)
				return;

			foreach (MimeApplication app in list) {
				ApplicationMenuItem i = new ApplicationMenuItem (this, app);
				i.Activated += HandleItemActivated;
				Append (i);
			}
		}
	
		public void AppendToMenu (Gtk.Menu menu)
		{
			if (list == null || list.Count == 0)
				return;

			Gtk.MenuItem open_with = new Gtk.MenuItem (Mono.Unix.Catalog.GetString ("Open With"));
			open_with.Submenu = this;
			open_with.ShowAll ();
			menu.Append (open_with);
		}

		private ArrayList GetApplications (string mime)
		{
			if (mime == null || mime == "")
				return null;

			ArrayList list = new ArrayList ();
		
			MimeApplication [] apps = Gnome.Vfs.Mime.GetAllApplications (mime);

			foreach (MimeApplication app in apps) {
				// Skip apps that don't take URIs
				if (! app.SupportsUris ())
					continue;
				
				if (! list.Contains (app))
					list.Add (app);
			}

			return list;
		}
	
		private void HandleItemActivated (object sender, EventArgs args)
		{
			if (ApplicationActivated != null)
				ApplicationActivated ((sender as ApplicationMenuItem).Application);
		}
	
		private class ApplicationMenuItem : ImageMenuItem {

			private MimeApplication application;
			public MimeApplication Application {
				get { return application; }
				set { application = value; }
			}

			public ApplicationMenuItem (OpenWithMenu menu, MimeApplication ma) : base (ma.Name)
			{
				this.application = ma;
				
				if (!menu.ShowIcons)
					return;
					
				if (ma.Icon == null)
					return;

				Gdk.Pixbuf pixbuf = null; 
				
				try {
					if (ma.Icon.StartsWith ("/"))
						pixbuf = new Gdk.Pixbuf (ma.Icon, 16, 16);
					else 
						pixbuf = IconTheme.Default.LoadIcon (ma.Icon, 16, 
										     (IconLookupFlags)0);
				} catch {
					pixbuf = null;
				}
				
				if (pixbuf != null)
						Image = new Gtk.Image (pixbuf);
			}
		}
	}
}
