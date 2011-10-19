using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using Mono.Unix;

using GConf;

namespace Beagle.Search.Tiles {

	public class ApplicationActivator : TileActivator {

		static bool checked_gconf = false;
		static bool disable_command_line = false;

		public ApplicationActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, null, "application/x-desktop"));
		}

		[DllImport ("libgnome-desktop-2")]
		static extern IntPtr gnome_desktop_item_new_from_uri (string uri, int flags, IntPtr error);

		[DllImport ("libgnome-desktop-2")]
		static extern string gnome_desktop_item_get_string (IntPtr ditem, string attr);

		[DllImport ("libgnome-desktop-2")]
		static extern void gnome_desktop_item_unref (IntPtr ditem);

		IntPtr ditem;

		~ApplicationActivator ()
		{
			if (ditem != IntPtr.Zero)
				gnome_desktop_item_unref (ditem);
		}

		static void CheckLockdown ()
		{
			GConf.Client client = new GConf.Client ();

			try {
				disable_command_line = (bool) client.Get ("/desktop/gnome/lockdown/disable_command_line");
			} catch {
				// The key isn't set for some reason
				disable_command_line = false;
			}

			checked_gconf = true;
		}

		// invalid .desktop files get filtered out by Validate(), so they won't
		// show up as Application tiles, but will show up as File tiles. But
		// valid .desktop files marked to not show up in GNOME get eaten by
		// BuildTile instead, so that they won't get picked up by the File tile.

		// FIXME: we shouldn't be hardcoding GNOME in BuildTile, it should depend
		// on what the running desktop is.

		public override bool Validate (Beagle.Hit hit)
		{
			if (!base.Validate (hit))
				return false;

			ditem = gnome_desktop_item_new_from_uri (hit.EscapedUri, 0, IntPtr.Zero);
			if (ditem == IntPtr.Zero)
				return false;
			
			// Make sure this is a real desktop file, not a .desktop.in
			string _name = gnome_desktop_item_get_string (ditem, "_Name");
			if (_name != null)
				return false;

			return true;
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			if (ditem == IntPtr.Zero)
				return null;

			string notshow = gnome_desktop_item_get_string (ditem, "NotShowIn");
			if (notshow != null && notshow.IndexOf ("GNOME") != -1)
				return null;

			string onlyshow = gnome_desktop_item_get_string (ditem, "OnlyShowIn");
			if (onlyshow != null && onlyshow.IndexOf ("GNOME") == -1)
				return null;


			if (!checked_gconf)
				CheckLockdown();

			if (disable_command_line) {
				string[] categories = hit.GetProperties ("fixme:Categories");

				if (categories != null && Array.IndexOf (categories, "TerminalEmulator") != -1)
					return null;
			}

			return new Application (hit, query, ditem);
		}
	}

	public class Application : TileTemplate {

		IntPtr ditem;

		public Application (Beagle.Hit hit, Beagle.Query query, IntPtr ditem) : this (hit, query)
		{
			this.ditem = ditem;
//			AddAction (new TileAction (Catalog.GetString ("Move to trash"), Gtk.Stock.Delete, MoveToTrash));
		}

		protected Application (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Application;
			Title = Hit.GetFirstProperty ("fixme:Name");
			Description = Hit ["fixme:Comment"];
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			Gdk.Pixbuf icon = null;
			string path = Hit ["fixme:Icon"];
			
			if (path != null && path != "") {
				try {
					if (path.StartsWith ("/")) {
						icon = new Gdk.Pixbuf (path);
					} else {
						if (path.EndsWith (".png")) 
							icon = WidgetFu.LoadThemeIcon (path.Substring (0, path.Length-4), size);
						else
							icon = WidgetFu.LoadThemeIcon (path, size);
					
						if (icon == null) {
							string kde_path = Beagle.Util.KdeUtils.LookupIcon (path);
							
							if (System.IO.File.Exists (kde_path))
								icon = new Gdk.Pixbuf (kde_path);
						}
					}
				} catch (Exception e) {
					Console.WriteLine ("Unable to load icon '{0}': {1}", path, e.Message);
				}
			}

			if (icon != null) {
				if (icon.Height > size) {
					int scaled_width = (int) ((double) size / (double) icon.Height * icon.Width);

					icon = icon.ScaleSimple (scaled_width, size, Gdk.InterpType.Bilinear);
				}

				image.Pixbuf = icon;
			} else
				base.LoadIcon (image, size);
		}

		[DllImport ("libgnome-desktop-2")]
		static extern int gnome_desktop_item_launch (IntPtr ditem, IntPtr file_list, int flags, IntPtr error);

		public override void Open ()
		{
			if (gnome_desktop_item_launch (ditem, IntPtr.Zero, 0, IntPtr.Zero) == -1)
				Console.WriteLine ("Unable to launch application");
		}

#if NOPE
		public void MoveToTrash ()
		{
			// FIXME: What is the default way to uninstall an application
			// in a distro-independent way?

			// FIXME: The chance that the code below works is 1:100 :-)
			ProcessStartInfo pi = new ProcessStartInfo ("rpm");
			pi.Arguments = String.Format ("-e {0}", Hit ["fixme:Exec"]);
			//Process.Start (pi); // FIXME: Safe sex

			Console.WriteLine ("Would run 'rpm {0}'", pi.Arguments);
		}
#endif
	}
}
