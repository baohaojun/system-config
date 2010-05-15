using System;
using System.Runtime.InteropServices;

namespace Beagle.Search {

	public static class WidgetFu {
		
		public static Gtk.Label NewLabel ()
		{
			Gtk.Label label = new Gtk.Label ();
			label.SetAlignment (0.0f, 0.5f);
			label.Selectable = true;
			return label;
		}

		public static Gtk.Label NewLabel (string text)
		{
			Gtk.Label label = NewLabel ();
			label.Text = text;
			return label;
		}

		public static Gtk.Label NewBoldLabel (string text)
		{
			Gtk.Label label = NewLabel ();
			label.UseMarkup = true;
			label.LabelProp = "<b>" + GLib.Markup.EscapeText (text) + "</b>";
			return label;
		}

		public static Gtk.Label NewGrayLabel ()
		{
			Gtk.Label label = NewLabel ();
			label.ModifyFg (Gtk.StateType.Normal, label.Style.Foreground (Gtk.StateType.Insensitive));
			return label;
		}

		public static Gtk.Label NewGrayLabel (string text)
		{
			Gtk.Label label = NewGrayLabel ();
			label.Text = text;
			return label;
		}

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern void gtk_label_set_ellipsize (IntPtr label, int mode);

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern void gtk_label_set_max_width_chars (IntPtr label, int max_width);

		public static void EllipsizeLabel (Gtk.Label label)
		{
			gtk_label_set_ellipsize (label.Handle, 3);
		}

		public static void EllipsizeLabel (Gtk.Label label, int maxWidth)
		{
			gtk_label_set_ellipsize (label.Handle, 3);
			gtk_label_set_max_width_chars (label.Handle, maxWidth);
		}

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern IntPtr gtk_icon_theme_get_default ();
		
		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern IntPtr gtk_icon_theme_lookup_icon (IntPtr theme, string name, int size, int flags, IntPtr error);		

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern int gtk_icon_info_get_base_size (IntPtr icon_info);

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern void gtk_icon_info_free (IntPtr icon_info);

		[DllImport ("libgtk-x11-2.0.so.0")]
		private static extern IntPtr gtk_icon_theme_load_icon (IntPtr theme, string name, int size, int flags, IntPtr error);		

		public static Gdk.Pixbuf LoadThemeIcon (string name, int size)
		{
			try {
				IntPtr info = gtk_icon_theme_lookup_icon (gtk_icon_theme_get_default (), name, size, 0, IntPtr.Zero);
				if (info == IntPtr.Zero)
					return null;

				int base_size = gtk_icon_info_get_base_size (info);

				// If the icon has no base size, or it would be huge
				// compared to what we asked for, scale it down.
				// 1.33334 is pretty arbitrary, roughly calculated
				// as the difference between icon sizes 48 and 64.
				if (base_size == 0 || base_size > size * 1.33334)
					base_size = size;
				gtk_icon_info_free (info);

				IntPtr native = gtk_icon_theme_load_icon (gtk_icon_theme_get_default (), name, base_size, 0, IntPtr.Zero);
				if (native != IntPtr.Zero) {
					Gdk.Pixbuf ret = (Gdk.Pixbuf) GLib.Object.GetObject(native, true);
					return ret;
				}
			} catch (System.Exception e) {
				System.Console.Write (e.ToString ());
			}
			return null;
		}		

		public static Gdk.Pixbuf LoadMimeIcon (string mimetype, int size)
		{
			Gtk.IconTheme icon_theme = Gtk.IconTheme.Default;
			Gnome.IconLookupResultFlags result;

			// FIXME when ximian bug #76540 is fixed
			// change "new Gnome.Vfs.FileInfo (IntPtr.Zero)" to "null"
			string icon_name = Gnome.Icon.Lookup (icon_theme, null, null, null, new Gnome.Vfs.FileInfo (IntPtr.Zero), mimetype, (Gnome.IconLookupFlags) 0, out result);

			if (icon_name == null)
				return null;

			Gtk.IconInfo icon_info = icon_theme.LookupIcon (icon_name, size, 0);

			if (icon_info == null)
				return null;
			try {
				return icon_info.LoadIcon ();
			} catch (System.Exception e) {
				System.Console.Write (e.ToString ());
			}
			return null;
		}

		[DllImport ("libbeagleuiglue.so")]
		private static extern void ui_glue_set_drag_pixbuf_from_image (IntPtr drag_context, IntPtr image);

		public static void SetDragImage (Gdk.DragContext context, Gtk.Image image)
		{
			ui_glue_set_drag_pixbuf_from_image (context.Handle, image.Handle);
		}
	}
}
