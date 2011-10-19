//
// GnomeFu.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2003, Mariano Cano P�rez <mariano.cano@hispalinux.es>
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

using Gtk;
using GLib;
using System;
using System.Runtime.InteropServices;
using System.IO;

namespace Beagle.Util {

	public class GnomeFu {

		private static Gtk.IconTheme icon_theme = null;

		// FIXME: When gtk-sharp 2.5 is a requirement
		// use Gnome.Vfs.MimeApplication stuff instead
		//[DllImport("libgnomevfs-2")]
		//static extern IntPtr gnome_vfs_mime_get_default_application(string mime_type);

		static GnomeFu ()
		{
			Gnome.Vfs.Vfs.Initialize ();
		}

		public static string GetMimeIconPath (string mimetype)
		{
			if (icon_theme == null)
				icon_theme = Gtk.IconTheme.Default;

			Gnome.IconLookupResultFlags result;

			// FIXME when ximian bug #76540 is fixed
			// change "new Gnome.Vfs.FileInfo (IntPtr.Zero)" to "null"
			string icon_name = Gnome.Icon.Lookup (icon_theme, null, null, null, new Gnome.Vfs.FileInfo (IntPtr.Zero), mimetype, (Gnome.IconLookupFlags) 0, out result);
			if (icon_name == null)
				return null;

			Gtk.IconInfo icon_info = icon_theme.LookupIcon (icon_name, 48, 0);
			if (icon_info == null)
				return null;

			return icon_info.Filename;
		}
	}

}
