using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class FileActivator : TileActivator {

		public FileActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new TileFile (hit, query);
		}
	}

	public class TileFile : TileTemplate {

		private static ThumbnailFactory thumbnailer = new ThumbnailFactory ();

		public TileFile (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Title = GetTitle (hit);
			
			if (Hit.FileInfo != null) {
				Timestamp = Hit.FileInfo.LastWriteTimeUtc;
				Description = Utils.NiceShortDate (Timestamp);
			}

			AddAction (new TileAction (Catalog.GetString ("Reveal in Folder"), RevealInFolder));
			AddAction (new TileAction (Catalog.GetString ("E-Mail"), Email));
			// AddAction (new TileAction (Catalog.GetString ("Instant-Message"), InstantMessage));
			AddAction (new TileAction (Catalog.GetString ("Move to Trash"), Gtk.Stock.Delete, MoveToTrash));

			if (! String.IsNullOrEmpty (Hit.GetFirstProperty ("dc:author"))) {
				AddAction(new TileAction (Catalog.GetString ("Find Documents From Same Author"), Gtk.Stock.Find, FindSameAuthor));
			}

			EnableOpenWith = true;
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			// The File tile doesn't respect the icon size because
			// 48 is too small for thumbnails

			if (!thumbnailer.SetThumbnailIcon (image, Hit, size))
				base.LoadIcon (image, size);

			// FIXME: Multiple emblems
			string emblem = Hit.GetFirstProperty ("nautilus:emblem");

			if (String.IsNullOrEmpty (emblem))
				return;

			Gdk.Pixbuf emblem_pixbuf = WidgetFu.LoadThemeIcon ("emblem-" + emblem, 24);

			if (emblem_pixbuf == null)
				return;

			Gdk.Pixbuf icon = image.Pixbuf.Copy ();

			// If the icon itself is smaller than our requested
			// emblem, just display the icon.

			if ((icon.Height < emblem_pixbuf.Height || icon.Width < emblem_pixbuf.Width) ||
			    (icon.Height < (emblem_pixbuf.Height * 2) && icon.Width < (emblem_pixbuf.Width * 2))) {
				icon.Dispose ();
				emblem_pixbuf.Dispose ();
				return;
			}

			emblem_pixbuf.Composite (icon, 0, 0, emblem_pixbuf.Width, emblem_pixbuf.Height,
						 0, 0, 1, 1, Gdk.InterpType.Bilinear, 255);
			emblem_pixbuf.Dispose ();

			image.Pixbuf.Dispose ();
			image.Pixbuf = icon;
		}

		protected static string GetTitle (Beagle.Hit hit)
		{
			string title;

			title = hit.GetFirstProperty ("dc:title");

			if (String.IsNullOrEmpty (title))
				title = hit.GetFirstProperty ("beagle:ExactFilename");

			return title;
		}

		public override void Open ()
		{
			base.OpenFromMime (Hit);
		}

		public void OpenWith ()
		{
			// FIXME: base.OpenWith
		}

		public void RevealInFolder ()
		{
			string path = Hit.FileInfo.DirectoryName;

			// FIXME: When nautilus implements this, then we should
			// also select the file in the folder.

			SafeProcess p = new SafeProcess ();

#if ENABLE_DESKTOP_LAUNCH
			p.Arguments = new string [] { "desktop-launch", path };
#elif ENABLE_XDG_OPEN
			p.Arguments = new string [] { "xdg-open", path };
#else
			p.Arguments = new string [] { "nautilus", "--no-desktop", path };
#endif
			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Cannot open folder: " + e);
			}
		}

		public void Email ()
		{
			try {
				OpenFromUri (String.Format ("mailto:?attach={0}", Hit.FileInfo.FullName));
			} catch (Exception e) {
				Console.WriteLine ("Error sending email: " + e);
			}
		}

		public void InstantMessage ()
		{
			// FIXME: base.InstantMessage
		}

		public void MoveToTrash ()
		{
			// FIXME: Ask for confirmation

			try {
				// FIXME: Check if KDE uses ~/.Trash too (there is a spec at fd.o)
				string trash_dir = System.IO.Path.Combine (Beagle.Util.PathFinder.HomeDir, ".Trash");

				// FIXME: This throws an exception if the file exists
				Hit.FileInfo.MoveTo (System.IO.Path.Combine (trash_dir, Hit.FileInfo.Name));
			} catch (Exception e) {
				Console.WriteLine (e);
			}
		}	

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("File:"), Hit.GetFirstProperty ("beagle:ExactFilename"));

			string title = Hit.GetFirstProperty ("dc:title");
			if (! String.IsNullOrEmpty (title))
				details.AddLabelPair (Catalog.GetString ("Title:"), title);

			details.AddLabelPair (Catalog.GetString ("Last Edited:"), Utils.NiceLongDate (Timestamp));

			if (Hit ["dc:author"] != null)
				details.AddLabelPair (Catalog.GetString ("Author:"), Hit ["dc:author"]);

			details.AddLabelPair (Catalog.GetString ("Full Path:"), Hit.Uri.LocalPath);
			details.AddSnippet ();

			return details;
		}

		public void FindSameAuthor()
		{
			SafeProcess p = new SafeProcess ();
			string author = Hit.GetFirstProperty("dc:author");
			if( String.IsNullOrEmpty(author))
				 author = Hit.GetFirstProperty("dc:creator");
			p.Arguments = new string [] { "beagle-search", String.Format ("author:{0} OR creator:{0}", author) };
			try {
				p.Start () ;
			} catch (Exception e) {
				Console.WriteLine ("Error launching new search: " + e.Message);
			}
			
		}
	}
}
