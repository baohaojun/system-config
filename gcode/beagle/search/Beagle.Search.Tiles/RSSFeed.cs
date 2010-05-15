using System;
using System.IO;
using System.Diagnostics;
using System.Collections;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class RSSFeedActivator : TileActivator {

		public RSSFeedActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "FeedItem", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new RSSFeed (hit, query);
		}
	}

	public class RSSFeed : TileTemplate {

		public RSSFeed (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Feed;

			Title = Hit ["dc:title"];
			Description = Hit ["dc:publisher"];
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			Gdk.Pixbuf pixbuf = null;

			string path = Hit ["fixme:cachedimg"];
			if (path != null && File.Exists (path)) {
				try {
					pixbuf = new Gdk.Pixbuf (path);
				} catch (GLib.GException) {
					// Catch in case of an invalid pixbuf.
				}
			}

			if (pixbuf != null && (pixbuf.Width > size || pixbuf.Height > size))
				pixbuf = pixbuf.ScaleSimple (size, size, Gdk.InterpType.Bilinear);

			if (pixbuf == null)
				pixbuf = WidgetFu.LoadThemeIcon ("gnome-fs-bookmark", size); // FIXME: RSS icon?

			image.Pixbuf = pixbuf;
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("Title:"), Hit ["dc:title"]);
			details.AddLabelPair (Catalog.GetString ("Site:"), Hit ["dc:identifier"]);
			details.AddLabelPair (Catalog.GetString ("Date Viewed:"), Utils.NiceLongDate (Timestamp));
			details.AddSnippet ();

			return details;
		}

		public override void Open ()
		{
			// If we are not a feed from Thunderbird just open based on mime
			if (Hit.GetFirstProperty ("fixme:client") != "thunderbird") {
				base.OpenFromUri (Hit ["dc:identifier"]);
				return;
			}

#if ENABLE_THUNDERBIRD			
			// Here's the Thunderbird specific part
			SafeProcess p = Thunderbird.GetSafeProcess ("-viewbeagle", Hit.GetFirstProperty ("fixme:uri"));
			
			try {
				p.Start ();
			} catch (SafeProcessException e) {
				Console.WriteLine ("Unable to run {0}: {1}", p.Arguments [0], e.Message);
			}
#endif
		}
	}
}
