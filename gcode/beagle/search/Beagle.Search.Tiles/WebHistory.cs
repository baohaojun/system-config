using System;
using Mono.Unix;

using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class WebHistoryActivator : TileActivator {

		public WebHistoryActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "WebHistory", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new WebHistory (hit, query);
		}
	}


	public class WebHistory : TileTemplate {

		public WebHistory (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Website;

			string title = hit.GetFirstProperty ("dc:title");
			if (String.IsNullOrEmpty (title))
				title = Hit.Uri.Host;

			Title = title;
			Description = hit.Uri.ToString ();

			AddAction (new TileAction ("Find more from same host", Gtk.Stock.Find, FindFromHost));
		}

		// We intentionally use a separate thumbnailer/thread from Tiles.File,
		// because the web thumbnailer is much slower and we don't want it to
		// hold up the image/document thumbnails

		static ThumbnailFactory thumbnailer = new ThumbnailFactory ();

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			if (!thumbnailer.SetThumbnailIcon (image, Hit, size))
				base.LoadIcon (image, size);
		}

		public override void Open ()
		{			
			base.OpenFromUri (UriFu.UriToEscapedString(Hit.Uri));
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("Title:"), Title);
			details.AddLabelPair (Catalog.GetString ("URL:"), Hit.Uri.ToString ());
			details.AddLabelPair (Catalog.GetString ("Accessed:"), Utils.NiceLongDate (Timestamp));
			details.AddSnippet ();

			return details;
		}
		
		public void FindFromHost()
		{
			
			SafeProcess p = new SafeProcess ();
			//string addr = Search.Tiles.Utils.GetFirstPropertyOfParent(Hit,"fixme:from_address");
			p.Arguments = new string [] { "beagle-search", String.Format ("host:{0}", Hit.Uri.Host) };
			try {
				p.Start () ;
			} catch (Exception e) {
				Console.WriteLine ("Error launching new search: " + e.Message);
			}
		}
	}
}
