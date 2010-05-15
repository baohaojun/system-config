using System;
using Mono.Unix;

using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class VideoActivator : TileActivator {

		public VideoActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "video/*"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Video (hit, query);
		}
	}

	public class Video : TileFile {

		public Video (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Video;

			Title = Hit ["beagle:ExactFilename"];

			if (! String.IsNullOrEmpty (Hit ["fixme:video:codec"]))
				Description = Hit ["fixme:video:codec"];
			
			if (! String.IsNullOrEmpty (Hit ["fixme:video:width"]))
				Description += String.Format (" ({0}x{1})", Hit ["fixme:video:width"], Hit ["fixme:video:height"]);			
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddTitleLabel (Title);
			details.AddTextLabel (Description);
			details.AddNewLine ();

			details.AddLabelPair (Catalog.GetString ("Modified:"), Utils.NiceVeryLongDate (Hit.FileInfo.LastWriteTime));
			details.AddLabelPair (Catalog.GetString ("Full Path:"), Hit.Uri.LocalPath);

			return details;
		}
	}
}
