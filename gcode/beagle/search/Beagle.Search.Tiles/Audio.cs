using System;
using Mono.Unix;

namespace Beagle.Search.Tiles {

	public class AudioActivator : TileActivator {

		public AudioActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "audio/*"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/ogg")); // FIXME: What about videos?
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Audio (hit, query);
		}
	}

	public class Audio : TileFile {

		public Audio (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Audio;

			// FIXME: Show album art if any. Needs implementation in Beagle.Util

			string title = Hit.GetFirstProperty ("dc:title");
			if (String.IsNullOrEmpty (title))
				title = Hit.GetFirstProperty ("fixme:title");

			if (! String.IsNullOrEmpty (title))
				Title = title;

			string artist = Hit.GetFirstProperty ("fixme:artist");
			if (! String.IsNullOrEmpty (artist))
				Description = artist;

			//AddAction (new TileAction (Catalog.GetString ("Add to Library"), AddToLibrary));
		}

		// FIXME: Check if Banshee exists and supports this?
		public void AddToLibrary ()
		{
		}
	}
}
