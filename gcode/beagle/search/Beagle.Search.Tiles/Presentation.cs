using System;
using Mono.Unix;

namespace Beagle.Search.Tiles {

	public class PresentationActivator : TileActivator {

		public PresentationActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.impress.template"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.impress"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.ms-powerpoint"));
			
			// OO 2.0 formats
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.presentation"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.presentation.template"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Presentation (hit, query);
		}
	}

	public class Presentation : TileFile {

		public Presentation (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Documents;

			if (Hit ["fixme:slide-count"] != null) {
				int count = Int32.Parse (Hit ["fixme:slide-count"]);
				Description = String.Format (Catalog.GetPluralString ("{0} slide", "{0} slides", count), count);
			}
		}
	}
}
