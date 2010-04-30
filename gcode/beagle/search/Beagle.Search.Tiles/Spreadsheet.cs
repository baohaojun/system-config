using System;
using Mono.Posix;

namespace Beagle.Search.Tiles {

	public class SpreadsheetActivator : TileActivator {

		public SpreadsheetActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.calc"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.calc.template"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/excel"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.ms-excel"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-excel"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-msexcel"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-gnumeric"));
			AddSupportedFlavor (new HitFlavor (null, "File", "text/spreadsheet"));
			
			// OO 2.0 formats
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.spreadsheet"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.spreadsheet.template"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Spreadsheet (hit, query);
		}
	}

	public class Spreadsheet : TileFile {

		public Spreadsheet (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Documents;

			// FIXME: Description = ???
		}
	}
}
