using System;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class TextDocumentActivator : TileActivator {

		public TextDocumentActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.writer"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.sun.xml.writer.template"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/msword"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.ms-word"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-msword"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/pdf"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-abiword"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/rtf"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/x-chm"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.text"));
			AddSupportedFlavor (new HitFlavor (null, "File", "application/vnd.oasis.opendocument.text.template"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new TextDocument (hit, query);
		}
	}

	public class TextDocument : TileFile {

		public TextDocument (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Documents;

			if (Hit ["fixme:page-count"] != null) {
				int count = Int32.Parse (Hit ["fixme:page-count"]);
				Description = String.Format (Catalog.GetPluralString ("{0} page", "{0} pages", count), count);
			}
			
			// These files generally have a default title or an auto-generated title.
			// So use the filename for these types of files.
			Title = hit.GetFirstProperty ("beagle:ExactFilename");
		}
	}
}
