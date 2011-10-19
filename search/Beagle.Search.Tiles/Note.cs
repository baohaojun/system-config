using System;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class NoteActivator : TileActivator {

		public NoteActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "Note", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new TileNote (hit, query);
		}
	}

	public class TileNote : TileTemplate {

		public TileNote (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Title = Hit.GetFirstProperty ("dc:title");
			Description = Utils.NiceShortDate (Timestamp);
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = WidgetFu.LoadThemeIcon ("tomboy", size);
		}

		public override void Open ()
		{
			SafeProcess p = new SafeProcess ();

			switch (Hit.Source) {
			case "Tomboy":
				// This doesn't work very well if you have multiple
				// terms that match.  Tomboy doesn't seem to have a way
				// to specify more than one thing to highlight.
				p.Arguments = new string [] { "tomboy",
							      "--open-note", Hit.EscapedUri,
							      "--highlight-search", Query.QuotedText };
				break;
			
			case "Labyrinth":
				p.Arguments = new string [] { "labyrinth", "-m", Hit.FileInfo.Name };
				break;

			case "EvolutionDataServer":
				p.Arguments = new string [] { "evolution", Hit.EscapedUri };
				break;
			}

			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Could not open note: " + e);
			}
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("Title:"), Title);
			details.AddLabelPair (Catalog.GetString ("Last Edited:"), Utils.NiceLongDate (Timestamp));
			
			if(! String.IsNullOrEmpty (Hit.GetFirstProperty ("note:tag"))) {
				string tags = String.Join (", ", Hit.GetProperties ("note:tag"));
				details.AddLabelPair (Catalog.GetString ("Tags:"), tags);
			}
			
			details.AddSnippet ();

			return details;
		}
	}
}
