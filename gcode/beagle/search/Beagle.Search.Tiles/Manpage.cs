//
// Manpage.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using Mono.Unix;

using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class ManpageActivator : TileActivator {

		public ManpageActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", null));
		}

		public override bool Validate (Beagle.Hit hit)
		{
			if (! base.Validate (hit))
				return false;
			
			if (hit ["beagle:FileType"] != "documentation")
				return false;
			
			Weight += 2;

			return true;
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Manpage (hit, query);
		}
	}

	public class Manpage : TileTemplate {

		string path = null;

		public Manpage (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			if (! String.IsNullOrEmpty (hit.GetFirstProperty ("dc:title")))
				Title = hit.GetFirstProperty ("dc:title");
			else
				Title = hit.GetFirstProperty ("beagle:ExactFilename");

			if (hit ["beagle:IsChild"] == "true")
				path =	hit.ParentUri.LocalPath;
			else
				path = hit.Uri.LocalPath;

			Description = hit.GetFirstProperty ("dc:subject") ?? Catalog.GetString ("Manual page");
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = WidgetFu.LoadThemeIcon ("gtk-help", size);
		}

		public override void Open ()
		{
			SafeProcess p = new SafeProcess ();
			p.Arguments = new string [] { "yelp", path };
			
			try {
				p.Start ();
			} catch {
				Console.WriteLine ("Failed to start '{0}'", p.Arguments [0]);
			}
		}
	}
}
