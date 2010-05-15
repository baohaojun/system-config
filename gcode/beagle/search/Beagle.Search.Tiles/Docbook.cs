//
// Docbook.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using Mono.Unix;

using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class DocbookActivator : TileActivator {

		public DocbookActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "DocbookEntry", null));
		}

		public override bool Validate (Beagle.Hit hit)
		{
			if (! base.Validate (hit))
				return false;
			
			// FIXME: We should be using the users locale and
			// if we fail to find a result fallback to the C locale.
			// However, there is no easy way of doing this now.
			// Environment.GetEnvironmentVariable ("LANG"))

			if (String.IsNullOrEmpty (hit ["fixme:language"]))
				return true;

			if (hit ["fixme:language"] == "C" || hit ["fixme:language"] == "en")
				return true;

			return false;
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Docbook (hit, query);
		}
	}

	public class Docbook : TileTemplate {

		public Docbook (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			if (! String.IsNullOrEmpty (hit.GetFirstProperty ("dc:title")))
				Title = hit.GetFirstProperty ("dc:title");
			else
				Title = hit.GetFirstProperty ("beagle:ExactFilename");

			if (hit ["beagle:IsChild"] == "true")
				Description = hit.GetFirstProperty ("parent:beagle:Filename");
			else
				Description = hit.GetFirstProperty ("beagle:Filename");
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = WidgetFu.LoadThemeIcon ("gtk-help", size);
		}

		public override void Open ()
		{
			SafeProcess p = new SafeProcess ();
			p.Arguments = new string [] { "yelp", Hit.Uri.LocalPath };
			
			try {
				p.Start ();
			} catch {
				Console.WriteLine ("Failed to start '{0}'", p.Arguments [0]);
			}
		}
	}
}
