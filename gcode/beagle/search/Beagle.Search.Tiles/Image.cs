using System;
using System.Diagnostics;
using Mono.Unix;

using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class ImageActivator : TileActivator {

		public ImageActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "File", "image/*"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Image (hit, query);
		}
	}

	public class Image : TileFile {

		public Image (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Image;

			Title = Hit ["beagle:ExactFilename"];

			if (Hit ["beagle:FilenameExtension"] != null && Hit ["beagle:FilenameExtension"].Length > 0)
				Description = Hit ["beagle:FilenameExtension"].Substring (1).ToUpper ();
			
			if (Hit ["fixme:width"] != null && Hit ["fixme:width"] != "")
				Description += String.Format (" {0}x{1}", Hit ["fixme:width"], Hit ["fixme:height"]);

			Description += String.Format (" ({0})", StringFu.FileLengthToString (Hit.FileInfo.Length));

			// AddAction (new TileAction (Catalog.GetString ("Add to Library"), Gtk.Stock.Add, AddToLibrary));
			AddAction (new TileAction (Catalog.GetString ("Set as Wallpaper"), SetAsWallpaper));
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			base.LoadIcon (image, size);

			// Draw the F-Spot overlay
			if (size > 32 && Hit ["fspot:IsIndexed"] == "true") {
				Gdk.Pixbuf emblem = WidgetFu.LoadThemeIcon ("f-spot", 24);
				Gdk.Pixbuf icon = image.Pixbuf.Copy ();

				if (icon == null || emblem == null)
					return;

				// FIXME: Ideally we'd composite into a fresh new pixbuf of
				// the correct size in this case, but really, who's going to
				// have images shorter or narrower than 16 pixels in f-spot??
				if (icon.Height < emblem.Height || icon.Width < emblem.Width) {
					icon.Dispose ();
					emblem.Dispose ();
					return;
				}

				emblem.Composite (icon, 0,  icon.Height - emblem.Height, emblem.Width,
						  emblem.Height, 0,  icon.Height - emblem.Height, 1,  1,
						  Gdk.InterpType.Bilinear, 255);

				image.Pixbuf.Dispose ();
				image.Pixbuf = icon;
			}
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			// FIXME: The icon needs a nice frame as in the spec (?)

			details.AddTitleLabel (Title);
			details.AddTextLabel (Description);
			details.AddNewLine ();

			string[] tags = Hit.GetProperties ("dc:subject");
			if (tags != null && tags.Length > 0)
				details.AddLabelPair (Catalog.GetString ("Tags:"), String.Join (", ", tags));

			details.AddLabelPair (Catalog.GetString ("Modified:"), Utils.NiceVeryLongDate (Hit.FileInfo.LastWriteTime));
			details.AddLabelPair (Catalog.GetString ("Full Path:"), Hit.Uri.LocalPath);

			// Get comments from the image.  FIXME: These should be unified into a single field.
			string comment = Hit.GetFirstProperty ("png:comment");

			if (String.IsNullOrEmpty (comment))
				comment = Hit.GetFirstProperty ("jfif:Comment");

			if (! String.IsNullOrEmpty (comment)) {
				details.AddLabelPair (Catalog.GetString ("Comment:"), comment);
				//details.AddNewLine ();
				//details.AddTextLabel (comment);
			}

			if (Hit ["fspot:Description"] != null && Hit ["fspot:Description"] != "") {
				details.AddNewLine ();
				details.AddTextLabel (Hit ["fspot:Description"]);
			}

			return details;
		}
		
#if NOT_YET
		// FIXME: fspot doesnt allow to import a particular file
		// only a whole directory
		public void AddToLibrary ()
		{
			// FIXME: check if f-spot is installed

			if (Hit ["fspot:IsIndexed"] == "true")
				return;

			SafeProcess p = new SafeProcess ();
			p.Arguments = new string[] { "f-spot", "--import", Hit.FileInfo.FullName };

			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Error launching F-Spot: " + e);
			}
		}
#endif
		
		public void SetAsWallpaper ()
		{
			int width = 0;
			int height = 0;

			if (Hit ["fixme:width"] != null && Hit ["fixme:width"] == "") {
				width = Int32.Parse (Hit ["fixme:width"]);
				height = Int32.Parse (Hit ["fixme:height"]);
			} else {
				if (! System.IO.File.Exists (Hit.FileInfo.FullName))
					return;

				Gdk.Pixbuf p = new Gdk.Pixbuf (Hit.FileInfo.FullName);
				width = p.Width;
				height = p.Height;
			}

			GConf.Client client = new GConf.Client ();
			client.Set ("/desktop/gnome/background/picture_filename", Hit.FileInfo.FullName);

			if (width <= 640) {
				if (width == height) {
					// Tile
					client.Set ("/desktop/gnome/background/picture_options",
						    "wallpaper");
				} else {
					// Center
					client.Set ("/desktop/gnome/background/picture_options",
						    "centered");
				}
			} else if (height >= width) {
				// Stretch vertically, but not horizontally
				client.Set ("/desktop/gnome/background/picture_options",
					    "scaled");
			} else {
				// Fit to screen
				client.Set ("/desktop/gnome/background/picture_options",
					    "stretched");
			}

			client.SuggestSync ();
		}
	}
}
