using System;
using Gtk;

namespace Beagle.Search.Tiles {

	public abstract class TileFlat : Tile {

		protected Gtk.Label Subject, From, Date;

		protected TileFlat (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Subject = WidgetFu.NewLabel ();
			WidgetFu.EllipsizeLabel (Subject, 40);
			HBox.PackStart (Subject, true, true, 3);

			From = WidgetFu.NewLabel ();
			From.UseMarkup = true;
			WidgetFu.EllipsizeLabel (From, 20);
			HBox.PackStart (From, false, false, 3);

			Date = WidgetFu.NewLabel ();
			HBox.PackStart (Date, false, false, 3);

			HBox.ShowAll ();
		}

		protected override void OnRealized ()
		{
			base.OnRealized ();

			if ((Icon.StorageType == ImageType.Empty || Icon.StorageType == ImageType.Pixbuf) &&
			    Icon.Pixbuf == null)
				LoadIcon (Icon, 16);
		}

		public Gtk.Label SubjectLabel {
			get { return Subject; }
		}

		public Gtk.Label FromLabel {
			get { return From; }
		}

		public Gtk.Label DateLabel {
			get { return Date; }
		}
	}
}
