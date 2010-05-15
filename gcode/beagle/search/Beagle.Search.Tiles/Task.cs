using System;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class TaskActivator : TileActivator {

		public TaskActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "Task", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Task (hit, query);
		}
	}

	public class Task : TileTemplate {

		public Task (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Calendar;

			Title = hit.GetFirstProperty ("fixme:summary");

			if (!String.IsNullOrEmpty (hit.GetFirstProperty ("fixme:description")))
				Description = Utils.TrimFirstLine (hit.GetFirstProperty ("fixme:description"));
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = WidgetFu.LoadThemeIcon ("stock_todo", size);
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("Title:"), Title);

			if (!String.IsNullOrEmpty (Description))
				details.AddLabelPair (Catalog.GetString ("Description:"), Description);
			
			if (!String.IsNullOrEmpty (Hit.GetFirstProperty ("fixme:starttime")))
				details.AddLabelPair (Catalog.GetString ("Date:"), Utils.NiceShortDate (Hit.GetFirstProperty ("fixme:starttime")));
			
			return details;
		}

		public override void Open ()
		{
			SafeProcess p = new SafeProcess ();
			p.Arguments = new string [] { "evolution", Hit.EscapedUri };

			try {
				p.Start ();
			} catch (SafeProcessException e) {
				Console.WriteLine (e.Message);
			}
		}
	}
}
