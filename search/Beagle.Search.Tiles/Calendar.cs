using System;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class CalendarActivator : TileActivator {

		public CalendarActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "Calendar", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Calendar (hit, query);
		}
	}

	public class Calendar : TileTemplate {

		public Calendar (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Calendar;

			string summary = hit.GetFirstProperty ("fixme:summary");
			string time = Utils.NiceShortDate (hit.GetFirstProperty ("fixme:starttime"));

			Title = (time == "") ? summary : time + ": " + summary;

			if (!String.IsNullOrEmpty (hit.GetFirstProperty ("fixme:description")))
				Description = Utils.TrimFirstLine (hit.GetFirstProperty ("fixme:description"));
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = WidgetFu.LoadThemeIcon ("stock_calendar", size);
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			details.AddLabelPair (Catalog.GetString ("Title:"), Title);

			if (!String.IsNullOrEmpty (Description))
				details.AddLabelPair (Catalog.GetString ("Description:"), Description);
			
			if (!String.IsNullOrEmpty (Hit.GetFirstProperty ("fixme:starttime"))) {
				Console.WriteLine ("1. " + Hit.GetFirstProperty ("fixme:starttime"));
				string time = Utils.NiceShortTime (Hit.GetFirstProperty ("fixme:starttime"));
				Console.WriteLine ("2. " + time);
				
				if (!String.IsNullOrEmpty (Hit.GetFirstProperty ("fixme:endtime")))
				    time = String.Format ("{0} - {1}", time, Utils.NiceShortTime (Hit.GetFirstProperty ("fixme:endtime")));

				details.AddLabelPair (Catalog.GetString ("Time:"), time);
			}

			if (!String.IsNullOrEmpty (Hit.GetFirstProperty ("fixme:location")))
				details.AddLabelPair (Catalog.GetString ("Location:"), Hit.GetFirstProperty ("fixme:location"));

			string[] attendees = Hit.GetProperties ("fixme:attendee");
			if (attendees != null && attendees.Length > 0)
				details.AddLabelPair (Catalog.GetString ("Attendees:"), String.Join (", ", attendees));

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
