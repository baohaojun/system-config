using System;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class ContactActivator : TileActivator {

		public ContactActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, "Contact", null));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new Contact (hit, query);
		}
	}

	public class Contact : TileTemplate {

		public Contact (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Contact;

			Title = hit.GetFirstProperty ("fixme:Name");
			Description = hit.GetFirstProperty ("fixme:Email");

			if (Hit.GetFirstProperty ("fixme:Email") != null)
				AddAction (new TileAction (Catalog.GetString ("Send Mail"), SendMail));
		}

		private Gdk.Pixbuf GetIcon (int size)
		{
			if (Hit.GetFirstProperty ("beagle:Photo") != null) {
				Gdk.Pixbuf icon = new Gdk.Pixbuf (Hit.GetFirstProperty ("beagle:Photo"));
				return icon.ScaleSimple (size, size, Gdk.InterpType.Bilinear);
			} else
				return WidgetFu.LoadThemeIcon ("stock_person", size);
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			image.Pixbuf = GetIcon (size);
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();
				
			details.AddTitleLabel (Title);
			
			string org = Hit.GetFirstProperty ("fixme:Org");
			string title = Hit.GetFirstProperty ("fixme:Title");
			string email = Hit.GetFirstProperty ("fixme:Email");
			string mobile_phone = Hit.GetFirstProperty ("fixme:MobilePhone");
			string work_phone = Hit.GetFirstProperty ("fixme:BusinessPhone");
			string home_phone = Hit.GetFirstProperty ("fixme:HomePhone");
			
			if (org != null && org != "")
				details.AddTextLabel (org);
			if (title != null && title != "")
				details.AddTextLabel (title);

			details.AddNewLine ();

			if (email != null && email != "")
				details.AddLabelPair (Catalog.GetString ("E-Mail:"), email);
			if (mobile_phone != null && mobile_phone != "")
				details.AddLabelPair (Catalog.GetString ("Mobile Phone:"), mobile_phone);
			if (work_phone != null && work_phone != "")
				details.AddLabelPair (Catalog.GetString ("Work Phone:"), work_phone);
			if (home_phone != null && home_phone != "")
				details.AddLabelPair (Catalog.GetString ("Home Phone:"), home_phone);
			
			return details;
		}

		public static SafeProcess GetClientProcess (string client, string uri)
		{
			SafeProcess p = null;

   			if (client == "evolution" || (client == null && uri.StartsWith ("contacts:"))) {
				p = new SafeProcess ();
				p.Arguments = new string [2];
				p.Arguments [0] = "evolution";
				p.Arguments [1] = uri;
			} else if (client == "thunderbird") {
				p = new SafeProcess ();
				p.Arguments = new string [4];
				p.Arguments [0] = "beagle-contactviewer";
				p.Arguments [1] = "--manager";
				p.Arguments [2] = "Thunderbird";
				p.Arguments [3] = uri;
			}

			return p;
                }


		public override void Open ()
		{
			SafeProcess p = GetClientProcess (Hit.GetFirstProperty ("fixme:client"), Hit.EscapedUri);

			if (p == null) {
				Console.WriteLine ("Opening contact '{0}' is unsupported!", Hit.EscapedUri);
				return;
			}
			
			try {
				p.Start ();
			} catch (SafeProcessException e) {
				Console.WriteLine (e.Message);
			}
		}

		private void SendMail ()
		{
			OpenFromUri ("mailto:" + Hit.GetFirstProperty ("fixme:Email"));
		}
	}
}
