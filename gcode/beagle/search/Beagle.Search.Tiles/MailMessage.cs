using System;
using System.Diagnostics;
using System.IO;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class MailMessageActivator : TileActivator {

		public MailMessageActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, null, "message/rfc822"));

			// This one allows us to handle HTML child indexables
			// that contain the actual body of the message as real
			// mail tiles.  We don't have to worry about them also
			// appearing as mail attachments because they aren't
			// considered attachments during indexing.
			AddSupportedFlavor (new HitFlavor (null, "MailMessage", "text/html"));
		}

		public override bool Validate (Beagle.Hit hit)
		{
			if (! base.Validate (hit))
				return false;
			
			if (hit ["beagle:HitType"] == "File") {
				// This handles a case when a file with the
				// message/rfc822 mimetype is indexed without
				// gmime. Thus we fail to extract any info and
				// this tile is useless.
				string subject = hit.GetFirstProperty ("dc:title");
				
				if (subject != null && subject != "")
					return true;

				return false;
			}

			return true;
		}
		
		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new MailMessage (hit, query);
		}
	}

	public class MailMessage : TileFlat {

		public MailMessage (Beagle.Hit hit, Beagle.Query query) : base (hit, query)
		{
			Group = TileGroup.Conversations;

			string title = Utils.GetFirstPropertyOfParent (hit, "dc:title");
			if (title == null || title == String.Empty)
				title = Catalog.GetString ("(untitled)");
			Subject.LabelProp = Title = title;

			From.LabelProp = "<b>" + GetAddress (hit) + "</b>";
			try {
				Timestamp = StringFu.StringToDateTime (Utils.GetFirstPropertyOfParent (hit, "fixme:date"));
				Date.LabelProp = Utils.NiceShortDate (Timestamp);
			} catch {}

			if (Utils.GetFirstPropertyOfParent (Hit, "fixme:client") == "evolution")
				AddAction (new TileAction (Catalog.GetString ("Send in Mail"), SendInMail));
			if ((Utils.GetFirstPropertyOfParent (hit, "fixme:isSent") == null)){
				AddAction ( new TileAction (Catalog.GetString ("Find Messages From Sender"), Gtk.Stock.Find, FindAllFromSender));
			}
		}

		protected override void LoadIcon (Gtk.Image image, int size)
		{
			if (Utils.GetFirstPropertyOfParent (Hit, "fixme:isAnswered") != null)
				image.Pixbuf = WidgetFu.LoadThemeIcon ("stock_mail-replied", size);
			else if (Utils.GetFirstPropertyOfParent (Hit, "fixme:isSeen") != null)
				image.Pixbuf = WidgetFu.LoadThemeIcon ("stock_mail-open", size);
			else
				image.Pixbuf = WidgetFu.LoadThemeIcon ("stock_mail", size);
		}

		private static string GetAddress (Beagle.Hit hit)
		{
			bool sent = (Utils.GetFirstPropertyOfParent (hit, "fixme:isSent") != null);
			string address = sent ? Utils.GetFirstPropertyOfParent (hit, "fixme:to") : Utils.GetFirstPropertyOfParent (hit, "fixme:from");

			if (address == null)
				return "";
			if (address.IndexOf (" <") != -1)
				address = address.Substring (0, address.IndexOf (" <"));

			return address;
		}

		protected override DetailsPane GetDetails ()
		{
			DetailsPane details = new DetailsPane ();

			bool sent = (Utils.GetFirstPropertyOfParent (Hit, "fixme:isSent") != null);

			details.AddLabelPair (Catalog.GetString ("Subject:"), SubjectLabel.Text);

			string label = sent ? Catalog.GetString ("To:") : Catalog.GetString ("From:");
			details.AddLabelPair (label, GetAddress (Hit));

			label = sent ? Catalog.GetString ("Date Sent:") : Catalog.GetString ("Date Received:");
			details.AddLabelPair (label, Utils.NiceLongDate (Timestamp));

			string folder = Hit.GetFirstProperty ("fixme:folder");

			if (folder != null)
				details.AddLabelPair (Catalog.GetString ("Folder:"), folder);
			
			details.AddSnippet ();
			
			return details;
		}

		public static SafeProcess GetClientProcess (Beagle.Hit hit)
		{
			string client = null;
			SafeProcess p = null;
			
			if (hit.ParentUri != null)
				client = Utils.GetFirstPropertyOfParent (hit, "fixme:client");
			else
				client = hit.GetFirstProperty ("fixme:client");
			
			if (client == "evolution") {
				p = new SafeProcess ();
				p.Arguments = new string [2];
				p.Arguments [0] = "evolution";
				p.Arguments [1] = (hit.ParentUri != null ? hit.EscapedParentUri : hit.EscapedUri);
			}
#if ENABLE_THUNDERBIRD
			else if (client == "thunderbird")  {
			     p = Thunderbird.GetSafeProcess ("-viewbeagle", hit.GetFirstProperty ("fixme:uri"));
			}
#endif

			return p;
		}

		public override void Open ()
		{
			SafeProcess p = GetClientProcess (Hit);
			
			if (p == null) {
				OpenFromMime (Hit);
				return;
			}

			try {
				p.Start ();
			} catch (SafeProcessException e) {
				Console.WriteLine ("Unable to run {0}: {1}", p.Arguments [0], e.Message);
			}
		}

		public void SendInMail ()
		{
			SafeProcess p = new SafeProcess ();

			string uri;
			if (Hit.ParentUri != null)
				uri = Hit.EscapedParentUri;
			else
				uri = Hit.EscapedUri;

			p.Arguments = new string [] { "evolution", String.Format ("{0};forward=attached", uri) };

			try {
				p.Start () ;
			} catch (Exception e) {
				Console.WriteLine ("Error launching Evolution composer: " + e.Message);
			}
		}
		public void FindAllFromSender()
		{
			SafeProcess p = new SafeProcess ();
			string addr = Hit.GetFirstProperty("fixme:from_address");
			p.Arguments = new string [] { "beagle-search", String.Format ("mailfromaddr:{0}", addr) };
			try {
				p.Start () ;
			} catch (Exception e) {
				Console.WriteLine ("Error launching new search: " + e.Message);
			}
		}
	}
}
