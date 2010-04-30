//
//  IndexInfo.cs
//
//  Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
//  Copyright (C) 2007 Lukas Lipka <lukaslipka@gmail.com>
//

using Gtk;
using System;
using System.Text;
using System.Collections.Generic;

using Mono.Unix;

using Beagle;

namespace Beagle.Search.Pages {

	public class IndexInfo : Base {

		private Gtk.Label note = null;
		private Gtk.Label label = null;

		public IndexInfo ()
		{
			HeaderIcon = WidgetFu.LoadThemeIcon ("dialog-information", 48);
			Header = Catalog.GetString ("Index Information");

			Gtk.Label description = new Gtk.Label ();
			description.Markup = Catalog.GetString ("Number of items currently indexed:");
			description.LineWrap = true;
			description.Justify = Justification.Left;
			description.SetAlignment (0.0f, 0.5f);

			Append (description, Gtk.AttachOptions.Fill, 0);

			label = new Gtk.Label ();
			label.LineWrap = true;
			label.SetAlignment (0.0f, 0.5f);
			label.Justify = Justification.Fill;
			
			Append (label, Gtk.AttachOptions.Expand | Gtk.AttachOptions.Fill, 0);

			note = new Gtk.Label ();
			note.Markup = Catalog.GetString ("<i>NOTE: The search service is still indexing new data.</i>");
			note.LineWrap = true;
			note.Justify = Justification.Fill;
			note.SetAlignment (0.0f, 0.5f);
			note.NoShowAll = true;
			
			Append (note, Gtk.AttachOptions.Fill, 0);
		}

		internal bool Refresh ()
		{
			DaemonInformationRequest request = new DaemonInformationRequest (false, false, true, true);
			DaemonInformationResponse response;

			try {
				response = (DaemonInformationResponse) request.Send ();
			} catch (Beagle.ResponseMessageException) {
				Console.WriteLine ("Could not connect to the daemon.");
				return false;
			}

			if (response.IsIndexing) {
				note.Show ();
			} else {
				note.Hide ();
			}

			int i = 0;
			StringBuilder sb = new StringBuilder ();

			foreach (QueryableStatus status in response.IndexStatus) {
				if (i++ > 20)
					break;

				// Skip all those metadata and networking services backends
				if (status.ItemCount < 0)
					continue;

				sb.AppendFormat ("<b>{0}</b>: {1}", status.Name, status.ItemCount);
				
				if (status.ProgressPercent > 0)
					sb.AppendFormat (" ({0})", status.ProgressPercent);

				sb.Append ("\n");
			}

			label.Markup = sb.ToString ();

			label.Show ();

			return true;
		}
	}
}
