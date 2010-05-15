using System;
using System.Diagnostics;
using Mono.Unix;
using Gtk;

namespace Beagle.Search.Pages {

	public delegate void DaemonStarted ();

	public class StartDaemon : Base {

		public event DaemonStarted DaemonStarted;

		public StartDaemon ()
		{
			HeaderIconFromStock = Stock.DialogError;
			Header = Catalog.GetString ("Search service not running");

			Append (Catalog.GetString ("The search service does not appear to be running. " +
						   "You can start it by clicking the button below."));

			Gtk.Button button = new Gtk.Button (Catalog.GetString ("Start search service"));
			button.Clicked += OnStartDaemon;
			button.Show ();

			Append (button);
		}

		private void OnStartDaemon (object o, EventArgs args)
		{
			DoStartDaemon (DaemonStarted);
		}

		internal static void DoStartDaemon (DaemonStarted DaemonStarted)
		{
			string beagled_filename = "beagled";

			Process daemon = new Process ();
			daemon.StartInfo.FileName  = beagled_filename;
			daemon.StartInfo.UseShellExecute = false;

			try {
				daemon.Start ();
			} catch (System.ComponentModel.Win32Exception e) {
				Console.WriteLine ("Unable to start daemon: {0}", e.Message);
			}
			
			// Give the daemon some time to start
			if (DaemonStarted != null)
				GLib.Timeout.Add (5000, delegate () {
								if (DaemonStarted != null)
									DaemonStarted ();
								return false;
							});
		}
	}
}
