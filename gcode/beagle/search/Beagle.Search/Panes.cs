using Gtk;
using Gdk;
using System;
using System.Collections;

namespace Beagle.Search {

	public class Panes : Gtk.VPaned {

		Gtk.ScrolledWindow mainSW, detailsSW;
		WhiteBox main, details;

		public Panes ()
		{
			Gtk.Viewport vp;

			mainSW = new Gtk.ScrolledWindow ();
			mainSW.SetPolicy (Gtk.PolicyType.Never, Gtk.PolicyType.Always);
			mainSW.ShadowType = Gtk.ShadowType.In;
			mainSW.SizeAllocated += MainResized;
			Pack1 (mainSW, true, false);

			vp = new Gtk.Viewport (null, null);
			vp.ResizeMode = Gtk.ResizeMode.Parent;
			vp.ShadowType = ShadowType.None;
			mainSW.Add (vp);
			vp.Show ();

			main = new WhiteBox ();
			vp.Add (main);
			main.Show ();

			detailsSW = new Gtk.ScrolledWindow ();
			detailsSW.SetPolicy (Gtk.PolicyType.Never, Gtk.PolicyType.Never);
			detailsSW.WidthRequest = 0;
			detailsSW.NoShowAll = true;
			detailsSW.ShadowType = Gtk.ShadowType.In;
			Pack2 (detailsSW, false, false);

			vp = new Gtk.Viewport (null, null);
			vp.ShadowType = ShadowType.None;
			detailsSW.Add (vp);
			vp.Show ();

			details = new WhiteBox ();
			vp.Add (details);
			details.Show ();
		}

		public Gtk.Widget MainContents {
			get {
				return main.Child;
			}
			set {
				if (main.Child != null)
					main.Remove (main.Child);
				if (value != null) {
					main.Add (value);
					if (value is Container)
						((Container)value).FocusVadjustment = mainSW.Vadjustment;
				}
			}
		}

		public Gtk.Widget Details {
			get {
				return details.Child;
			}
			set {
				if (details.Child != null)
					details.Remove (details.Child);
				if (value != null)
					details.Add (value);
			}
		}

		public void ToggleDetails (bool visible)
		{
			if (visible)
				detailsSW.Show ();
			else
				detailsSW.Hide ();
		}

		private void MainResized (object obj, Gtk.SizeAllocatedArgs args)
		{
			// If the details pane pops up and covers the selected tile,
			// fix it.

			Gtk.Container mainChild = main.Child as Gtk.Container;
			if (mainChild != null) {
				Gtk.Widget focusChild = mainChild.FocusChild;
				mainChild.FocusChild = null;
				mainChild.FocusChild = focusChild;
			}
		}

		public class WhiteBox : Gtk.EventBox
		{
			public WhiteBox () : base ()
			{
				AppPaintable = true;
			}

			protected override bool OnExposeEvent (Gdk.EventExpose evt)
			{
				if (!IsDrawable)
					return false;

				if (evt.Window == GdkWindow) {
					GdkWindow.DrawRectangle (Style.BaseGC (State), true,
								 evt.Area.X, evt.Area.Y,
								 evt.Area.Width, evt.Area.Height);
				}

				if (Child != null)
					PropagateExpose (Child, evt);

				return false;
			}
		}
	}
}
