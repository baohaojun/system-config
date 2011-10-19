using System;
using Gtk;

namespace Beagle.Search.Tiles {

	public class ActionMenuItem : ImageMenuItem
	{
		private TileActionDelegate action_delegate;
		
		public ActionMenuItem (TileAction action) : base (action.Name)
		{
			this.action_delegate = action.Action;

			if (action.Stock != null) {
				Gtk.Image image = new Gtk.Image ();
				image.SetFromStock (action.Stock, IconSize.Menu);
				image.Show ();
				this.Image = image;
			}
		}

		protected override void OnActivated ()
		{
			if (action_delegate != null)
				action_delegate ();
		}
	}
}
