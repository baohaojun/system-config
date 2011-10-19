using Gtk;
using System;
using Mono.Unix;

namespace Beagle.Search.Pages {

	public class Base : EventBox {

		private static Gdk.Pixbuf arrow = WidgetFu.LoadThemeIcon ("go-next", 16);

		private Gtk.Table table;
		private Gtk.Image header_icon;
		private Gtk.Label header_label;

		public Base ()
		{
			table = new Gtk.Table (1, 2, false);
			table.RowSpacing = 12;
			table.ColumnSpacing = 12;

			header_icon = new Gtk.Image ();
			table.Attach (header_icon, 0, 1, 0, 1, Gtk.AttachOptions.Fill, Gtk.AttachOptions.Fill, 0, 5);

			header_label = new Gtk.Label ();
			header_label.SetAlignment (0.0f, 0.5f);
			table.Attach (header_label, 1, 2, 0, 1, Gtk.AttachOptions.Fill | Gtk.AttachOptions.Expand, 0, 0, 5);

			table.ShowAll ();			

			Add (table);
		}

		protected override void OnRealized ()
		{
			base.OnRealized ();
			base.ModifyBg (Gtk.StateType.Normal, Style.Base (Gtk.StateType.Normal));
		}

		public void Append (string tip)
		{
			uint row = table.NRows;

			Gtk.Image image = new Gtk.Image (arrow);
			image.Show ();
			table.Attach (image, 0, 1, row, row + 1, Gtk.AttachOptions.Fill, Gtk.AttachOptions.Fill, 0, 0);

			Gtk.Label label = new Gtk.Label ();
			label.Markup = tip;
			label.LineWrap = true;
			label.Justify = Justification.Fill;
			label.SetAlignment (0.0f, 0.5f);
			label.ModifyFg (Gtk.StateType.Normal, label.Style.Foreground (Gtk.StateType.Insensitive));
			label.Show ();
			table.Attach (label, 1, 2, row, row + 1, Gtk.AttachOptions.Expand | Gtk.AttachOptions.Fill, 0, 0, 0);
		}

		public void Append (Gtk.Widget widget, Gtk.AttachOptions x, Gtk.AttachOptions y)
		{
			uint row = table.NRows;
			table.Attach (widget, 0, 2, row, row + 1, x, y, 0, 0);
		}

		public void Append (Gtk.Widget widget)
		{
			Append (widget, 0, 0);
		}

		protected override void OnSizeRequested (ref Gtk.Requisition req)
		{
			req = table.SizeRequest ();
		}

		protected override void OnSizeAllocated (Gdk.Rectangle allocation)
		{
			base.OnSizeAllocated (allocation);

			Gtk.Requisition table_req = table.ChildRequisition;
			allocation.X = Math.Max ((allocation.Width - table_req.Width) / 2, 0);
			allocation.Y = Math.Max ((allocation.Height - table_req.Height) / 2, 0);
			allocation.Width = table_req.Width;
			allocation.Height = table_req.Height;

			table.SizeAllocate (allocation);
		}

		public Gdk.Pixbuf HeaderIcon {
			set { header_icon.Pixbuf = value; }
		}

		public string HeaderIconFromStock {
			set { header_icon.SetFromStock (value, Gtk.IconSize.Dialog); }
		}

		public string Header {
			set { header_label.Markup = "<big><b>" + value + "</b></big>"; }
		}
	}
}
