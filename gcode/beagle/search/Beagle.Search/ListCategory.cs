using Gtk;
using Gdk;
using System;
using System.Collections;

namespace Beagle.Search {

	public class ListCategory : Category {

		private Gtk.SizeGroup col1, col2, col3;

		public ListCategory (Tiles.TileGroupInfo info) : base (info, 1)
		{
			col1 = new Gtk.SizeGroup (Gtk.SizeGroupMode.Horizontal);
			col2 = new Gtk.SizeGroup (Gtk.SizeGroupMode.Horizontal);
			col3 = new Gtk.SizeGroup (Gtk.SizeGroupMode.Horizontal);
		}

		protected override void OnAdded (Gtk.Widget widget)
		{
			base.OnAdded (widget);

			Tiles.TileFlat tile = widget as Tiles.TileFlat;
			if (tile != null) {
				col1.AddWidget (tile.SubjectLabel);
				col2.AddWidget (tile.FromLabel);
				col3.AddWidget (tile.DateLabel);
			}
		}

		protected override void OnSizeRequested (ref Requisition req)
		{
			Requisition headerReq, tileReq;

			headerReq = header.SizeRequest ();

			tileReq.Width = tileReq.Height = 0;
			foreach (Widget w in AllTiles) {
				tileReq = w.SizeRequest ();
				req.Width = Math.Max (req.Width, tileReq.Width);
				req.Height = Math.Max (req.Height, tileReq.Height);
			}

			req.Height = (Expanded) ?
			       	headerReq.Height + PageSize * tileReq.Height :
				headerReq.Height + 2;

			req.Width = Math.Max (headerReq.Width + headerReq.Height,
					      tileReq.Width + 2 * headerReq.Height);

			req.Width += (int)(2 * BorderWidth);
			req.Height += (int)(2 * BorderWidth);
		}

		protected override void OnSizeAllocated (Rectangle allocation)
		{
			Requisition headerReq;
			Rectangle childAlloc;

			base.OnSizeAllocated (allocation);

			headerReq = header.ChildRequisition;

			childAlloc.X = allocation.X + (int)BorderWidth;
			childAlloc.Width = allocation.Width - (int)BorderWidth;
			childAlloc.Y = allocation.Y + (int)BorderWidth;
			childAlloc.Height = headerReq.Height;
			header.Allocation = childAlloc;

			// Indent the tiles by the same distance as the height of the header.
			childAlloc.X += headerReq.Height;
			childAlloc.Width -= headerReq.Height;

			foreach (Widget w in VisibleTiles) {
				childAlloc.Y += childAlloc.Height;
				childAlloc.Height = w.ChildRequisition.Height;
				w.Allocation = childAlloc;
			}
		}
	}
}
