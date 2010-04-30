using Gtk;
using Gdk;
using System;
using System.Collections;
using System.Collections.Generic;

using Beagle.Search.Tiles;

namespace Beagle.Search {

	public class TileCategory : Category {

		Gtk.SizeGroup size_group;

		public TileCategory (Tiles.TileGroupInfo info, Gtk.SizeGroup size_group) : base (info, 2)
		{
			this.size_group = size_group;
		}

		protected override void OnAdded (Gtk.Widget widget)
		{
			size_group.AddWidget (widget);
			base.OnAdded (widget);
		}

		protected override void OnRemoved (Gtk.Widget widget)
		{
			base.OnRemoved (widget);
			size_group.RemoveWidget (widget);
		}

		protected override void OnSizeRequested (ref Requisition req)
		{
			Requisition headerReq, tileReq;

			req.Height = req.Width = 0;
			headerReq = header.SizeRequest ();

			tileReq.Width = tileReq.Height = 0;
			foreach (Widget w in AllTiles) {
				tileReq = w.SizeRequest ();
				req.Width = Math.Max (req.Width, tileReq.Width);
				req.Height = Math.Max (req.Height, tileReq.Height);
			}

			// req is now the max width/height of a single tile. Indent
			// req.Width, and use that as our width request, so that the
			// minimum width you can resize the category to is wide enough to
			// fit a whole column. But request a req.Height that is only tall
			// enough to fit PageSize tiles if we get the number of columns
			// we'd wanted. (OnSizeAllocated will force a recalculation with
			// fewer columns if we don't get enough width.)
			req.Width += 2 * headerReq.Height;
			req.Height *= (PageSize + Columns - 1) / Columns;

			if (!Expanded)
				req.Height = 2;  // keep a thin line of background.
			
			// Add height for the header, and update the width if the header
			// is wider than the tile area

			req.Height += headerReq.Height;
			req.Width = Math.Max (req.Width, headerReq.Width);

			// Handle BorderWidth
			req.Width += (int)(2 * BorderWidth);
			req.Height += (int)(2 * BorderWidth);
		}

		protected override void OnSizeAllocated (Rectangle allocation)
		{
			Requisition headerReq, tileReq;
			Rectangle childAlloc;
			int col, i, tilesWidth, maxcols;
			IList<Tile> tiles = VisibleTiles;

			base.OnSizeAllocated (allocation);

			headerReq = header.ChildRequisition;

			childAlloc.X = allocation.X + (int)BorderWidth;
			childAlloc.Width = allocation.Width - (int)BorderWidth;
			childAlloc.Y = allocation.Y + (int)BorderWidth;
			childAlloc.Height = headerReq.Height;
			header.Allocation = childAlloc;

			if (tiles.Count == 0)
				return;

			tileReq = ((Gtk.Widget)tiles[0]).ChildRequisition;
			if (tileReq.Width == 0)
				return;

			tilesWidth = allocation.Width - (int)(2 * BorderWidth) - headerReq.Height;
			maxcols = tilesWidth / tileReq.Width;
			if (maxcols != Columns) {
				Columns = maxcols;
				QueueResize ();
				return;
			}

			childAlloc.X += headerReq.Height;
			childAlloc.Y += childAlloc.Height;
			childAlloc.Width = tileReq.Width;
			childAlloc.Height = tileReq.Height;

			for (i = col = 0; i < tiles.Count; i++) {
				((Gtk.Widget)tiles[i]).Allocation = childAlloc;

				col = (col + 1) % Columns;
				if (col == 0) {
					childAlloc.X = (int)BorderWidth + headerReq.Height;
					childAlloc.Y += childAlloc.Height;
				} else
					childAlloc.X += childAlloc.Width;
			}
		}
	}
}
