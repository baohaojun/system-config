//
// Category.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Collections.Generic;

using Gtk;
using Gdk;

using Mono.Unix;

using Beagle.Search.Tiles;

namespace Beagle.Search {

	public abstract class Category : Container {

		private SortedTileList tiles = null;
		private int page = 0;

		protected Gtk.HBox header;
		private Gtk.Label position;
		private Gtk.Button prev, next;
		private Gtk.Expander headerExpander;
		private int fewRows, manyRows, columns;
		private int few, many;
		private bool extended, expanded;
		private ScopeType scope;

		public Category (Tiles.TileGroupInfo info, int columns)
		{
			WidgetFlags |= WidgetFlags.NoWindow;

			header = new Gtk.HBox (false, 0);

			headerExpander = new Gtk.Expander ("<big><b>" + GLib.Markup.EscapeText (info.Name) + "</b></big>");
			((Gtk.Label) headerExpander.LabelWidget).SetAlignment (0.0f, 0.5f);
			headerExpander.UseMarkup = true;
			headerExpander.UseUnderline = true;
			headerExpander.Show ();
			header.PackStart (headerExpander, true, true, 0);

			headerExpander.Activated += OnActivated;
			
			scope = Tiles.Utils.TileGroupToScopeType(info.Group);
			
			position = new Gtk.Label ();
			position.ModifyFg (Gtk.StateType.Normal, position.Style.Base (Gtk.StateType.Selected));
			header.PackStart (position, false, false, 0);
			position.Show ();

			prev = MakeButton (header, Gtk.Stock.GoBack, OnPrev);
			next = MakeButton (header, Gtk.Stock.GoForward, OnNext);

			header.Show ();
			header.Parent = this;
			header.SizeRequested += HeaderSizeRequested;

			tiles = new SortedTileList (Beagle.Search.SortType.Relevance);
			page = 0;

			fewRows = info.Rows;
			manyRows = info.Rows * 2;
			Columns = columns;

			UpdateButtons ();
			//headerExpander.Expanded = true;
			Expanded = true;	
		}

		private Gtk.Button MakeButton (Gtk.HBox header, string icon, EventHandler handler)
		{
			Gtk.Button button = new Gtk.Button ();
			Gtk.Image img = new Gtk.Image (icon, Gtk.IconSize.Button);
			button.Add (img);
			button.Relief = Gtk.ReliefStyle.None;
			button.Clicked += handler;

			header.PackStart (button, false, false, 0);
			button.ShowAll ();

			return button;
		}

		public bool Expanded {
			get { return expanded; }
			set {
				expanded = value;

				if (expanded)
					ShowTiles (false);
				else
					HideTiles ();

				headerExpander.Expanded = expanded;
			}
		}

		protected int Columns {
			get { return columns; }
			set {
				HideTiles ();

				columns = value;
				few = fewRows * columns;
				many = manyRows * columns;

				ShowTiles (true);
			}
		}

		private void HeaderSizeRequested (object obj, Gtk.SizeRequestedArgs args)
		{
			Gtk.Requisition req = args.Requisition;
			Gtk.Requisition labelReq = headerExpander.ChildRequisition;

			req.Height = (int) (labelReq.Height * 1.5);

			int icon_height, icon_width;

			if (Gtk.Icon.SizeLookup (Gtk.IconSize.Button, out icon_height, out icon_width) && req.Height < icon_height * 1.5)
				req.Height = (int) (icon_height * 1.5);

			args.Requisition = req;
		}

		void UpdateButtons ()
		{
			if (tiles.Count <= FirstVisible && page > 0) {
				// The page we were viewing disappeared
				page--;
			}

			prev.Sensitive = (page != 0);
			next.Sensitive = (tiles.Count > LastVisible + 1);

			if (tiles.Count > 0) {
				if (FirstVisible == 0 && LastVisible + 1 == tiles.Count)
					position.LabelProp = String.Format (Catalog.GetPluralString ("{0} result", "{0} results", tiles.Count), tiles.Count);
				else
					position.LabelProp = String.Format (Catalog.GetString ("{0}-{1} of {2}"), FirstVisible + 1, LastVisible + 1, tiles.Count);
			} else
				position.LabelProp = "";
		}

		protected override void OnAdded (Gtk.Widget widget)
		{
			HideTiles ();

			widget.ChildVisible = false;			
			widget.Parent = this;

			tiles.Add ((Tiles.Tile)widget);

			if (Expanded)
				ShowTiles (true);
		}

		protected override void OnRemoved (Gtk.Widget widget)
		{
			HideTiles ();

			widget.Unparent ();

			tiles.Remove ((Tiles.Tile)widget);

			if (Expanded)
				ShowTiles (true);
		}

		private Tiles.Tile lastTarget;
		private bool hadFocus;

		void HideTiles ()
		{
			lastTarget = null;
			foreach (Tiles.Tile tile in VisibleTiles) {
				if (tile.HasFocus || lastTarget == null) {
					lastTarget = tile;
					hadFocus = tile.HasFocus;
				}
				tile.ChildVisible = false;
			}
			QueueResize ();
		}

		void ShowTiles (bool recenter)
		{
			if (recenter && lastTarget != null) {
				int index = tiles.IndexOf (lastTarget);
				if (hadFocus || page > 0) {
					if (index < few)
						page = 0;
					else if (extended)
						page = index / (manyRows * columns);
					else
						page = ((index - few) / (manyRows * columns)) + 1;
				}
			}

			foreach (Tiles.Tile tile in VisibleTiles) {
				tile.ChildVisible = true;
				if (tile == lastTarget && hadFocus && !tile.HasFocus)
					tile.GrabFocus ();
			}

			UpdateButtons ();
			QueueResize ();
		}

		private bool showingMany {
			get {
				// Show extra tiles on every page after the first, unless
				// there are only two pages and the second one only has
				// enough tiles to fit the "fewer" size.
				return (page > 0 && tiles.Count > 2 * few) || extended;
			}
		}

		void OnPrev (object obj, EventArgs args)
		{
			HideTiles ();
			page--;
			if (!Expanded)
				OnActivated (obj, args);
			else
				ShowTiles (false);
		}

		void OnNext (object obj, EventArgs args)
		{
			HideTiles ();
			page++;
			if (!Expanded)
				OnActivated (obj, args);
			else
				ShowTiles (false);
		}

		protected void OnActivated (object obj, EventArgs args)
		{
			Expanded = !Expanded;
			CategoryToggle (scope);
		}

		public delegate void CategoryToggleDelegate (ScopeType scope);
		public event CategoryToggleDelegate CategoryToggle;

		protected int PageSize {
			get {
				return Math.Min (showingMany ? many : few, tiles.Count);
			}
		}

		protected int FirstVisible {
			get {
				if (page == 0)
					return 0;
				else if (extended)
					return page * many;
				else
					return few + (page - 1) * many;
			}
		}

		protected int LastVisible {
			get {
				return Math.Min (FirstVisible + PageSize, tiles.Count) - 1;
			}
		}

		protected IList<Tile> VisibleTiles {
			get {
				return tiles.GetRange (FirstVisible, LastVisible - FirstVisible + 1);
			}
		}

		public IEnumerable<Tile> AllTiles {
			get {
				return tiles;
			}
		}

		protected override void ForAll (bool include_internals, Callback callback)
		{
			foreach (Widget w in (SortedTileList)tiles.Clone ())
				callback (w);
			if (include_internals)
				callback (header);
		}

		protected override bool OnExposeEvent (Gdk.EventExpose evt)
		{
			Rectangle headerRect, bgRect;
			int headerHeight = header.Allocation.Height;
			Gdk.Color white, bg, mid;

			headerRect = bgRect = Allocation;
			headerRect.Height = headerHeight;
			bgRect.Y += headerHeight;
			bgRect.Height -= headerHeight;

			GdkWindow.DrawRectangle (Style.BaseGC (State), true, bgRect);

			white = Style.Base (State);
			bg = Style.Background (State);

			mid = new Gdk.Color ();
			mid.Red = (ushort)((white.Red + bg.Red) / 2);
			mid.Green = (ushort)((white.Green + bg.Green) / 2);
			mid.Blue = (ushort)((white.Blue + bg.Blue) / 2);
			Style.BaseGC (State).RgbFgColor = mid;
			GdkWindow.DrawRectangle (Style.BaseGC (State), true, headerRect);
			Style.BaseGC (State).Foreground = white;

			return base.OnExposeEvent (evt);
		}

		public void Clear ()
		{
			while (tiles.Count > 0)
				Remove (tiles[tiles.Count - 1]);
		}

		public bool Empty {
			get { return tiles.Count == 0; }
		}

		public int Count {
			get { return tiles.Count; }
		}

		public SortType SortType {
			set {
				HideTiles ();
				tiles.SortType = value;
				ShowTiles (true);
			}
		}

		public void Select (bool focus, bool extended)
		{
			if (extended) {
				HideTiles ();
				this.extended = extended;
				ShowTiles (false);
			}
			if (focus && !Empty)
				((Gtk.Widget)VisibleTiles[0]).GrabFocus ();
		}
	}
}
