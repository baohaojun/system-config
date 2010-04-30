using System;
using System.Collections;

using Gtk;
using Mono.Posix;

using Beagle;
using Beagle.Search.Tiles;

namespace Beagle.Search {

	public delegate void TileHandler (Tile tile);

	public enum MatchType {
		None,
		NoneInScope,
		Matched
	}

	public class GroupView : VBox {

		public delegate void CategoryToggledDelegate (ScopeType catScope);

		private Hashtable categories;
		private Gtk.SizeGroup tileSizeGroup;
		private Gtk.Widget selection;

		public event CategoryToggledDelegate CategoryToggled;
		public event TileHandler TileSelected;

		public GroupView () : base (false, 0)
		{
			categories = new Hashtable ();
			tileSizeGroup = new Gtk.SizeGroup (Gtk.SizeGroupMode.Both);			

			foreach (TileGroupInfo info in Utils.GroupInfo) {
				Category box = null;
								
				if (info.Group == TileGroup.Conversations)
					box = new ListCategory (info);
				else
					box = new TileCategory (info, tileSizeGroup);

				box.NoShowAll = true;
				box.CategoryToggle += OnCategoryToggle;
				PackStart (box, false, false, 0);

				categories [info.Group] = box;
			}
		}
		
		public void AddHit (Tile tile)
		{
			tile.Show ();
			tile.Selected += OnTileSelected;

			Category box = (Category)categories [tile.Group];
			box.Add (tile);

			if (GroupInScope (tile.Group))
				box.Show ();
		}

		public void SubtractHit (Uri uri)
		{
			foreach (Category box in categories.Values) {
				foreach (Tile tile in box.AllTiles) {
					if (tile.Hit.Uri.Equals (uri)) {
						if (tile.State == StateType.Selected)
							OnTileSelected (null, EventArgs.Empty);

						box.Remove (tile);

						if (box.Count < 1)
							box.Hide ();

						return;
					}
				}
			}
		}

		public void Finished (bool grabFocus)
		{
			Category first = null;
			bool others = false;

			foreach (Category category in categories.Values) {
				if (category.Visible) {
					if (first == null)
						first = category;
					else {
						others = true;
						break;
					}
				}
			}

			if (first != null)
				first.Select (grabFocus, !others);
		}

		public int TileCount {
			get {
				int count = 0;

				foreach (Category category in categories.Values) {
					if (category.Visible)
						count += category.Count;
				}

				return count;
			}
		}

		public MatchType MatchState {
			get {
				bool hiddenMatches = false;
				foreach (Category category in categories.Values) {
					if (category.Visible)
						return MatchType.Matched;
					else if (!category.Empty)
						hiddenMatches = true;
				}

				return hiddenMatches ? MatchType.NoneInScope : MatchType.None;
			}
		}

		private void OnTileSelected (object tile, EventArgs args)
		{
			if (tile == selection)
				return;

			if (TileSelected != null)
				TileSelected ((Tile)tile);

			if (selection != null)
				selection.State = StateType.Normal;

			selection = (Gtk.Widget)tile;

			if (selection != null)
				selection.State = StateType.Selected;
		}

		private ScopeType scope;
		public ScopeType Scope {
			set {
				scope = value;
				foreach (TileGroup group in categories.Keys) {
					Category category = (Category)categories[group];
					if (!GroupInScope (group))
						category.Expanded = false;
					else if (!category.Empty)
						category.Expanded = true;
				}
			}
		}

		private bool GroupInScope (TileGroup group)
		{
			ScopeType scopetype = Utils.TileGroupToScopeType (group);
			return (scope & scopetype) == scopetype;
		}

		public SortType SortType {
			set {
				foreach (Category category in categories.Values)
					category.SortType = value;
			}
		}

		public void Clear ()
		{
			foreach (Category box in categories.Values) {
				box.Clear ();
				box.Hide ();
			}
		}

		private void OnCategoryToggle (ScopeType category_scope)
		{
			// we're not using the set function cause we directly
			// close/open the expander in Category.cs
			scope = scope ^ category_scope;
			
			if (CategoryToggled != null)
				CategoryToggled (category_scope);
		}
	}
}
