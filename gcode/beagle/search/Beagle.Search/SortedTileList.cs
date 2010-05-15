//
// SortedTileList.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Collections;
using System.Collections.Generic;

using Beagle.Search.Tiles;

namespace Beagle.Search {

	public class SortedTileList : IEnumerable<Tile> {

		private List<Tile> tiles = null;

		private SortType sort_type;
		private TileComparer comparer = null;

		public SortedTileList (SortType sort)
		{
			this.tiles = new List<Tile> ();
			this.SortType = sort;
		}

		public SortedTileList (SortType sort, Tile[] tiles_array)
		{
			this.tiles = new List<Tile> (tiles_array);
			this.SortType = sort;
		}

		public int Add (Tile tile)
		{
			int index = tiles.BinarySearch (tile, comparer);

			if (index >= 0)
				throw new ArgumentException ("duplicate");

			tiles.Insert (~index, tile);

			return ~index;
		}

		public void Clear ()
		{
			tiles.Clear ();
		}

		public bool Contains (Tile tile)
		{
			return tiles.Contains (tile);
		}

		public int IndexOf (Tile tile)
		{
			return tiles.IndexOf (tile);
		}

		public void Remove (Tile tile)
		{
			int index = tiles.BinarySearch (tile, comparer);
			
			if (index >= 0)
				tiles.RemoveAt (index);
		}

		public void RemoveAt (int index)
		{
			tiles.RemoveAt (index);
		}

		public Tile this [int index] {
			get { return tiles[index]; }
		}

		public int Count {
			get { return tiles.Count; }
		}

		public IEnumerator<Tile> GetEnumerator ()
		{
			return tiles.GetEnumerator ();
		}


		IEnumerator IEnumerable.GetEnumerator() {
			return GetEnumerator();
		}
		
		public object Clone ()
		{
			return new SortedTileList (sort_type, tiles.ToArray ());
		}

		public IList<Tile> GetRange (int index, int count)
		{
			return tiles.GetRange (index, count);
		}

		public SortType SortType {
			get { return sort_type; }
			set {
				sort_type = value;

				switch (sort_type) {
					case SortType.Relevance:
					default:
						comparer = new RelevanceComparer ();
					break;
					case SortType.Name:
						comparer = new NameComparer ();
					break;
					case SortType.Modified:
						comparer = new DateComparer ();
					break;
				}

				tiles.Sort (comparer);
			}
		}
	
		private abstract class TileComparer : IComparer<Tile> {
			
			public int Compare (Tile x, Tile y)
			{
				int ret = TileCompare (x, y);
				
				if (ret == 0)
					ret = -x.Timestamp.CompareTo (y.Timestamp);
				
				if (ret == 0)
					ret = x.GetHashCode ().CompareTo (y.GetHashCode ());
				
				return ret;
			}
			
			public abstract int TileCompare (Tile x, Tile y);
		}
		
		private class RelevanceComparer : TileComparer {
			
			public override int TileCompare (Tile x, Tile y)
			{
				return -x.Score.CompareTo (y.Score);
			}
		}
		
		private class NameComparer : TileComparer {
			
			public override int TileCompare (Tile x, Tile y)
			{
				return String.Compare (x.Title, y.Title, true);
			}
		}
		
		private class DateComparer : TileComparer {
			
			public override int TileCompare (Tile x, Tile y)
			{
				return -x.Timestamp.CompareTo (y.Timestamp);
			}
		}
	}
}

