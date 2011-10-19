using System;

namespace Beagle.Search.Tiles {

	public delegate void TileActionDelegate ();

	public class TileAction {

		public TileAction (string name, TileActionDelegate action)
		{
			this.name = name;
			this.action = action;
		}

		public TileAction (string name, string stock, TileActionDelegate action)
		{
			this.name = name;
			this.stock = stock;
			this.action = action;
		}

		private string name;
		public string Name {
			get { return name; }
			set { name = value; }
		}

		private TileActionDelegate action;
		public TileActionDelegate Action {
			get { return action; }
			set { action = value; }
		}

		private string stock;
		public string Stock {
			get { return stock; }
			set { stock = value; }
		}
	}
}
