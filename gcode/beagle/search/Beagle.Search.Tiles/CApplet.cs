using System;
using System.Collections;
using System.Diagnostics;
using Mono.Posix;

namespace Beagle.Search.Tiles {

	public class CAppletActivator : TileActivator {

		public CAppletActivator () : base ()
		{
			AddSupportedFlavor (new HitFlavor (null, null, "application/x-desktop"));
		}

		public override Tile BuildTile (Beagle.Hit hit, Beagle.Query query)
		{
			return new CApplet (hit, query);
		}

		public override bool Validate (Beagle.Hit hit)
		{
			if (! base.Validate (hit))
				return false;

			ICollection categories = hit.GetProperties ("fixme:Categories");

			if (categories == null || categories.Count < 1)
				return false;

			foreach (string cat in categories) {
				if (cat == "Settings") {
					Weight += 1;
					return true;
				}
			}
			
			return false;
		}
	}

	public class CApplet : Application {

		public CApplet (Beagle.Hit hit, Beagle.Query query) : base (hit, query) {}

		public override void Open ()
		{
			Process p = new Process ();
			p.StartInfo.UseShellExecute = true;
			p.StartInfo.FileName = Hit ["fixme:Exec"];
			
			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Unable to run {0}: {1}", p.StartInfo.FileName, e.Message);
			}
		}
	}
}
