using Gtk;
using System;
using Mono.Unix;

namespace Beagle.Search.Pages {

	public class RootUser : Base {

		public RootUser ()
		{
			HeaderIconFromStock = Stock.DialogError;
			Header = Catalog.GetString ("Beagle cannot be run as root");

			Append (Catalog.GetString ("For security reasons, Beagle cannot be run as root. " +
						   "You should restart as a regular user."));
		}
	}
}
