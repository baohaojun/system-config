using Gtk;
using System;
using Mono.Unix;

namespace Beagle.Search.Pages {

	public class QuickTips : Base {

		private static string[] tips = new string[] {
			Catalog.GetString ("You can use upper and lower case; search is case-insensitive."),
			Catalog.GetString ("To search for optional terms, use OR.  ex: <b>George OR Ringo</b>"),
			Catalog.GetString ("To exclude search terms, use the minus symbol in front, such as <b>-cats</b>"),
			Catalog.GetString ("When searching for a phrase, add quotes. ex: <b>\"There be dragons\"</b>"),
			Catalog.GetString ("Specify the extension to search among files with that extension. ex: <b>bean soup .html</b>")
		};

		public QuickTips ()
		{
			HeaderIcon = WidgetFu.LoadThemeIcon ("dialog-information", 48);
			Header = Catalog.GetString ("Quick Tips");
			
			foreach (string tip in tips)
				Append (tip);
		}
	}
}
