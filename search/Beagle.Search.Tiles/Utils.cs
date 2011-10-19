using System;
using System.Globalization;
using Mono.Unix;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public struct TileGroupInfo {
		public TileGroup Group;
		public string Name;
		public int Rows;

		public TileGroupInfo (TileGroup group, string name, int rows)
		{
			Group = group;
			Name = name;
			Rows = rows;
		}
	};

	public static class Utils {

		public static TileGroupInfo[] GroupInfo = new TileGroupInfo[] {
			new TileGroupInfo (TileGroup.Application,
					   Catalog.GetString ("Applications"), 1),
			new TileGroupInfo (TileGroup.Contact,
					   Catalog.GetString ("Contacts"), 2),
			new TileGroupInfo (TileGroup.Calendar,
					   Catalog.GetString ("Calendar Events"), 2),
			new TileGroupInfo (TileGroup.Folder,
					   Catalog.GetString ("Folders"), 2),
			new TileGroupInfo (TileGroup.Image,
					   Catalog.GetString ("Images"), 2),
			new TileGroupInfo (TileGroup.Audio,
					   Catalog.GetString ("Audio"), 2),
			new TileGroupInfo (TileGroup.Video,
					   Catalog.GetString ("Video"), 2),
			new TileGroupInfo (TileGroup.Documents,
					   Catalog.GetString ("Documents"), 2),
			new TileGroupInfo (TileGroup.Conversations,
					   Catalog.GetString ("Conversations"), 5),
			new TileGroupInfo (TileGroup.Website,
					   Catalog.GetString ("Websites"), 2),
			new TileGroupInfo (TileGroup.Feed,
					   Catalog.GetString ("News Feeds"), 2),
			new TileGroupInfo (TileGroup.Archive,
					   Catalog.GetString ("Archives"), 2),
		};

		public static ScopeType TileGroupToScopeType (TileGroup group)
		{
			switch (group) {

			case TileGroup.Application:
				return ScopeType.Applications;

			case TileGroup.Calendar:
				return ScopeType.Calendar;

			case TileGroup.Contact:
				return ScopeType.Contacts;

			// This TileGroup exists but does not seem to be used
			// case TileGroup.Folder:
			case TileGroup.Documents:
				return ScopeType.Documents;

			case TileGroup.Conversations:
				return ScopeType.Conversations;

			case TileGroup.Image:
				return ScopeType.Images;

			case TileGroup.Audio:
				return ScopeType.Media;
				
			case TileGroup.Video:
				return ScopeType.Media;

			case TileGroup.Folder:
				return ScopeType.Folders;
				
			case TileGroup.Website:
				return ScopeType.Websites;

			case TileGroup.Feed:
				return ScopeType.Feeds;

			case TileGroup.Archive:
				return ScopeType.Archives;
				
			}

			Console.WriteLine ("Error: Could not find ScopeType for Group: {0}",group);
			return ScopeType.Nothing;
		}

		public static string GetFirstPropertyOfParent (Beagle.Hit hit, string prop)
		{
			if (hit.ParentUri == null)
				return hit.GetFirstProperty (prop);
			else
				return hit.GetFirstProperty ("parent:" + prop);
		}

		public static string TrimFirstLine (string text)
		{
			int newline = text.IndexOf ('\n');

			if (newline == -1)
				return text;

			return String.Format ("{0}...", text.Substring (0, newline));
		}

		private static DateTimeFormatInfo DateTimeFormat = CultureInfo.CurrentCulture.DateTimeFormat;
		private static string ShortMonthDayPattern = DateTimeFormat.MonthDayPattern.Replace ("MMMM", "MMM");
		private static string ShortYearMonthPattern = DateTimeFormat.YearMonthPattern.Replace ("MMMM", "MMM");
		private static string MonthDayPattern = DateTimeFormat.MonthDayPattern;
		private static string LongDatePattern = DateTimeFormat.LongDatePattern.Replace ("dddd, ", "").Replace ("dddd ", "").Replace (" dddd", "");

		private static string NiceDatePattern (DateTime dt, string month_day_pattern, string year_month_pattern)
		{
			if (dt.Year <= 1970)
				return "-";

			dt = dt.ToLocalTime ().Date;
			DateTime today = DateTime.Today;
			TimeSpan one_day = new TimeSpan (TimeSpan.TicksPerDay);

			if (dt == today - one_day)
				return Catalog.GetString ("Yesterday");
			else if (dt == today)
				return Catalog.GetString ("Today");
			else if (dt == today + one_day)
				return Catalog.GetString ("Tomorrow");

			TimeSpan span;

			if (today > dt)
				span = today - dt;
			else
				span = dt - today;

			if (span.TotalDays < 7)
				return dt.ToString ("dddd"); // "Tuesday"
			else if (dt.Year == today.Year || span.TotalDays < 180)
				return dt.ToString (month_day_pattern);
			else
				return dt.ToString (year_month_pattern);
		}

		public static string NiceShortDate (string timestamp)
		{
			DateTime dt;

			try {
				dt = StringFu.StringToDateTime (timestamp);
			} catch {
				return "";
			}

			return NiceShortDate (dt);
		}

		public static string NiceShortDate (DateTime dt)
		{
			// "Jul 4" and "Jan 2001", respectively.
			return NiceDatePattern (dt, ShortMonthDayPattern, ShortYearMonthPattern);
		}

		public static string NiceLongDate (string timestamp)
		{
			DateTime dt;

			try {
				dt = StringFu.StringToDateTime (timestamp);
			} catch {
				return "";
			}

			return NiceLongDate (dt);
		}

		public static string NiceLongDate (DateTime dt)
		{
			// "July 4" and "January 7, 2001", respectively.
			return NiceDatePattern (dt, MonthDayPattern, LongDatePattern);
		}

		public static string NiceVeryLongDate (string timestamp)
		{
			DateTime dt;

			try {
				dt = StringFu.StringToDateTime (timestamp);
			} catch {
				return "";
			}

			return NiceVeryLongDate (dt);
		}

		public static string NiceVeryLongDate (DateTime dt)
		{
			if (dt.Year <= 1970)
				return "-";

			dt = dt.ToLocalTime ().Date;
			DateTime today = DateTime.Today;
			TimeSpan one_day = new TimeSpan (TimeSpan.TicksPerDay);

			if (dt == today - one_day)
				return Catalog.GetString ("Yesterday");
			else if (dt == today)
				return Catalog.GetString ("Today");
			else if (dt == today + one_day)
				return Catalog.GetString ("Tomorrow");

			TimeSpan span;
			bool future;

			if (today > dt) {
				span = today - dt;
				future = false;
			} else {
				span = dt - today;
				future = true;
			}

			if (span.TotalDays < 7)
				return dt.ToString ("dddd"); // "Tuesday"
			else if (span.TotalDays < 30 && ! future)
				return String.Format (Catalog.GetPluralString ("{0} week ago", "{0} weeks ago", span.Days / 7) + " ({1:MMMM d, yyyy})", span.Days / 7, dt);
			else if (span.TotalDays < 30 && future)
				return String.Format (Catalog.GetPluralString ("In {0} week", "In {0} weeks", span.Days / 7) + " {1:MMMM d, yyyy})", span.Days / 7, dt);
			else if (span.TotalDays < 365 + 180 && ! future) // Let's say a year and a half to stop saying months
				return String.Format (Catalog.GetPluralString ("{0} month ago", "{0} months ago", span.Days / 30) + " ({1:MMMM d, yyyy})", span.Days / 30, dt);
			else if (span.TotalDays < 365 + 180 && future)
				return String.Format (Catalog.GetPluralString ("In {0} month", "In {0} months", span.Days / 30) + " ({1:MMMM d, yyyy})", span.Days / 30, dt);
			else if (! future)
				return String.Format (Catalog.GetPluralString ("{0} year ago", "{0} years ago", span.Days / 365) + " ({1:MMMM d, yyyy})", span.Days / 365, dt);
			else
				return String.Format (Catalog.GetPluralString ("In {0} year", "In {0} years", span.Days / 365) + " ({1:MMMM d, yyyy})", span.Days / 365, dt);
		}

		public static string NiceShortTime (DateTime dt)
		{
			return StringFu.DateTimeToPrettyString (dt.ToLocalTime ());
		}

		public static string NiceShortTime (string timestamp)
		{
			DateTime dt;

			try {
				dt = StringFu.StringToDateTime (timestamp);

			} catch {
				return "";
			}

			return NiceShortTime (dt);
		}
	}
}
