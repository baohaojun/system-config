//
// Timeline.cs: Store chronological events
//
// Copyright (C) 2004 Novell, Inc.
//
// AUTHORS:
//   Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Collections;

namespace Beagle.Util {

	public class Timeline {

		private DateTime TodaySpan;
		private DateTime YesterdaySpan;
		private DateTime ThisWeekSpan;
		private DateTime LastWeekSpan;

		private ArrayList today;
		private ArrayList yesterday;
		private ArrayList thisweek;
		private ArrayList lastweek;
		private ArrayList thismonth;
		private ArrayList thisyear;
		private ArrayList older;

		public Timeline ()
		{
			today = new ArrayList ();
			yesterday = new ArrayList ();
			thisweek = new ArrayList ();
			lastweek = new ArrayList ();
			thismonth = new ArrayList ();
			thisyear = new ArrayList ();
			older = new ArrayList ();

			UpdateTimespan ();
		}

		public void UpdateTimespan ()
		{
			TodaySpan = DateTime.Today;
			YesterdaySpan = TodaySpan.AddDays (-1);
			ThisWeekSpan = TodaySpan.AddDays (-((int)DateTime.Now.DayOfWeek));
			LastWeekSpan = ThisWeekSpan.AddDays (-7);
		}

		private class TimelineEvent {
			private DateTime timestamp;
			private object obj;

			public DateTime Timestamp {
				get { return timestamp; }
			}

			public object Object {
				get { return obj; }
			}

			public TimelineEvent (object obj, DateTime timestamp)
			{
				this.timestamp = timestamp;
				this.obj = obj;
			}
		}

		private class ReverseChronoComparer : IComparer {
			
			public int Compare (object x, object y)
			{
				return ((TimelineEvent) y).Timestamp.CompareTo (((TimelineEvent) x).Timestamp);
			}
		}

		private static ReverseChronoComparer rev_cmp = new ReverseChronoComparer ();

		private bool IsThisYear (TimelineEvent e)
		{
			if (e.Timestamp.Year == DateTime.Today.Year)
				return true;
			return false;
		}

		private bool IsThisMonth (TimelineEvent e)
		{
			if (IsThisYear (e) && e.Timestamp.Month == DateTime.Today.Month)
				return true;
			return false;
		}
		
		private bool IsLastWeek (TimelineEvent e)
		{
			if (e.Timestamp >= LastWeekSpan && e.Timestamp < ThisWeekSpan)
				return true;
			return false;
		}

		private bool IsThisWeek (TimelineEvent e)
		{
			if (e.Timestamp >= ThisWeekSpan)
				return true;
			return false;
		}

		private bool IsToday (TimelineEvent e)
		{
			if (IsThisYear (e) && e.Timestamp.DayOfYear == DateTime.Today.DayOfYear)
					return true;
			return false;
		}

		private bool IsYesterday (TimelineEvent e)
		{
			if (e.Timestamp >= YesterdaySpan)
				return true;
			return false;
		}

		public void Add (object obj, DateTime timestamp)
		{
			TimelineEvent te = new TimelineEvent (obj, timestamp);

			if (IsToday (te)) today.Add (te);
			else if (IsYesterday (te)) yesterday.Add (te);
			else if (IsThisWeek (te)) thisweek.Add (te);
			else if (IsLastWeek (te)) lastweek.Add (te);
			else if (IsThisMonth (te)) thismonth.Add (te);
			else if (IsThisYear (te)) thisyear.Add (te);
			else older.Add (te);
		}

		private ArrayList GetObjects (ArrayList list)
		{
			ArrayList sort = new ArrayList ();
                
			list.Sort (rev_cmp);
                
			foreach (TimelineEvent timeevent in list)
				sort.Add (timeevent.Object);

			return sort;
		}

		public ArrayList Today {
			get { return GetObjects (today); }
		}

		public ArrayList Yesterday {
			get { return GetObjects (yesterday); }
		}

		public ArrayList ThisWeek {
			get { return GetObjects (thisweek); }
		}

		public ArrayList LastWeek {
			get { return GetObjects (lastweek); }
		}

		public ArrayList ThisMonth {
			get { return GetObjects (thismonth); }
		}

		public ArrayList ThisYear {
			get { return GetObjects (thisyear); }
		}

		public ArrayList Older {
			get { return GetObjects (older); }
		}
	
	}

#if false
	public class Driver {
		public static void Main (string [] args)
		{
			Timeline t = new Timeline ();
			t.Add (DateTime.Now, DateTime.Now);
			t.Add (new DateTime (2004, 12, 25), new DateTime (2004, 12, 25)); //Yesterday
			t.Add (new DateTime (2004, 12, 12), new DateTime (2004, 12, 1)); //This Month
			t.Add (new DateTime (2004, 11, 9 ), new DateTime (2004, 11, 9));
			
			
			//DEBUG
			Console.WriteLine ("Today:");
			foreach (DateTime d in t.Today)
				Console.WriteLine ("\t" + d);
			
			Console.WriteLine ("Yesterday:");
			foreach (DateTime d in t.Yesterday)
				Console.WriteLine ("\t" + d);
			
			Console.WriteLine ("ThisWeek:");
			foreach (DateTime d in t.ThisWeek)
				Console.WriteLine ("\t" + d);
			
			Console.WriteLine ("ThisMonth:");
			foreach (DateTime d in t.ThisMonth)
				Console.WriteLine ("\t" + d);
			
			Console.WriteLine ("ThisYear:");
			foreach (DateTime d in t.ThisYear)
				Console.WriteLine ("\t" + d);
			
			Console.WriteLine ("Older:");
			foreach (DateTime d in t.Older)
			Console.WriteLine ("\t" + d);
			
		}
	}
#endif

}
