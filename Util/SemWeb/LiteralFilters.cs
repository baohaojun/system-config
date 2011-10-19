using System;
using System.Collections;

namespace SemWeb {
	using SemWeb.Filters;
	
	public abstract class LiteralFilter {
		public abstract bool Filter(Literal value, SelectableSource targetModel);
	
		public static LiteralFilter Create(CompType type, object value) {
			if (value is string)
				return new StringCompareFilter((string)value, type);
			if (value is decimal
				|| value is byte || value is sbyte
				|| value is short || value is ushort
				|| value is int || value is uint
				|| value is long || value is ulong
				|| value is float || value is double)
				return new NumericCompareFilter(((IConvertible)value).ToDecimal(null), type);
			if (value is DateTime)
				return new DateTimeCompareFilter((DateTime)value, type);
			if (value is TimeSpan)
				return new TimeSpanCompareFilter((TimeSpan)value, type);
			
			throw new ArgumentException("Invalid type: " + value.GetType());
		}
		
		internal bool CompareFilter(int cmp, CompType type) {
			switch (type) {
			case CompType.LT: return cmp < 0;
			case CompType.LE: return cmp <= 0;
			case CompType.NE: return cmp != 0;
			case CompType.EQ: return cmp == 0;
			case CompType.GT: return cmp > 0;
			case CompType.GE: return cmp >= 0;
			default: throw new ArgumentException(type.ToString());
			}
		}
		
		public enum CompType {
			LT, LE, NE, EQ, GT, GE
		}
		
		public static bool MatchesFilters(Resource literal, LiteralFilter[] filters, SelectableSource targetModel) {
			if (literal is Literal)
				return MatchesFilters((Literal)literal, filters, targetModel);
			return false;
		}
		
		private static bool MatchesFilters(Literal literal, LiteralFilter[] filters, SelectableSource targetModel) {
			foreach (LiteralFilter filter in filters)
				if (!filter.Filter(literal, targetModel))
					return false;
			return true;
		}
	}
}

namespace SemWeb.Filters {
	public class FilterSink : StatementSink {
		LiteralFilter[] filters;
		StatementSink sink;
		SelectableSource model;
		public FilterSink(LiteralFilter[] filters, StatementSink sink, SelectableSource model) {
			this.filters = filters;
			this.sink = sink;
			this.model = model;
		}
		public bool Add(Statement s) {
			if (filters != null && filters.Length > 0
				&& !LiteralFilter.MatchesFilters(s.Object, filters, model))
				return true;
			return sink.Add(s);
		}
	}

	public class StringCompareFilter : LiteralFilter {
		public readonly string Pattern;
		public readonly CompType Type;
		public StringCompareFilter(string pattern, CompType type) {
			Pattern = pattern;
			Type = type;
		}
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			int c = v.CompareTo(Pattern);
			return CompareFilter(c, Type);
		}
	}	

	public class StringContainsFilter : LiteralFilter {
		public readonly string Pattern;
		public StringContainsFilter(string pattern) {
			Pattern = pattern;
		}
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			return v.IndexOf(Pattern) != -1;
		}
	}
	
	public class StringStartsWithFilter : LiteralFilter {
		public readonly string Pattern;
		public StringStartsWithFilter(string pattern) {
			Pattern = pattern;
		}
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			return v.StartsWith(Pattern);
		}
	}

	public class StringEndsWithFilter : LiteralFilter {
		public readonly string Pattern;
		public StringEndsWithFilter(string pattern) {
			Pattern = pattern;
		}
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			return v.EndsWith(Pattern);
		}
	}

	public class NumericCompareFilter : LiteralFilter {
		public readonly Decimal Number;
		public readonly CompType Type;

		public NumericCompareFilter(Decimal number, CompType type) {
			Number = number;
			Type = type;
		}
		
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			try {
				Decimal i = Decimal.Parse(v);
				int c = i.CompareTo(Number);
				return CompareFilter(c, Type);
			} catch (Exception) {
				return false;
			}
		}
	}

	public class DateTimeCompareFilter : LiteralFilter {
		public readonly DateTime Value;
		public readonly CompType Type;
		
		public DateTimeCompareFilter(DateTime datetime, CompType type) {
			Value = datetime;
			Type = type;
		}
		
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			try {
				DateTime i = DateTime.Parse(v);
				int c = i.CompareTo(Value);
				return CompareFilter(c, Type);
			} catch (Exception) {
				return false;
			}
		}
	}
	
	public class TimeSpanCompareFilter : LiteralFilter {
		public readonly TimeSpan Value;
		public readonly CompType Type;
		
		public TimeSpanCompareFilter(TimeSpan timespan, CompType type) {
			Value = timespan;
			Type = type;
		}
		
		public override bool Filter(Literal resource, SelectableSource targetModel) {
			string v = resource.Value;
			try {
				TimeSpan i = TimeSpan.Parse(v);
				int c = i.CompareTo(Value);
				return CompareFilter(c, Type);
			} catch (Exception) {
				return false;
			}
		}
	}	
}
