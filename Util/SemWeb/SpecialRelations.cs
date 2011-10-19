using System;

using SemWeb;

namespace SemWeb.Inference {
	public abstract class RdfRelation : SemWeb.Query.RdfFunction {
		public override Resource Evaluate (Resource[] args) {
			Resource r = null;
			if (Evaluate(args, ref r))
				return r;
			return null;
		}

		public abstract bool Evaluate(Resource[] args, ref Resource @object);
	}
	
	namespace Relations {
		#if !SILVERLIGHT
		internal abstract class MathUnaryRelation : RdfRelation {
			protected abstract Decimal EvaluateForward(Decimal left);
			protected abstract Decimal EvaluateReverse(Decimal right);
		
			public override bool Evaluate(Resource[] args, ref Resource @object) {
				if (args.Length != 1) return false;
				if (args[0] == null && @object == null) return false;
				if ((args[0] != null && !(args[0] is Literal)) || (@object != null && !(@object is Literal))) return false;

				try {
				
				if (args[0] == null) {
					Decimal right = (Decimal)Convert.ChangeType( ((Literal)@object).ParseValue() , typeof(Decimal) );
					Decimal left = EvaluateReverse(right);
					if (left == Decimal.MinValue) return false;
					args[0] = Literal.FromValue(left);
					return true;
				} else {
					Decimal left = (Decimal)Convert.ChangeType( ((Literal)args[0]).ParseValue() , typeof(Decimal) );
					Decimal right  = EvaluateForward(left);
					if (@object == null) {
						@object = Literal.FromValue(right);
						return true;
					} else {
						Decimal right2 = (Decimal)Convert.ChangeType( ((Literal)@object).ParseValue() , typeof(Decimal) );
						return right == right2;
					}
				}
				
				} catch (FormatException) {
					return false;
				}
			}
		}
	
		internal class MathAbsoluteValueRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#absoluteValue"; } }
			protected override Decimal EvaluateForward(Decimal left) { return left >= 0 ? left : -left; }
			protected override Decimal EvaluateReverse(Decimal right) { return Decimal.MinValue; }
		}
		internal class MathCosRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#cos"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)Math.Cos((double)left); }
			protected override Decimal EvaluateReverse(Decimal right) { return (Decimal)Math.Acos((double)right); }
		}
		internal class MathDegreesRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#degrees"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)((double)left * Math.PI / 180.0); }
			protected override Decimal EvaluateReverse(Decimal right) { return (Decimal)((double)right * 180.0 / Math.PI); }
		}
		internal class MathEqualToRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#equalTo"; } }
			protected override Decimal EvaluateForward(Decimal left) { return left; }
			protected override Decimal EvaluateReverse(Decimal right) { return right; }
		}
		internal class MathNegationRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#negation"; } }
			protected override Decimal EvaluateForward(Decimal left) { return -left; }
			protected override Decimal EvaluateReverse(Decimal right) { return -right; }
		}
		internal class MathRoundedRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#rounded"; } }
			protected override Decimal EvaluateForward(Decimal left) { return Decimal.Floor(left); }
			protected override Decimal EvaluateReverse(Decimal right) { return Decimal.MinValue; }
		}
		internal class MathSinRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#sin"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)Math.Sin((double)left); }
			protected override Decimal EvaluateReverse(Decimal right) { return (Decimal)Math.Asin((double)right); }
		}
		internal class MathSinhRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#sinh"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)Math.Sinh((double)left); }
			protected override Decimal EvaluateReverse(Decimal right) { return Decimal.MinValue; }
		}
		internal class MathTanRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#tan"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)Math.Tan((double)left); }
			protected override Decimal EvaluateReverse(Decimal right) { return (Decimal)Math.Atan((double)right); }
		}
		internal class MathTanhRelation : MathUnaryRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#tanh"; } }
			protected override Decimal EvaluateForward(Decimal left) { return (Decimal)Math.Tanh((double)left); }
			protected override Decimal EvaluateReverse(Decimal right) { return Decimal.MinValue; }
		}

		internal abstract class MathPairRelation : RdfRelation {
			protected abstract Decimal Evaluate(Decimal left, Decimal right);
		
			public override bool Evaluate(Resource[] args, ref Resource @object) {
				if (args.Length != 2) return false;
				if (args[0] == null || !(args[0] is Literal)) return false;
				if (args[1] == null || !(args[1] is Literal)) return false;

				try {
				
				Decimal left = (Decimal)Convert.ChangeType( ((Literal)args[0]).ParseValue() , typeof(Decimal) );
				Decimal right = (Decimal)Convert.ChangeType( ((Literal)args[1]).ParseValue() , typeof(Decimal) );
				Resource newvalue = Literal.FromValue(Evaluate(left, right));
				if (@object == null) {
					@object = newvalue;
					return true;
				} else {
					return @object.Equals(newvalue);
				}

				} catch (FormatException) {
					return false;
				}
			}
		}

		internal class MathAtan2Relation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#atan2"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return (Decimal)Math.Atan2((double)left, (double)right); }
		}
		internal class MathDifferenceRelation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#difference"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return left - right; }
		}
		internal class MathExponentiationRelation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#exponentiation"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return (Decimal)Math.Pow((double)left, (double)right); }
		}
		internal class MathIntegerQuotientRelation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#integerQuotient"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return Decimal.Floor((left / right)); }
		}
		internal class MathQuotientRelation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#quotient"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return left / right; }
		}
		internal class MathRemainderRelation : MathPairRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#remainder"; } }
			protected override Decimal Evaluate(Decimal left, Decimal right) { return left % right; }
		}

		internal abstract class MathListRelation : RdfRelation {
			protected abstract Decimal InitialValue { get; }
			protected abstract Decimal Combine(Decimal left, Decimal right);
		
			public override bool Evaluate(Resource[] args, ref Resource @object) {
				Decimal sum = InitialValue;
				foreach (Resource r in args) {
					if (r == null) return false;
					if (!(r is Literal)) return false;
					try {
						Decimal v = (Decimal)Convert.ChangeType( ((Literal)r).ParseValue() , typeof(Decimal) );
						sum = Combine(sum, v);
					} catch (FormatException) {
						return false;
					}
				}
				Resource newvalue = Literal.FromValue(sum);
				if (@object == null) {
					@object = newvalue;
					return true;
				} else {
					return @object.Equals(newvalue);
				}
			}
		}
	
		internal class MathSumRelation : MathListRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#sum"; } }
			protected override Decimal InitialValue { get { return Decimal.Zero; } }
			protected override Decimal Combine(Decimal left, Decimal right) { return left + right; }
		}
		internal class MathProductRelation : MathListRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#product"; } }
			protected override Decimal InitialValue { get { return Decimal.One; } }
			protected override Decimal Combine(Decimal left, Decimal right) { return left * right; }
		}
		
		internal abstract class MathComparisonRelation : RdfRelation {
			public override Resource Evaluate (Resource[] args) {
				if (args.Length != 2)
					throw new InvalidOperationException("This relation takes two arguments.");
					
				Resource left = args[0];
				Resource right = args[1];
				bool result = Evaluate(new Resource[] { left }, ref right);
				return Literal.FromValue(result);
			}
			
			public abstract bool Evaluate(Decimal left, Decimal right);
		
			public override bool Evaluate(Resource[] args, ref Resource @object) {
				if (args.Length != 1) return false;
				if (args[0] == null || @object == null) return false;
				if (!(args[0] is Literal) || !(@object is Literal)) return false;
				
				try {
					Decimal left = (Decimal)Convert.ChangeType( ((Literal)args[0]).ParseValue() , typeof(Decimal) );
					Decimal right = (Decimal)Convert.ChangeType( ((Literal)@object).ParseValue() , typeof(Decimal) );
					return Evaluate(left, right);
				} catch (FormatException) {
					return false;
				}				
			}
		}

		internal class MathGreaterThanRelation : MathComparisonRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#greaterThan"; } }
			public override bool Evaluate(Decimal left, Decimal right) {
				return left > right;
			}
		}
		internal class MathLessThanRelation : MathComparisonRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#lessThan"; } }
			public override bool Evaluate(Decimal left, Decimal right) {
				return left < right;
			}
		}
		internal class MathNotGreaterThanRelation : MathComparisonRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#notGreaterThan"; } }
			public override bool Evaluate(Decimal left, Decimal right) {
				return !(left > right);
			}
		}
		internal class MathNotLessThanRelation : MathComparisonRelation {
			// NOTE: The schema lists this as "notlessThan" with a lowercase
			// L! I've put it in here with a capital L.
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#notLessThan"; } }
			public override bool Evaluate(Decimal left, Decimal right) {
				return !(left < right);
			}
		}
		internal class MathNotEqualToRelation : MathComparisonRelation {
			public override string Uri { get { return "http://www.w3.org/2000/10/swap/math#notEqualTo"; } }
			public override bool Evaluate(Decimal left, Decimal right) {
				return !(left == right);
			}
		}
		#endif
	}
	
}
