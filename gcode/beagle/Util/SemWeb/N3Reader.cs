using System;
using System.Collections;
using System.IO;
using System.Text;

using SemWeb;
using SemWeb.Util;

namespace SemWeb {

	public class N3Reader : RdfReader {
		Resource PrefixResource = new Literal("@prefix");
		Resource KeywordsResource = new Literal("@keywords");
		Resource BaseResource = new Literal("@base");
		
		TextReader sourcestream;
		bool closed;

		NamespaceManager namespaces = new NamespaceManager();

		Entity entRDFTYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
		Entity entRDFFIRST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
		Entity entRDFREST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
		Entity entRDFNIL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
		//Entity entOWLSAMEAS = "http://www.w3.org/2002/07/owl#sameAs";
		Entity entDAMLEQUIV = "http://www.daml.org/2000/12/daml+oil#equivalentTo";
		Entity entLOGIMPLIES = "http://www.w3.org/2000/10/swap/log#implies";
		Entity entGRAPHCONTAINS = "http://razor.occams.info/code/semweb/internaluris/graphContains";
		
		public N3Reader(TextReader source) {
			this.sourcestream = source;
		}
		
		public N3Reader(string sourcefile) {
			this.sourcestream = GetReader(sourcefile);
			BaseUri = "file:" + sourcefile + "#";
		}

		protected override void Dispose() {
			if (!closed)
				sourcestream.Close();
			closed = true;
		}

		private struct ParseContext {
			public MyReader source;
			public StatementSink store;
			public NamespaceManager namespaces;
			public UriMap namedNode;
			public Hashtable anonymous;
			public Hashtable variables;
			public Entity meta;
			public bool UsingKeywords;
			public Hashtable Keywords;
			public Entity overrideMeta;
			
			public Location Location { get { return new Location(source.Line, source.Col); } }
		}
		
		public override void Select(StatementSink store) {
			ParseContext context = new ParseContext();
			context.source = new MyReader(sourcestream);
			context.store = store;
			context.namespaces = namespaces;
			context.namedNode = new UriMap();
			context.anonymous = new Hashtable();
			context.variables = new Hashtable();
			context.meta = Meta;
			
			while (ReadStatement(context)) { }
			
			Dispose();
		}
		
		private bool ReadStatement(ParseContext context) {
			Location loc = context.Location;
			
			bool reverse, forgetBNode;
			Resource subject = ReadResource(context, true, out reverse, out forgetBNode);
			if (subject == null) return false;
			if (reverse) OnError("is...of not allowed on a subject", loc);
			
			if ((object)subject == (object)PrefixResource) {
				loc = context.Location;
				string qname = ReadToken(context.source, context) as string;
				if (qname == null || !qname.EndsWith(":")) OnError("When using @prefix, the prefix identifier must end with a colon", loc);
				
				loc = context.Location;
				bool fb2;
				Resource uri = ReadResource(context, false, out reverse, out fb2);
				if (uri == null) OnError("Expecting a URI", loc);
				if (reverse) OnError("is...of not allowed here", loc);
				namespaces.AddNamespace(uri.Uri, qname.Substring(0, qname.Length-1));
				
				loc = context.Location;
				char punc = ReadPunc(context.source);
				if (punc != '.')
					OnError("Expected a period but found '" + punc + "'", loc);
				return true;
			}
			
			if ((object)subject == (object)KeywordsResource) {
				context.UsingKeywords = true;
				context.Keywords = new Hashtable();
				while (true) {
					ReadWhitespace(context.source);
					if (context.source.Peek() == '.') {
						context.source.Read();
						break;
					}
				
					loc = context.Location;
					string tok = ReadToken(context.source, context) as string;
					if (tok == null)
						OnError("Expecting keyword names", loc);
						
					context.Keywords[tok] = tok;
				}
				return true;
			}
			
			if ((object)subject == (object)BaseResource) {
				loc = context.Location;
				bool fb2;
				Resource uri = ReadResource(context, false, out reverse, out fb2);
				if (uri == null || uri.Uri == null) OnError("Expecting a URI", loc);
				if (reverse) OnError("is...of not allowed here", loc);
				BaseUri = uri.Uri;
				
				loc = context.Location;
				char punc = ReadPunc(context.source);
				if (punc != '.')
					OnError("Expected a period but found '" + punc + "'", loc);
				return true;
			}

			// It's possible to just assert the presence of an entity
			// by following the entity with a period, or a } to end
			// a reified context.
			if (NextPunc(context.source) == '.') {
				context.source.Read();
				if (forgetBNode) DoForget(subject, context);
				return true;
			}
			if (NextPunc(context.source) == '}') {
				context.source.Read();
				if (forgetBNode) DoForget(subject, context);
				return false; // end of block
			}
			
			// Read the predicates for this subject.
			char period = ReadPredicates(subject, context);
			loc = context.Location;
			if (period != '.' && period != '}')
				OnError("Expected a period but found '" + period + "'", loc);
			if (period == '}') return false;
			if (forgetBNode) DoForget(subject, context);
			return true;
		}
		
		private char ReadPredicates(Resource subject, ParseContext context) {			
			char punctuation = ';';
			while (punctuation == ';') {
				punctuation = ReadPredicate(subject, context);

				// if we read a semicolon, we may still be done
				// if it's followed by a period (end of statement)
				// or bracket (end of bnode), or brace (end of formula, N3).
				if (punctuation == ';') {
					int npunc = NextPunc(context.source);
					if (npunc == (int)'.' || npunc == (int)']' || npunc == (int)'}')
						return ReadPunc(context.source);
				}
			}
			return punctuation;
		}
		
		private char ReadPredicate(Resource subject, ParseContext context) {
			bool reverse, forgetBNode;
			Location loc = context.Location;
			Resource predicate = ReadResource(context, false, out reverse, out forgetBNode);
			if (predicate == null) OnError("Expecting a predicate", loc);
			if (predicate is Literal) OnError("Predicates cannot be literals", loc);
			
			if (predicate == entGRAPHCONTAINS) {
				context.overrideMeta = subject as Entity;
			} else {
				context.overrideMeta = null;
			}
			
			char punctuation = ',';
			while (punctuation == ',') {
				ReadObject(subject, (Entity)predicate, context, reverse);
				loc = context.Location;
				punctuation = ReadPunc(context.source);
			}
			if (punctuation != '.' && punctuation != ';' && punctuation != ']' && punctuation != '}')
				OnError("Expecting a period, semicolon, comma, close-bracket, or close-brace but found '" + punctuation + "'", loc);
			
			if (forgetBNode) DoForget(predicate, context);
			
			return punctuation;
		}
		
		private void ReadObject(Resource subject, Entity predicate, ParseContext context, bool reverse) {
			bool reverse2, forgetBNode;
			Location loc = context.Location;
			Resource value = ReadResource(context, false, out reverse2, out forgetBNode);
			if (value == null) OnError("Expecting a resource or literal object", loc);
			if (reverse2) OnError("is...of not allowed on objects", loc);
			
			loc = context.Location;
			if (predicate == entGRAPHCONTAINS) {
				// don't add the statement, it was enough to associate the meta node
			} else if (!reverse) {
				if (subject is Literal) OnError("Subjects of statements cannot be literals", loc);			
				Add(context.store, new Statement((Entity)subject, predicate, value, context.meta), loc);
			} else {
				if (value is Literal) OnError("A literal cannot be the object of a reverse-predicate statement", loc);
				Add(context.store, new Statement((Entity)value, predicate, subject, context.meta), loc);
			}

			if (forgetBNode) DoForget(value, context);
		}
		
		private void ReadWhitespace(MyReader source) {
			while (true) {
				while (char.IsWhiteSpace((char)source.Peek()))
					source.Read();
				
				if (source.Peek() == '#') {
					while (true) {
						int c = source.Read();
						if (c == -1 || c == 10 || c == 13) break;
					}
					continue;
				}
				
				break;
			}
		}
		
		private char ReadPunc(MyReader source) {
			ReadWhitespace(source);
			int c = source.Read();
			if (c == -1)
				OnError("End of file expecting punctuation", new Location(source.Line, source.Col));
			return (char)c;
		}
		
		private int NextPunc(MyReader source) {
			ReadWhitespace(source);
			return source.Peek();
		}
		
		private void ReadEscapedChar(char c, StringBuilder b, MyReader source, Location loc) {
			if (c == 'n') b.Append('\n');
			else if (c == 'r') b.Append('\r');
			else if (c == 't') b.Append('\t');
			else if (c == '\\') b.Append('\\');		
			else if (c == '"') b.Append('"');
			else if (c == '\'') b.Append('\'');
			else if (c == 'a') b.Append('\a');
			else if (c == 'b') b.Append('\b');
			else if (c == 'f') b.Append('\f');
			else if (c == 'v') b.Append('\v');
			else if (c == '\n') { }
			else if (c == '\r') { }
			else if (c == 'u' || c == 'U') {
				StringBuilder num = new StringBuilder();
				if (c == 'u')  {
					num.Append((char)source.Read()); // four hex digits
					num.Append((char)source.Read());
					num.Append((char)source.Read());
					num.Append((char)source.Read());
				} else {
					source.Read(); // two zeros
					source.Read();
					num.Append((char)source.Read()); // six hex digits
					num.Append((char)source.Read());
					num.Append((char)source.Read());
					num.Append((char)source.Read());
					num.Append((char)source.Read());
					num.Append((char)source.Read());
				}
				
				int unicode = int.Parse(num.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier);
				b.Append((char)unicode); // is this correct?
				
			} else if (char.IsDigit((char)c) || c == 'x')
				OnError("Octal and hex byte-value escapes are deprecated and not supported", loc);
			else
				OnError("Unrecognized escape character: " + (char)c, loc);
		}
		
		private StringBuilder readTokenBuffer = new StringBuilder();
		
		private object ReadToken(MyReader source, ParseContext context) {
			ReadWhitespace(source);
			
			Location loc = new Location(source.Line, source.Col);
			
			int firstchar = source.Read();
			if (firstchar == -1)
				return "";
			
			StringBuilder b = readTokenBuffer; readTokenBuffer.Length = 0;
			b.Append((char)firstchar);

			if (firstchar == '<') {
				// This is a URI or the <= verb.  URIs can be escaped like strings, at least in the NTriples spec.
				bool escaped = false;
				while (true) {
					int c = source.Read();
					if (c == -1) OnError("Unexpected end of stream within a token beginning with <", loc);
					
					if (b.Length == 2 && c == '=')
						return "<="; // the <= verb
					
					if (escaped) {
						ReadEscapedChar((char)c, b, source, loc);
						escaped = false;
					} else if (c == '\\') {
						escaped = true;
					} else {
						b.Append((char)c);
						if (c == '>') // end of the URI
							break;
					}
				}
				
			} else if (firstchar == '"') {
				// A string: ("""[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*""")|("[^"\\]*(?:\\.[^"\\]*)*")
				// What kind of crazy regex is this??
				b.Length = 0; // get rid of the open quote
				bool escaped = false;
				bool triplequoted = false;
				while (true) {
					int c = source.Read();
					if (c == -1) OnError("Unexpected end of stream within a string", loc);
					
					if (b.Length == 0 && c == (int)'"' && source.Peek() == (int)'"') {
						triplequoted = true;
						source.Read();
						continue;
					}
					
					if (!escaped && c == '\\')
						escaped = true;
					else if (escaped) {
						ReadEscapedChar((char)c, b, source, loc);
						escaped = false;
					} else {
						if (c == '"' && !triplequoted)
							break;
						if (c == '"' && source.Peek() == '"' && source.Peek2() == '"' && triplequoted)
							break;
						b.Append((char)c);
					}
				}
				
				if (triplequoted) { // read the extra end quotes
					source.Read();
					source.Read();
				}
				
				string litvalue = b.ToString();
				string litlang = null;
				string litdt = null;

				// Strings can be suffixed with @langcode or ^^symbol (but not both?).
				if (source.Peek() == '@') {
					source.Read();
					b.Length = 0;
					while (char.IsLetterOrDigit((char)source.Peek()) || source.Peek() == (int)'-')
						b.Append((char)source.Read());
					litlang = b.ToString();
				} else if (source.Peek() == '^' && source.Peek2() == '^') {
					loc = new Location(source.Line, source.Col);
					source.Read();
					source.Read();
					litdt = ReadToken(source, context).ToString(); // better be a string URI
					if (litdt.StartsWith("<") && litdt.EndsWith(">"))
						litdt = litdt.Substring(1, litdt.Length-2);
					else if (litdt.IndexOf(":") != -1) {
						Resource r = ResolveQName(litdt, context, loc);
						if (r.Uri == null)
							OnError("A literal datatype cannot be an anonymous entity", loc);
						litdt = r.Uri;
					}
				}
				
				return new Literal(litvalue, litlang, litdt);

			} else if (char.IsLetter((char)firstchar) || firstchar == '?' || firstchar == '@' || firstchar == ':' || firstchar == '_') {
				// Something starting with @
				// A QName: ([a-zA-Z_][a-zA-Z0-9_]*)?:)?([a-zA-Z_][a-zA-Z0-9_]*)?
				// A variable: \?[a-zA-Z_][a-zA-Z0-9_]*
				while (true) {
					int c = source.Peek();
					if (c == -1 || (!Entity.ValidateUriIsIUnreserved((char)c) && c != ':') || c == '.') break;
					b.Append((char)source.Read());
				}
			
			} else if (char.IsDigit((char)firstchar) || firstchar == '+' || firstchar == '-') {
				while (true) {
					int ci = source.Peek();
					if (ci == -1) break;
					if (ci == ']' || ci == ')' || ci == '}') break;
					
					// punctuation followed by a space means the punctuation is
					// punctuation, and not part of this token
					if (!char.IsDigit((char)ci) && source.Peek2() != -1 && char.IsWhiteSpace((char)source.Peek2()))
						break;
					
					char c = (char)ci;
					if (char.IsWhiteSpace(c)) break;
					
					b.Append((char)source.Read());
				}
				
			} else if (firstchar == '=') {
				if (source.Peek() == (int)'>')
					b.Append((char)source.Read());
				
				if (source.Peek() == (int)':' && source.Peek2() == (int)'>') { // SPECIAL EXTENSION "=:>"
					b.Append((char)source.Read());
					b.Append((char)source.Read());
				}
			
			} else if (firstchar == '[') {
				// The start of an anonymous node.

			} else if (firstchar == '{') {
				return "{";

			} else if (firstchar == '(') {
				return "(";
			} else if (firstchar == ')') {
				return ")";

			} else {
				while (true) {
					int c = source.Read();
					if (c == -1) break;
					if (char.IsWhiteSpace((char)c)) break;
					b.Append((char)c);
				}
				OnError("Invalid token: " + b.ToString(), loc);
			}
			
			return b.ToString();
		}
		
		private Resource ReadResource(ParseContext context, bool allowDirective, out bool reverse, out bool forgetBNode) {
			Location loc = context.Location;
			
			Resource res = ReadResource2(context, allowDirective, out reverse, out forgetBNode);
			
			ReadWhitespace(context.source);
			while (context.source.Peek() == '!' || context.source.Peek() == '^' || (context.source.Peek() == '.' && context.source.Peek2() != -1 && char.IsLetter((char)context.source.Peek2())) ) {
				int pathType = context.source.Read();
				
				bool reverse2, forgetBNode2;
				loc = context.Location;
				Resource path = ReadResource2(context, false, out reverse2, out forgetBNode2);
				if (reverse || reverse2) OnError("is...of is not allowed in path expressions", loc);
				if (!(path is Entity)) OnError("A path expression cannot be a literal", loc);
				
				Entity anon = new BNode();
				
				Statement s;
				if (pathType == '!' || pathType == '.') {
					if (!(res is Entity)) OnError("A path expression cannot contain a literal: " + res, loc);
					s = new Statement((Entity)res, (Entity)path, anon, context.meta);
				} else {
					s = new Statement(anon, (Entity)path, res, context.meta);
				}
				
				Add(context.store, s, loc);
				
				if (forgetBNode) DoForget(res, context);
				if (forgetBNode2) DoForget(path, context);
				
				res = anon;
				forgetBNode = true;

				ReadWhitespace(context.source);
			}
				
			return res;
		}			
		
		private Entity GetResource(ParseContext context, string uri) {
			if (!ReuseEntities)
				return new Entity(uri);
		
			Entity ret = (Entity)context.namedNode[uri];
			if (ret != null) return ret;
			ret = new Entity(uri);
			context.namedNode[uri] = ret;
			return ret;
		}

		private Resource ResolveQName(string str, ParseContext context, Location loc) {
			int colon = str.IndexOf(":");
			string prefix = str.Substring(0, colon);
			if (prefix == "_") {
				Resource ret = (Resource)context.anonymous[str];
				if (ret == null) {
					ret = new BNode(str.Substring(colon+1));
					context.anonymous[str] = ret;
				}
				return ret;
			} else if (prefix == "" && context.namespaces.GetNamespace(prefix) == null) {
				return GetResource(context, (BaseUri == null ? "#" : BaseUri) + str.Substring(colon+1));
			} else {
				string ns = context.namespaces.GetNamespace(prefix);
				if (ns == null)
					OnError("Prefix is undefined: " + str, loc);
				if (prefix != "")
					Namespaces.AddNamespace(ns, prefix);
				return GetResource(context, ns + str.Substring(colon+1));
			}
		}
			
		private Resource ReadResource2(ParseContext context, bool allowDirective, out bool reverse, out bool forgetBNode) {
			reverse = false;
			forgetBNode = false;
			
			Location loc = context.Location;
			
			object tok = ReadToken(context.source, context);
			if (tok is Literal)
				return (Literal)tok;
			
			string str = (string)tok;
			if (str == "")
				return null;
				
			// Directives
			
			if (str == "@prefix") {
				if (allowDirective)
					return PrefixResource;
				else
					OnError("The directive '" + str + "' is not allowed here", loc);
			}

			if (str == "@keywords") {
				if (allowDirective)
					return KeywordsResource;
				else
					OnError("The directive '" + str + "' is not allowed here", loc);
			}

			if (str == "@base") {
				if (allowDirective)
					return BaseResource;
				else
					OnError("The directive '" + str + "' is not allowed here", loc);
			}
			
			// @ Keywords

			if (context.UsingKeywords && context.Keywords.Contains(str))
				str = "@" + str;
			if (!context.UsingKeywords &&
				( str == "a" || str == "has" || str == "is"))
				str = "@" + str;
			
			// Standard Keywords
			// TODO: Turn these off with @keywords
			
			if (str == "@a")
				return entRDFTYPE;
				
			if (str == "=")
				return entDAMLEQUIV;
			if (str == "=>")
				return entLOGIMPLIES;
			if (str == "<=") {
				reverse = true;
				return entLOGIMPLIES;
			}
			if (str == "=:>") // SPECIAL EXTENSION!
				return entGRAPHCONTAINS;

			if (str == "@has") // ignore this token
				return ReadResource2(context, false, out reverse, out forgetBNode);
			
			if (str == "@is") {
				// Reverse predicate
				bool reversetemp;
				Resource pred = ReadResource2(context, false, out reversetemp, out forgetBNode);
				reverse = true;
				
				string of = ReadToken(context.source, context) as string;
				if (of == null) OnError("End of stream while expecting 'of'", loc);
				if (of == "@of"
					|| (!context.UsingKeywords && of == "of")
					|| (context.UsingKeywords && context.Keywords.Contains("of") && of == "of"))
					return pred;
				OnError("Expecting token 'of' but found '" + of + "'", loc);
				return null; // unreachable
			}
			
			if (str.StartsWith("@"))
				OnError("The " + str + " directive is not supported", loc);

			// URI
			
			if (str.StartsWith("<") && str.EndsWith(">")) {
				string uri = GetAbsoluteUri(BaseUri, str.Substring(1, str.Length-2));
				string urierror = Entity.ValidateUri(uri);
				if (urierror != null)
					OnWarning(urierror, loc);
				return GetResource(context, uri);
			}
			
			// VARIABLE
			
			if (str[0] == '?') {
				string name = str.Substring(1);
				Entity varb = (Entity)context.variables[name];
				if (varb == null) {
					varb = new Variable(name);
					AddVariable((Variable)varb);
					context.variables[name] = varb;
				}
				return varb;
			}
			
			// QNAME

			if (str.IndexOf(":") != -1)
				return ResolveQName(str, context, loc);
				
			// ANONYMOUS
			
			if (str == "[") {
				Entity ret = new BNode();
				ReadWhitespace(context.source);
				if (context.source.Peek() != ']') {
					char bracket = ReadPredicates(ret, context);
					if (bracket == '.')
						bracket = ReadPunc(context.source);
					if (bracket != ']')
						OnError("Expected a close bracket but found '" + bracket + "'", loc);
				} else {
					context.source.Read();
				}
				forgetBNode = true;
				return ret;
			}
			
			// LIST
			
			if (str == "(") {
				// A list
				Entity head = null, ent = null;
				while (true) {
					bool rev2, fb2;
					Resource res = ReadResource(context, false, out rev2, out fb2);
					if (res == null)
						break;
					
					if (ent == null) {
						ent = new BNode();
						head = ent;
					} else {
						Entity sub = new BNode();
						Add(context.store, new Statement(ent, entRDFREST, sub, context.meta), loc);
						ent = sub;
					}
					
					Add(context.store, new Statement(ent, entRDFFIRST, res, context.meta), loc);
					if (fb2) DoForget(res, context);
				}
				if (head == null) // No list items.
					head = entRDFNIL; // according to Turtle spec
				else
					Add(context.store, new Statement(ent, entRDFREST, entRDFNIL, context.meta), loc);
				
				return head;
			}
			
			if (str == ")")
				return null; // Should I use a more precise end-of-list return value?
			
			// FORMULA
			
			if (str == "{") {
				// ParseContext is a struct, so this gives us a clone.
				ParseContext newcontext = context;
				
				// The formula is denoted by a blank node, unless we set
				// the override meta flag above.
				if (context.overrideMeta == null)
					newcontext.meta = new BNode();
				else
					newcontext.meta = context.overrideMeta;
				
				// According to the spec, _:xxx anonymous nodes are
				// local to the formula.  But ?$variables (which aren't
				// mentioned in the spec) are treated as global names.
				newcontext.anonymous = new Hashtable();
				
				while (NextPunc(context.source) != '}' && ReadStatement(newcontext)) { }
				ReadWhitespace(context.source);
				if (context.source.Peek() == '}') context.source.Read();
				
				return newcontext.meta;
			}
			
			// NUMERIC LITERAL
			
			// In Turtle, numbers are restricted to [0-9]+, and are datatyped xsd:integer.
			double numval;
			#if !SILVERLIGHT
			if (double.TryParse(str, System.Globalization.NumberStyles.Any, null, out numval)) {
			#else
			bool ok = true;
			numval = 0;
			try {
				numval = double.Parse(str);
			} catch (Exception) {
				ok = false;
			}
			if (ok) {
			#endif
				if (numval >= long.MinValue && numval <= long.MaxValue && numval == (double)(long)numval)
					return new Literal(((long)numval).ToString(), null, NS.XMLSCHEMA + "integer");
				else
					return new Literal(numval.ToString(), null, NS.XMLSCHEMA + "double");
			}
			
			//BOOLEAN LITERAL
			
			if (str == "true" || str == "false") {
			  return new Literal(str,null,NS.XMLSCHEMA+"boolean");
			}
			
			// If @keywords is used, alphanumerics that aren't keywords
			// are local names in the default namespace.
			if (context.UsingKeywords && char.IsLetter(str[0])) {
				if (BaseUri == null)
					OnError("The document contains an unqualified name but no BaseUri was specified: \"" + str + "\"", loc);
				return GetResource(context, BaseUri + str);
			}
			
			// NOTHING MATCHED
			
			OnError("Invalid token: " + str, loc);
			return null;
		}
		
		private void Add(StatementSink store, Statement statement, Location position) {
			store.Add(statement);
		}
		
		private void OnError(string message, Location position) {
			throw new ParserException(message + ", line " + position.Line + " col " + position.Col);
		}
		private void OnWarning(string message, Location position) {
			base.OnWarning(message + ", line " + position.Line + " col " + position.Col);
		}
		/*private void OnError(string message, Location position, Exception cause) {
			throw new ParserException(message + ", line " + position.Line + " col " + position.Col, cause);
		}
		private void OnWarning(string message, Location position, Exception cause) {
			OnWarning(message + ", line " + position.Line + " col " + position.Col);
		}*/
		
		void DoForget(Resource ent, ParseContext context) {
			CanForgetBNodes x = context.store as CanForgetBNodes;
			if (x == null) return;
			x.ForgetBNode((BNode)ent);
		}
	}

	internal class MyReader {
		TextReader r;
		public MyReader(TextReader reader) { r = reader; }
		
		public int Line = 1;
		public int Col = 0;
		
		int[] peeked = new int[2];
		int peekCount = 0;
		
		public Location Location { get { return new Location(Line, Col); } }
		
		public int Peek() {
			if (peekCount == 0) {
				peeked[0] = r.Read();
				peekCount = 1;
			}
			return peeked[0];
		}
		
		public int Peek2() {
			Peek();
			if (peekCount == 1) {
				peeked[1] = r.Read();
				peekCount = 2;
			}
			return peeked[1];
		}

		public int Read() {
			int c;
			
			if (peekCount > 0) {
				c = peeked[0];
				peeked[0] = peeked[1];
				peekCount--;
			} else {
				c = r.Read();
			}
			
			if (c == '\n') { Line++; Col = 0; }
			else { Col++; }
			
			return c;
		}
		
	}

	internal struct Location {
		public readonly int Line, Col;
		public Location(int line, int col) { Line = line; Col = col; }
	}
		
}
