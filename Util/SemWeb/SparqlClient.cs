using System;
using System.Collections;
#if DOTNET2
using System.Collections.Generic;
#endif
using System.IO;
using System.Text;
using System.Xml;
 
using SemWeb;
using SemWeb.Query;
using SemWeb.Util;
 
namespace SemWeb.Remote {

	public interface SparqlSource {
		void RunSparqlQuery(string sparqlQuery, TextWriter output);
		void RunSparqlQuery(string sparqlQuery, out bool askResult);
		void RunSparqlQuery(string sparqlQuery, StatementSink statementResults);
		void RunSparqlQuery(string sparqlQuery, QueryResultSink selectResults);
	}

	#if false
	public class SparqlHttpSource : QueryableSource, SparqlSource {
		static bool Debug = System.Environment.GetEnvironmentVariable("SEMWEB_DEBUG_HTTP") != null;
	
		string url;
		
		public SparqlHttpSource(string url) {
			this.url = url;
		}

		public bool Distinct { get { return false; } }

		public void RunSparqlQuery(string sparqlQuery, TextWriter output) {
			Load(sparqlQuery, output);
		}
		
		public void RunSparqlQuery(string sparqlQuery, out bool askResult) {
			BooleanWrap bw = new BooleanWrap();
			Load(sparqlQuery, bw);
			askResult = bw.value;
		}
		
		public void RunSparqlQuery(string sparqlQuery, StatementSink statementResults) {
			Load(sparqlQuery, statementResults);
		}
		
		public void RunSparqlQuery(string sparqlQuery, QueryResultSink selectResults) {
			Load(sparqlQuery, selectResults);
		}
		
		public bool Contains(Resource resource) {
			throw new NotImplementedException();
		}
		
		public bool Contains(Statement template) {
			return Select(template, null, true);
		}
		
		public void Select(StatementSink sink) {
			Select(Statement.All, sink);
		}
		
		public void Select(Statement template, StatementSink sink) {
			Select(template, sink, false);
		}
		
		bool Select(Statement template, StatementSink sink, bool ask) {
			return Select(
				template.Subject == null ? null : new Entity[] { template.Subject },
				template.Predicate == null ? null : new Entity[] { template.Predicate },
				template.Object == null ? null : new Resource[] { template.Object },
				template.Meta == null ? null : new Entity[] { template.Meta },
				null,
				0,
				sink,
				ask
				);
		}
		
		public void Select(SelectFilter filter, StatementSink sink) {
			Select(filter.Subjects, filter.Predicates, filter.Objects, filter.Metas, filter.LiteralFilters, filter.Limit, sink, false);
		}
		
		bool Select(Entity[] subjects, Entity[] predicates, Resource[] objects, Entity[] metas, LiteralFilter[] litFilters, int limit, StatementSink sink, bool ask) {
			// TODO: Change meta into named graphs.  Anything but a null or DefaultMeta
			// meta returns no statements immediately.
			if (metas != null && (metas.Length != 1 || metas[0] != Statement.DefaultMeta))
				return false;
		
			string query;
			bool nonull = false;
		
			if (subjects != null && subjects.Length == 1
				&& predicates != null && predicates.Length == 1
				&& objects != null && objects.Length == 1) {
				query = "ASK WHERE { " + S(subjects[0], null) + " " + S(predicates[0], null) + " " + S(objects[0], null) + "}";
				nonull = true;
			} else {
				if (ask)
					query = "ASK { ";
				else
					query = "SELECT * WHERE { ";
				query += S(subjects, "subject");
				query += " ";
				query += S(predicates, "predicate");
				query += " ";
				query += S(objects, "object");
				query += " . ";
				query += SL(subjects, "subject", false);
				query += SL(predicates, "predicate", false);
				query += SL(objects, "object", false);
				query += " }";
				
				// TODO: Pass literal filters to server.
			}
			
			if (limit >= 1)
				query += " LIMIT " + limit;
				
			Statement d = new Statement(
				(subjects != null && subjects.Length == 1) ? subjects[0] : null,
				(predicates != null && predicates.Length == 1) ? predicates[0] : null,
				(objects != null && objects.Length == 1) ? objects[0] : null);
			
			if (ask || nonull) {
				BooleanWrap bw = new BooleanWrap();
				Load(query, bw);
				if (ask)
					return bw.value;
				else if (bw.value)
					sink.Add(new Statement(subjects[0], predicates[0], objects[0]));
				return false;
			} else {
				Load(query, new QueryToStatements(sink, litFilters, d));
				return true;
			}
		}
		
		class QueryToStatements : QueryResultSink {
			StatementSink sink;
			LiteralFilter[] litFilters;
			Statement d;
			int si = -1, pi = -1, oi = -1;
			
			public QueryToStatements(StatementSink sink, LiteralFilter[] litFilters, Statement d) {
				this.sink = sink;
				this.litFilters = litFilters;
				this.d = d;
			}
			
			public override void Init(Variable[] variables) {
				for (int i = 0; i < variables.Length; i++) {
					if (variables[i].LocalName == "subject") si = i;
					if (variables[i].LocalName == "predicate") pi = i;
					if (variables[i].LocalName == "object") oi = i;
				}
			}
			
			public override bool Add(VariableBindings result) {
				Resource subj = si == -1 ? d.Subject : result.Values[si];
				Resource pred = pi == -1 ? d.Predicate : result.Values[pi];
				Resource obj = oi == -1 ? d.Object : result.Values[oi];
				if (!(subj is Entity) || !(pred is Entity)) return true;
				if (litFilters != null && !LiteralFilter.MatchesFilters(obj, litFilters, null)) return true;
				return sink.Add(new Statement((Entity)subj, (Entity)pred, obj));
			}

		}
		
		string S(Resource[] r, string v) {
			if (r == null || r.Length != 1) return "?" + v;
			return S(r[0], null);
		}
		string SL(Resource[] r, string v, bool includeIfJustOne) {
			if (r == null || (r.Length <= 1 && !includeIfJustOne)) return "";
			StringBuilder ret = new StringBuilder();
			ret.Append("FILTER(");
			bool first = true;
			for (int i = 0; i < r.Length; i++) {
				if (r[i].Uri == null) continue;
				if (!first) ret.Append(" || "); first = false;
				ret.Append('?');
				ret.Append(v);
				ret.Append("=<");
				if (r[i].Uri != null)
					ret.Append(r[i].Uri);
				ret.Append('>');
			}
			ret.Append(").\n");
			if (first) return "";
			return ret.ToString();
		}
		#if !DOTNET2
		string SL(object r, string v, bool includeIfJustOne) {
			return SL((Resource[])new ArrayList((ICollection)r).ToArray(typeof(Resource)), v, includeIfJustOne);
		}
		#else
		string SL(ICollection<Resource> r, string v, bool includeIfJustOne) {
			return SL(new List<Resource>(r).ToArray(), v, includeIfJustOne);
		}
		#endif
		
		string S(Resource r, string v) {
			if (r == null || r is Variable) {
				return v;
			} else if (r is Literal) {
				return r.ToString();
			} else if (r.Uri != null) {
				if (r.Uri.IndexOf('>') != -1)
					throw new ArgumentException("Invalid URI: " + r.Uri);
				return "<" + r.Uri + ">";
			} else {
				throw new NotSupportedException("Blank node in select not supported.");
			}
		}
		
		class BooleanWrap {
			public bool value;
		}
		
		void Load(string query, object outputObj) {
			string qstr = "query=" + System.Web.HttpUtility.UrlEncode(query);
			
			string method = "POST";
			
			System.Net.WebRequest rq;
			
			if (Debug) {
				Console.Error.WriteLine("> " + url);
				Console.Error.WriteLine(query);
				Console.Error.WriteLine();
			}
			
			if (method == "GET") {
				string qurl = url + "?" + qstr;
				rq = System.Net.WebRequest.Create(qurl);
			} else {
				Encoding encoding = new UTF8Encoding(); // ?
				byte[] data = encoding.GetBytes(qstr);

				rq = System.Net.WebRequest.Create(url);
				rq.Method = "POST";
				rq.ContentType="application/x-www-form-urlencoded";
				rq.ContentLength = data.Length;
				
				using (Stream stream = rq.GetRequestStream())
					stream.Write(data, 0, data.Length);
			}
			
			System.Net.HttpWebResponse resp = (System.Net.HttpWebResponse)rq.GetResponse();
			try {
				string mimetype = resp.ContentType;
				if (mimetype.IndexOf(';') > -1)
					mimetype = mimetype.Substring(0, mimetype.IndexOf(';'));
					
				ProcessResponse(mimetype, resp.GetResponseStream(), outputObj);
			} finally {
				resp.Close();
			}
		}
		
		public static void ParseSparqlResponse(Stream sparqlResponse, QueryResultSink queryResults) {
			ProcessResponse(null, sparqlResponse, queryResults);
		}
		
		public static void ParseSparqlResponse(Stream sparqlResponse, out bool askResult) {
			BooleanWrap bw = new BooleanWrap();
			ProcessResponse(null, sparqlResponse, bw);
			askResult = bw.value;
		}

		private static void ProcessResponse(string mimetype, Stream stream, object outputObj) {
		
			// If the user wants the output sent to a TextWriter, copy the response from
			// the response stream to the TextWriter. TODO: Get encoding from HTTP header.
			if (outputObj is TextWriter) {
				TextWriter tw = (TextWriter)outputObj;
				using (StreamReader reader = new StreamReader(stream, System.Text.Encoding.UTF8)) {
					char[] buffer = new char[512];
					while (true) {
						int len = reader.Read(buffer, 0, buffer.Length);
						if (len <= 0) break;
						tw.Write(buffer, 0, len);
					}
				}
				tw.Flush();
				return;
			}
			
			// If the user wants a boolean out of this, then we're expecting a
			// SPARQL XML Results document with a boolean response element.
			if (outputObj is BooleanWrap) {
				BooleanWrap bw = (BooleanWrap)outputObj;
				
				if (mimetype != null && mimetype != "application/sparql-results+xml" && mimetype != "text/xml")
					throw new ApplicationException("The result of the query was not a SPARQL Results document.");
					
				XmlReader xmldoc = new XmlTextReader(stream);
				{
					// Move to the document element
					while (xmldoc.Read())
						if (xmldoc.NodeType == XmlNodeType.Element) break;
						
					// Just check that it has the right local name.
					if (xmldoc.LocalName != "sparql" || xmldoc.IsEmptyElement)
						throw new ApplicationException("Invalid server response: Not a SPARQL results document.");
								
					// Move to the next node.
					while (xmldoc.Read())
						if (xmldoc.NodeType == XmlNodeType.Element) break;
						
					// If it's a head node, skip it.
					if (xmldoc.LocalName == "head") {
						xmldoc.Skip();
						// Move to the 'boolean' element, it better be next
						while (xmldoc.Read())
							if (xmldoc.NodeType == XmlNodeType.Element) break;
					}
						
					if (xmldoc.LocalName != "boolean")
						throw new ApplicationException("Invalid server response: Missing 'boolean' element.");
					
					string value = xmldoc.ReadElementString();
					bw.value = (value == "true");
				}
						
				return;
			}
			
			// If the user wants statements out of the response, read it with an RDFReader.
			if (outputObj is StatementSink) {
				// If the mime type is application/sparql-results+xml, just try to
				// read it as if it were an RDF/XML MIME type.
				if (mimetype != null && mimetype == "application/sparql-results+xml") mimetype = "text/xml";
				using (RdfReader reader = RdfReader.Create(mimetype, stream))
					reader.Select((StatementSink)outputObj);
				return;
			}
			
			// If the user wants query result bindings, read the response XML.
			if (outputObj is QueryResultSink) {
				QueryResultSink sink = (QueryResultSink)outputObj;
			
				if (mimetype != null && mimetype != "application/sparql-results+xml" && mimetype != "text/xml")
					throw new ApplicationException("The result of the query was not a SPARQL Results document.");
					
				ArrayList variableNames = new ArrayList();
				ArrayList variables = new ArrayList();
				Variable[] variablesArray = null;
				Hashtable bnodes = new Hashtable();

				XmlReader xmldoc = new XmlTextReader(stream);
				{
					// Move to the document element
					while (xmldoc.Read())
						if (xmldoc.NodeType == XmlNodeType.Element) break;
						
					// Just check that it has the right local name.
					if (xmldoc.LocalName != "sparql" || xmldoc.IsEmptyElement)
						throw new ApplicationException("Invalid server response: Not a SPARQL results document.");
								
					// Move to the 'head' node, it better be the first element
					while (xmldoc.Read())
						if (xmldoc.NodeType == XmlNodeType.Element) break;
						
					if (xmldoc.LocalName != "head" || xmldoc.IsEmptyElement)
						throw new ApplicationException("Invalid server response: Missing head full element.");
						
					// Read the head element
					while (xmldoc.Read()) {
						if (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "variable") {
							if (xmldoc.GetAttribute("name") == null)
								throw new ApplicationException("Invalid server response: Head/variable node missing name attribute.");
							variableNames.Add(xmldoc.GetAttribute("name"));
							variables.Add(new Variable(xmldoc.GetAttribute("name")));
							if (!xmldoc.IsEmptyElement) xmldoc.Skip();
						} else if (xmldoc.NodeType == XmlNodeType.EndElement) {
							break;
						}
					}
					
					// Move to the 'results' element, it better be next
					while (xmldoc.Read())
						if (xmldoc.NodeType == XmlNodeType.Element) break;
						
					if (xmldoc.LocalName != "results")
						throw new ApplicationException("Invalid server response: Missing results element.");
					
					variablesArray = (Variable[])variables.ToArray(typeof(Variable));
					sink.Init(variablesArray);
					
					// Read the results
					
					while (xmldoc.Read()) {
						if (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "result") {
							// Read the bindings in this result
							Resource[] valuesArray = new Resource[variablesArray.Length];
							while (xmldoc.Read()) {
								if (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "binding") {
									if (xmldoc.IsEmptyElement)
										throw new ApplicationException("Invalid server response: Binding element empty.");
									if (xmldoc.GetAttribute("name") == null)
										throw new ApplicationException("Invalid server response: Result binding node missing name attribute.");
									int vIndex = variableNames.IndexOf(xmldoc.GetAttribute("name"));
									if (vIndex == -1)
										throw new ApplicationException("Invalid server response: Result binding name does not match a variable in the head.");
									
									Resource value = null;
									
									while (xmldoc.Read()) {
										if (xmldoc.NodeType == XmlNodeType.Whitespace || xmldoc.NodeType == XmlNodeType.SignificantWhitespace) continue;
										if (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "uri") {
											value = new Entity(xmldoc.ReadElementString());
											if (!xmldoc.IsEmptyElement) xmldoc.Skip();
										} else if  (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "literal") {
											string lang = xmldoc.XmlLang;
											if (lang == "") lang = null;
											string dt = xmldoc.GetAttribute("datatype");
											value = new Literal(xmldoc.ReadElementString(), lang, dt);
											if (!xmldoc.IsEmptyElement) xmldoc.Skip();
										} else if  (xmldoc.NodeType == XmlNodeType.Element && xmldoc.LocalName == "bnode") {
											string id = xmldoc.ReadElementString();
											if (bnodes.ContainsKey(id)) {
												value = (BNode)bnodes[id];
											} else {
												value = new BNode(id);
												bnodes[id] = value;
											}
											if (!xmldoc.IsEmptyElement) xmldoc.Skip();
										} else {
											throw new ApplicationException("Invalid server response: Invalid content in binding node.");
										}
										break;
									}
									if (value == null)
										throw new ApplicationException("Invalid server response: Result binding value is invalid.");
									
									valuesArray[vIndex] = value;

								} else if (xmldoc.NodeType == XmlNodeType.EndElement) {
									break;
								}
							}
							
							sink.Add(new VariableBindings(variablesArray, valuesArray));
							
						} else if (xmldoc.NodeType == XmlNodeType.EndElement) {
							break;
						}
					}

					sink.Finished();
				}
			}
		}
		
		public MetaQueryResult MetaQuery(Statement[] graph, QueryOptions options) {
			MetaQueryResult ret = new MetaQueryResult();
			ret.QuerySupported = true;
			return ret;
		}

		public void Query(Statement[] graph, QueryOptions options, QueryResultSink sink) {
			StringBuilder query = new StringBuilder();
			
			query.Append("SELECT ");

			// Get a list of variables and map them to fresh names
			#if !DOTNET2
			Hashtable variableNames = new Hashtable();
			#else
			Dictionary<Variable,string> variableNames = new Dictionary<Variable,string>();
			#endif
			Hashtable variableNames2 = new Hashtable();
			foreach (Statement s in graph) {
				for (int j = 0; j < 3; j++) {
					Variable v = s.GetComponent(j) as Variable;
					if (v == null) continue;
					if (variableNames.ContainsKey(v)) continue;
					variableNames2["v" + variableNames.Count] = v;
					variableNames[v] = "?v" + variableNames.Count;
				}
			}
			
			// What variables will we select on?
			ArrayList selectedVars = new ArrayList();
			foreach (Variable v in
				options.DistinguishedVariables != null
					? options.DistinguishedVariables
					: variableNames.Keys) {
				if (!variableNames.ContainsKey(v)) continue; // in case distinguished variables list
															 // has more than what actually appears in query
				query.Append(variableNames[v]);
				query.Append(' ');
				selectedVars.Add(v);
			}
			
			// Bnodes are not allowed here -- we can't query on them.
			foreach (Statement s in graph) {
				for (int j = 0; j < 3; j++) {
					if (s.GetComponent(j) is BNode && !(s.GetComponent(j) is Variable)) {
						Variable[] varArray = (Variable[])selectedVars.ToArray(typeof(Variable));
						sink.Init(varArray);
						sink.Finished();
						return;
					}
				}
			}
			
			// Build the graph pattern.
			query.Append("WHERE {\n");
			
			ResSet firstVarUse = new ResSet();
			foreach (Statement s in graph) {
				for (int j = 0; j < 3; j++) {
					Resource r = s.GetComponent(j);
					query.Append(S(r, r is Variable && variableNames.ContainsKey((Variable)r) ? (string)variableNames[(Variable)r] : null));
					query.Append(" ");
				}
				query.Append(" . \n");
				if (options.VariableKnownValues != null) {
					for (int j = 0; j < 3; j++) {
						Resource r = s.GetComponent(j);
						if (firstVarUse.Contains(r)) continue;
						firstVarUse.Add(r);
						if (r is Variable && variableNames.ContainsKey((Variable)r) &&
						#if !DOTNET2
						options.VariableKnownValues.Contains(r)
						#else
						options.VariableKnownValues.ContainsKey((Variable)r)
						#endif
						)
							query.Append(SL(options.VariableKnownValues[(Variable)r], (string)variableNames[(Variable)r], true));
					}
				}
				// And what about meta...?
			}
			
			query.Append("}");
			
			if (options.Limit > 0) {
				query.Append(" LIMIT ");
				query.Append(options.Limit);
			}
			
			Load(query.ToString(), new QueryResultsWrapper(sink, variableNames2));
		}
		
		class QueryResultsWrapper : QueryResultSink {
			QueryResultSink sink;
			Hashtable variableNames;
			Variable[] vars;
			
			public QueryResultsWrapper(QueryResultSink sink, Hashtable variableNames) {
				this.sink = sink;
				this.variableNames = variableNames;
			}
			
			public override void Init(Variable[] variables) {
				vars = new Variable[variables.Length];
				for (int i = 0; i < variables.Length; i++)
					vars[i] = (Variable)variableNames[variables[i].LocalName];
				sink.Init(vars);
			}
			
			public override bool Add(VariableBindings result) {
				#if !DOTNET2
				return sink.Add(new VariableBindings(vars, result.Values));
				#else
				Resource[] vals = new Resource[result.Values.Count];
				result.Values.CopyTo(vals, 0);
				return sink.Add(new VariableBindings(vars, vals));
				#endif
			}

			public override void Finished() {
				sink.Finished();
			}
			
			public override void AddComments(string comments) {
				sink.AddComments(comments);
			}
		}
	}
	#endif
}

namespace SemWeb.Query {
	public class SparqlXmlQuerySink : QueryResultSink {
		System.Xml.XmlWriter output;
		
		int blankNodeCounter = 0;
		Hashtable blankNodes = new Hashtable();
		
		public const string MimeType = "application/sparql-results+xml";
		
		private static System.Xml.XmlWriter GetWriter(System.IO.TextWriter writer) {
			System.Xml.XmlTextWriter w = new System.Xml.XmlTextWriter(writer);
			w.Formatting = System.Xml.Formatting.Indented;
			return w;
		}
		
		public SparqlXmlQuerySink(TextWriter output)
		 : this(GetWriter(output)) {
		}

		public SparqlXmlQuerySink(System.Xml.XmlWriter output) {
			this.output = output;
		}
		
		public override void AddComments(string comments) {
			if (comments != null && comments.Length > 0)
				output.WriteComment(comments);
		}
		
		public override void Init(Variable[] variables) {
			output.WriteStartElement("sparql");
			output.WriteAttributeString("xmlns", "http://www.w3.org/2005/sparql-results#");
			output.WriteStartElement("head");
			foreach (Variable var in variables) {
				if (var.LocalName == null) continue;
				output.WriteStartElement("variable");
				output.WriteAttributeString("name", var.LocalName);
				output.WriteEndElement();
			}
			output.WriteEndElement(); // head
			output.WriteStartElement("results");
			
			// instead of <results>, we might want <boolean>true</boolean>
		}
		
		public override bool Add(VariableBindings result) {
			output.WriteStartElement("result");
			for (int i = 0; i < result.Count; i++) {
				Variable var = result.Variables[i];
				Resource val = result.Values[i];
				
				if (var.LocalName == null) continue;
				if (val == null) continue;
				
				output.WriteStartElement("binding");
				output.WriteAttributeString("name", var.LocalName);
				
				if (val.Uri != null) {
					output.WriteElementString("uri", val.Uri);
				} else if (val is Literal) {
					output.WriteStartElement("literal");
					Literal literal = (Literal)val;
					if (literal.DataType != null)
						output.WriteAttributeString("datatype", literal.DataType);
					if (literal.Language != null)
						output.WriteAttributeString("xml", "lang", null, literal.Language);
					output.WriteString(literal.Value);
					output.WriteEndElement();				
				} else {
					string id;
					if (blankNodes.ContainsKey(val))
						id = (string)blankNodes[val];
					else {
						id = "r" + (++blankNodeCounter);
						blankNodes[val] = id;
					}
					output.WriteStartElement("bnode");
					output.WriteString(id);
					output.WriteEndElement();
				}
				
				output.WriteEndElement();
			}
			output.WriteEndElement();
			
			return true;
		}
		
		public override void Finished() {
			output.WriteEndElement(); // results
			output.WriteEndElement(); // sparql
			output.Flush();
		}
	}
	
}	
