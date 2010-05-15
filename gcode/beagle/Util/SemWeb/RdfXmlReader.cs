using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Xml;

using SemWeb.Util;

namespace SemWeb {
	public class RdfXmlReader : RdfReader {
		// TODO: Make some of the errors warnings.
	
		XmlReader xml;
		bool closed;
		
		Hashtable blankNodes = new Hashtable();
		UriMap namedNodes = new UriMap();
		Hashtable seenIDs = new Hashtable();
		
		StatementSink storage;
		
		static readonly Entity
			rdfType = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
			rdfFirst = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first",
			rdfRest = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest",
			rdfNil = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
			rdfSubject = "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject",
			rdfPredicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate",
			rdfObject = "http://www.w3.org/1999/02/22-rdf-syntax-ns#object",
			rdfStatement = "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement";
		
		#if !SILVERLIGHT
		public RdfXmlReader(XmlDocument document) {
			xml = new XmlBaseAwareReader(new XmlNodeReader(document));
			LoadNamespaces();
		}
		#endif
		
		public RdfXmlReader(XmlReader document) {
			#if !DOTNET2
				XmlValidatingReader reader = new XmlValidatingReader(document); // decodes entity definitions
				reader.ValidationType = ValidationType.None;
			#elif SILVERLIGHT
				XmlReader reader = document;
			#else
				XmlReaderSettings settings = new XmlReaderSettings();
				settings.ValidationType = ValidationType.None;
				settings.ProhibitDtd = false;
				settings.CloseInput = true;
				XmlReader reader = XmlReader.Create(document, settings);
			#endif
        
			xml = new XmlBaseAwareReader(reader);
			LoadNamespaces();
		}
		
		#if !SILVERLIGHT
		public RdfXmlReader(TextReader document) : this(new XmlTextReader(document)) {
		}
		#else
		public RdfXmlReader(TextReader document) {
			throw new NotSupportedException("Reading RDF/XML from a TextReader is not supported in the Silverlight build of the SemWeb library. Someone needs to fix this.");
		}
		#endif

		public RdfXmlReader(Stream document) : this(new StreamReader(document)) {
		}

		public RdfXmlReader(TextReader document, string baseUri) : this(document) {
			BaseUri = baseUri;
		}

		public RdfXmlReader(Stream document, string baseUri) : this(new StreamReader(document), baseUri) {
		}

		public RdfXmlReader(string file, string baseUri) : this(GetReader(file), baseUri) {
		}

		public RdfXmlReader(string file) : this(GetReader(file), "file:///" + file) {
		}
		
		protected override void Dispose() {
			if (!closed)
				xml.Close();
			closed = true;
		}

		private void LoadNamespaces() {
			// Move to the document element and load any namespace
			// declarations on the node.

			while (xml.Read()) {
				if (xml.NodeType != XmlNodeType.Element) continue;

				if (xml.MoveToFirstAttribute()) {
					do {
						if (xml.Prefix == "xmlns")
							Namespaces.AddNamespace(xml.Value, xml.LocalName);
					} while (xml.MoveToNextAttribute());
					xml.MoveToElement();
				}
				break;
			}
		}

		public override void Select(StatementSink storage) {
			// Read past the processing instructions to
			// the document element.  If it is rdf:RDF,
			// then process the description nodes within it.
			// Otherwise, the document element is itself a
			// description.
			
			this.storage = storage;

			bool first = true; // on the first iteration don't
											   // advance to the next node -- we already did that
			while (first || xml.Read()) {
				first = false;

				if (xml.NodeType != XmlNodeType.Element) continue;
				
				if (xml.NamespaceURI == NS.RDF && xml.LocalName == "RDF" ) {
					// If there is an xml:base here, set BaseUri so
					// the application can recover it.  It doesn't
					// affect parsing since the xml:base attribute
					// will override BaseUri.
					string xmlbase = xml.GetAttribute("xml:base");
					if (xmlbase != null) BaseUri = xmlbase;
					
					while (xml.Read()) {
						if (xml.NodeType == XmlNodeType.Element)
							ParseDescription();
					}
					
				} else {
					ParseDescription();
				
				}
				break;
			}
			
			Dispose();
		}
		
		private string CurNode() {
			if (xml.NamespaceURI == "")
				OnWarning("Element node must be qualified (" + xml.Name + ")");
			if (xml.Prefix != "")
				Namespaces.AddNamespace(xml.NamespaceURI, xml.Prefix);

			// This probably isn't quite right, but compensates
			// for a corresponding hash issue in XmlWriter.
			if (xml.NamespaceURI == "" && BaseUri == null)
				return "#" + xml.LocalName;

			return CheckUri(xml.NamespaceURI + xml.LocalName);
		}
		
		private string CheckUri(string uri) {
			string error = Entity.ValidateUri(uri);
			if (error != null)
				OnWarning("The URI <" + uri + "> is not valid: " + error);
			return uri;
		}
		
		private int isset(string attribute) {
			return attribute != null ? 1 : 0;
		}
		
		private string Unrelativize(string uri) {
			return CheckUri(GetAbsoluteUri(xml.BaseURI != "" ? xml.BaseURI : BaseUri, uri));
		}
		
		private Entity GetBlankNode(string nodeID) {
			if (blankNodes.ContainsKey(nodeID))
				return (Entity)blankNodes[nodeID];
			
			Entity entity = new BNode(nodeID);
			blankNodes[nodeID] = entity;

			return entity;
		}
		
		private Entity GetNamedNode(string uri) {
			if (!ReuseEntities)
				return new Entity(uri);
		
			Entity ret = (Entity)namedNodes[uri];
			if (ret != null) return ret;
			ret = new Entity(uri);
			namedNodes[uri] = ret;
			return ret;
		}
		
		private Entity ParseDescription() {
			// The XmlReader is positioned on an element node
			// that is a description of an entity.
			// On returning, the reader is positioned after the
			// end element of the description node.
			
			string nodeID = xml.GetAttribute("nodeID", NS.RDF);
			string about = xml.GetAttribute("about", NS.RDF);
			string ID = xml.GetAttribute("ID", NS.RDF);
			if (isset(nodeID) + isset(about) + isset(ID) > 1)
				OnError("An entity description cannot specify more than one of rdf:nodeID, rdf:about, and rdf:ID");

			if (nodeID != null && !IsValidXmlName(nodeID))
				OnWarning("'" + nodeID + "' is not a valid XML Name");
			if (ID != null && !IsValidXmlName(ID))
				OnWarning("'" + ID + "' is not a valid XML Name");
				
			Entity entity;
			
			if (about != null)
				entity = GetNamedNode(Unrelativize(about));
			else if (ID != null) {
				entity = GetNamedNode(Unrelativize("#" + ID));
				
				if (seenIDs.ContainsKey(entity.Uri))
					OnWarning("Two descriptions should not use the same rdf:ID: <" + entity.Uri + ">");
				seenIDs[entity.Uri] = seenIDs;
			} else if (nodeID != null)
				entity = GetBlankNode(nodeID);
			else
				entity = new BNode();
			
			// If the name of the element is not rdf:Description,
			// then the name gives its type.
			string curnode = CurNode();
			if (curnode != NS.RDF + "Description") {
				if (IsRestrictedName(curnode) || IsDeprecatedName(curnode))
					OnError(xml.Name + " cannot be the type of a resource.");
				if (curnode == NS.RDF + "li") OnError("rdf:li cannot be the type of a resource");
				storage.Add(new Statement(entity, rdfType, (Entity)curnode, Meta));
			}
			
			ParsePropertyAttributes(entity);
			ParsePropertyNodes(entity);
			
			return entity;
		}
		
		private bool ParsePropertyAttributes(Entity entity) {
			bool foundAttrs = false;
			
			if (!xml.MoveToFirstAttribute()) return false;
			do {
				// Propery attributes in the default namespace
				// should be ignored.
				if (xml.NamespaceURI == "")
					continue;
			
				string curnode = CurNode();
				
				// rdf:type is interpreted with an entity object,
				// not a literal object.
				if (curnode == NS.RDF + "type") {
					storage.Add(new Statement(entity, rdfType, (Entity)xml.Value, Meta));
					foundAttrs = true;
					continue;
				}
				
				// Properties which are not recognized as property
				// attributes and should be ignored.
				if (IsRestrictedName(curnode))
					continue;
				if (IsDeprecatedName(curnode))
					OnError(xml.Name + " is deprecated.");
				
				// Properties which are invalid as attributes.
				if (curnode == NS.RDF + "li")
					OnError("rdf:li is not a valid attribute");
				if (curnode == NS.RDF + "aboutEach" || curnode == NS.RDF + "aboutEachPrefix")
					OnError("rdf:aboutEach has been removed from the RDF spec");
				
				// Unrecognized attributes in the xml namespace should be ignored.
				if (xml.Prefix == "xml") continue;
				if (xml.Prefix == "xmlns") continue;
				if (curnode == "http://www.w3.org/2000/xmlns/xmlns") continue;
				
				// This is a literal property attribute.
				string lang = xml.XmlLang != "" ? xml.XmlLang : null;
				storage.Add(new Statement(entity, curnode,
					new Literal(xml.Value, lang, null), Meta));
				Namespaces.AddNamespace(xml.NamespaceURI, xml.Prefix);
				foundAttrs = true;
					
			} while (xml.MoveToNextAttribute());
			
			xml.MoveToElement();
			
			return foundAttrs;
		}
		
		private void ParsePropertyNodes(Entity subject) {
			// The reader is positioned within a description node.
			// On returning, the reader is positioned after the
			// end element of the description node.
			
			if (xml.IsEmptyElement) return;
			
			int liIndex = 1;
			
			while (xml.Read()) {
				if (xml.NodeType == XmlNodeType.EndElement)
					break;

				if (xml.NodeType == XmlNodeType.Text) {
					OnWarning("Text \"" + xml.Value + "\" ignored in description node");
					continue;
				}

				if (xml.NodeType != XmlNodeType.Element)
					continue;
				
				ParseProperty(subject, ref liIndex);
			}
		}
		
		private void ParseProperty(Entity subject, ref int liIndex) {
			// The reader is positioned on a propert node,
			// and on returning the reader is positioned past
			// that node.
			
			// Get all of the attributes before we move the reader forward.
			
			string nodeID = xml.GetAttribute("nodeID", NS.RDF);
			string resource = xml.GetAttribute("resource", NS.RDF);
			
			string parseType = xml.GetAttribute("parseType", NS.RDF);
			string datatype = xml.GetAttribute("datatype", NS.RDF);
			
			string lang = xml.XmlLang != "" ? xml.XmlLang : null;

			string predicate = CurNode();
			if (predicate == NS.RDF + "li")
				predicate = NS.RDF + "_" + (liIndex++);

			if (IsRestrictedName(predicate))
				OnError(xml.Name + " cannot be used as a property name.");
			if (IsDeprecatedName(predicate))
				OnError(xml.Name + " has been deprecated and cannot be used as a property name.");

			string ID = xml.GetAttribute("ID", NS.RDF);

			if (nodeID != null && !IsValidXmlName(nodeID))
				OnWarning("'" + nodeID + "' is not a valid XML Name");
			if (ID != null && !IsValidXmlName(ID))
				OnWarning("'" + ID + "' is not a valid XML Name");
				
			Resource objct = null;
			if (nodeID != null || resource != null) {
				if (isset(nodeID) + isset(resource) > 1)
					OnError("A predicate node cannot specify more than one of rdf:nodeID and rdf:resource");
					
				if (parseType != null || datatype != null)
					OnError("The attributes rdf:parseType and rdf:datatype are not valid on a predicate with a rdf:nodeID or rdf:resource attribute");
					
				// Object is an entity given by nodeID or resource.
				if (nodeID != null)
					objct = GetBlankNode(nodeID);
				else if (resource != null)
					objct = GetNamedNode(Unrelativize(resource));
					
				ParsePropertyAttributes((Entity)objct);
				
				// No children are allowed in this element.
				if (!xml.IsEmptyElement)
				while (xml.Read()) {
					if (xml.NodeType == XmlNodeType.EndElement) break;
					if (xml.NodeType == XmlNodeType.Whitespace) continue;
					if (xml.NodeType == XmlNodeType.Comment) continue;
					if (xml.NodeType == XmlNodeType.ProcessingInstruction) continue;
					OnError("Content is not allowed within a property with a rdf:nodeID or rdf:resource attribute");
				}
			
			} else if (parseType != null && parseType == "Literal") {
				if (datatype != null)
					OnError("The attribute rdf:datatype is not valid on a predicate whose parseType is Literal.");

				datatype = "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral";
				
				if (ParsePropertyAttributes(new BNode()))
					OnError("Property attributes are not valid when parseType is Literal");

				// TODO: Do we canonicalize according to:
				// http://www.w3.org/TR/2002/REC-xml-exc-c14n-20020718/ ?
				
				objct = new Literal(xml.ReadInnerXml(), null, datatype);

			} else if (parseType != null && parseType == "Resource") {
				objct = new BNode();
				
				ParsePropertyAttributes((Entity)objct);
				if (!xml.IsEmptyElement)
					ParsePropertyNodes((Entity)objct);
				
			} else if (parseType != null && parseType == "Collection") {
				Entity collection = new BNode();
				Entity lastnode = collection;
				bool empty = true;
				
				ParsePropertyAttributes(collection);
				
				if (!xml.IsEmptyElement)
				while (xml.Read()) {
					if (xml.NodeType == XmlNodeType.EndElement) break;
					if (xml.NodeType != XmlNodeType.Element) continue;
					
					if (!empty) {
						Entity next = new BNode();
						storage.Add(new Statement(lastnode, rdfRest, next, Meta));
						lastnode = next;
					}
					
					Entity item = ParseDescription();
					storage.Add(new Statement(lastnode, rdfFirst, item, Meta));
					
					empty = false;
				}

				storage.Add(new Statement(lastnode, rdfRest, rdfNil, Meta));
				
				if (empty)
					objct = rdfNil;
				else
					objct = collection;
					
			} else if (parseType != null) {
				OnError("Invalid value for parseType: '" + parseType + "'");
				
			} else if (datatype != null) {
				// Note that any xml:lang is discarded.
				
				if (ParsePropertyAttributes(new BNode()))
					OnError("Property attributes are not valid when a datatype is given");
					
				if (xml.IsEmptyElement) {
					objct = new Literal("", null, datatype);
				} else {
					#if !DOTNET2
					objct = new Literal(xml.ReadString(), null, datatype);
					#else
					objct = new Literal(xml.ReadElementContentAsString(), null, datatype);
					#endif
					if (xml.NodeType != XmlNodeType.EndElement)
						OnError("XML markup may not appear in a datatyped literal property.");
				}
			
			} else {
				// We don't know whether the contents of this element
				// refer to a literal or an entity.  If an element is
				// a child of this node, then it must be an entity.
				// If the property has predicate attributes, then it
				// is an anonymous entity.  Otherwise the text content
				// is the literal value.
				
				objct = new BNode();
				if (ParsePropertyAttributes((Entity)objct)) {
					// Found property attributes.  There should be no other internal content?
					
					if (!xml.IsEmptyElement)
					while (xml.Read()) {
						if (xml.NodeType == XmlNodeType.EndElement) break;
						if (xml.NodeType == XmlNodeType.Whitespace) continue;
						if (xml.NodeType == XmlNodeType.Comment) continue;
						if (xml.NodeType == XmlNodeType.ProcessingInstruction) continue;
						OnError(xml.NodeType + " is not allowed within a property with property attributes");
					}
					
				} else {
					StringBuilder textcontent = new StringBuilder();
					bool hadText = false;
					bool hadElement = false;
					
					if (!xml.IsEmptyElement)
					while (xml.Read()) {
						if (xml.NodeType == XmlNodeType.EndElement) break;
						if (xml.NodeType == XmlNodeType.Element) {
							if (hadText)
								OnError("Both text and elements are present as a property value");
							if (hadElement)
								OnError("A property node cannot contain more than one entity description.  " + objct + " already found.");
							
							hadElement = true;
							
							objct = ParseDescription();
							
						} else if (xml.NodeType == XmlNodeType.Text || xml.NodeType == XmlNodeType.SignificantWhitespace) {
							if (hadElement)
								OnError("Both text and elements are present as a property value");
							textcontent.Append(xml.Value);
							hadText = true;
						} else {
							textcontent.Append(xml.Value);
						}
					}
					
					if (!hadElement)
						objct = new Literal(textcontent.ToString(), lang, null);
				}
			}
				
			storage.Add(new Statement(subject, predicate, objct, Meta));
			
			if (ID != null) {
				// In addition to adding the statement as normal, also
				// add a reified statement.
				Entity statement = GetNamedNode(Unrelativize("#" + ID));;
				storage.Add(new Statement(statement, rdfType, rdfStatement, Meta));
				storage.Add(new Statement(statement, rdfSubject, subject, Meta));
				storage.Add(new Statement(statement, rdfPredicate, (Entity)predicate, Meta));
				storage.Add(new Statement(statement, rdfObject, objct, Meta));
			}
		}
		
		private bool IsRestrictedName(string name) {
			if (name == NS.RDF + "RDF") return true;
			if (name == NS.RDF + "Description") return true;
			if (name == NS.RDF + "ID") return true;
			if (name == NS.RDF + "about") return true;
			if (name == NS.RDF + "parseType") return true;
			if (name == NS.RDF + "resource") return true;
			if (name == NS.RDF + "nodeID") return true;
			if (name == NS.RDF + "datatype") return true;
			return false;
		}
		
		private bool IsDeprecatedName(string name) {
			if (name == NS.RDF + "bagID") return true;
			if (name == NS.RDF + "aboutEach") return true;
			if (name == NS.RDF + "aboutEachPrefix") return true;
			return false;
		}

		private bool IsValidXmlName(string name) {
			// I'm not sure what's supposed to be valid, but this is just for warnings anyway.
			// CombiningChar and Extender characters are omitted.
			if (name.Length == 0) return false;
			char f = name[0];
			if (!char.IsLetter(f) && f != '_') return false;
			for (int i = 1; i < name.Length; i++) {
				f = name[i];
				if (!char.IsLetter(f) && !char.IsDigit(f) && f != '.' && f != '-' && f != '_')
					return false;
			}
			return true;
		}
		
		private void OnError(string message) {
			if (xml is IXmlLineInfo && ((IXmlLineInfo)xml).HasLineInfo()) {
				IXmlLineInfo line = (IXmlLineInfo)xml;
				message += ", line " + line.LineNumber + " col " + line.LinePosition;
			}
			throw new ParserException(message);
		}

		private new void OnWarning(string message) {
			if (xml is IXmlLineInfo && ((IXmlLineInfo)xml).HasLineInfo()) {
				IXmlLineInfo line = (IXmlLineInfo)xml;
				message += ", line " + line.LineNumber + " col " + line.LinePosition;
			}
			base.OnWarning(message);
		}
		
		private class XmlBaseAwareReader : XmlReader {
			XmlReader _reader;
			Stack _bases = new Stack();
		    Uri _baseUri;
		    bool temp = false;
		    XmlResolver resolver = new XmlUrlResolver();

		    public XmlBaseAwareReader(XmlReader reader) {
		    	_reader = reader;
	        	if (_reader.BaseURI != "")
	        		_baseUri = new Uri(_reader.BaseURI);
		    }

		    public override string BaseURI {
		        get { return _baseUri == null ? "" : _baseUri.AbsoluteUri; }
		    }

		    public override bool Read()
		    {        
		        if (!_reader.Read()) return false;
		        
		        if (temp) { // last element was an empty element
		        	_baseUri = (Uri)_bases.Pop();
		        	temp = false;
		        }

				if (_reader.NodeType == XmlNodeType.EndElement)
					_baseUri = (Uri)_bases.Pop();
		        
		        if (_reader.NodeType == XmlNodeType.Element) {
		            string baseAttr = _reader.GetAttribute("xml:base");
		            if (_reader.IsEmptyElement && baseAttr == null) {
		            	// do nothing : there is no EndElement, so no pop
		            } else {
		            	_bases.Push(_baseUri); // even if no change, there will be a pop
			            if (baseAttr != null)
	            			_baseUri = resolver.ResolveUri(_baseUri, baseAttr);
		            		
		            	// if this is an empty element, there is no EndElement,
		            	// so we must do a pop before processing the next node.
		            	temp = _reader.IsEmptyElement;
			    	}
		    	}
		        return true;            
		    }

			public override void Close () { _reader.Close(); }
			public override string GetAttribute (int i) { return _reader.GetAttribute(i); }
			public override string GetAttribute (string name) { return _reader.GetAttribute(name); }
			public override string GetAttribute (string localName, string namespaceName) { return _reader.GetAttribute(localName, namespaceName); }
			public override bool IsStartElement () { return _reader.IsStartElement(); }
			public override bool IsStartElement (string name) { return _reader.IsStartElement(name); }
			public override bool IsStartElement (string localName, string namespaceName) { return _reader.IsStartElement(localName, namespaceName); }
			public override string LookupNamespace (string prefix) { return _reader.LookupNamespace(prefix); }
			public override void MoveToAttribute (int i) { _reader.MoveToAttribute(i); }
			public override bool MoveToAttribute (string name) { return _reader.MoveToAttribute(name); }
			public override bool MoveToAttribute (string localName, string namespaceName) { return _reader.MoveToAttribute(localName, namespaceName); }
			public override XmlNodeType MoveToContent () { return _reader.MoveToContent(); }
			public override bool MoveToElement () { return _reader.MoveToElement(); }
			public override bool MoveToFirstAttribute () { return _reader.MoveToFirstAttribute(); }
			public override bool MoveToNextAttribute () { return _reader.MoveToNextAttribute(); }
			public override void ReadEndElement () { _reader.ReadEndElement(); }
			public override string ReadInnerXml () { return _reader.ReadInnerXml(); }
			public override string ReadOuterXml () { return _reader.ReadOuterXml(); }
			public override void ReadStartElement () { _reader.ReadStartElement(); }
			public override void ReadStartElement (string name) { _reader.ReadStartElement(name); }
			public override void ReadStartElement (string localName, string namespaceName) { _reader.ReadStartElement(localName, namespaceName); }
			public override void Skip () { _reader.Skip(); }

			#if !SILVERLIGHT
			public override bool ReadAttributeValue () { return _reader.ReadAttributeValue(); }
			public override string ReadElementString () { return _reader.ReadElementString(); }
			public override string ReadElementString (string name) { return _reader.ReadElementString(name); }
			public override string ReadElementString (string localName, string namespaceName) { return _reader.ReadElementString(localName, namespaceName); }
			public override string ReadString () { return _reader.ReadString(); }
			public override void ResolveEntity () { _reader.ResolveEntity(); }
			#endif
			
			public override int AttributeCount { get { return _reader.AttributeCount; } }
			public override int Depth { get { return _reader.Depth; } }
			public override bool EOF { get { return _reader.EOF; } }
			public override bool HasAttributes { get { return _reader.HasAttributes; } }
			public override bool IsDefault { get { return _reader.IsDefault; } }
			public override bool IsEmptyElement { get { return _reader.IsEmptyElement; } }
			public override string this [int i] { get { return _reader[i]; } }
			public override string this [string name] { get { return _reader[name]; } }
			public override string this [string localName, string namespaceName] { get { return _reader[localName, namespaceName]; } }
			public override string LocalName { get { return _reader.LocalName; } }
			public override string Name { get { return _reader.Name; } }
			public override string NamespaceURI { get { return _reader.NamespaceURI; } }
			public override XmlNameTable NameTable { get { return _reader.NameTable; } }
			public override XmlNodeType NodeType { get { return _reader.NodeType; } }
			public override string Prefix { get { return _reader.Prefix; } }
			public override ReadState ReadState { get { return _reader.ReadState; } }
			public override string Value { get { return _reader.Value; } }
			public override string XmlLang { get { return _reader.XmlLang; } }
			public override XmlSpace XmlSpace { get { return _reader.XmlSpace; } }
			
			#if !SILVERLIGHT
			public override bool CanResolveEntity { get { return _reader.CanResolveEntity; } }
			public override bool HasValue { get { return _reader.HasValue; } }
			public override char QuoteChar { get { return _reader.QuoteChar; } }
			#endif
		}		
	}
}

