using System;
using System.Collections;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;

using SemWeb;

namespace SemWeb.Util {
	public class XPathSemWebNavigator : XPathNavigator {
		Store model;
		NamespaceManager nsmgr;
		NSWrap nswrap;
		
		ArrayList stack = new ArrayList();
		Position current;
		
		private static Entity ExpandEntitiesOfType = new BNode();
		
		Entity rdfType = NS.RDF+"type";
		
		private class Position {
			public int Index;
			public bool FirstChild, LastChild;
			
			public Entity Predicate;
			public Resource Object;
			
			public Position[] Children;
		}
		
		private class NSWrap : XsltContext {
			NamespaceManager nsmgr;
			
			public NSWrap(NamespaceManager nsmgr) : base(new NameTable()) { this.nsmgr = nsmgr; }
			
			public override bool HasNamespace (string prefix) {
				return LookupNamespace(prefix) != null;
			}
			
			public override string LookupNamespace (string prefix) {
				return nsmgr.GetNamespace(prefix);
			}
			public override string LookupPrefix (string uri) {
				return nsmgr.GetPrefix(uri);
			}
			
			// These don't really do anything (yet?).
			
			public override bool Whitespace {
				get { return false; }
			} 
			
			public override int CompareDocument (string baseUri, string nextbaseUri) {
				return baseUri.CompareTo(nextbaseUri);
			}
			
			public override bool PreserveWhitespace (System.Xml.XPath.XPathNavigator node) {
				return false;
			}
			
			public override IXsltContextFunction ResolveFunction (string prefix, string name, System.Xml.XPath.XPathResultType[] ArgTypes) {
				return null;
			}

			public override IXsltContextVariable ResolveVariable (string prefix, string name) {
				return null;
			}
		}

		public XPathSemWebNavigator(Store model, NamespaceManager namespaces) : this(ExpandEntitiesOfType, model, namespaces, null) { }

		public XPathSemWebNavigator(Entity root, Store model, NamespaceManager namespaces) : this(root, model, namespaces, null) { }
		
		private XPathSemWebNavigator(Entity root, Store model, NamespaceManager namespaces, string exapandThisPredicate) {
			this.model = model;
			
			if (!(namespaces is SemWeb.IO.AutoPrefixNamespaceManager))
				namespaces = new SemWeb.IO.AutoPrefixNamespaceManager(namespaces);
			this.nsmgr = namespaces;
			nswrap = new NSWrap(nsmgr);
			
			Position start = new Position();
			start.FirstChild = true;
			start.LastChild = true;
			start.Predicate = root; // a trick to make sure the URI info for the root reflects the root
			start.Object = root;
			if (exapandThisPredicate != null)
				Expand(start, exapandThisPredicate);
			current = start;
		}
		
		private XPathSemWebNavigator(XPathSemWebNavigator clone) {
			MoveTo(clone);
		}
		
		private void Expand(Position p) {
			Expand(p, null);
		}
		
		private void Expand(Position p, string expandOnlyThisPredicate) {
			if (!(p.Object is Entity)) {
				p.Children = new Position[0];
				return;
			}
			
			ArrayList children = new ArrayList();
			int ctr = 0;
			
			if (p.Object == ExpandEntitiesOfType) {
				if (expandOnlyThisPredicate == null) {
					// Get a list of entities and their types.
					foreach (Statement s in model.Select(new Statement(null, rdfType, null))) {
						if (!(s.Object is Entity)) continue;
						Position c = new Position();
						c.Index = ctr++;
						c.Predicate = (Entity)s.Object;
						c.Object = s.Subject;
						children.Add(c);
					}
				} else {
					foreach (Entity e in model.GetEntitiesOfType(expandOnlyThisPredicate)) {
						Position c = new Position();
						c.Predicate = expandOnlyThisPredicate;
						c.Index = ctr++;
						c.Object = e;
						children.Add(c);
					}
				}
			} else {

			if (expandOnlyThisPredicate == null || !expandOnlyThisPredicate.StartsWith("!")) {
				Statement q = new Statement(
					(Entity)p.Object,
					expandOnlyThisPredicate == null ? (Entity)null : (Entity)expandOnlyThisPredicate,
					null);
				
				foreach (Statement s in model.Select(q)) {
					Position c = new Position();
					c.Index = ctr++;
					c.Predicate = s.Predicate;
					c.Object = s.Object;
					children.Add(c);
				}
			}
			
			if (expandOnlyThisPredicate == null || expandOnlyThisPredicate.StartsWith("!")) {
				Statement q = new Statement(
					null,
					expandOnlyThisPredicate == null ? (Entity)null : (Entity)expandOnlyThisPredicate.Substring(1),
					p.Object);
				
				foreach (Statement s in model.Select(q)) {
					Position c = new Position();
					c.Index = ctr++;
					c.Predicate = "!" + s.Predicate;
					c.Object = s.Subject;
					children.Add(c);
				}
			}
			
			}
			
			p.Children = (Position[])children.ToArray(typeof(Position));
			
			if (p.Children.Length > 0) {
				p.Children[0].FirstChild = true;
				p.Children[p.Children.Length-1].LastChild = true;
			}
		}

		public override string BaseURI { get { return ""; } }

		public override bool HasAttributes { get { return false; } }

		public override bool HasChildren { get { return true; } }

		public override bool IsEmptyElement { get { return false; } }

		public override string LocalName {
			get {
				string p, l;
				if (current.Predicate == ExpandEntitiesOfType)
					return "root";
				if (current.Predicate.Uri == null)
					return "anonymous";
				if (nsmgr.Normalize(current.Predicate.Uri, out p, out l))
					return l;
				throw new InvalidOperationException("The local name of " + current.Predicate.Uri + " could not be determined.");
			}
		}

		public override string Name {
			get {
				if (current.Predicate == ExpandEntitiesOfType)
					return "root";
				if (current.Predicate.Uri == null)
					return "anonymous";
				return nsmgr.Normalize(current.Predicate.Uri);
			}
		}

		public override string NamespaceURI {
			get {
				string p, l;
				if (current.Predicate.Uri == null)
					return "anonymous";
				if (nsmgr.Normalize(current.Predicate.Uri, out p, out l))
					return nsmgr.GetNamespace(p);
				throw new InvalidOperationException("The namespace URI of " + current.Predicate.Uri + " could not be determined.");
			}
		}

		public override XmlNameTable NameTable {
			get {
				return null;
			}
		}

		public override XPathNodeType NodeType {
			get {
				if (stack.Count == 0)
					return XPathNodeType.Root;
				return XPathNodeType.Element;
			}
		}

		public override string Prefix {
			get {
				string p, l;
				if (nsmgr.Normalize(current.Predicate.Uri, out p, out l))
					return p;
				throw new InvalidOperationException("The prefix of " + current.Predicate.Uri + " could not be determined.");
			}
		}

		public override string Value {
			get {
				if (current.Predicate == ExpandEntitiesOfType)
					return "root";
				if (current.Object is Literal)
					return ((Literal)current.Object).Value;
				if (current.Object.Uri == null)
					return "";
				return current.Object.Uri;
			}
		}

		public override string XmlLang { get { return ""; } }

		public override XPathNavigator Clone () {
			return new XPathSemWebNavigator(this);
		}

		//public virtual XmlNodeOrder ComparePosition (XPathNavigator nav)
		
		public override string GetAttribute (string localName, string namespaceURI) {
			return "";
		}

		public override string GetNamespace (string name) {
			return nsmgr.GetNamespace(name);
		}
		
		public override bool IsSamePosition (XPathNavigator other) {
			if (!(other is XPathSemWebNavigator)) return false;
			XPathSemWebNavigator o = (XPathSemWebNavigator)other;
			return (o.current == current);
		}

		public override bool MoveTo (XPathNavigator other) {
			XPathSemWebNavigator clone = other as XPathSemWebNavigator;
			if (clone == null) return false;
			this.model = clone.model;
			this.nsmgr = clone.nsmgr;
			this.nswrap = clone.nswrap;
			this.stack = (ArrayList)clone.stack.Clone();
			this.current = clone.current;
			return true;
		}

		public override bool MoveToAttribute (string localName, string namespaceURI) { return false; }

		public override bool MoveToNamespace (string name) { return false; }

		public override bool MoveToFirst () {
			return MoveToFirstChild();
		}

		public override void MoveToRoot () {
			if (stack.Count == 0) return;
			current = (Position)stack[0];
			stack.Clear();
		}

		public override bool MoveToFirstAttribute () { return false; }

		public override bool MoveToFirstChild () {
			if (current.Children == null) Expand(current);
			if (current.Children.Length == 0) return false;
			stack.Add(current);
			current = current.Children[0];
			return true;
		}

		public override bool MoveToFirstNamespace (XPathNamespaceScope namespaceScope) { return false; }

		public override bool MoveToId (string id) { return false; }

		public override bool MoveToNext () {
			if (current.LastChild) return false;
			current = ((Position)stack[stack.Count-1]).Children[current.Index+1];
			return true;
		}

		public override bool MoveToNextAttribute () { return false; }

		public override bool MoveToNextNamespace (XPathNamespaceScope namespaceScope) { return false; }

		public override bool MoveToParent () {
			if (stack.Count == 0) return false;
			current = (Position)stack[stack.Count-1];
			stack.RemoveAt(stack.Count-1);
			return true;
		}

		public override bool MoveToPrevious () {
			if (current.FirstChild) return false;
			current = ((Position)stack[stack.Count-1]).Children[current.Index-1];
			return true;
		}
		
		public override XPathNodeIterator SelectChildren (string name, string namespaceURI) {
			if (current.Object is Literal) throw new InvalidOperationException("The navigator is positioned on a literal element.");
			return new XPathSemWebNavigator((Entity)current.Object, model, nsmgr, namespaceURI + name).SelectChildren(XPathNodeType.All);
		}
		
		public override XPathNodeIterator Select (XPathExpression expr) {
			expr.SetContext(nswrap);
			return base.Select(expr);
		}
		
		public override object Evaluate (XPathExpression expr) {
			expr.SetContext(nswrap);
			return base.Evaluate(expr);
		}
		
		public override object Evaluate (XPathExpression expr, XPathNodeIterator context) {
			expr.SetContext(nswrap);
			return base.Evaluate(expr, context);
		}
	}
}
