using System;
using System.Collections;

namespace SemWeb {
	
	internal class NS {
		public const string RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
		public const string RDFS = "http://www.w3.org/2000/01/rdf-schema#";
	
		public const string XMLSCHEMA = "http://www.w3.org/2001/XMLSchema#";
	
		/*Entity entRDFTYPE = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";
		Entity entRDFFIRST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first";
		Entity entRDFREST = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest";
		Entity entRDFNIL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil";
		Entity entOWLSAMEAS = "http://www.w3.org/2002/07/owl#sameAs";
		Entity entLOGIMPLIES = "http://www.w3.org/2000/10/swap/log#implies";*/
	}
	
	public class NamespaceManager {
		NamespaceManager parent;
		Hashtable atob = new Hashtable();
		Hashtable btoa = new Hashtable();
		
		public NamespaceManager() : this (null) {
		}
		
		public NamespaceManager(NamespaceManager parent) {
			this.parent = parent;
		}
		
		public virtual void AddNamespace(string uri, string prefix) {
			atob[uri] = prefix;
			btoa[prefix] = uri;
		}
		
		public void AddFrom(NamespaceManager nsmgr) {
			foreach (string uri in nsmgr.GetNamespaces())
				AddNamespace(uri, nsmgr.GetPrefix(uri));
		}

		public virtual string GetNamespace(string prefix) {
			string ret = (string)btoa[prefix];
			if (ret != null) return ret;
			if (parent != null) return parent.GetNamespace(prefix);
			return null;
		}
		
		public virtual string GetPrefix(string uri) {
			string ret = (string)atob[uri];
			if (ret != null) return ret;
			if (parent != null) return parent.GetPrefix(uri);
			return null;
		}
		
		public bool Normalize(string uri, out string prefix, out string localname) {
			int hash = uri.LastIndexOf('#');
			if (hash > 0) {
				prefix = GetPrefix(uri.Substring(0, hash+1));
				if (prefix != null) {
					localname = uri.Substring(hash+1);
					return true;
				}
			}
			
			hash = uri.LastIndexOf('/');
			if (hash > 0) {
				prefix = GetPrefix(uri.Substring(0, hash+1));
				if (prefix != null) {
					localname = uri.Substring(hash+1);
					return true;
				}
			}
			
			prefix = null;
			localname = null;
			
			return false;
		}
		
		public string Normalize(string uri) {
			string prefix, localname;
			if (Normalize(uri, out prefix, out localname)) {
				bool ok = true;
				if (localname.Length == 0) ok = false;
				else if (!char.IsLetter(localname[0]) && localname[0] != '_') ok = false;
				foreach (char c in localname)
					if (!char.IsLetterOrDigit(c) && c != '-' && c != '_')
						ok = false;
				if (ok)
					return prefix + ":" + localname;
			}
			return "<" + uri + ">";
		}
		
		public string Resolve(string qname) {
			int colon = qname.IndexOf(':');
			if (colon == -1) throw new ArgumentException("Invalid qualified name.");
			string prefix = qname.Substring(0, colon);
			string ns = GetNamespace(prefix);
			if (ns == null) throw new ArgumentException("The prefix " + prefix + " is not declared.");
			return ns + qname.Substring(colon+1);
		}
		
		public ICollection GetNamespaces() {
			if (parent == null) return atob.Keys;
			ArrayList items = new ArrayList(atob.Keys);
			foreach (string ns in parent.GetNamespaces())
				if (!items.Contains(ns))
					items.Add(ns);
			return items;
		}

		public ICollection GetPrefixes() {
			if (parent == null) return atob.Values;
			ArrayList items = new ArrayList(atob.Values);
			foreach (string ns in parent.GetPrefixes())
				if (!items.Contains(ns))
					items.Add(ns);
			return items;
		}
	}
}

namespace SemWeb.IO {
	using SemWeb;
	
	internal class AutoPrefixNamespaceManager : NamespaceManager {
		int counter = 0;
		
		public AutoPrefixNamespaceManager() : this (null) {
		}
		
		public AutoPrefixNamespaceManager(NamespaceManager parent) : base(parent) {
		}
		
		public override string GetPrefix(string uri) {
			string ret = base.GetPrefix(uri);
			if (ret != null) return ret;
			
			if (uri == "http://www.w3.org/1999/02/22-rdf-syntax-ns#" && GetNamespace("rdf") == null)
				ret = "rdf";
			else if (uri == "http://www.w3.org/2000/01/rdf-schema#" && GetNamespace("rdfs") == null)
				ret = "rdfs";
			else if (uri == "http://www.w3.org/2002/07/owl#" && GetNamespace("owl") == null)
				ret = "owl";
			else if (uri == "http://purl.org/dc/elements/1.1/" && GetNamespace("dc") == null)
				ret = "dc";
			else if (uri == "http://xmlns.com/foaf/0.1/" && GetNamespace("foaf") == null)
				ret = "foaf";
			else			
				ret = "autons" + (counter++);
			AddNamespace(uri, ret);
			return ret;
		}
	}
}
