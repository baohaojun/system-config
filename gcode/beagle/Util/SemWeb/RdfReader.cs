using System;
using System.Collections;
using System.IO;

#if !DOTNET2
using VariableSet = System.Collections.Hashtable;
using VariableList = System.Collections.ICollection;
using WarningsList = System.Collections.ArrayList;
#else
using VariableSet = System.Collections.Generic.Dictionary<SemWeb.Variable,SemWeb.Variable>;
using VariableList = System.Collections.Generic.ICollection<SemWeb.Variable>;
using WarningsList = System.Collections.Generic.List<string>;
#endif
 
namespace SemWeb {
	public class ParserException : ApplicationException {
		public ParserException (string message) : base (message) {}
		public ParserException (string message, Exception cause) : base (message, cause) {}
	}

	public abstract class RdfReader : StatementSource, IDisposable {
		Entity meta = Statement.DefaultMeta;
		string baseuri = null;
		WarningsList warnings = new WarningsList();
		VariableSet variables = new VariableSet();
		bool reuseentities = false;
		NamespaceManager nsmgr = new NamespaceManager();

		public Entity Meta {
			get {
				return meta;
			}
			set {
				meta = value;
			}
		}
		
		public string BaseUri {
			get {
				return baseuri;
			}
			set {
				baseuri = value;
			}
		}
		
		public bool ReuseEntities {
			get {
				return reuseentities;
			}
			set {
				reuseentities = value;
			}
		}
		
		bool StatementSource.Distinct { get { return false; } }
		
		public NamespaceManager Namespaces { get { return nsmgr; } }
		
		public VariableList Variables { get { return variables.Keys; } }
		
		#if !DOTNET2
		public IList Warnings { get { return ArrayList.ReadOnly(warnings); } }
		#else
		public System.Collections.Generic.ICollection<string> Warnings { get { return warnings.AsReadOnly(); } }
		#endif
		
		protected void AddVariable(Variable variable) {
			variables[variable] = variable;
		}

		public abstract void Select(StatementSink sink);
		
		protected virtual void Dispose() {
		}
		
		void IDisposable.Dispose() {
			Dispose();
		}
		
		internal static string NormalizeMimeType(string type) {
			switch (type) {
				case "text/xml":
				case "application/xml":
				case "application/rdf+xml":
					return "xml";

				case "text/n3":
				case "text/rdf+n3":
				case "application/n3":
				case "application/turtle":
				case "application/x-turtle":
					return "n3";
			}
			
			return type;
		}
		
		public static RdfReader Create(string type, string source) {
			type = NormalizeMimeType(type);
		
			switch (type) {
				case "xml":
					return new RdfXmlReader(source);
				case "n3":
					return new N3Reader(source);
				default:
					throw new ArgumentException("Unknown parser or MIME type: " + type);
			}
		}
		
		public static RdfReader Create(string type, Stream source) {
			type = NormalizeMimeType(type);

			switch (type) {
				case "xml":
					return new RdfXmlReader(source);
				case "n3":
					return new N3Reader(new StreamReader(source, System.Text.Encoding.UTF8));
				default:
					throw new ArgumentException("Unknown parser or MIME type: " + type);
			}
		}

		#if false
		public static RdfReader LoadFromUri(Uri webresource) {
			// TODO: Add Accept header for HTTP resources.
			
			System.Net.WebRequest rq = System.Net.WebRequest.Create(webresource);
			System.Net.WebResponse resp = rq.GetResponse();
			
			string mimetype = resp.ContentType;
			if (mimetype.IndexOf(';') > -1)
				mimetype = mimetype.Substring(0, mimetype.IndexOf(';'));
				
			mimetype = NormalizeMimeType(mimetype.Trim());
			
			RdfReader reader;
			
			if (mimetype == "xml" || mimetype == "application/rss+xml")
				reader = new RdfXmlReader(resp.GetResponseStream());
					
			else if (mimetype == "n3")
				reader = new N3Reader(new StreamReader(resp.GetResponseStream(), System.Text.Encoding.UTF8));
			
			else if (webresource.LocalPath.EndsWith(".rdf") || webresource.LocalPath.EndsWith(".xml") || webresource.LocalPath.EndsWith(".rss"))
				reader = new RdfXmlReader(resp.GetResponseStream());
			
			else if (webresource.LocalPath.EndsWith(".n3") || webresource.LocalPath.EndsWith(".ttl") || webresource.LocalPath.EndsWith(".nt"))
				reader = new N3Reader(new StreamReader(resp.GetResponseStream(), System.Text.Encoding.UTF8));

			else
				throw new InvalidOperationException("Could not determine the RDF format of the resource.");
				
			reader.BaseUri = resp.ResponseUri.ToString();
			
			return reader;
		}
		#endif
		
		internal static TextReader GetReader(string file) {
			if (file == "-") return Console.In;
			return new StreamReader(file);
		}
		
		protected void OnWarning(string message) {
			warnings.Add(message);
		}
		
		internal string GetAbsoluteUri(string baseuri, string uri) {
			if (baseuri == null) {
				if (uri == "")
					throw new ParserException("An empty relative URI was found in the document but could not be converted into an absolute URI because no base URI is known for the document.");
				return uri;
			}
			if (uri.IndexOf(':') != -1) return uri;
			#if !SILVERLIGHT
			try {
				UriBuilder b = new UriBuilder(baseuri);
				b.Fragment = null; // per W3 RDF/XML test suite
				return new Uri(b.Uri, uri, true).ToString();
			} catch (UriFormatException) {
				return baseuri + uri;
			}			
			#else
			return baseuri + uri;
			#endif
		}

	}
	
	internal class MultiRdfReader : RdfReader {
		private ArrayList parsers = new ArrayList();
		
		public ArrayList Parsers { get { return parsers; } }
		
		public override void Select(StatementSink storage) {
			foreach (RdfReader p in Parsers)
				p.Select(storage);
		}
	}
}

