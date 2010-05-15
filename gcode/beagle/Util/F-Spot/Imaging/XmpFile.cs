using System.Xml;
using System.Collections;
using SemWeb;

namespace FSpot.Xmp {
	public class XmpFile : SemWeb.StatementSource, SemWeb.StatementSink
	{
		MetadataStore store;

                // false seems like a safe default
                public bool Distinct {
                        get { return false; }
                }

		public MetadataStore Store {
			get { return store; }
			set { store = value; }
		}

		public XmpFile (System.IO.Stream stream) : this ()
		{
			Load (stream);
		}
		
		public XmpFile ()
		{
			store = new MetadataStore ();
		}
		
		public void Load (System.IO.Stream stream)
		{
			try {
				RdfXmlReader reader = new RdfXmlReader (stream);
				reader.BaseUri = MetadataStore.FSpotXMPBase;
				store.Import (reader);
				//Dump ();
			} catch (System.Exception e) {
				Beagle.Util.Log.Error (e, "Error loading TIFF file");
			}
		}

		private class XmpWriter : RdfXmlWriter {
			public XmpWriter (XmlDocument dest) : base (dest)
			{
				BaseUri = MetadataStore.FSpotXMPBase;
			}
			
			public override void Add (Statement stmt) 
			{
				string predicate = stmt.Predicate.Uri;
				string prefix;
				string localname;

				// Fill in the namespaces with nice prefixes
				if (MetadataStore.Namespaces.Normalize (predicate, out prefix, out localname)) {
					if (prefix != null)
						Namespaces.AddNamespace (predicate.Remove (predicate.Length - localname.Length, localname.Length), prefix);
				}
				base.Add (stmt);
			}
		}

		public void Save (System.IO.Stream stream)
		{
			try {
				XmlTextWriter text;
				RdfXmlWriter writer;
                                XmlDocument rdfdoc = new XmlDocument();

                                // first, construct the rdf guts, semweb style
                                writer = new XmpWriter (rdfdoc);
				//writer.Namespaces.Parent = MetadataStore.Namespaces;
				writer.Write (store);
				writer.Close ();
			       
                                // now construct the xmp wrapper packet
				text = new XmlTextWriter (stream, System.Text.Encoding.UTF8);
 				text.Formatting = Formatting.Indented;
                        
                                text.WriteProcessingInstruction ("xpacket", "begin=\"\ufeff\" id=\"W5M0MpCehiHzreSzNTczkc9d\"");
                                text.WriteStartElement ("x:xmpmeta");
                                text.WriteAttributeString ("xmlns", "x", null, "adobe:ns:meta/");

				((XmlElement)rdfdoc.ChildNodes[1]).RemoveAttribute ("xml:base");
				rdfdoc.ChildNodes[1].WriteTo (text);

                                // now close off the xmp packet
                                text.WriteEndElement ();
                                text.WriteProcessingInstruction ("xpacket", "end=\"r\"");
				text.Close ();
				
			} catch (System.Exception e) {
				//System.Console.WriteLine (e);
			}
		}

		public bool Add (Statement stmt)
		{
			return ((SemWeb.StatementSink)store).Add (stmt);
		}
		
		public void Select (SemWeb.StatementSink sink)
		{
			store.Select (sink);
		}

		public void Dump ()
		{
			foreach (SemWeb.Statement stmt in store) {
				System.Console.WriteLine(stmt);
			}
		}

#if TEST_XMP
		static void Main (string [] args)
		{
			XmpFile xmp = new XmpFile (System.IO.File.OpenRead (args [0]));
			//xmp.Store.Dump ();
#if false
			System.IO.StreamReader stream = new System.IO.StreamReader (System.IO.File.OpenRead (args [0]));

			while (stream.BaseStream.Position < stream.BaseStream.Length) {
				//System.Console.WriteLine (stream.ReadLine ());
			}
#endif
		}
#endif
	}
}
