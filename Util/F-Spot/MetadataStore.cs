using SemWeb;
using SemWeb.Util;
using Mono.Unix;
using FSpot.Xmp;

namespace FSpot {
        internal class Description {
		string predicate;
		string description;
		string title;
		ValueFormat formater;
		
		static System.Collections.Hashtable table;

		static Description ()
		{
			Description [] preset = new Description [] {
				new Description ("dc:creator", Catalog.GetString ("Creator")),
				new Description ("dc:title", Catalog.GetString ("Title")),
				new Description ("dc:rights", Catalog.GetString ("Copyright")),
				new Description ("dc:subject", Catalog.GetString ("Subject and Keywords")),
				new Description ("tiff:Compression", Catalog.GetString ("Compression"), 
						 typeof (FSpot.Tiff.Compression)),
				new Description ("tiff:PlanarConfiguration", Catalog.GetString ("Planar Configuration"), 
						 typeof (FSpot.Tiff.PlanarConfiguration)),
				new Description ("tiff:Orientation", Catalog.GetString ("Orientation"), 
						 typeof (PixbufOrientation)),
				new Description ("tiff:PhotometricInterpretation", Catalog.GetString ("Photometric Interpretation"), 
						 typeof (FSpot.Tiff.PhotometricInterpretation)),
				new Description ("tiff:ResolutionUnit", Catalog.GetString ("Resolution Unit"),
						 typeof (FSpot.Tiff.ResolutionUnit)),
				new Description ("exif:ExposureProgram", Catalog.GetString ("Exposure Program"), 
						 typeof (FSpot.Tiff.ExposureProgram)),
				new Description ("exif:MeteringMode", Catalog.GetString ("Metering Mode"), 
						 typeof (FSpot.Tiff.MeteringMode)),
				new Description ("exif:ExposureMode", Catalog.GetString ("Exposure Mode"), 
						 typeof (FSpot.Tiff.ExposureMode)),
				new Description ("exif:CustomRendered", Catalog.GetString ("Custom Rendered"), 
						 typeof (FSpot.Tiff.CustomRendered)),
				new Description ("exif:ComponentsConfiguration", Catalog.GetString ("Components Configuration"),
						 typeof (FSpot.Tiff.ComponentsConfiguration)),
				new Description ("exif:LightSource", Catalog.GetString ("Light Source"),
						 typeof (FSpot.Tiff.LightSource)),
				new Description ("exif:SensingMethod", Catalog.GetString ("Sensing Method"),
						 typeof (FSpot.Tiff.SensingMethod)),
				new Description ("exif:ColorSpace", Catalog.GetString ("Color Space"),
						 typeof (FSpot.Tiff.ColorSpace)),
				new Description ("exif:WhiteBalance", Catalog.GetString ("White Balance"),
						 typeof (FSpot.Tiff.WhiteBalance)),
				new Description ("exif:FocalPlaneResolutionUnit", Catalog.GetString ("Focal Plane Resolution Unit"),
						 typeof (FSpot.Tiff.ResolutionUnit)),
				new Description ("exif:FileSource", Catalog.GetString ("File Source Type"),
						 typeof (FSpot.Tiff.FileSource)),
				new Description ("exif:SceneCaptureType", Catalog.GetString ("Scene Capture Type"),
						 typeof (FSpot.Tiff.SceneCaptureType)),
				new Description ("exif:GainControl", Catalog.GetString ("Gain Control"),
						 typeof (FSpot.Tiff.GainControl)),
				new Description ("exif:Contrast", Catalog.GetString ("Contrast"),
						 typeof (FSpot.Tiff.Contrast)),
				new Description ("exif:Saturation", Catalog.GetString ("Saturation"),
						 typeof (FSpot.Tiff.Saturation)),
				new Description ("exif:Sharpness", Catalog.GetString ("Sharpness"),
						 typeof (FSpot.Tiff.Sharpness)),
				new Description ("exif:SceneType", Catalog.GetString ("Scene Type"),
						 typeof (FSpot.Tiff.SceneType))



			};
			
			table = new System.Collections.Hashtable ();

			foreach (Description d in preset) {
				table [MetadataStore.Namespaces.Resolve (d.predicate)] = d;
			}
		}
		
		public Description (string predicate, string title) : this (predicate, title, null, null) {}

		public Description (string predicate, string title, string description) : this (predicate, title, description, null) {}
		
		public Description (string predicate, string title, System.Type type) : this (predicate, title)
		{
			formater = new ValueFormat (type);
		}

		public Description (string predicate, string title, string description, ValueFormat formater)
		{
			this.predicate = predicate;
			this.title = title;
			this.formater = formater;
		}
		
		public static void GetDescription (MemoryStore store, Statement stmt, out string label, out string value)
		{
			string predicate = stmt.Predicate.Uri;

			Description d = (Description) table [predicate];

			label = System.IO.Path.GetFileName (predicate);
			value = null;

			if (stmt.Object is SemWeb.Literal)
			        value = ((SemWeb.Literal)(stmt.Object)).Value;

			if (d != null) {
				label = d.title;
				if (d.formater != null && stmt.Object is Literal)
					value = d.formater.GetValue (store, (SemWeb.Literal)stmt.Object);

			} else {
				Statement sstmt = new Statement (stmt.Predicate,
								 (Entity)MetadataStore.Namespaces.Resolve ("rdfs:label"),
								 null);
				
				foreach (Statement tstmt in MetadataStore.Descriptions.Select (sstmt))
					if (tstmt.Object is SemWeb.Literal)
						label = ((SemWeb.Literal)(tstmt.Object)).Value;
			}
			return;
		}
	}
	
        internal class ValueFormat 
	{
		System.Type type;
		
		public ValueFormat (System.Type type)
		{
			this.type = type;
		}

		public virtual string GetValue (MemoryStore store, SemWeb.Literal obj)
		{
			string result = obj.Value;

			if (type.IsEnum) {
				try {
					object o = System.Enum.Parse (type, obj.Value);
					result = o.ToString ();
				} catch (System.Exception e) {
					System.Console.WriteLine ("Value \"{2}\" not found in {0}{3}{1}", type, e, result, System.Environment.NewLine);
				}
			}
			/*
			else if (type == typeof (Rational)) {
				object o = FSpot.Tiff.Rational.Parse (obj.Value);
			} 
			*/
			return result;
		}
	}
	
	public class MetadataStore : MemoryStore
	{
		public static NamespaceManager Namespaces;
		private static MetadataStore descriptions;
		
		public const string PhotoshopNS = "http://ns.adobe.com/photoshop/1.0/";
		public const string Iptc4xmpCoreNS = "http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/";
		public const string DcNS = "http://purl.org/dc/elements/1.1/";
		public const string XmpNS = "http://ns.adobe.com/xap/1.0/";
		public const string XmpidqNS = "http://ns.adobe.com/xmp/Identifier/qual/1.0/";
		public const string XmpRightsNS = "http://ns.adobe.com/xap/1.0/rights/";
		public const string XmpBJNS = "http://ns.adobe.com/xap/1.0/bj/";
		public const string XmpMMNS = "http://ns.adobe.com/xap/1.0/mm/";
		public const string ExifNS = "http://ns.adobe.com/exif/1.0/";
		public const string TiffNS = "http://ns.adobe.com/tiff/1.0/";
		public const string RdfNS = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
		public const string RdfsNS = "http://www.w3.org/2000/01/rdf-schema#";
		public const string IViewNS = "http://ns.iview-multimedia.com/mediapro/1.0/";
		public const string XmlNS = "http://iptc.org/std/Iptc4xmpCore/1.0/xmlns/";
		public const string CcNS = "http://creativecommons.org/ns#";

		// FIXME this needs to be parsable by System.Uri
		public const string FSpotXMPBase = "http://fakebase.f-spot.org/internal/";

		static MetadataStore ()
		{
			Namespaces = new NamespaceManager ();
			
			Namespaces.AddNamespace (PhotoshopNS, "photoshop");
			Namespaces.AddNamespace (Iptc4xmpCoreNS, "Iptc4xmpCore");
			Namespaces.AddNamespace (DcNS, "dc");
			Namespaces.AddNamespace (XmpNS, "xmp");
			Namespaces.AddNamespace (XmpidqNS, "xmpidq");
			Namespaces.AddNamespace (XmpRightsNS, "xmpRights");
			Namespaces.AddNamespace (XmpBJNS, "xmpBJ");
			Namespaces.AddNamespace (XmpMMNS, "xmpMM");
			Namespaces.AddNamespace (ExifNS, "exif");
			Namespaces.AddNamespace (TiffNS, "tiff");
			Namespaces.AddNamespace (RdfNS, "rdf");
			Namespaces.AddNamespace (RdfsNS, "rdfs");
			Namespaces.AddNamespace (IViewNS, "mediapro");
			Namespaces.AddNamespace (CcNS, "cc");
		}

		public static MetadataStore Descriptions {
			get {
				if (descriptions == null) {
					descriptions = new MetadataStore ();
					System.IO.Stream stream = System.Reflection.Assembly.GetCallingAssembly ().GetManifestResourceStream ("dces.rdf");
					if (stream != null) {
						descriptions.Import (new RdfXmlReader (stream));
					} else {
						System.Console.WriteLine ("Can't find resource");
					}
				}
				
				return descriptions;
			}
		}

		public void Dump ()
		{
#if enable_debug
			XmpFile xmp = new XmpFile ();
			xmp.Store = this;
			xmp.Save (System.Console.OpenStandardOutput ());
#endif
		}

		public static void AddLiteral (StatementSink sink, string predicate, string type, SemWeb.Literal value)
		{
			Entity empty = new BNode ();
			Statement top = new Statement (FSpotXMPBase, (Entity)MetadataStore.Namespaces.Resolve (predicate), empty);
			Statement desc = new Statement (empty, 
							(Entity)MetadataStore.Namespaces.Resolve ("rdf:type"), 
							(Entity)MetadataStore.Namespaces.Resolve (type));
			sink.Add (desc);
			Statement literal = new Statement (empty,
							   (Entity)MetadataStore.Namespaces.Resolve ("rdf:li"),
							   value);
			sink.Add (literal);
			sink.Add (top);
		}

		public static void AddLiteral (StatementSink sink, string predicate, string value)
		{
			Statement stmt = new Statement (FSpotXMPBase,
							(Entity)MetadataStore.Namespaces.Resolve (predicate), 
							new SemWeb.Literal (value));
			sink.Add (stmt);
		}

		public static void Add (StatementSink sink, string predicate, string type, string [] values)
		{
			Add (sink, FSpotXMPBase, predicate, type, values);
		}

		public void Delete (string predicate)
		{
			System.Collections.ArrayList to_remove = new System.Collections.ArrayList ();
			foreach (Statement stmt in this) {
				if (stmt.Predicate == MetadataStore.Namespaces.Resolve (predicate)) {
					to_remove.Add (stmt);
					break;
				}
			}

			foreach (Statement stmt in to_remove)
				this.Remove (stmt);
		}

		public void Update (string predicate, string value1)
		{
			// Delete first..
			Delete (predicate);
			// Add after...
			AddLiteral (this, predicate, value1);
		}

		public void Update (string predicate, string type, string [] values)
		{
			Entity anon = null;

			System.Collections.ArrayList to_remove = new System.Collections.ArrayList ();
			foreach (Statement stmt in this) {
				if (stmt.Predicate == MetadataStore.Namespaces.Resolve (predicate)) {
					if (type != null) // only look further if we have a type.
						anon = (Entity) stmt.Object;
					to_remove.Add (stmt);
					break;
				}
			}

			if (anon != null) {
				foreach (Statement stmt in this) {
					if (stmt.Subject == anon) {
						to_remove.Add (stmt);
					}
				}
			}

			foreach (Statement stmt in to_remove)
				this.Remove (stmt);

			if (values.Length > 0) {
				Add (this, predicate, type, values);
                        }
		}
		
		public static void Add (StatementSink sink, Entity subject, string predicate, string type, string [] values)
		{
			if (values == null) {
				System.Console.WriteLine ("{0} has no values; skipping", predicate);
				return;
			}

                        Entity empty = new SemWeb.BNode();
			Statement top = new Statement (subject, (Entity)MetadataStore.Namespaces.Resolve (predicate), empty);
			Statement desc = new Statement (empty, 
							(Entity)MetadataStore.Namespaces.Resolve ("rdf:type"), 
							(Entity)MetadataStore.Namespaces.Resolve (type));
			sink.Add (desc);
			foreach (string value in values) {
				Statement literal = new Statement (empty,
								   (Entity)MetadataStore.Namespaces.Resolve ("rdf:li"),
								   new SemWeb.Literal (value, null, null));
				sink.Add (literal);
			}
			sink.Add (top);
		}

		private class StatementWriter : StatementSink 
		{
			string name;
			public StatementWriter (string name)
			{
				this.name = name;
			}

			public bool Add (Statement stmt)
			{
				string predicate = stmt.Predicate.ToString ();

				if (predicate.StartsWith (name))
					System.Console.WriteLine ("----------- {0}", stmt);

				return true;
			}
		}

		private class SelectFirst : StatementSink
		{
			public Statement Statement;

			public bool Add (Statement stmt)
			{
				this.Statement = stmt;
				return false;
			}
		}			

		public void DumpNode (XPathSemWebNavigator navi, int depth)
		{
			do { 
				System.Console.WriteLine ("node [{0}] {1} {2}", depth, navi.Name, navi.Value);
			} while (navi.MoveToNext ());
		}
	       
	}	       
}
