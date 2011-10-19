using SemWeb;
using Mono.Unix;

namespace FSpot.Iptc {
	public enum Format
	{
		Unknown,
		String,
		Numeric,
		Binary,
		Byte,
		Short,
		Int,
		Date,
		Time
	};

	public enum Record
	{
		Envelope = 1 << 8,
		Application = 2 << 8,
		NewsphotoParameter = 3 << 8,
		NotAllocated1 = 4 << 8,
		NotAllocated2 = 5 << 8,
		AbstractRelationship = 6 << 8,
		PreObjectData = 7 << 8,
		ObjectData = 8 << 8,
		PostObjectData = 9 << 8
	}

	public enum DataSetID
	{
		ModelVersion        = Record.Envelope | 0,
		Destination         = Record.Envelope | 5,
		FileFormat          = Record.Envelope | 20,
		FileFormatVersion   = Record.Envelope | 22,
		ServiceIdentifier   = Record.Envelope | 30,
		EnvelopeNumber      = Record.Envelope | 40,
		ProductID           = Record.Envelope | 50,
		EnvelopePriority    = Record.Envelope | 60,
		DateSent            = Record.Envelope | 70,
		TimeSent            = Record.Envelope | 80,
		CodedCharacterSet   = Record.Envelope | 90,
		UNO                 = Record.Envelope | 100,
		ARMIdentifier       = Record.Envelope | 120,
		ARMVersion          = Record.Envelope | 122,

		RecordVersion            = Record.Application | 0,
		ObjectTypeReference      = Record.Application | 3,
		ObjectAttributeReference = Record.Application | 4,
		ObjectName               = Record.Application | 5,
		EditStatus               = Record.Application | 7,
		EditorialUpdate          = Record.Application | 8,
		Urgency                  = Record.Application | 10,
		SubjectReference         = Record.Application | 12,
		Category                 = Record.Application | 15,
		SupplementalCategory     = Record.Application | 20,
		FixtureIdentifier        = Record.Application | 22,
		Keywords                 = Record.Application | 25,
		ContentLocationCode      = Record.Application | 26,
		ContentLocationName      = Record.Application | 27,
		ReleaseDate              = Record.Application | 30,
		ReleaseTime              = Record.Application | 35,
		ExpirationDate           = Record.Application | 37,
		ExpirationTime           = Record.Application | 38,
		SpecialInstructions      = Record.Application | 40,
		ActionAdvised            = Record.Application | 42,
		ReferenceService         = Record.Application | 45,
		ReferenceDate            = Record.Application | 47,
		ReferenceNumber          = Record.Application | 50,
		DateCreated              = Record.Application | 55,
		TimeCreated              = Record.Application | 60,
		DigitalCreationDate      = Record.Application | 62,
		DigitalCreationTime      = Record.Application | 63,
		OriginatingProgram       = Record.Application | 65,
		ProgramVersion           = Record.Application | 70,
		ObjectCycle              = Record.Application | 75,
		ByLine                   = Record.Application | 80,
		ByLineTitle              = Record.Application | 85,
		City                     = Record.Application | 90,
		Sublocation              = Record.Application | 92,
		ProvinceState            = Record.Application | 95,
		PrimaryLocationCode      = Record.Application | 100,
		PrimaryLocationName      = Record.Application | 101,
		OriginalTransmissionReference = Record.Application | 103,
		Headline                 = Record.Application | 105,
		Credit                   = Record.Application | 110,
		Source                   = Record.Application | 115,
		CopyrightNotice          = Record.Application | 116,
		Contact                  = Record.Application | 118,
		CaptionAbstract          = Record.Application | 120,
		WriterEditor             = Record.Application | 122,
		RasterizedCaption        = Record.Application | 125,
		ImageType                = Record.Application | 130,
		ImageOrientation         = Record.Application | 131,
		LanguageIdentifier       = Record.Application | 135,
		AudioType                = Record.Application | 150,
		AudioSamplingRate        = Record.Application | 151,
		AudioSamplingReduction   = Record.Application | 152,
		AudioDuration            = Record.Application | 153,
		AudioOutcue              = Record.Application | 154,
		ObjectDataPreviewFileFormat = Record.Application | 200,
		ObjectDataPreviewFileFormatVersion  = Record.Application | 201,
		ObjectDataPreviewData    = Record.Application | 202,
		
		SizeMode                 = Record.PreObjectData | 10,
		MaxSubfileSize           = Record.PreObjectData | 20,
		ObjectDataSizeAnnounced  = Record.PreObjectData | 90,
		MaximumObjectDataSize    = Record.PreObjectData | 95,

		Subfile                  = Record.ObjectData | 10,

		ConfirmedObjectDataSize  = Record.PostObjectData | 10
	}

	public class DataSetInfo 
	{
		DataSetID ID;
		public string Name;
		public string Description;
		public string XmpName;
		bool Mandatory;
		bool Repeatable;
		uint MinSize;
		uint MaxSize;
		public Format Format;
		
		public static System.Collections.Hashtable IDTable;

		static DataSetInfo ()
		{
			IDTable = new System.Collections.Hashtable ();
			foreach (DataSetInfo info in datasets) {
				IDTable [info.ID] = info;
			}
		}
		
		public override string ToString ()
		{
		        return System.String.Format ("{0}-({1},{2},{3},{4})", Name, Mandatory, Repeatable, MinSize, MaxSize);
		}

		private static DataSetInfo [] datasets = {
			new DataSetInfo (DataSetID.ModelVersion, Format.Short, "Model Version", true, false, 2, 2, 
					 Catalog.GetString ("IPTC Information Interchange Model (IIM) Version number")),
			new DataSetInfo (DataSetID.Destination, Format.String, "Destination", false, true, 0, 1024, 
					 Catalog.GetString ("OSI Destination routing information")),
			new DataSetInfo (DataSetID.FileFormat, Format.Short, "File Format", true, false, 2, 2, 
					 Catalog.GetString ("IPTC file format")),
			new DataSetInfo (DataSetID.ServiceIdentifier, Format.String, "Service Identifier", true, false, 0, 10, 
					 Catalog.GetString ("Identifies the provider and product")),
			new DataSetInfo (DataSetID.EnvelopeNumber, Format.Numeric, "Envelope Number", true, false, 8, 8, 
					 Catalog.GetString ("A unique number identifying the envelope")), // FIXME
			new DataSetInfo (DataSetID.ProductID, Format.Numeric, "Product I.D.", false, true, 0, 32, 
					 Catalog.GetString ("A unique number")), // FIXME
			new DataSetInfo (DataSetID.EnvelopePriority, Format.Numeric, "Envelope Priority", false, false, 1, 1, 
					 Catalog.GetString ("The envelope handling priority between 1 (most urgent) and 9 (least urgent)")),
			new DataSetInfo (DataSetID.DateSent, Format.Date, "Date Sent", true, false, 8, 8, 
					 Catalog.GetString ("The year, month and day (CCYYMMDD) the service sent the material")),
			new DataSetInfo (DataSetID.TimeSent, Format.Date, "Time Sent", false, false, 11, 11, 
					 Catalog.GetString ("The hour, minute and second (HHMMSS) the service sent the material")),
			new DataSetInfo (DataSetID.CodedCharacterSet, Format.Time, "Coded Character Set", false, false, 0, 32, 
					 Catalog.GetString ("The character set designation")), // FIXME
			new DataSetInfo (DataSetID.UNO, Format.String, "Unique Name of Object", false, false, 14, 80,
					 Catalog.GetString ("External globally unique object identifier")),
			// UCD : IPR  : ODE            : OVI
			//   8 :[1-32]:[61 - IPR count]:[1-9]

			new DataSetInfo (DataSetID.ARMIdentifier, Format.Short, "ARM Identifier", false, false, 2, 2,
					 Catalog.GetString ("Abstract Relationship Method (ARM) identifier")),
			new DataSetInfo (DataSetID.ARMVersion, Format.Short, "ARM Version", false, false, 2, 2,
					 Catalog.GetString ("Abstract Relationship Method (ARM) version number.")),
			
			new DataSetInfo (DataSetID.RecordVersion, Format.Short, "Record Version", false, false, 2, 2,
					 Catalog.GetString ("Number identifying the IIM version this application record uses")),
			new DataSetInfo (DataSetID.ObjectTypeReference, Format.String, "Object Type Reference", false, false, 3, 64,
					 Catalog.GetString ("Object type reference")), // FIXME
			// Object Type Number : Object Type Name
			//                  2 : [0-64]

			new DataSetInfo (DataSetID.ObjectAttributeReference, Format.String, "Object Attribute Reference", false, true, 4, 68,
					 Catalog.GetString ("Object attribute reference")), // FIXME
			
			// Object Attribute number : Object Attribute Name
			//                       3 : [0-64]
			
			new DataSetInfo (DataSetID.ObjectName, Format.String, "Object Name", false, false, 4, 68,
					 Catalog.GetString ("Object name"), "dc:title"), // FIXME
			new DataSetInfo (DataSetID.EditStatus, Format.String, "Edit Status", false, false, 0, 64,
					 Catalog.GetString ("Status of the objectdata according to the provider")),
			new DataSetInfo (DataSetID.EditorialUpdate, Format.String, "Object Name", false, false, 4, 68,
					 Catalog.GetString ("Object name")), // FIXME
			new DataSetInfo (DataSetID.Sublocation, Format.String, "Location", false, false, 0, 32,
					 Catalog.GetString ("Location within a city or area where the object originates"),
					 "Iptc4xmpCore:Location"),
			new DataSetInfo (DataSetID.City, Format.String, "City", false, false, 0, 32,
					 Catalog.GetString ("Name of the city the content is focussing on"),
					 "photoshop:City"),
			new DataSetInfo (DataSetID.CopyrightNotice, Format.String, "Copyright Notice", false, false, 0, 128,
					 Catalog.GetString ("Copyright information for"),
					 "dc:rights"),
			new DataSetInfo (DataSetID.PrimaryLocationName, Format.String, "Country", false, false, 0, 64,
					 Catalog.GetString ("Full name of the country of the focus of the content"),
					 "photoshop:Country"),
			new DataSetInfo (DataSetID.PrimaryLocationCode, Format.String, "ISO Country Code", false, false, 0, 3,
					 Catalog.GetString ("Two or three letter ISO3166 code of the country of the focus of the content"),
					 "Iptc4xmpCore:CountryCode"),
			new DataSetInfo (DataSetID.ByLine, Format.String, "Creator", false, false, 0, 32,
					 Catalog.GetString ("Creator of the content"),  // FIXME
					 "dc:creator"),
			new DataSetInfo (DataSetID.Credit, Format.String, "Provider", false, false, 0, 32,
					 Catalog.GetString ("Provider of the object"),
					 "photoshop:Credit"),
			new DataSetInfo (DataSetID.ByLineTitle, Format.String, "Creator's Jobtitle", false, true, 0, 32,
					 Catalog.GetString ("The title of the author or creator"),
					 "photoshop:AuthorsPosition"),
			new DataSetInfo (DataSetID.WriterEditor, Format.String, "Caption/Description writer", false, true, 0, 32,
					 Catalog.GetString ("The person involved in writing, editing or " +
								       "correcting the object data or caption/abstract"),
					 "photoshop:CaptionWriter"),
			new DataSetInfo (DataSetID.Headline, Format.String, "Headline", false, false, 0, 256,
					 Catalog.GetString ("Headline of the content"),
					 "photoshop:Headline"),
			new DataSetInfo (DataSetID.SpecialInstructions, Format.String, "Instructions", false, false, 0, 256,
					 Catalog.GetString ("Instructions from the creator to the receiver not covered by other fields"),
					 "photoshop:Instructions"),
			new DataSetInfo (DataSetID.ObjectAttributeReference, Format.String, "Intellectual genre", false, true, 4, 68,
					 Catalog.GetString ("Intellectual genre of the object"),
					 "Iptc4xmpCore:IntellectualGenre"),
			// Object Attribute number : Object Attribute Name
			//                       3 : [0-64]
		};		

		public static DataSetInfo FindInfo (DataSetID id)
		{
			foreach (DataSetInfo info in datasets)
				if (id == (DataSetID)info.ID)
					return info;
						
			return new DataSetInfo (id, Format.Unknown, "Unknown", false, false, 3, 64,
						Catalog.GetString ("Unknown IIM DataSet"));
		}

		protected DataSetInfo (DataSetID id, Format format, string name, bool mandatory, bool repeatable, uint min, uint max, string description) : this (id, format, name, mandatory, repeatable, min, max, description, null)
		{ }

		protected DataSetInfo (DataSetID id, Format format, string name, bool mandatory, bool repeatable, uint min, uint max, string description, string xmpname)
		{
			ID = id;
			Name = name;
			Description = description;
			Format = format;
		        Mandatory = mandatory;
			Repeatable = repeatable;
			MinSize = min;
			MaxSize = max;
			XmpName = xmpname;
		}
	}

	public class DataSet 
	{
		public byte RecordNumber;
		public byte DataSetNumber;
		public byte [] Data;
		
		const byte TagMarker = 0x1c;
		const ushort LengthMask = 1 << 15;

		public void Load (System.IO.Stream stream)
		{
			byte [] rec = new byte [5];
			int len = stream.Read (rec, 0, rec.Length);
			if (rec [0] != TagMarker) {
				throw new System.Exception (System.String.Format ("Invalid tag marker found {0} != {1} with {2} bytes remaining {3}", 
							    rec[0].ToString ("x"), TagMarker.ToString ("x"), stream.Length - stream.Position, len));
			}
			RecordNumber = rec [1];
			DataSetNumber = rec [2];

			ulong length = FSpot.BitConverter.ToUInt16 (rec, 3, false);			

			if ((length & (LengthMask)) > 0) {
				// Note: if the high bit of the length is set the record is more than 32k long
				// and the length is stored in what would normaly be the record data, so we read
				// that data convert it to a long and continue on.
				ushort lsize = (ushort)((ushort)length & ~LengthMask);
				if (lsize > 8)
					throw new System.Exception ("Wow, that is a lot of data");

				byte [] ldata = new byte [8];
				stream.Read (ldata, 8 - lsize, lsize);
				length = FSpot.BitConverter.ToUInt64 (ldata, 0, false);
			}

			// FIXME if the length is greater than 32768 we re
			Data = new byte [length];
			stream.Read (Data, 0, Data.Length);
		}

		public DataSetID ID {
			get {
				return (DataSetID) (RecordNumber << 8 | DataSetNumber);
			}
		}

		public string XmpPredicate {
			get {
				DataSetInfo info = (DataSetInfo) DataSetInfo.IDTable [this.ID];
				if (info != null && info.XmpName != null) {
					return MetadataStore.Namespaces.Resolve (info.XmpName);
				}
				return null;
			}
		}

		public void Save (System.IO.Stream stream)
		{
			stream.WriteByte (TagMarker);
			stream.WriteByte (RecordNumber);
			stream.WriteByte (DataSetNumber);
			if (Data.Length < LengthMask) {
				byte [] len = FSpot.BitConverter.GetBytes ((ushort)Data.Length, false);
				stream.Write (len, 0, len.Length);
			} else {
				byte [] len =  FSpot.BitConverter.GetBytes ((ushort)LengthMask & 8, false);
				stream.Write (len, 0, len.Length);
				len = FSpot.BitConverter.GetBytes ((ulong) Data.Length, false);
				stream.Write (len, 0, len.Length);
			}
			stream.Write (Data, 0, Data.Length);
		}

		public string XmpObject 
		{
			get {
				//DataSetInfo info = (DataSetInfo) DataSetInfo.IDTable [this.ID];
				//if (info != null && info.Format == Format.String) {
					return System.Text.Encoding.UTF8.GetString (this.Data);
					//}
					//return null;
			}
		}
	}

	public class IptcFile : SemWeb.StatementSource
	{
		System.Collections.ArrayList sets = new System.Collections.ArrayList ();
		public System.Collections.ArrayList Sets {
			get { return sets; }
		}

                // False seems a safe default
                public bool Distinct {
                        get { return false; }
                }
		
		public IptcFile (System.IO.Stream stream)
		{
			Load (stream);
		}

		public void Select (SemWeb.StatementSink sink)
		{
			Entity keywords = null;

			foreach (DataSet data in sets) {
				switch (data.ID) {
				case DataSetID.CopyrightNotice:
					MetadataStore.AddLiteral (sink, "dc:rights", "rdf:Alt", new SemWeb.Literal (data.XmpObject, "x-default", null));
					break;
				case DataSetID.ByLine:
					MetadataStore.AddLiteral (sink, "dc:creator", "rdf:Seq", new SemWeb.Literal (data.XmpObject, "x-default", null));
					break;
				case DataSetID.CaptionAbstract:
					MetadataStore.AddLiteral (sink, "dc:description", "rdf:Alt", new SemWeb.Literal (data.XmpObject, "x-default", null));
					break;
				case DataSetID.ObjectName:
					MetadataStore.AddLiteral (sink, "dc:title", "rdf:Alt", new SemWeb.Literal (data.XmpObject, "x-default", null));
					break;
				case DataSetID.Keywords:
					if (keywords == null) {
						keywords = new BNode ();
						sink.Add (new Statement (MetadataStore.FSpotXMPBase, 
									 MetadataStore.Namespaces.Resolve ("dc:subject"),
									 keywords)); 
						sink.Add (new Statement (keywords, 
									 (Entity)MetadataStore.Namespaces.Resolve ("rdf:type"),
									 (Entity)MetadataStore.Namespaces.Resolve ("rdf:Bag")));
					}
					sink.Add (new Statement (keywords, 
								 MetadataStore.Namespaces.Resolve ("rdf:li"), 
								 new SemWeb.Literal (data.XmpObject, "x-default", null)));
					break;
				default:
					if (data.XmpPredicate != null) {
						sink.Add (new Statement (MetadataStore.FSpotXMPBase, 
									 (Entity)data.XmpPredicate, 
									 new SemWeb.Literal (data.XmpObject)));
					}
					break;
				}
			}
		}
		
		public void Load (System.IO.Stream stream)
		{
			while (stream.Position < stream.Length) {
				DataSet data = new DataSet ();
				
				try {
					data.Load (stream);
				} catch (System.Exception) {
					//System.Console.WriteLine (e.ToString ());
				}
				//DataSetInfo info = DataSetInfo.FindInfo (data.ID);
				//System.Console.WriteLine ("{0}:{1} - {2} {3}", data.RecordNumber, data.DataSetNumber, 
				//			  data.ID.ToString (), info.Description);
				sets.Add (data);
			}
		}

		public void Save (System.IO.Stream stream) 
		{
			foreach (DataSet data in sets) {
				data.Save (stream);
			}
		}
	}
}
