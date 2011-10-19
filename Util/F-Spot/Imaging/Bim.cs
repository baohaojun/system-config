namespace FSpot.Bim {
	public enum EntryType : ushort {
		ObsoleteImageInfo = 0x03e8,  
		MacPrintManager = 0x03e9,
		MacPrintXML = 0x03ea,
		ObsoleteIndexedColorTable = 0x03eb,
		ResolutionInfo = 0x03ed,
		AlphaChannelNames = 0x03ee,
		DisplayInfo = 0x03ef,
		Caption = 0x03f0,
		Border = 0x03f1,
		BackgroundColor = 0x03f2,
		PrintFlags = 0x03f3,
		GrayHalftone = 0x03f4,
		ColorHalftone = 0x03f5,
		DuotoneHalftone = 0x03f6,
		GrayTransfer = 0x03f7,
		ColorTransfer = 0x03f8,
		DuotoneTransfer = 0x03f9,
		DuotoneInformation = 0x03fa,
		BWDot = 0x03fb,
		ObsoleteUnknown1 = 0x03fc,
		LayerState = 0x0400,
		WorkingPath = 0x0401,
		LayerGroupInfomation = 0x0402,
		ObsoleteUnknown2 = 0x0403,
		IPTCNAA = 0x0404,
		RawImageMode = 0x0405,
		JpegQuality = 0x0406,
		GridGuideInformation = 0x0408,
		ThumbnailResource = 0x0409,
		CopyrightFlag = 0x040a,
		URL = 0x040b,
		ThumbnailResource2 = 0x040c,
		GlobalAngle = 0x040d,
		ColorSamplers = 0x040e,
		ICCProfile = 0x040f,
		Watermark = 0x0410,
		ICCUntagged = 0x0411,
		EffectsVisible = 0x0412,
		SpotHalftone = 0x0413,
		LastID = 0x0414,
		UnicodeAlphaNames = 0x0415,
		IndexedColorTable = 0x0416,
		TransparentIndex = 0x0417,
		GlobalAltitude = 0x0419,
		Slices = 0x041a,
		WorkflowURL = 0x041b,
		XPEPJump = 0x041c,
		AlphaIdentifiers = 0x041d,
		URLList = 0x041e,
		VersionInfo = 0x0421,
		XMP = 0x0424,
		FirstPath = 0x07d0,
		LastPAth = 0x0bb6,
		ClippingPathName = 0x0bb7,
		PrintFlags2 = 0x2710,
	}

	/*
	  From what I can see it looks like these resources are a just
	  a list of records starting with a 8Bim\0 followed by a 16 bit
	  value that is the record type then a 8 bit offset and 32bit length. IPTC data is
	  type 0x0404, I don't know any other types at the moment.

	  see http://www.fine-view.com/jp/lab/doc/ps6ffspecsv2.pdf and the section on image resource blocks.
	*/

	public class Entry
	{
		public ushort  Type;
		public string  Name;
		public byte [] Data;

		const string Marker = "8BIM";
		public Entry ()
		{

		}

		public int Load (System.IO.Stream stream)
		{
			byte [] header = new byte [6];
			
			stream.Read (header, 0, header.Length);
			if (System.Text.Encoding.ASCII.GetString (header, 0, 4) != Marker)
				throw new System.Exception ("missing header");
			
			Type = FSpot.BitConverter.ToUInt16 (header, 4, false);

		        int name_length = stream.ReadByte ();
			if (name_length > 0) {
				byte [] name_data = new byte [name_length];
				stream.Read (name_data, 0, name_length);
				Name = System.Text.Encoding.ASCII.GetString (name_data);
			}
			
			if (name_length % 2 == 0)
				stream.ReadByte ();

			stream.Read (header, 0, 4);
			uint length = FSpot.BitConverter.ToUInt32 (header, 0, false);

			Data = new byte [length];
			stream.Read (Data, 0, Data.Length);

			if (Data.Length % 2 > 0)
				stream.ReadByte ();

			return header.Length + Data.Length;
		}
		
		public void Save (System.IO.Stream stream) 
		{
			byte [] tmp;
			tmp = System.Text.Encoding.ASCII.GetBytes (Marker);
			stream.Write (tmp, 0, tmp.Length);
			tmp = FSpot.BitConverter.GetBytes (Type, false);
			stream.Write (tmp, 0, tmp.Length);

			// Write the name
			stream.WriteByte ((byte)Name.Length);
			tmp = System.Text.Encoding.ASCII.GetBytes (Name);
			stream.Write (tmp, 0, tmp.Length);

			// Pad the name
			if (tmp.Length % 2 == 0)
				stream.WriteByte (0);

			// Write the data
			tmp  = FSpot.BitConverter.GetBytes ((uint)Data.Length, false);
			stream.Write (tmp, 0, tmp.Length);

			stream.Write (Data, 0, Data.Length);
			// Pad the data
			if (Data.Length % 2 > 0)
				stream.WriteByte (0);
		}
	}

	public class BimFile : SemWeb.StatementSource
	{
                // False seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		System.Collections.ArrayList entries = new System.Collections.ArrayList ();

		public BimFile (System.IO.Stream stream)
		{
			Load (stream);
		}
		
		public void Select (SemWeb.StatementSink sink)
		{
			foreach (Entry e in entries) {
				EntryType type = (EntryType) e.Type;

				switch (type) {
				case EntryType.IPTCNAA:
					System.IO.Stream iptcstream = new System.IO.MemoryStream (e.Data);
					FSpot.Iptc.IptcFile iptc = new FSpot.Iptc.IptcFile (iptcstream);
					iptc.Select (sink);
					break;
				case EntryType.XMP:
					System.IO.Stream xmpstream = new System.IO.MemoryStream (e.Data);
					FSpot.Xmp.XmpFile xmp = new FSpot.Xmp.XmpFile (xmpstream);
					xmp.Select (sink);
					break;
				default:
					break;
				}
			}
		}

		public Entry FindEntry (EntryType type)
		{
			foreach (Entry current in entries)
				if (current.Type == (ushort)type)
					return current;

			return null;
		}

		public void Load (System.IO.Stream stream)
		{
			while (stream.Position < stream.Length)
			{
				Entry current = new Entry ();
				current.Load (stream);
				//System.Console.WriteLine ("read {0} - {1}", ((EntryType)current.Type).ToString (), current.Name);
				try {
					//System.Console.WriteLine (System.Text.Encoding.ASCII.GetString (current.Data));
				} catch (System.Exception e) {
					Beagle.Util.Log.Error (e, "Error loading BIM file");
				}
				entries.Add (current);
			}
		}

		public void Save (System.IO.Stream stream)
		{
			foreach (Entry e in entries) {
				e.Save (stream);
			}
		}
	}
}



