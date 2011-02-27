using System;

namespace FSpot.Ciff {
	public enum Tag {
		// Byte valuesad
		NullRecord = 0x0000,
		FreeBytes = 0x0001,
		CanonColorInfo1 = 0x0032,

		// ASCII strings
		CanonFileDescription = 0x0805,
		UserComment = 0x0805, // FIXME this is the same as above, is it correct?
		CanonRawMakeModel = 0x080a, 
		CanonFirmwareVersion = 0x080b,
		ComponentVersion = 0x08c,
		ROMOperationMode = 0x08d,
		OwnerName = 0x0810,
		CanonImageType = 0x0815,
		OriginalFileName = 0x0816,
		ThumbnailFileName = 0x0817,
		
		// Short values
		TargetImageType = 0x100a,
		ShutterReleaseMethod = 0x1010,
		ShutterReleaseTiming = 0x1011,
		ReleaseSetting = 0x1016,
		BaseISO = 0x101c,

		Uknown2 = 0x1028,

		FocalLength = 0x1029,
		CanonShotInfo = 0x102a,
		CanonColorInfo2 = 0x102c,
		CanonCameraSettings = 0x102d,
		WhiteSample = 0x1030,
		SensorInfo = 0x1031,
		CanonCustomFunctions = 0x1033,
		CanonPictureInfo = 0x1038,

		Unknown3 = 0x1039,
		Unknown4 = 0x1093,
		Unknown5 = 0x10a8,
		
		WhiteBalanceTable = 0x10a9,
		
		Unknown6 = 0x10aa,

		ColorTemperature = 0x10ae,
		ColorSapce = 0x10b4,
		
		Unknown7 = 0x10b5,
		unknown8 = 0x10c0,
		Unknown9 = 0x10c1,

		ImageFormat = 0x1803,
		RecordID = 0x1804,
		SelfTimerTime = 0x1806,
		TargetDistanceSetting = 0x1807,
		SerialNumber = 0x180b,
		TimeStamp = 0x180e,
		ImageSpec = 0x1810,
		FlashInfo = 0x1813,
		MeasuredEV = 0x1814,
		FileNumber = 0x1817,
		ExposureInfo = 0x1818,
		
		Unknown10 = 0x1834,

		DecoderTable = 0x1835,
		
		Unknown11 = 0x183b,

		// Image Data
		RawData = 0x2005,
		JpgFromRaw = 0x2007,
		ThumbnailImage = 0x2008,

		// Directories
		ImageDescrption = 0x2804,
		CameraObject = 0x2807,
		ShootingRecord = 0x3002,
		MeasuredInfo = 0x3003,
		CameraSpecification = 0x3004,
		ImageProps = 0x300a,
		ExifInformation = 0x300b
	}

	

	public struct ImageSpec {
		public uint ImageWidth;  // Number of horizontal pixels
		public uint ImageHeight; // Number of vertical pixels
		public float PixelAspectRatio;
		public int RotationAngle;  // degrees counter clockwise to rotate (orientation)
		public uint ComponentBitDepth; // bits per component
		public uint ColorBitDepth; // bits per component * channels
		public uint ColorBW; //  byte wise:  0 gray - 1 color ; byte 2 use aspect ratio ; 3 and 4 reserved

		public ImageSpec (byte [] data, bool little)
		{
			ImageWidth = BitConverter.ToUInt32 (data, 0, little);
			ImageHeight = BitConverter.ToUInt32 (data, 4, little);

			PixelAspectRatio = BitConverter.ToSingle (data, 8, little);
			RotationAngle = BitConverter.ToInt32 (data, 12, little);
			ComponentBitDepth = BitConverter.ToUInt32 (data, 16, little);
			ColorBitDepth = BitConverter.ToUInt32 (data, 20, little);
			ColorBW = BitConverter.ToUInt32 (data, 24, little);
			//System.Console.WriteLine ("0x{0}", ColorBW.ToString ("x"));
		}

		public PixbufOrientation Orientation {
			get {
				int angle = RotationAngle % 360;
				if (angle < 45)
					return PixbufOrientation.TopLeft;
				else if (angle < 135)
					return PixbufOrientation.RightTop;
				else if (angle < 225)
					return PixbufOrientation.BottomRight;
				else if (angle < 315)
					return PixbufOrientation.LeftBottom;
				else
					return PixbufOrientation.TopLeft;
			}
		}

		public bool IsColor {
			get {
				return (ColorBW & 1) > 0;

			}
		}

		public bool HasSquarePixels {
			get {
				return (ColorBW & 1 << 1) == 0;
			}
		}
	}

	public struct CaptureTime {
		uint seconds;
		int tz_offset;
		uint tz_data;

		public CaptureTime (byte [] data, bool little)
		{
			seconds = BitConverter.ToUInt32 (data, 0, little);
			tz_offset = BitConverter.ToInt32 (data, 4, little);
			tz_data = BitConverter.ToUInt32 (data, 8, little);
		}

		public System.DateTime LocalTime {
			get {
				return new System.DateTime (1970, 1, 1).AddSeconds (seconds);
			}
		}

		public bool HasTimezone {
			get {
				return ((1 << 31 & tz_data) > 0);
			}
		}

		public override string ToString ()
		{
			string tz = String.Empty;
			
			if (HasTimezone)
				if (tz_offset != 0)
					tz = System.String.Format ("{0}{1}:{2}", 
								   seconds > 0 ? "+" : "-", 
								   tz_offset / 3600, 
								   tz_offset / 60 % 60);
				else 
					tz = "Z";

		       
			return System.String.Format ("{0}{1}", LocalTime.ToString ("yyyy-MM-ddThh:mm:ss"), tz);
		}
	}

	public enum EntryType : ushort {
		Byte = 0x0000,
		Ascii = 0x0800,
		Short = 0x1000,
		Int = 0x1800,
		Struct = 0x2000,
		Directory1 = 0x2800,
		Directory2 = 0x2800,
	}
	
	public enum Mask {
		StorageFormat = 0xc000,
		Type = 0x3800,
		ID = 0x07ff,
	}

	/* See http://www.sno.phy.queensu.ca/~phil/exiftool/canon_raw.html */
	public struct Entry {
		internal Tag Tag;
		internal uint Size;
		internal uint Offset;

		public Entry (byte [] data, int pos, bool little)
		{
			Tag = (Tag) BitConverter.ToUInt16 (data, pos, little);
			Size = BitConverter.ToUInt32 (data, pos + 2, little);
			Offset = BitConverter.ToUInt32 (data, pos + 6, little);
		}	
		
		public EntryType Type {
			get {
				return GetType (Tag);
			}
		}

		public static EntryType GetType (Tag tag) 
		{
			EntryType type = (EntryType) ((ushort)tag & (ushort)Mask.Type);
			return type;
		}

		public static bool IsDirectory (Tag tag)
		{
			EntryType type = GetType (tag);
			return (type == EntryType.Directory1 || type == EntryType.Directory2);
		}
	}

	public class ImageDirectory {
		System.Collections.ArrayList entry_list;
		uint Count;
		bool little;
		uint start;
		long DirPosition; 
		System.IO.Stream stream;

		public ImageDirectory (System.IO.Stream stream, uint start, long end, bool little)
		{
			this.start = start;
			this.little = little;
			this.stream = stream;

			entry_list = new System.Collections.ArrayList ();
			
			stream.Position = end - 4;
			byte [] buf = new byte [10];
			stream.Read (buf, 0, 4);
			uint directory_pos  = BitConverter.ToUInt32 (buf, 0, little);
			DirPosition = start + directory_pos;

			stream.Position = DirPosition;
			stream.Read (buf, 0, 2);

			Count = BitConverter.ToUInt16 (buf, 0, little);
			
			for (int i = 0; i < Count; i++)
			{
				stream.Read (buf, 0, 10);
				//System.Console.WriteLine ("reading {0} {1}", i, stream.Position);
				Entry entry = new Entry (buf, 0, little);
				entry_list.Add (entry);
			}
		}			

		public void Dump ()
		{
			//System.Console.WriteLine ("Dumping directory with {0} entries", entry_list.Count);
			for (int i = 0; i < entry_list.Count; i++) {
				Entry e = (Entry) entry_list[i];
				//System.Console.WriteLine ("\tentry[{0}] = {1}.{6}.{5}({4}).{2}-{3}", 
				//			  i, e.Tag, e.Size, e.Offset, e.Tag.ToString ("x"), (uint)e.Tag & ~(uint)Mask.StorageFormat, e.Type); 
			}
		}

		public ImageDirectory ReadDirectory (Tag tag)
		{
			foreach (Entry e in entry_list) {
				if (e.Tag == tag) {
					uint subdir_start = this.start + e.Offset;
					ImageDirectory subdir = new ImageDirectory (stream, subdir_start, subdir_start + e.Size, little);
					return subdir;
				}
			}
			return null;
		}
		
		public byte [] ReadEntry (int pos)
		{
			Entry e = (Entry) entry_list [pos];

			stream.Position = this.start + e.Offset;			

			byte [] data = new byte [e.Size];
			stream.Read (data, 0, data.Length);

			return data;
		}
		
		public byte [] ReadEntry (Tag tag) 
		{
			int pos = 0;
			foreach (Entry e in entry_list) {
				if (e.Tag == tag)
					return ReadEntry (pos);
				pos++;
			}
			return null;
		}
	}
	
	public class CiffFile : FSpot.ImageFile , SemWeb.StatementSource {
		public ImageDirectory root;
		private uint version;
		bool little;
		System.IO.Stream stream;

                // False seems a safe default
                public bool Distinct {
                        get { return false; }
                }
		
		public ImageDirectory Root {
			get {
				if (root == null) {
					stream = Open ();
					root = Load (stream);
				}
				
			        return root;
			}
		}

		public CiffFile (Uri uri) : base (uri)
		{
		}

		public void Select (SemWeb.StatementSink sink)
		{
			byte [] data = null;
			ImageDirectory props = Root.ReadDirectory (Tag.ImageProps);
			ImageDirectory camera = props.ReadDirectory (Tag.CameraObject);

			data = props.ReadEntry (Tag.TimeStamp);
			if (data != null)
				MetadataStore.AddLiteral (sink, "xmp:CreateDate", new CaptureTime (data, little).ToString ());

			data = props.ReadEntry (Tag.ImageSpec);
			if (data != null) {
				ImageSpec spec = new ImageSpec (data, little);
				MetadataStore.AddLiteral (sink, "tiff:Orientation", ((int)spec.Orientation).ToString ());
				MetadataStore.AddLiteral (sink, "tiff:ImageWidth", spec.ImageWidth.ToString ());
				MetadataStore.AddLiteral (sink, "tiff:ImageLength", spec.ImageHeight.ToString ());
				string comp = spec.ComponentBitDepth.ToString ();

				if (spec.IsColor) {
					MetadataStore.Add (sink, "tiff:BitsPerSample", "rdf:Seq", new string [] { comp, comp, comp });
				} else {
					MetadataStore.Add (sink, "tiff:BitsPerSample", "rdf:Seq", new string [] { comp });
				}					
				
				if (!spec.HasSquarePixels) {
					MetadataStore.AddLiteral (sink, "tiff:XResolution", 
								  (1000000 * spec.PixelAspectRatio).ToString ());
					MetadataStore.AddLiteral (sink, "tiff:YResolution", 
								  (1000000 * (1 / spec.PixelAspectRatio)).ToString ());
				}
					
			}
			
			data = camera.ReadEntry (Tag.CanonRawMakeModel);
			if (data != null) {
				string make_model = System.Text.Encoding.ASCII.GetString (data, 0, data.Length - 1);
				string [] vals = make_model.Split (new char [] {'\0'});
				MetadataStore.AddLiteral (sink, "tiff:Make", vals [0]); 
				MetadataStore.AddLiteral (sink, "tiff:Model", vals [1]); 
			}

			/*
			// FIXME this doesn't appear to be ascii.
			data = camera.ReadEntry (Tag.OwnerName);
			if (data != null) {
				string name = System.Text.Encoding.ASCII.GetString (data, 0, data.Length - 1);
				MetadataStore.AddLiteral (sink, "dc:creator", "rdf:Seq", new SemWeb.Literal (name));
			}
			*/
		}


		protected ImageDirectory Load (System.IO.Stream stream) 
		{
			byte [] header = new byte [26];  // the spec reserves the first 26 bytes as the header block
			stream.Read (header, 0, header.Length);

			uint start;
			
			little = (header [0] == 'I' && header [1] == 'I');
			
			start = BitConverter.ToUInt32 (header, 2, little);
			
			// HEAP is the type CCDR is the subtype
			if (System.Text.Encoding.ASCII.GetString (header, 6, 8) != "HEAPCCDR") 
				throw new ImageFormatException ("Invalid Ciff Header Block");
			
			version =  BitConverter.ToUInt32 (header, 14, little);

			//
			
			long end = stream.Length;
			return new ImageDirectory (stream, start, end, little);
		}

		public uint Version {
			get { return version; }
		}

		public override PixbufOrientation GetOrientation ()
		{
			PixbufOrientation orientation = PixbufOrientation.TopLeft;
			ImageDirectory props = Root.ReadDirectory (Tag.ImageProps);
		       	byte [] data = props.ReadEntry (Tag.ImageSpec);
			
			if (data != null)
				orientation = new ImageSpec (data, little).Orientation;
			//else 
				//System.Console.WriteLine ("NO ORIENTATION");

			return orientation;
		}

		public override System.DateTime Date {
			get {
				ImageDirectory props = Root.ReadDirectory (Tag.ImageProps);
				byte [] date = props.ReadEntry (Tag.TimeStamp);

				if (date == null) {
					//System.Console.WriteLine ("NO DATE");
					return base.Date;
				}

				return new CaptureTime (date, little).LocalTime.ToUniversalTime ();
			}
		}

		public override System.IO.Stream PixbufStream ()
		{
			byte [] data = GetEmbeddedJpeg ();
			
			if (data != null)
				return new System.IO.MemoryStream (data);
			else	
				return DCRawFile.RawPixbufStream (uri);
		}

#if false
		public override Gdk.Pixbuf Load (int width, int height)
		{
			Gdk.Pixbuf full = this.Load ();
			Gdk.Pixbuf scaled  = PixbufUtils.ScaleToMaxSize (full, width, height);
			full.Dispose ();
			return scaled;
		}
#endif
		public void Dump ()
		{
			Root.Dump ();
			ImageDirectory props = Root.ReadDirectory (Tag.ImageProps);
			props.Dump ();
			/*
				 string path = "out2.jpg";
			System.IO.File.Delete (path);

			System.IO.Stream output = System.IO.File.Open (path, System.IO.FileMode.OpenOrCreate);
			byte [] data = GetEmbeddedThumbnail ();
			//System.Console.WriteLine ("data length {0}", data != null ? data.Length : -1);
			output.Write (data, 0, data.Length);
			output.Close ();
			*/
		}

		public byte [] GetEmbeddedJpeg ()
		{
			return Root.ReadEntry (Tag.JpgFromRaw);
		}

		public byte [] GetEmbeddedThumbnail ()
		{
			return Root.ReadEntry (Tag.ThumbnailImage); 
		}
		
		/*
		public static void Main (string [] args)
		{
			CiffFile ciff = new CiffFile (args [0]);
			ciff.Dump ();
		}
		*/

		protected override void Close ()
		{
			if (stream != null) {
				stream.Close ();
				stream = null;
			}
		}
	}
}
