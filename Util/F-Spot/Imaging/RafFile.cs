namespace FSpot.Raf {
	// This is reverse engineered from looking at the sample files I have
	// from what I can tell the file is always BigEndian, although the embedded jpeg may not be
	// and there is a start long offset at 0x54 (or possibly 0x56 if it is a short) that points to
	// the start of the embedded jpeg and followed by a long length that gives the length of the jpeg
	// data.   
	//
	// Following that there seem to be more offsets and lengths (probably for the raw data) that I haven't
	// completely figured out yet.  More to follow.

	// ALL the sample files I have begin with "FUJIFILMCCD-RAW "

	
	public class WhiteBalance {
		// see dcraw parse_fuli
		public WhiteBalance (System.IO.Stream stream)
		{

		}
	}
	
	public class RafFile : ImageFile, SemWeb.StatementSource {

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public RafFile (System.Uri uri) : base (uri)
		{
		}

		private Exif.ExifData exif_data;
		public Exif.ExifData ExifData {
			get {
				if (exif_data == null)
					exif_data = new Exif.ExifData(uri.LocalPath);
				//System.Console.WriteLine ("loading exif data");
				return exif_data;
			}
		}
		
		public override PixbufOrientation GetOrientation (){
			PixbufOrientation orientation = PixbufOrientation.TopLeft;

			Exif.ExifEntry e = this.ExifData.GetContents (Exif.Ifd.Zero).Lookup (Exif.Tag.Orientation);
			if (e != null) {
				ushort [] value = e.GetDataUShort ();
				orientation = (PixbufOrientation) value [0];
			}

			if (orientation < PixbufOrientation.TopLeft || orientation > PixbufOrientation.LeftBottom)
				orientation = PixbufOrientation.TopLeft;

			return orientation;
		}

		public RafFile (string path) : base (path)
		{
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
		public override Gdk.Pixbuf Load ()
		{
			return new Gdk.Pixbuf (PixbufStream ());
		}

		public override Gdk.Pixbuf Load (int width, int height)
		{
			Gdk.Pixbuf full = this.Load ();
			Gdk.Pixbuf rotated = PixbufUtils.TransformOrientation (full, this.GetOrientation(), true);
			Gdk.Pixbuf scaled  = PixbufUtils.ScaleToMaxSize (rotated, width, height);
			full.Dispose ();
			return scaled;
		}
#endif
		public void Select (SemWeb.StatementSink sink)
		{
			byte [] data = GetEmbeddedJpeg ();
			if (data != null) {
				System.IO.Stream stream = new System.IO.MemoryStream (data);
				JpegHeader header = new JpegHeader (stream);
				header.Select (sink);
			}
		}

		private byte [] GetEmbeddedJpeg ()
		{
			using (System.IO.Stream stream = Open ()) {
				stream.Position = 0x54;
				byte [] data = new byte [24];
				stream.Read (data, 0, data.Length);
				uint jpeg_offset = BitConverter.ToUInt32 (data, 0, false);
				uint jpeg_length = BitConverter.ToUInt32 (data, 4, false);

				// FIXME implement wb parsing
				//uint wb_offset = BitConverter.ToUInt32 (data, 8, false);
				//uint wb_length = BitConverter.ToUInt32 (data, 12, false);
				
				// FIXME implement decoding
				//uint raw_offset = BitConverter.ToUInt32 (data, 16, false);
				//uint raw_length = BitConverter.ToUInt32 (data, 20, false);

				byte [] image = new byte [jpeg_length];
				stream.Position = jpeg_offset;
				stream.Read (image, 0, image.Length);
				return image;
			}

		}
	}
}
