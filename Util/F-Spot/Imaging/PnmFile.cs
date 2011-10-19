using SemWeb;
using System;
using System.IO;

#if ENABLE_NUNIT
using NUnit.Framework;
#endif

namespace FSpot.Pnm {
	public class PnmFile : ImageFile, StatementSource {

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public PnmFile (Uri uri) : base (uri) 
		{
		}

		public PnmFile (string path) : base (path) 
		{
		}

		public class Header {
			public string Magic;
			public int Width;
			public int Height;
			public ushort Max;
			
			public Header (Stream stream)
			{
				Magic = GetString (stream);
				Width = int.Parse (GetString (stream));
				Height = int.Parse (GetString (stream));
				Max = ushort.Parse (GetString (stream));
			}

			public bool IsDeep {
				get {
					return Max > 256;
				}
			}

			public void Dump ()
			{
				System.Console.WriteLine ("Loading ({0} - {1},{2} - {3})", 
							  Magic, Width, Height, Max);
			}
		}

		public void Select (StatementSink sink)
		{
			using (Stream stream = Open ()) {
				Header header = new Header (stream);
				MetadataStore.AddLiteral (sink, "tiff:ImageWidth", header.Width.ToString ());
				MetadataStore.AddLiteral (sink, "tiff:ImageLength", header.Height.ToString ());
				string bits = header.IsDeep ? "16" : "8";
				MetadataStore.Add (sink, "tiff:BitsPerSample", "rdf:Seq", new string [] { bits, bits, bits });
			}
		}
		
		public override Stream PixbufStream ()
		{
			Stream stream = Open ();
			Header header = new Header (stream);
			if (header.IsDeep)
				return null;

			stream.Position = 0;
			return stream;
		}

		static char EatComment (Stream stream)
		{
			char c;
			do {
				c = (char)stream.ReadByte ();
				
			} while (c != '\n' && c != '\n');
			
			return c;
		}

		static string GetString (Stream stream)
		{
			System.Text.StringBuilder builder = new System.Text.StringBuilder ();

			char c;
			do {
				c = (char)stream.ReadByte ();
				if (c == '#')
					c = EatComment (stream);

			} while (char.IsWhiteSpace (c));
			
			while (! char.IsWhiteSpace (c)) {
				builder.Append (c);
				c = (char)stream.ReadByte ();				
			}
			
			return builder.ToString ();
		}

		public static ushort [] ReadShort (Stream stream, int width, int height, int channels)
		{
			int length = width * height * channels;
			ushort [] data = new ushort [length];
			byte [] tmp = new byte [2];

			for (int i = 0; i < length; i++)
			{
				stream.Read (tmp, 0, tmp.Length);
				data [i] = BitConverter.ToUInt16 (tmp, 0, false);
			}
			return data;
		}

#if false
		static Gdk.Pixbuf LoadRGB16 (Stream stream, int width, int height)
		{
			Gdk.Pixbuf pixbuf = new Gdk.Pixbuf (Gdk.Colorspace.Rgb, false, 8, width, height);
			unsafe {
				byte *pixels = (byte *)pixbuf.Pixels;
				int length = width * 6;
				byte [] buffer = new byte [length];
				
				for (int row = 0; row < height; row++) {
					stream.Read (buffer, 0, buffer.Length);
					for (int i = 0; i < width * 3; i++) {
						pixels [i] = (byte) (BitConverter.ToUInt16 (buffer, i * 2, false) >> 8);
					}
					pixels += pixbuf.Rowstride;
				}
			}
			return pixbuf;
		}

		static Gdk.Pixbuf LoadRGB8 (Stream stream, int width, int height)
		{
			Gdk.Pixbuf pixbuf = new Gdk.Pixbuf (Gdk.Colorspace.Rgb, false, 8, width, height);
			unsafe {
				byte *pixels = (byte *)pixbuf.Pixels;
				byte [] buffer = new byte [width * 3];
				
				for (int i = 0; i < height; i++) {
					stream.Read (buffer, 0, buffer.Length);
					    
					System.Runtime.InteropServices.Marshal.Copy (buffer, 0, 
										     (System.IntPtr)pixels, buffer.Length);
					
					pixels += pixbuf.Rowstride; 
				}
			}
			return pixbuf;
		}

		static PixelBuffer LoadBufferRGB16 (Stream stream, int width, int height)
		{
			PixelBuffer pix = new UInt16Buffer (width, height);
			int count = width * 3;
			byte [] buffer = new byte [count * 2];

			for (int row = 0; row < height; row++) {
				int len = 0;
				while (len < buffer.Length) {
					int read = stream.Read (buffer, len, buffer.Length - len);
					if (read < 0)
						break;
					len += read;
				}

				pix.Fill16 (row, 0, buffer, 0, count, false);
			}

			return pix;
		}

		static PixelBuffer LoadBufferRGB8 (Stream stream, int width, int height)
		{
			PixelBuffer pix = new UInt16Buffer (width, height);
			int length = width * 3;
			byte [] buffer = new byte [length];
			
			for (int row = 0; row < height; row++) {
				stream.Read (buffer, 0, buffer.Length);
				pix.Fill8 (row, 0, buffer, 0, buffer.Length);
			}

			return pix;
		}

		public static FSpot.Imaging.PixelBuffer LoadBuffer (Stream stream)
		{

			Header header = new Header (stream);
			header.Dump (); 

			switch (header.Magic) {
			case "P6":
				if (header.IsDeep)
					return LoadBufferRGB16 (stream, header.Width, header.Height);
				else
					return LoadBufferRGB8 (stream, header.Width, header.Height);
			default:
				throw new System.Exception (System.String.Format ("unknown pnm type {0}", header.Magic));
			}			
		}

		public override Gdk.Pixbuf Load ()
		{
			try {
				using (Stream stream = Open ()) {
					Gdk.Pixbuf pixbuf = PnmFile.Load (stream);
					return pixbuf;
				}
			} catch (System.Exception e) {
				//System.Console.WriteLine (e.ToString ());
			}
			return null;
		}

		public override Gdk.Pixbuf Load (int width, int height)
		{
			return PixbufUtils.ScaleToMaxSize (this.Load (), width, height);
		}

		public override void Save (Gdk.Pixbuf pixbuf, System.IO.Stream stream)
		{
			if (pixbuf.HasAlpha)
				throw new NotImplementedException ();

			// FIXME this should be part of the header class
			string header = String.Format ("P6\n"
						       + "#Software: {0} {1}\n"
						       + "{2} {3}  #Width and Height\n"
						       + "255\n", 
						       FSpot.Defines.PACKAGE,
						       FSpot.Defines.VERSION,
						       pixbuf.Width, 
						       pixbuf.Height);
						       
			byte [] header_bytes = System.Text.Encoding.UTF8.GetBytes (header);
			stream.Write (header_bytes, 0, header.Length);
										 
			unsafe {
				byte * src_pixels = (byte *) pixbuf.Pixels;
				int src_stride = pixbuf.Rowstride;
				int count = pixbuf.Width * pixbuf.NChannels;
				int height = pixbuf.Height;

				for (int y = 0; y < height; y++) {
					for (int x = 0; x < count; x++) {
						stream.WriteByte (* (src_pixels + x));
					}
					src_pixels += src_stride;
				}
			}
		}

		public static Gdk.Pixbuf Load (Stream stream)
		{
			Header header = new Header (stream);
			header.Dump ();

			switch (header.Magic) {
			case "P6":
				if (header.IsDeep) {
#if SKIP_BUFFER					
					return LoadRGB16 (stream, header.Width, header.Height);
#else
					stream.Position = 0;
					FSpot.Imaging.PixelBuffer image = FSpot.Pnm.PnmFile.LoadBuffer (stream);
					Gdk.Pixbuf result = image.ToPixbuf (Cms.Profile.CreateStandardRgb ());
					return result;
#endif
				} else
					return LoadRGB8 (stream, header.Width, header.Height);
			default:
				throw new System.Exception (System.String.Format ("unknown pnm type {0}", header.Magic));
			}			
		}
#endif
	}

#if ENABLE_NUNIT
	[TestFixture]
	public class Tests {
		[Test]
		public void SaveLoad ()
		{
			using (Gdk.Pixbuf pixbuf = new Gdk.Pixbuf (null, "f-spot-32.png")) {
				Gdk.Pixbuf source = pixbuf;
				if (pixbuf.HasAlpha)
					source = PixbufUtils.Flatten (pixbuf);

				string path = ImageFile.TempPath ("test.ppm");
				PnmFile pnm = new PnmFile (path);
				using (Stream stream = File.OpenWrite (path)) {
					pnm.Save (source, stream);
				}

				pnm = new PnmFile (path);

				using (Gdk.Pixbuf saved = pnm.Load ()) {
					Assert.IsNotNull (saved);
					Assert.AreEqual (saved.Width, source.Width);
					Assert.AreEqual (saved.Height, source.Height);
				}
				
				if (source != pixbuf)
					source.Dispose ();

				File.Delete (path);
			}
		}
	}	
#endif
}
