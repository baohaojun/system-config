using ICSharpCode.SharpZipLib.Zip.Compression;
using SemWeb;
using System.IO;
using FSpot.Xmp;
using System.Collections;
using System.Reflection;

#if ENABLE_NUNIT
using NUnit.Framework;
#endif

namespace FSpot.Png {
	public class PngFile : ImageFile, SemWeb.StatementSource {
		PngHeader header;

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		private PngHeader Header {
			get {
				if (header == null) {
					using (System.IO.Stream input = Open ()) {
					        header = new PngHeader (input);
					}
				}
				
				return header;
			}
		}

		public System.Collections.ArrayList Chunks {
			get { return Header.Chunks; }
		}

		public PngFile (System.Uri uri) : base (uri)
		{
		}

		public PngFile (string path) : base (path)
		{
		}


		/**
		   Title 	Short (one line) title or caption for image 
		   Author 	Name of image's creator
		   Description 	Description of image (possibly long)
		   Copyright 	Copyright notice
		   Creation Time 	Time of original image creation
		   Software 	Software used to create the image
		   Disclaimer 	Legal disclaimer
		   Warning 	Warning of nature of content
		   Source 	Device used to create the image
		   Comment 	Miscellaneous comment
		   
		   xmp is XML:com.adobe.xmp

		   Other keywords may be defined for other purposes. Keywords of general interest can be registered with th
		*/
		public void Select (SemWeb.StatementSink sink)
		{
			foreach (Chunk c in Chunks) {
				if (c is IhdrChunk) {
					IhdrChunk ih = c as IhdrChunk;
					MetadataStore.AddLiteral (sink, "tiff:ImageWidth", ih.Width.ToString ());
					MetadataStore.AddLiteral (sink, "tiff:ImageLength", ih.Height.ToString ());
				} else if(c is TimeChunk) {
					TimeChunk tc = c as TimeChunk;

					MetadataStore.AddLiteral (sink, "xmp:ModifyDate", tc.Time.ToString ("yyyy-MM-ddThh:mm:ss"));
				} else if (c is TextChunk) {
					TextChunk text = c as TextChunk;

					switch (text.Keyword) {
					case "XMP":
					case "XML:com.adobe.xmp":
						using (System.IO.Stream xmpstream = new System.IO.MemoryStream (text.TextData)) {
							FSpot.Xmp.XmpFile xmp = new FSpot.Xmp.XmpFile (xmpstream);
							xmp.Select (sink);
						}
						break;
					case "Comment":
						MetadataStore.AddLiteral (sink, "exif:UserComment", text.Text);
						break;
					case "Software":
						MetadataStore.AddLiteral (sink, "xmp:CreatorTool", text.Text);
						break;
					case "Title":
						MetadataStore.AddLiteral (sink, "dc:title", "rdf:Alt", new SemWeb.Literal (text.Text, "x-default", null));
						break;
					case "Author":
						MetadataStore.AddLiteral (sink, "dc:creator", "rdf:Seq", new SemWeb.Literal (text.Text));
						break;
					case "Copyright":
						MetadataStore.AddLiteral (sink, "dc:rights", "rdf:Alt", new SemWeb.Literal (text.Text, "x-default", null));
						break;
					case "Description":
						MetadataStore.AddLiteral (sink, "dc:description", "rdf:Alt", new SemWeb.Literal (text.Text, "x-default", null));
						break;
					case "Creation Time":
						try {
							System.DateTime time = System.DateTime.Parse (text.Text);
							MetadataStore.AddLiteral (sink, "xmp:CreateDate", time.ToString ("yyyy-MM-ddThh:mm:ss"));
						} catch (System.Exception e) {
							//System.Console.WriteLine (e.ToString ());
						}
						break;
					}
				} else if (c is ColorChunk) {
					ColorChunk color = (ColorChunk)c;
					string [] whitepoint = new string [2];
					whitepoint [0] = color.WhiteX.ToString ();
					whitepoint [1] = color.WhiteY.ToString ();
					MetadataStore.Add (sink, "tiff:WhitePoint", "rdf:Seq", whitepoint);
					int i = 0;
					string [] rgb = new string [6];
					rgb [i++] = color.RedX.ToString ();
					rgb [i++] = color.RedY.ToString ();
					rgb [i++] = color.GreenX.ToString ();
					rgb [i++] = color.GreenY.ToString ();
					rgb [i++] = color.BlueX.ToString ();
					rgb [i++] = color.BlueY.ToString ();
					MetadataStore.Add (sink, "tiff:PrimaryChromaticities", "rdf:Seq", rgb);
				} else if (c.Name == "sRGB") {
					MetadataStore.AddLiteral (sink, "exif:ColorSpace", "1");
				} else if (c is PhysChunk) {
					PhysChunk phys = (PhysChunk)c;
					uint denominator = (uint) (phys.InMeters ? 100 : 1);
					
					MetadataStore.AddLiteral (sink, "tiff:ResolutionUnit", phys.InMeters ? "3" : "1");
					MetadataStore.AddLiteral (sink, "tiff:XResolution", new FSpot.Tiff.Rational (phys.PixelsPerUnitX, denominator).ToString ());
					MetadataStore.AddLiteral (sink, "tiff:YResolution", new FSpot.Tiff.Rational (phys.PixelsPerUnitY, denominator).ToString ());
				}
			}
		}

		public class ZtxtChunk : TextChunk {
			//public static string Name = "zTXt";

			protected bool compressed = true;
			public bool Compressed {
				get {
					return compressed;
				}
			}
			
			byte compression;
			public byte Compression {
			        get {
					return compression;
				}
				set {
					if (compression != 0)
						throw new System.Exception ("Unknown compression method");
				}
			}

			public ZtxtChunk (string keyword, string text) : base ()
			{
				Name = "zTXt";
				Compression = 0;
				this.keyword = keyword;
			}

			public ZtxtChunk (string name, byte [] data) : base (name, data)
			{
			}			

			protected ZtxtChunk ()
			{
			}

			public override void SetText (string text)
			{
				/* FIXME this is broken */
				text_data = encoding.GetBytes (text);
				data = Chunk.Deflate (text_data, 0, text_data.Length);
			}
			
			public override void Load (byte [] data) 
			{
				int i = 0;
				keyword = GetString (ref i);
				i++;
				Compression = data [i++];

				text_data = Chunk.Inflate (data, i, data.Length - i);
			}
		}

		public class PhysChunk : Chunk {
			public PhysChunk (string name, byte [] data) : base (name, data) {}
			
			public uint PixelsPerUnitX {
				get {
					return BitConverter.ToUInt32 (data, 0, false);
				}
			}

			public uint PixelsPerUnitY {
				get {
					return BitConverter.ToUInt32 (data, 4, false);
				}
			}
			
			public bool InMeters {
				get {
					return data [8] == 0;
				}
			}
		}
		
		public class TextChunk : Chunk {
			//public static string Name = "tEXt";

			protected string keyword;
			protected byte [] text_data;
			protected System.Text.Encoding encoding = Latin1;

			public static System.Text.Encoding Latin1 = System.Text.Encoding.GetEncoding (28591);

			public TextChunk (string name, byte [] data) : base (name, data) 
			{
			}

			protected TextChunk ()
			{
			}


			public TextChunk (string keyword, string text)
			{
				this.Name = "tEXt";
				this.keyword = keyword;
				SetText (text);
			}

			public override void Load (byte [] data)
			{
				int i = 0;

				keyword = GetString (ref i);
				i++;
				int len = data.Length - i;
				text_data = new byte [len];
				System.Array.Copy (data, i, text_data, 0, len);
			}

			public string Keyword {
				get {
					return keyword;
				}
			}

			public byte [] TextData {
				get {
					return text_data;
				}
			}
			
			public virtual void SetText (string text)
			{
				text_data = encoding.GetBytes (text);

				byte [] keyword_data = Latin1.GetBytes (keyword);
				data = new byte [keyword_data.Length + 1 + text_data.Length];
				System.Array.Copy (keyword_data, 0, data, 0, keyword_data.Length);
				data [keyword_data.Length] = 0;
				System.Array.Copy (text_data, 0, data, keyword_data.Length + 1, text_data.Length);
			}

			public string Text {
				get {
					return encoding.GetString (text_data, 0, text_data.Length);
				}
			}
		}
		
		public class IccpChunk : Chunk {
			string keyword;
			byte [] profile;

			public IccpChunk (string name, byte [] data) : base (name, data) {}
			
			public override void Load (byte [] data)
			{
				int i = 0;
				keyword = GetString (ref i);
				i++;
				int compression = data [i++];
				if (compression != 0)
					throw new System.Exception ("Unknown Compression type");

				profile = Chunk.Inflate (data, i, data.Length - i);
			}

			public string Keyword {
				get {
					return keyword;
				}
			}
			
			public byte [] Profile {
				get {
					return profile;
				}
			}
		}

		public class ItxtChunk : ZtxtChunk{
			//public static string Name = "zTXt";

			string Language;
			string LocalizedKeyword;

			public override void Load (byte [] data)
			{
				int i = 0;
				keyword = GetString (ref i);
				i++;
				compressed = (data [i++] != 0);
				Compression = data [i++];
				Language = GetString (ref i);
				i++;
				LocalizedKeyword = GetString (ref i, System.Text.Encoding.UTF8);
				i++;

				if (Compressed) {
					text_data = Chunk.Inflate (data, i, data.Length - i);
				} else {
					int len = data.Length - i;
					text_data = new byte [len];
					System.Array.Copy (data, i, text_data, 0, len);
				}
			}

			public override void SetText (string text)
			{
				byte [] raw = System.Text.Encoding.UTF8.GetBytes (text);
				SetText (raw);
			}

			public void SetText (byte [] raw)
			{
				using (MemoryStream stream = new MemoryStream ()) {
					byte [] tmp;
					
					text_data = raw;

					tmp = Latin1.GetBytes (keyword);
					stream.Write (tmp, 0, tmp.Length);
					stream.WriteByte (0);
					
					stream.WriteByte ((byte)(compressed ? 1 : 0));
					stream.WriteByte (Compression);
					
					if (Language != null && Language != System.String.Empty) {
						tmp = Latin1.GetBytes (Language);
						stream.Write (tmp, 0, tmp.Length);
					}
					stream.WriteByte (0);
					
					if (LocalizedKeyword != null && LocalizedKeyword != System.String.Empty) {
						tmp = System.Text.Encoding.UTF8.GetBytes (LocalizedKeyword);
						stream.Write (tmp, 0, tmp.Length);
					}
					stream.WriteByte (0);
				
					if (compressed) {
						tmp = Deflate (text_data, 0, text_data.Length);
						stream.Write (tmp, 0, tmp.Length);
					} else {
						stream.Write (text_data, 0, text_data.Length);
					}
					this.data = stream.ToArray ();
				}
			}

			public ItxtChunk (string name, byte [] data) : base (name, data) 
			{
				this.Name = name;
				encoding = System.Text.Encoding.UTF8;
			}

			public ItxtChunk (string keyword, string language, bool compressed) : base ()
			{
				encoding = System.Text.Encoding.UTF8;
				this.Name = "iTXt";
				this.keyword = keyword;
				this.Language = language;
				this.LocalizedKeyword = System.String.Empty;
				this.compressed = compressed;
				this.Compression = 0;
			}
		}

		public class TimeChunk : Chunk {
			//public static string Name = "tIME";

			System.DateTime time;

			public System.DateTime Time {
				get {
					return new System.DateTime (FSpot.BitConverter.ToUInt16 (data, 0, false),
								    data [2], data [3], data [4], data [5], data [6]);

				}
				set {
					byte [] year = BitConverter.GetBytes ((ushort)value.Year, false);
					data [0] = year [0];
					data [1] = year [1];
					data [2] = (byte) value.Month;
					data [3] = (byte) value.Day;
					data [4] = (byte) value.Hour;
					data [6] = (byte) value.Minute;
					data [7] = (byte) value.Second;
				}
			}
			
			public TimeChunk (string name, byte [] data) : base (name, data) {}
			
			public TimeChunk ()
			{
				this.Name = "tIME";
				this.Time = System.DateTime.Now;
			}
		}
#if false
		public class StandardRgbChunk : Chunk {
			public StandardRgbChunk (string name, byte [] data) : base (name, data) {}
			
			public Cms.Intent RenderingIntent {
				get {
					return (Cms.Intent) data [0];
				}
			}
		}
#endif
		public class GammaChunk : Chunk {
			public GammaChunk (string name, byte [] data) : base (name, data) {}
			private const int divisor = 100000;

			public double Gamma {
				get {
					return FSpot.BitConverter.ToUInt32 (data, 0, false) / (double) divisor;
				}
			}
		}
		
		public class ColorChunk : Chunk {
			// FIXME this should be represented like a tiff rational
			public const uint Denominator = 100000;

			public ColorChunk (string name, byte [] data) : base (name, data) {}

			public FSpot.Tiff.Rational WhiteX {
				get {
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 0, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational WhiteY {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 4, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational RedX {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 8, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational RedY {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 12, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational GreenX {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 16, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational GreenY {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 20, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational BlueX {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 24, false), Denominator);
				}
			}
			public FSpot.Tiff.Rational BlueY {
				get { 
					return new FSpot.Tiff.Rational (FSpot.BitConverter.ToUInt32 (data, 28, false), Denominator);
				}
			}
		}

		public enum ColorType : byte {
			Gray = 0,
			Rgb = 2,
			Indexed = 3,
			GrayAlpha = 4,	
			RgbA = 6
		};
		
		public enum CompressionMethod : byte {
			Zlib = 0
		};
		
		public enum InterlaceMethod : byte {
			None = 0,
			Adam7 = 1
		};

		public enum FilterMethod : byte {
			Adaptive = 0
		}

		// Filter Types Show up as the first byte of each scanline
		public enum FilterType  {
			None = 0,
			Sub = 1,
			Up = 2,
			Average = 3,
			Paeth = 4
		};

		public class IhdrChunk : Chunk {
			public uint Width;
			public uint Height;
			public byte Depth;
			public ColorType Color;
			public PngFile.CompressionMethod Compression;
			public FilterMethod Filter;
			public InterlaceMethod Interlace;

			public IhdrChunk (string name, byte [] data) : base (name, data) {}
			
			public override void Load (byte [] data)
			{
				Width = BitConverter.ToUInt32 (data, 0, false);
				Height = BitConverter.ToUInt32 (data, 4, false);
				Depth = data [8];
				Color = (ColorType) data [9];
				//if (Color != ColorType.Rgb)
				//	throw new System.Exception (System.String.Format ("unsupported {0}", Color));

				this.Compression = (CompressionMethod) data [10];
				if (this.Compression != CompressionMethod.Zlib)
					throw new System.Exception (System.String.Format ("unsupported {0}", Compression));

				Filter = (FilterMethod) data [11];
				if (Filter != FilterMethod.Adaptive)
					throw new System.Exception (System.String.Format ("unsupported {0}", Filter));
					
				Interlace = (InterlaceMethod) data [12];
				//if (Interlace != InterlaceMethod.None)
				//	throw new System.Exception (System.String.Format ("unsupported {0}", Interlace));

			}

			public int ScanlineComponents {
				get {
					switch (Color) {
					case ColorType.Gray:
					case ColorType.Indexed:
						return 1;
					case ColorType.GrayAlpha:
						return 2;
					case ColorType.Rgb:
						return 3;
					case ColorType.RgbA:
						return 4;
					default:
						throw new System.Exception (System.String.Format ("Unknown format {0}", Color));
					}
				}
			}

			public uint GetScanlineLength (int pass)
			{
				uint length = 0;
				if (Interlace == InterlaceMethod.None) {
					int bits = ScanlineComponents * Depth;
					length = (uint) (this.Width * bits / 8);

					// and a byte for the FilterType
					length ++;
				} else {
					throw new System.Exception (System.String.Format ("unsupported {0}", Interlace));
				}

				return length;
			}
		}

		public class Crc {
			static uint [] lookup;
			uint value = 0xffffffff;
			uint length;
			System.IO.Stream stream;

			public uint Value {
				get { return (value ^ 0xffffffff); }
			}

			public uint Length {
				get { return length; }
			}

			static Crc () {
				lookup = new uint [265];
				uint c, n;
				int k;
				
				for (n = 0; n < 256; n++) {
					c = n;
					for (k = 0; k < 8; k++) {
						if ((c & 1) != 0)
							c = 0xedb88320 ^ (c >> 1);
						else
							c = c >> 1;
					}
					lookup [n] = c;
				}
			}

			public Crc ()
			{
			}

			public Crc (System.IO.Stream stream)
			{
				this.stream = stream;
			}
			
			public void Write (byte [] buffer)
			{
				Write (buffer, 0, buffer.Length);
			}

			public void Write (byte [] buffer, int offset, int len)
			{
				for (int i = offset; i < len; i++) 
					value = lookup [(value ^ buffer[i]) & 0xff] ^ (value >> 8); 

				length += (uint)len;

				if (stream != null)
					stream.Write (buffer, offset, len);
			}

			public void WriteSum ()
			{
				byte [] data = BitConverter.GetBytes (Value, false);
				stream.Write (data, 0, data.Length);
			}
		}

		public class Chunk {
			public string Name;
			protected byte [] data;
			protected static System.Collections.Hashtable name_table;

			public byte [] Data {
				get {
					return data;
				}
				set {
					Load (value);
				}
			}
			
			static Chunk () 
			{

				name_table = new System.Collections.Hashtable ();
				name_table ["iTXt"] = typeof (ItxtChunk);
				name_table ["tXMP"] = typeof (ItxtChunk);
				name_table ["tEXt"] = typeof (TextChunk);
				name_table ["zTXt"] = typeof (ZtxtChunk);
				name_table ["tIME"] = typeof (TimeChunk);
				name_table ["iCCP"] = typeof (IccpChunk);
				name_table ["IHDR"] = typeof (IhdrChunk);
				name_table ["cHRM"] = typeof (ColorChunk);
				name_table ["pHYs"] = typeof (PhysChunk);
				name_table ["gAMA"] = typeof (GammaChunk);
#if false
				name_table ["sRGB"] = typeof (StandardRgbChunk);
#endif
			}
			
			protected Chunk ()
			{
			}
			
			public Chunk (string name, byte [] data) 
			{
				this.Name = name;
				this.data = data;
				Load (data);
			}
			
			protected string GetString  (ref int i, System.Text.Encoding enc) 
			{
				for (; i < data.Length; i++) {
					if (data [i] == 0)
						break;
				}	
				
				return enc.GetString (data, 0, i);
			}

			protected string GetString  (ref int i) 
			{
				return GetString (ref i, TextChunk.Latin1);
			}

			public virtual void Load (byte [] data)
			{
				
			}
			
			public virtual void Save (System.IO.Stream stream)
			{
				byte [] name_bytes = System.Text.Encoding.ASCII.GetBytes (Name);
				byte [] length_bytes = BitConverter.GetBytes ((uint)data.Length, false);
				stream.Write (length_bytes, 0, length_bytes.Length);
				Crc crc = new Crc (stream);
				crc.Write (name_bytes);
				crc.Write (data);
				crc.WriteSum ();
			}

			public bool Critical {
				get {
					return !System.Char.IsLower (Name, 0);
				}
			}

			public bool Private {
				get {
					return System.Char.IsLower (Name, 1);
				}
			}
			
			public bool Reserved {
				get {
					return System.Char.IsLower (Name, 2);
				}
			}
			
			public bool Safe {
				get {
					return System.Char.IsLower (Name, 3);
				}
			}

			public bool CheckCrc (uint value)
			{
				byte [] name = System.Text.Encoding.ASCII.GetBytes (Name);
				Crc crc = new Crc ();
				crc.Write (name);
				crc.Write (data);

				return crc.Value == value;
			}

			public static Chunk Generate (string name, byte [] data)
			{
				System.Type t = (System.Type) name_table [name];

				Chunk chunk;
				if (t != null)
					chunk = (Chunk) System.Activator.CreateInstance (t, new object[] {name, data});
				else
				        chunk = new Chunk (name, data);

				return chunk;
			}

			public static byte [] Inflate (byte [] input, int start, int length)
			{
				System.IO.MemoryStream output = new System.IO.MemoryStream ();
				Inflater inflater = new Inflater ();
				
				inflater.SetInput (input, start, length);
				
				byte [] buf = new byte [1024];
				int inflate_length;
				while ((inflate_length = inflater.Inflate (buf)) > 0)
					output.Write (buf, 0, inflate_length);
				
				output.Close ();
				return output.ToArray ();
			}

			public static byte [] Deflate (byte [] input, int offset, int length)
			{
				System.IO.MemoryStream output = new System.IO.MemoryStream ();
				Deflater deflater = new Deflater ();
				deflater.SetInput (input, offset, length);
				
				byte [] buf = new byte [1024];
				int deflate_length;
				while ((deflate_length = deflater.Deflate (buf)) > 0)
					output.Write (buf, 0, deflate_length);

				output.Close ();
				return output.ToArray ();
			}
		}

		public class ChunkInflater {
			private Inflater inflater;
			private System.Collections.ArrayList chunks;

			public ChunkInflater ()
			{
				inflater = new Inflater ();
				chunks = new System.Collections.ArrayList ();
			}

			public bool Fill () 
			{
				while (inflater.IsNeedingInput && chunks.Count > 0) {
					inflater.SetInput (((Chunk)chunks[0]).Data);
					////System.Console.WriteLine ("adding chunk {0}", ((Chunk)chunks[0]).Data.Length);
					chunks.RemoveAt (0);
				}
				return true;
			}
			
			public int Inflate (byte [] data, int start, int length)
			{
				int result = 0;
				do {
					Fill ();
					result += inflater.Inflate (data, start + result, length - result);
					//System.Console.WriteLine ("Attempting Second after fill Inflate {0} {1} {2}", attempt, result, length - result);
				} while (result < length && chunks.Count > 0);
				
				return result;
			}
		       
			public void Add (Chunk chunk)
			{
				chunks.Add (chunk);
			}
		}

#if false
		public class ScanlineDecoder {
			int width;
			int height;
			int row;
			int col;
			ChunkInflater inflater;
			byte [] buffer;

			public ScanlineDecoder (ChunkInflater inflater, uint width, uint height)
			{
				this.inflater = inflater;
				this.row = 0;
				this.height = (int)height;
				this.width = (int)width;
				
				buffer = new byte [width * height];

				Fill ();
			}

			public void Fill () 
			{
				for (; row < height; row ++) { 
					col = inflater.Inflate (buffer, row * width, width);
					
					if (col < width) {
						inflater.Fill ();
						//System.Console.WriteLine ("short read missing {0} {1} {2}", width - col, row, height);
					}
				}
			}
			
			private static byte PaethPredict (byte a, byte b, byte c)
			{
				int p = a + b - c;
				int pa = System.Math.Abs (p - a);
				int pb = System.Math.Abs (p - b);
				int pc = System.Math.Abs (p - c);
				if (pa <= pb && pa <= pc)
					return a;
				else if (pb <= pc)
					return b;
				else 
					return c;
			}

			public void ReconstructRow (int row, int channels)
			{
				int offset = row * width;
				FilterType type = (FilterType) buffer [offset];
				byte a = 0;
				byte x;
				byte b;
				byte c = 0;
				
				offset++;
				//buffer [offset++] = 0;
				
				int prev_line;

				//System.Console.WriteLine ("type = {0}", type);
				for (int col = 1; col < this.width;  col++) {
					x = buffer [offset];

					prev_line = offset - width;

					a = col <= channels ? (byte) 0 : (byte) buffer [offset - channels];
					b = (prev_line) < 0 ? (byte) 0 : (byte) buffer [prev_line];
					c = (prev_line) < 0 || (col <= channels) ? (byte) 0 : (byte) buffer [prev_line - channels];

#if false
					switch (type) {
					case FilterType.None:
						break;
					case FilterType.Sub:
						x = (byte) (x + a);
						break;
					case FilterType.Up:
						x = (byte) (x + b);
						break;
					case FilterType.Average:
						x = (byte) (x + ((a + b) >> 1));
						break;
					case FilterType.Paeth:
						x = (byte) (x + PaethPredict (a, b, c));
						break;
					default:					
						throw new System.Exception (System.String.Format ("Invalid FilterType {0}", type));
					}
#else
					if (type == FilterType.Sub) {
						x = (byte) (x + a);
					} else if (type == FilterType.Up) {
						x = (byte) (x + b);
					} else if (type == FilterType.Average) {
						x = (byte) (x + ((a + b) >> 1));
					} else if (type == FilterType.Paeth) {
						int p = a + b - c;
						int pa = System.Math.Abs (p - a);
						int pb = System.Math.Abs (p - b);
						int pc = System.Math.Abs (p - c);
						if (pa <= pb && pa <= pc)
							x = (byte)(x + a);
						else if (pb <= pc)
							x = (byte)(x + b);
						else 
							x = (byte)(x + c);
					}
#endif
					//System.Console.Write ("{0}.", x);
					buffer [offset ++] = x;
				}

			}

			public unsafe void UnpackRGBIndexedLine (Gdk.Pixbuf dest, int line, int depth, byte [] palette, byte [] alpha)
			{
				int pos = line * width + 1;
				byte * pixels = (byte *) dest.Pixels;
				
				pixels += line * dest.Rowstride;
				int channels = dest.NChannels;
				int div = (8 / depth);
				byte mask = (byte)(0xff >> (8 - depth));

				for (int i = 0; i < dest.Width; i++) {
					int val = buffer [pos + i / div];
					int shift = (8 - depth) - (i % div) * depth;

					val = (byte) ((val & (byte)(mask << shift)) >> shift);

					pixels [i * channels] = palette [val * 3];
					pixels [i * channels + 1] = palette [val * 3 + 1];
					pixels [i * channels + 2] = palette [val * 3 + 2];

					if (channels > 3 && alpha != null) 
						pixels [i * channels + 3] = val < alpha.Length ? alpha [val] : (byte)0xff; 
				}
			}

			public unsafe void UnpackRGB16Line (Gdk.Pixbuf dest, int line, int channels)
			{
				int pos = line * width + 1;
				byte * pixels = (byte *) dest.Pixels;
				
				pixels += line * dest.Rowstride;
				
				if (dest.NChannels != channels)
					throw new System.Exception ("bad pixbuf format");

				int i = 0;
				int length = dest.Width * channels;
				while (i < length) {
					pixels [i++] = (byte) (BitConverter.ToUInt16 (buffer, pos, false) >> 8);
					pos += 2;
				}

			}

			public unsafe void UnpackRGB8Line (Gdk.Pixbuf dest, int line, int channels)
			{
				int pos = line * width + 1;
				byte * pixels = (byte *) dest.Pixels;

				pixels += line * dest.Rowstride;
				if (dest.NChannels != channels)
					throw new System.Exception ("bad pixbuf format");

				System.Runtime.InteropServices.Marshal.Copy (buffer, pos, 
									     (System.IntPtr)pixels, dest.Width * channels);

			}

			public unsafe void UnpackGrayLine (Gdk.Pixbuf dest, int line, int depth, bool alpha)
			{
				int pos = line * width + 1;
				byte * pixels = (byte *) dest.Pixels;
				
				pixels += line * dest.Rowstride;
				int div = (8 / depth);
				byte mask = (byte)(0xff >> (8 - depth));
				int length = dest.Width * (alpha ? 2 : 1);
				
				for (int i = 0; i < length; i++) {
					byte val = buffer [pos + i / div];
					int shift = (8 - depth) - (i % div) * depth;

					if (depth != 8) {
						val = (byte) ((val & (byte)(mask << shift)) >> shift);
						val = (byte) (((val * 0xff) + (mask >> 1)) / mask); 
					}
					
					if (!alpha || i % 2 == 0) {
						pixels [0] = val;
						pixels [1] = val;
						pixels [2] = val;
						pixels += 3;
					} else {
						pixels [0] = val;
						pixels ++;
					}
				}
			}

			public unsafe void UnpackGray16Line (Gdk.Pixbuf dest, int line, bool alpha)
			{
				int pos = line * width + 1;
				byte * pixels = (byte *) dest.Pixels;

				pixels += line * dest.Rowstride;

				int i = 0;
				while (i < dest.Width) {
					byte val = (byte) (BitConverter.ToUInt16 (buffer, pos, false) >> 8);
					pixels [0] = val;
					pixels [1] = val;
					pixels [2] = val;
					if (alpha) {
						pos += 2;
						pixels [3] = (byte)(BitConverter.ToUInt16 (buffer, pos, false) >> 8);
					}
					pos += 2;
					pixels += dest.NChannels;
					i++;
				}
			}

			
		}
		
		public Gdk.Pixbuf GetPixbuf ()
		{
			ChunkInflater ci = new ChunkInflater ();
			Chunk palette = null;
			Chunk transparent = null;

			foreach (Chunk chunk in Chunks) {
				if (chunk.Name == "IDAT")
					ci.Add (chunk);
				else if (chunk.Name == "PLTE") 
					palette = chunk;
				else if (chunk.Name == "tRNS")
					transparent = chunk;
			}

			IhdrChunk ihdr = (IhdrChunk) Chunks [0];
			//System.Console.WriteLine ("Attempting to to inflate photo {0}.{1}({2}, {3})", ihdr.Color, ihdr.Depth, ihdr.Width, ihdr.Height);
			ScanlineDecoder decoder = new ScanlineDecoder (ci, ihdr.GetScanlineLength (0), ihdr.Height);
			decoder.Fill ();
			//Gdk.Pixbuf pixbuf = decoder.GetPixbuf ();

			//System.Console.WriteLine ("XXXXXXXXXXXXXXXXXXXXXXXXXXX Inflate ############################");

			bool alpha = (ihdr.Color == ColorType.GrayAlpha || ihdr.Color == ColorType.RgbA || transparent != null);

			Gdk.Pixbuf pixbuf = new Gdk.Pixbuf (Gdk.Colorspace.Rgb, 
							    alpha, 8, (int)ihdr.Width, (int)ihdr.Height);
			
			for (int line = 0; line < ihdr.Height; line++) {
				switch (ihdr.Color) {
				case ColorType.Rgb:
					if (ihdr.Depth == 16) {
						decoder.ReconstructRow (line, 6);
						decoder.UnpackRGB16Line (pixbuf, line, 3);
					} else {
						decoder.ReconstructRow (line, 3);
						decoder.UnpackRGB8Line (pixbuf, line, 3);
					}
					break;
				case ColorType.RgbA:
					if (ihdr.Depth == 16) {
						decoder.ReconstructRow (line, 8);
						decoder.UnpackRGB16Line (pixbuf, line, 4);						
					} else {
						decoder.ReconstructRow (line, 4);
						decoder.UnpackRGB8Line (pixbuf, line, 4);
					}
					break;
				case ColorType.GrayAlpha:
					switch (ihdr.Depth) {
					case 16:
						decoder.ReconstructRow (line, 4);
						decoder.UnpackGray16Line (pixbuf, line, true);
						break;
					default:
						decoder.ReconstructRow (line, 2);
						decoder.UnpackGrayLine (pixbuf, line, ihdr.Depth, true);
						break;
					}
					break;
				case ColorType.Gray:
					switch (ihdr.Depth) {
					case 16:
						decoder.ReconstructRow (line, 2);
						decoder.UnpackGray16Line (pixbuf, line, false);
						break;
					default:
						decoder.ReconstructRow (line, 1);
						decoder.UnpackGrayLine (pixbuf, line, ihdr.Depth, false);
						break;
					}
					break;
				case ColorType.Indexed:
					decoder.ReconstructRow (line, 1);
					decoder.UnpackRGBIndexedLine (pixbuf, 
								      line, 
								      ihdr.Depth, 
								      palette.Data, 
								      transparent != null ? transparent.Data : null);
					break;
				default:
					throw new System.Exception (System.String.Format ("unhandled color type {0}", ihdr.Color));
				}
			}
			return pixbuf;
		}
#endif
		private static byte [] magic = new byte [] { 137, 80, 78, 71, 13, 10, 26, 10 };

				
		public class PngHeader {
			ArrayList chunk_list;
			
			public ArrayList Chunks { 
				get { return chunk_list; }
			}
			
			public PngHeader (Stream stream) 
			{
				byte [] heading = new byte [8];
				byte [] crc_data = new byte [4];
				stream.Read (heading, 0, heading.Length);
				
				for (int i = 0; i < heading.Length; i++)
				if (heading [i] != magic [i])
					throw new System.Exception ("Invalid PNG magic number");
				
				chunk_list = new System.Collections.ArrayList ();
				
				for (int i = 0; stream.Read (heading, 0, heading.Length) == heading.Length; i++) {
					uint length = BitConverter.ToUInt32 (heading, 0, false);
					string name = System.Text.Encoding.ASCII.GetString (heading, 4, 4);
					byte [] data = new byte [length];
					if (length > 0)
						stream.Read (data, 0, data.Length);
					
					stream.Read (crc_data, 0, 4);
					uint crc = BitConverter.ToUInt32 (crc_data, 0, false);
					
					Chunk chunk = Chunk.Generate (name, data);
					if (! chunk.CheckCrc (crc))
						throw new System.Exception ("chunk crc check failed");
					
					//System.Console.Write ("read one {0} {1}", chunk, chunk.Name);
					chunk_list.Add (chunk);
					
#if false			       
					if (chunk is TextChunk) {
						TextChunk text = (TextChunk) chunk;
						System.Console.Write (" Text Chunk {0} {1}", 
								      text.Keyword, System.String.Empty, System.String.Empty);
					}
					
					TimeChunk time = chunk as TimeChunk;
					if (time != null)
						System.Console.Write(" Time {0}", time.Time);

					//System.Console.WriteLine (System.String.Empty);
#endif
					
					if (chunk.Name == "IEND")
						break;
				}
			}

			internal string LookupText (string keyword)
			{
				TextChunk chunk = LookupTextChunk (keyword);
				if (chunk != null)
					return chunk.Text;

				return null;
			}
			
			internal TextChunk LookupTextChunk (string keyword)
			{
				foreach (Chunk chunk in Chunks) {
					TextChunk text = chunk as TextChunk;
					if (text != null && text.Keyword == keyword)
						return text;
				}
				return null;	
			}
			
			internal void Insert (Chunk chunk)
			{
				// FIXME The point of this function is to enforce ordering constraints
				// it obviously isn't complete right now.
				if (chunk is IhdrChunk)
				Chunks.Insert (0, chunk);
				else if (chunk is TextChunk)
					Chunks.Insert (1, chunk);
				else
					throw new System.Exception ("Uknown ordering for chunk");
			}
			
			public void Save (System.IO.Stream stream)
			{
				stream.Write (magic, 0, magic.Length);
				foreach (Chunk chunk in Chunks) {
					chunk.Save (stream);
				}
			}
		}


		public void Save (System.IO.Stream stream)
		{
			Header.Save (stream);
		}

#if false
		public void Save (string path)
		{
			string  temp_path = path + ".tmp.png";
			using (System.IO.Stream output = System.IO.File.OpenWrite (temp_path)) {
				Save (output);
			}
			if (FSpot.Utils.Unix.Rename (temp_path, path) < 0) {
				System.IO.File.Delete (temp_path);
				throw new System.Exception (System.String.Format ("Unable to rename {0} to {1}", temp_path, path));
			}
		}

		public override void Save (Gdk.Pixbuf pixbuf, System.IO.Stream stream)
		{
			byte [] buffer = PixbufUtils.Save (pixbuf, "png", null, null);
			using (MemoryStream mem = new MemoryStream (buffer)) {
				PngHeader converted = new PngHeader (mem);
				
				/* FIXME we need to update the XMP metadata here */
				foreach (Chunk c in Chunks) {
					if (c is TextChunk) {
						converted.Insert (c);
					}
				}
				
				converted.Save (stream);
			}
		}

		public override Cms.Profile GetProfile ()
		{
			ColorChunk color = null;
			IccpChunk icc = null;
			GammaChunk gamma = null;
			StandardRgbChunk srgb = null;
			double gamma_value = 2.2;
			ColorCIExyY red = new ColorCIExyY (0.64, 0.33, 1.0);
			ColorCIExyY green = new ColorCIExyY (0.3, 0.6, 1.0);
			ColorCIExyY blue = new ColorCIExyY (0.15, 0.06, 1.0);
			ColorCIExyY whitepoint = new ColorCIExyY (0.3127, 0.329, 1.0);
			ColorCIExyYTriple chroma = new ColorCIExyYTriple (red, green, blue);

			//System.Console.WriteLine ("Trying to get profile");

			foreach (Chunk chunk in Chunks) {
				if (color == null) 
					color = chunk as ColorChunk;
				if (icc == null)
					icc = chunk as IccpChunk;
				if (srgb == null)
					srgb = chunk as StandardRgbChunk;
				if (gamma == null)
					gamma = chunk as GammaChunk;
			}
			
			//System.Console.WriteLine ("color: {0} icc: {1} srgb: {2} gamma: {3}", color, icc, srgb, gamma);

			if (icc != null) {
				try {
					return new Profile (icc.Profile);
				} catch (System.Exception ex) {
					//System.Console.WriteLine ("Error trying to decode embedded profile" + ex.ToString ());
				}
			}

			if (srgb != null)
				return Profile.CreateStandardRgb ();

			if (gamma != null)
				gamma_value = 1 / gamma.Gamma;
			
			if (color != null) {
				whitepoint = new ColorCIExyY (color.WhiteX.Value, color.WhiteY.Value, 1.0);
				red = new ColorCIExyY (color.RedX.Value, color.RedY.Value, 1.0);
				green = new ColorCIExyY (color.GreenX.Value, color.GreenY.Value, 1.0);
				blue = new ColorCIExyY (color.BlueX.Value, color.BlueY.Value, 1.0);
				chroma = new ColorCIExyYTriple (red, green, blue);
			}

			if (color != null || gamma != null) {
				GammaTable table = new GammaTable (1024, gamma_value);
				return new Profile (whitepoint, chroma, new GammaTable [] {table, table, table});
			}
			
			return null;
		}
#endif
		public override string Description {
			get {
				string description = Header.LookupText ("Description");

				if (description != null)
					return description;
				else
					return Header.LookupText ("Comment");
			}
		}

		public void SetDescription (string description) 
		{
			TextChunk text = null;
			text = Header.LookupTextChunk ("Description");
			
			if (text != null)
				text.SetText (description);
			else 
				Header.Insert (new TextChunk ("Description", description));
		}

		public XmpFile GetXmp ()
		{
			TextChunk xmpchunk  = Header.LookupTextChunk ("XML:com.adobe.xmp");
			if (xmpchunk == null)
				xmpchunk = Header.LookupTextChunk ("XMP");

			if (xmpchunk == null)
				return null;
			
			using (MemoryStream stream = new MemoryStream (xmpchunk.TextData)) {
				return new XmpFile (stream);
			}
		}

		public void SetXmp (XmpFile xmp)
		{
			TextChunk text = null;

			text = Header.LookupTextChunk ("XML:com.adobe.xmp");
			if (text != null)
				Chunks.Remove (text);
			
			text = Header.LookupTextChunk ("XMP");
			if (text != null)
				Chunks.Remove (text);

			ItxtChunk itext = new ItxtChunk ("XML:com.adobe.xmp", "en", false);
			using (MemoryStream stream = new MemoryStream ()) {
				xmp.Save (stream);
				itext.SetText (stream.ToArray ());
			}
			Header.Insert (itext);
		}

		public override System.DateTime Date {
			get {
				// FIXME: we should first try parsing the
				// LookupText ("Creation Time") as a valid date

				foreach (Chunk chunk in Chunks) {
					TimeChunk time = chunk as TimeChunk;
					if (time != null)
						return time.Time.ToUniversalTime ();
				}
				return base.Date;
			}
		}
		
#if ENABLE_NUNIT
		[TestFixture]
		public class Tests {
			public Tests ()
			{
				Gnome.Vfs.Vfs.Initialize ();
				Gtk.Application.Init ();
			}

			[Test]
			public void Save ()
			{
				Gdk.Pixbuf test = new Gdk.Pixbuf (null, "f-spot-32.png");
				string path = ImageFile.TempPath ("joe.png");
				test.Save (path, "png");
				PngFile pimg = new PngFile (path);

				string desc = "this is a png test";
				string desc2 = "\000xa9 Novell Inc.";
				pimg.SetDescription (desc);
				using (Stream stream = File.OpenWrite (path)) {
					pimg.Save (stream);
				}
				PngFile mod = new PngFile (path);
				Assert.AreEqual (mod.Orientation, PixbufOrientation.TopLeft);
				Assert.AreEqual (mod.Description, desc);
				pimg.SetDescription (desc2);

				using (Stream stream = File.OpenWrite (path)) {
					pimg.Save (stream);
				}
				mod = new PngFile (path);
				Assert.AreEqual (mod.Description, desc2);
				
				File.Delete (path);
			}

			[Test]
			public void Load ()
			{
				string desc = "(c) 2004 Jakub Steiner\n\nCreated with The GIMP";
				Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly ();
				string path  = ImageFile.TempPath ("maddy.png");
				using (Stream output = File.OpenWrite (path)) {
					using (Stream source = assembly.GetManifestResourceStream ("f-spot-adjust-colors.png")) {
						byte [] buffer = new byte [256];
						while (source.Read (buffer, 0, buffer.Length) > 0) {
							output.Write (buffer, 0, buffer.Length);
						}
					}
				}
				PngFile pimg = new PngFile (path);
				Assert.AreEqual (pimg.Description, desc);

				File.Delete (path);
			}
		}
#endif

#if false
		public class ImageFile {
			string Path;
			public ImageFile (string path)
			{
				this.Path = path;
			}
		}

		public static void Main (string [] args) 
		{
			System.Collections.ArrayList failed = new System.Collections.ArrayList ();
			Gtk.Application.Init ();
			foreach (string path in args) {
				Gtk.Window win = new Gtk.Window (path);
				Gtk.HBox box = new Gtk.HBox ();
				box.Spacing = 12;
				win.Add (box);
				Gtk.Image image;
				image = new Gtk.Image ();

				System.DateTime start = System.DateTime.Now;
				System.TimeSpan one = start - start;
				System.TimeSpan two = start - start;
				try {
					start = System.DateTime.Now;
					image.Pixbuf = new Gdk.Pixbuf (path);
					one = System.DateTime.Now - start;
				}  catch (System.Exception e) {
				}
				box.PackStart (image);

				image = new Gtk.Image ();
				try {
					start = System.DateTime.Now;
					PngFile png = new PngFile (path);
					image.Pixbuf = png.GetPixbuf ();
					two = System.DateTime.Now - start;
				} catch (System.Exception e) {
					failed.Add (path);
					//System.Console.WriteLine ("Error loading {0}", path);
					//System.Console.WriteLine (e.ToString ());
				}

				//System.Console.WriteLine ("{2} Load Time {0} vs {1}", one.TotalMilliseconds, two.TotalMilliseconds, path); 
				box.PackStart (image);
				win.ShowAll ();
			}
			
			//System.Console.WriteLine ("{0} Failed to Load", failed.Count);
			foreach (string fail_path in failed) {
				//System.Console.WriteLine (fail_path);
			}

			Gtk.Application.Run ();
		}
#endif
	}
}
