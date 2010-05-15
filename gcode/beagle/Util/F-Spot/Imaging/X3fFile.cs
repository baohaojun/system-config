using System;
using System.IO;
using FSpot;
using SemWeb;

namespace FSpot.X3f {
	internal class Info {
		ushort major_version;
		ushort minor_version;
		byte [] uid;
		uint mark_bits;
		uint cols;
		uint rows;
		uint rotation;

		public PixbufOrientation Orientation {
			get {
				switch (rotation) {
				case 0:
					return PixbufOrientation.TopLeft;
				case 270:
					return PixbufOrientation.LeftBottom;
				case 180:
					return PixbufOrientation.BottomRight;
				case 90:
					return PixbufOrientation.RightTop;
				default:
					return PixbufOrientation.TopLeft;
				}
			}
		}

		public uint Width {
			get { return cols; }
		}
		
		public uint Height {
			get { return rows; }
		}

		public Info (Stream stream)
		{
			byte [] buffer = new byte [4];
			stream.Read (buffer, 0, buffer.Length);
			if (System.Text.Encoding.ASCII.GetString (buffer, 0, buffer.Length) != "FOVb")
				throw new ImageFormatException ("invalid Foveon magic number");
			
			stream.Read (buffer, 0, buffer.Length);
			major_version = BitConverter.ToUInt16 (buffer, 0, true);
			minor_version = BitConverter.ToUInt16 (buffer, 2, true);

			uid = new byte [16];
			stream.Read (uid, 0, uid.Length);

			stream.Read (buffer, 0, buffer.Length);
			mark_bits = BitConverter.ToUInt32 (buffer, 0, true);
			stream.Read (buffer, 0, buffer.Length);
			cols = BitConverter.ToUInt32 (buffer, 0, true);
			stream.Read (buffer, 0, buffer.Length);
			rows = BitConverter.ToUInt32 (buffer, 0, true);
			stream.Read (buffer, 0, buffer.Length);
			rotation = BitConverter.ToUInt32 (buffer, 0, true);
		}

		public override string ToString ()
		{
			return String.Format ("x3file ({0}.{1}, mark_bits={2}) ({3}, {4})",
					      major_version, minor_version,
					      mark_bits, Width, Height);
		}

	}

	public class X3fFile : DCRawFile, SemWeb.StatementSource {
		Info info;

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		internal Info Info {
			get {
				if (info == null)
					info = new Info (Open ());

				return info;
			}
		}

		public X3fFile (string path) : base (path)
		{
		}

		public X3fFile (System.Uri uri) : base (uri)
		{
		}
		
		public void Select (StatementSink sink)
		{
			MetadataStore.AddLiteral (sink, "tiff:Orientation", ((int)Info.Orientation).ToString ());
			MetadataStore.AddLiteral (sink, "tiff:ImageWidth", Info.Width.ToString ());
			MetadataStore.AddLiteral (sink, "tiff:ImageLength", Info.Height.ToString ());
		}

		public override PixbufOrientation GetOrientation ()
		{
			return Info.Orientation;
		}

	}
}
