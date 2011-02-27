using FSpot.Tiff;

namespace FSpot.Mrw {
	// Minolta raw format
	// see http://www.dalibor.cz/minolta/raw_file_format.htm for details
	// note that the blocks can be in any order.

	public class Block {
		protected byte [] name;
		protected uint  Length;
		protected long Start;
		protected System.IO.Stream stream;
		byte [] data;

		public Block (System.IO.Stream stream)
		{
			this.stream = stream;
			Start = stream.Position;
			name = new byte [4];
			byte [] tmp = new byte [8];
			stream.Read (tmp, 0, tmp.Length);
			System.Array.Copy (tmp, name, name.Length);
			//System.Console.WriteLine (this.Name);
			Length = BitConverter.ToUInt32 (tmp, name.Length, false);
			stream.Position = stream.Position + Length;
		}

		public string Name {
			get {
				return System.Text.Encoding.ASCII.GetString (this.name, 1, 3);
			}
		}

		public byte [] Data {
			get {
				if (data == null)
					data = ReadData ();
				
				return data;
			}
		}

		public static Block Create (System.IO.Stream stream)
		{
			byte [] tmp = new byte [4];
			stream.Read (tmp, 0, tmp.Length);
			stream.Position -= 4;
			string name = System.Text.Encoding.ASCII.GetString (tmp, 1, 3);
			switch (name) {
			case "TTW":
				return new TtwBlock (stream);
			case "PRD":
				return new PrdBlock (stream);
			default:
				return new Block (stream);
			}
		}

		protected byte [] ReadData ()
		{
			stream.Position = Start + 8;
			byte [] data = new byte [this.Length];
			stream.Read (data, 0, data.Length);

			return data;
		}
	}

	public class PrdBlock : Block {
		public PrdBlock (System.IO.Stream stream) : base (stream)
		{

		}

		public ulong Version {
			get {
				return BitConverter.ToUInt64 (this.Data, 0, false);
			}
		}
		
		public ushort CCDSizeY {
			get {
				return BitConverter.ToUInt16 (this.Data, 8, false);
			}
		}

		public ushort CCDSizeX {
			get {
				return BitConverter.ToUInt16 (this.Data, 10, false);
			}
		}

		public ushort ImageSizeY {
			get {
				return BitConverter.ToUInt16 (this.Data, 12, false);
			}
		}

		public ushort ImageSizeX {
			get {
				return BitConverter.ToUInt16 (this.Data, 14, false);
			}
		}
		
		public byte Depth {
			get {
				return this.Data [16];
			}
		}

		public byte SampleDepth {
			get {
				return this.Data [17];
			}
		}
	}

	internal class TtwBlock : Block {
		FSpot.Tiff.Header header;

		public TtwBlock (System.IO.Stream stream) : base (stream)
		{
			if (this.Name != "TTW")
				throw new System.Exception (System.String.Format ("invalid block name {0}", this.Name));
		}
		
		public FSpot.Tiff.Header TiffHeader {
			get {
				if (header == null) {
					try {
						System.IO.MemoryStream mem = new System.IO.MemoryStream (this.Data);
						//System.Console.WriteLine ("before header");
						header = new Header (mem);
					} catch (System.Exception e) {
						//System.Console.WriteLine (e.ToString ());
					}
				}
				
				return header;
			}
		}
	}

	internal class MrmBlock : Block {
		Block [] blocks;

		public MrmBlock (System.IO.Stream stream) : base (stream) {}

		protected void Load ()
		{
			stream.Position = Start + 8;
			System.Collections.ArrayList list = new System.Collections.ArrayList ();
			
			while (stream.Position < Start + 8 + Length) {
				list.Add (Block.Create (stream));
			}
			blocks = (Block []) list.ToArray (typeof (Block));
		}

		public Block [] Blocks {
			get {
				if (blocks == null) {
					Load ();
				}

				return blocks;
			}
		}
		
	}
	
	public class MrwFile : ImageFile, SemWeb.StatementSource {
		MrmBlock mrm;
		FSpot.Tiff.Header header;

		public MrwFile (System.Uri uri) : base (uri)
		{
		}

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public MrwFile (string path) : base (path)
		{
		}

		public FSpot.Tiff.Header Header {
			get {
				if (mrm == null)
					LoadBlocks ();
				
				return header;
			}
		}

		public override System.DateTime Date
		{
			get {
				DirectoryEntry e = this.Header.Directory.Lookup (TagId.DateTime);
				
				if (e != null)
					return DirectoryEntry.DateTimeFromString (e.StringValue).ToUniversalTime ();
				else
					return base.Date;
			}
		}
		
		public void Select (SemWeb.StatementSink sink)
		{
			this.Header.Select (sink);
		}

		public override System.IO.Stream PixbufStream ()
		{
			return DCRawFile.RawPixbufStream (uri);
		}
		
#if false
		public override Gdk.Pixbuf Load ()
		{
			using (System.IO.Stream stream = Open ()) {
				return new Gdk.Pixbuf (PixbufStream ());
			}
		}

		public override Gdk.Pixbuf Load (int width, int height)
		{
			return PixbufUtils.ScaleToMaxSize (this.Load (), width, height);
		}
#endif
		protected void LoadBlocks () 
		{
			using (System.IO.Stream file = Open ()) {
				mrm = new MrmBlock (file);
				try {
					foreach (Block b in mrm.Blocks) {
						if (b is TtwBlock) {
							TtwBlock ttw = (TtwBlock) b;
							header = ttw.TiffHeader;
							//Header.Dump ("TTW:");
							break;
						}
					}
				} catch (System.Exception e) {
					//System.Console.WriteLine (e.ToString ());
				}
			}
		}
	}

}
