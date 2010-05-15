using System.IO;

namespace FSpot {
	public class OrderedWriter {
		Stream stream;
		bool is_little;

		public Stream Stream {
			get { return stream; }
		}

		public OrderedWriter (Stream stream, bool is_little)
		{
			this.stream = stream;
			this.is_little = is_little;
		}

		public void Write (byte b)
		{
			stream.WriteByte (b);
		}

		public void Write (uint val)
		{
			byte [] value = FSpot.BitConverter.GetBytes (val, is_little);
			stream.Write (value, 0, value.Length);
		}

		public void Write (ushort val)
		{
			byte [] value = FSpot.BitConverter.GetBytes (val, is_little);
			stream.Write (value, 0, value.Length);
		}
	}
}
