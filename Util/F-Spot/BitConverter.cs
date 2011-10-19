using System;
using System.Runtime.InteropServices;

namespace FSpot {
	public class BitConverter {
		public static uint Swap (uint val, bool little) 
		{
			return (little != System.BitConverter.IsLittleEndian) ?
				((uint) ((((uint) (val) & (uint) 0x000000ffU) << 24) |
					 (((uint) (val) & (uint) 0x0000ff00U) <<  8) | 
					 (((uint) (val) & (uint) 0x00ff0000U) >>  8) |
					 (((uint) (val) & (uint) 0xff000000U) >> 24)))
				: val;
		}
		
		public static ushort Swap (ushort val, bool little)
		{
			return (little != System.BitConverter.IsLittleEndian) ?
				((ushort) ((ushort)(val >> 8) | (ushort)(val << 8)))
				: val;
		}

		public static ushort Swap (ushort val)
		{
			return ((ushort) ((ushort)(val >> 8) | (ushort)(val << 8)));
		}
		
		public static ulong Swap (ulong val, bool little)
		{
		        return (little != System.BitConverter.IsLittleEndian) ?
			((ulong) ((((ulong) (val) & (ulong) 0x00000000000000ffU) << 56) |     
				  (((ulong) (val) & (ulong) 0x000000000000ff00U) << 40) |	
				  (((ulong) (val) & (ulong) 0x0000000000ff0000U) << 24) |
				  (((ulong) (val) & (ulong) 0x00000000ff000000U) <<  8) |
				  (((ulong) (val) & (ulong) 0x000000ff00000000U) >>  8) |	
				  (((ulong) (val) & (ulong) 0x0000ff0000000000U) >> 24) |
				  (((ulong) (val) & (ulong) 0x00ff000000000000U) >> 40) |
				  (((ulong) (val) & (ulong) 0xff00000000000000U) >> 56)))
				: val;
		}
		
		public static byte [] GetBytes (uint val, bool little) 
		{
			val = Swap (val, little);
			return System.BitConverter.GetBytes (val);
		}
		
		public static byte [] GetBytes (ushort val, bool little)
		{
			val = Swap (val, little);
			return System.BitConverter.GetBytes (val);
		}

		public static byte [] GetBytes (ulong val, bool little)
		{
			val = Swap (val, little);
			return System.BitConverter.GetBytes (val);
		}
		
		public static ushort ToUInt16 (byte [] data, int position, bool little)
		{
			ushort val = System.BitConverter.ToUInt16 (data, position);
			return Swap (val, little);
		}

		public static uint ToUInt32 (byte [] data, int position, bool little)
		{
			uint val = System.BitConverter.ToUInt32 (data, position);
			return Swap (val, little);
		}

		public static float ToSingle (byte [] data, int position, bool little)
		{
			float retval;
			unsafe {
				uint * ptr;
				ptr = (uint *)&retval;
				*ptr = ToUInt32 (data, position, little);
			}
			return retval;
		}

		public static int ToInt32 (byte [] data, int position, bool little)
		{
			return unchecked ((int) ToUInt32 (data, position, little));
		}

		public static ulong ToUInt64 (byte [] data, int position, bool little)
		{
			ulong val = System.BitConverter.ToUInt64(data, position);
			return Swap (val, little);
		}
	}
}
