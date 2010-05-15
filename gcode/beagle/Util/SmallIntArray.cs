//
// Logger.cs
//
// Copyright (C) 2005 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Collections;

namespace Beagle.Util {

	public class SmallIntArray {

		private int length;
		private int bits_per_int;
		private int max_int;
		private BetterBitArray bit_array;

		public SmallIntArray (int length, int max_int)
		{
			this.length = length;
			this.max_int = max_int;

			this.bits_per_int = 0;
			while (max_int != 0) {
				++bits_per_int;
				max_int = max_int >> 1;
			}

			this.bit_array = new BetterBitArray (this.length * this.bits_per_int);
		}

		public int this [int index] {
			get { return Get (index); }
			set { Set (index, value); }
		}

		public int Length {
			get { return length; }
		}

		public int Count {
			get { return length; }
		}

		public int MaxInteger {
			get { return max_int; }
		}

		public int Get (int index)
		{
			int value, i, j;
			value = 0;
			i = index * bits_per_int;
			for (j = 0; j < bits_per_int; ++j) {
				if (j != 0)
					value <<= 1;
				if (bit_array.Get (i+j))
					value |= 1;
			}
			return value;
			
		}

		public void Set (int index, int value)
		{
			if (value < 0 || value > max_int)
				throw new ArgumentException ("invalid value");
			int i, j;
			i = (index + 1) * bits_per_int - 1;
			for (j = 0; j < bits_per_int; ++j) {
				bit_array.Set (i-j, (value & 1) == 1);
				value = value >> 1;
			}
		}

		public void SetAll (int value)
		{
			if (value == 0) {
				bit_array.SetAll (false);
				return;
			}

			// This is not particularly efficient
			for (int i = 0; i < length; ++i)
				Set (i, value);
		}

		public void Incr (BetterBitArray bit_array)
		{
			if (bit_array.Length != length)
				throw new Exception ("Incr BetterBitArray has wrong length!");

			int i = 0;
			while (i < length) {
				i = bit_array.GetNextTrueIndex (i);
				if (i >= length)
					break;
				Set (i, Get (i) + 1);
				++i;
			}
		}
		
		public bool ContainsNonZero ()
		{
			return bit_array.ContainsTrue ();
		}

		public int GetNextNonZeroIndex (int start)
		{
			if (start >= length)
				return length;
			if (start < 0)
				start = 0;
			return bit_array.GetNextTrueIndex (start * bits_per_int) / bits_per_int;
		}
	}
}
