//
// Bit Array.cs
//
// Authors:
// Ben Maurer (bmaurer@users.sourceforge.net)
// Modified by Jon Trowbridge (trow@novell.com)
//
// (C) 2003 Ben Maurer
//

//
// Copyright (C) 2004 Novell, Inc (http://www.novell.com)
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//

using System;
using System.Collections;

namespace Beagle.Util {
	[Serializable]
	public class BetterBitArray : ICollection, ICloneable {
		int [] _array;
		int _length;
		int _version = 0;

#region Constructors
		public BetterBitArray (BetterBitArray bits)
		{
			if (bits == null)
				throw new ArgumentNullException ("bits");

			_length = bits._length;
			_array = new int [(_length + 31) / 32];

			Array.Copy(bits._array, _array, _array.Length);
		}

		public BetterBitArray (bool [] values)
		{
			if (values == null)
				throw new ArgumentNullException ("values");
	    
			_length = values.Length;
			_array = new int [(_length + 31) / 32];
			
			for (int i = 0; i < values.Length; i++)
				this [i] = values [i];
		}

		public BetterBitArray (byte [] bytes)
		{
			if (bytes == null)
				throw new ArgumentNullException ("bytes");

			_length = bytes.Length * 8;
			_array = new int [(_length + 31) / 32];

			for (int i = 0; i < bytes.Length; i++)
				setByte (i, bytes [i]);
		}
		
		public BetterBitArray (int [] values)
		{
			if (values == null)
				throw new ArgumentNullException ("values");
						
			int arrlen = values.Length;
			_length = arrlen*32;
			_array = new int [arrlen];
			Array.Copy (values, _array, arrlen);
		}
		
		public BetterBitArray (int length)
		{
			if (length < 0)
				throw new ArgumentOutOfRangeException ("length");
			
			_length = length;
			_array = new int [(_length + 31) / 32];
			_contains_true = ContainsTrueState.No; // better
			_cached_true_count = 0; // better
		}

		public BetterBitArray (int length, bool defaultValue) : this (length)
		{
			if (defaultValue) {
				for (int i = 0; i < _array.Length; i++)
				_array[i] = ~0;
				_contains_true = ContainsTrueState.Yes; // better
				_cached_true_count = length; // better
			} else {
				_contains_true = ContainsTrueState.No;
				_cached_true_count = 0;
			}
		}
		
		private BetterBitArray (int [] array, int length)
		{
			_array = array;
			_length = length;
		}
#endregion
#region Utility Methods
		
		byte getByte (int byteIndex)
		{
			int index = byteIndex / 4;
			int shift = (byteIndex % 4) * 8;
			
			int theByte = _array [index] & (0xff << shift);
			
			return (byte)((theByte >> shift) & 0xff);
		}
		
		void setByte (int byteIndex, byte value)
		{
			int index = byteIndex / 4;
			int shift = (byteIndex % 4) * 8;
			
			// clear the byte
			_array [index] &= ~(0xff << shift);
			// or in the new byte
			_array [index] |= value << shift;
			
			_version++;
		}
		
		void checkOperand (BetterBitArray operand)
		{
			if (operand == null)
				throw new ArgumentNullException ();
			if (operand._length != _length)
				throw new ArgumentException ();
		}
#endregion

		public int Count {
			get { return _length; }
		}
		
		public bool IsReadOnly {
			get { return false; }
		}
		
		public bool IsSynchronized {
			get { return false; }
		}
		
		public bool this [int index] {
			get { return Get (index); }
			set { Set (index, value); }			
		}
		
		public int Length {
			get { return _length; }
			set {
				if (value < 0)
					throw new ArgumentOutOfRangeException ();
				
				int newLen = value;
				if (_length != newLen) {
					int numints = (newLen + 31) / 32;
					int [] newArr = new int [numints];
					int copylen = (numints > _array.Length) ? _array.Length : numints;
					Array.Copy (_array, newArr, copylen);
					
					// set the internal state
					_array = newArr;
					_length = newLen;
					_version++;
				}
			}
		}
		
		public object SyncRoot {
			get { return this; }
		}

		public object Clone ()
		{
			// LAMESPEC: docs say shallow, MS makes deep.
			return new BetterBitArray (this);
		}
		
		public void CopyTo (Array array, int index)
		{
			if (array == null)
				throw new ArgumentNullException ("array");
			if (index < 0)
				throw new ArgumentOutOfRangeException ("index");
			
			if (array.Rank != 1)
				throw new ArgumentException ("array", "Array rank must be 1");
			
			if (index >= array.Length)
				throw new ArgumentException ("index", "index is greater than array.Length");
			
			// in each case, check to make sure enough space in array
			
			if (array is bool []) {
				if (array.Length - index < _length)
					 throw new ArgumentException ();
				
				bool [] barray = (bool []) array;
				
				// Copy the bits into the array
				for (int i = 0; i < _length; i++)
					barray[index + i] = this [i];
				
			} else if (array is byte []) {
				int numbytes = (_length + 7) / 8;
				
				if ((array.Length - index) < numbytes)
					 throw new ArgumentException ();
				
				byte [] barray = (byte []) array;
				// Copy the bytes into the array
				for (int i = 0; i < numbytes; i++)
					barray [index + i] = getByte (i);
				
			} else if (array is int []) {
				
				Array.Copy (_array, 0, array, index, (_length + 31) / 32);
				
			} else {
				throw new ArgumentException ("array", "Unsupported type");
			}
		}

		public BetterBitArray Not ()
		{
			int ints = (_length + 31) / 32;
			for (int i = 0; i < ints; i++)
				_array [i] = ~_array [i];
			
			_version++;
			_contains_true = ContainsTrueState.Maybe; // better
			_cached_true_count = -1; // better
			return this;
		}
		
		public BetterBitArray And (BetterBitArray value)
		{
			checkOperand (value);
			
			int ints = (_length + 31) / 32;
			for (int i = 0; i < ints; i++)
				_array [i] &= value._array [i];
			
			_version++;
			_contains_true = ContainsTrueState.Maybe; // better
			_cached_true_count = -1; // better
			return this;
		}

		public BetterBitArray AndNot (BetterBitArray value)
		{
			checkOperand (value);
			
			int ints = (_length + 31) / 32;
			for (int i = 0; i < ints; i++)
				_array [i] &= ~value._array [i];
			
			_version++;
			_contains_true = ContainsTrueState.Maybe; // better
			_cached_true_count = -1; // better
			return this;
		}
		
		public BetterBitArray Or (BetterBitArray value)
		{
			checkOperand (value);

			int ints = (_length + 31) / 32;
			for (int i = 0; i < ints; i++)
				_array [i] |= value._array [i];
			
			_version++;
			if (_contains_true == ContainsTrueState.Yes
			    || value._contains_true == ContainsTrueState.Yes)
				_contains_true = ContainsTrueState.Yes;
			else
				_contains_true = ContainsTrueState.Maybe; // better
			_cached_true_count = -1; // better
			return this;
		}

		public BetterBitArray Xor (BetterBitArray value)
		{
			checkOperand (value);

			int ints = (_length + 31) / 32;
			for (int i = 0; i < ints; i++)
				_array [i] ^= value._array [i];

			_version++;
			_contains_true = ContainsTrueState.Maybe; // better
			_cached_true_count = -1; // better
			return this;
		}
		
		public bool Get (int index)
		{
			if (index < 0 || index >= _length)
				throw new ArgumentOutOfRangeException ();
			
			// better
			bool rv;
			rv = (_array [index / 32] & (1 << (index % 32))) != 0;
			if (rv)
				_contains_true = ContainsTrueState.Yes;
			return rv;
		}
		
		public void Set (int index, bool value)
		{
			if (index < 0 || index >= _length)
				throw new ArgumentOutOfRangeException ();

			// better
			if (_cached_true_count != -1) {
				bool old;
				old = (_array [index / 32] & (1 << (index % 32))) != 0;
				if (old)
					--_cached_true_count;
				if (value)
					++_cached_true_count;
			}
			
			if (value) {
				_array [index / 32] |=  (1 << (index % 32));
				_contains_true = ContainsTrueState.Yes; // better
			} else {
				_array [index / 32] &= ~(1 << (index % 32));
				if (_contains_true == ContainsTrueState.Yes) // better
					_contains_true = ContainsTrueState.Maybe;
			}
		
			_version++;
		}
		
		public void SetAll (bool value)
		{
			if (value) {
				for (int i = 0; i < _array.Length; i++)
					_array[i] = ~0;
				_contains_true = ContainsTrueState.Yes; // better
				_cached_true_count = _length;
			}
			else {
				Array.Clear (_array, 0, _array.Length);
				_contains_true = ContainsTrueState.No; // better
				_cached_true_count = 0; // better
			}

			_version++;
		}

		public IEnumerator GetEnumerator ()
		{
			return new BetterBitArrayEnumerator (this);
		}

		[Serializable]
		class BetterBitArrayEnumerator : IEnumerator, ICloneable {
			
			BetterBitArray _bitArray;
			bool _current;
			int _index, _version;
			
			public object Clone () {
				return MemberwiseClone ();
			}
			    
			public BetterBitArrayEnumerator (BetterBitArray ba)
			{
				_index = -1;
				_bitArray = ba;
				_version = ba._version;
			}

			public object Current {
				get {
					if (_index == -1)
						throw new InvalidOperationException ("Enum not started");
					if (_index >= _bitArray.Count)
						throw new InvalidOperationException ("Enum Ended");
					
					return _current;
				}
			}

			public bool MoveNext ()
			{
				checkVersion ();

				if (_index < (_bitArray.Count - 1)) {
					_current = _bitArray [++_index];
					return true;
				}
				else
					_index = _bitArray.Count;
				
				return false;
			}

			public void Reset ()
			{
				checkVersion ();
				_index = -1;
			}
			
			void checkVersion ()
			{
				if (_version != _bitArray._version)
					throw new InvalidOperationException ();
			}
		}

		////////////////////////////////////////////////////////////////////////////
		
		//
		// The "Better" parts are below
		//

		static private int [] true_count = new int [256];
		
		static BetterBitArray ()
		{
			for (int i = 0; i < 256; ++i) {
				int x = i;
				int count = 0;
				while (x != 0) {
					if ((x & 1) == 1)
						++count;
					x >>= 1;
				}
				true_count [i] = count;
			}
		}

		private enum ContainsTrueState {
			Yes,
			No,
			Maybe
		}

		// Just in case, start in an ambiguous state.
		private ContainsTrueState _contains_true = ContainsTrueState.Maybe;
		private int _cached_true_count = -1;

		// Returns the number of array elements that are set to true.
		public int TrueCount {
			get {
				if (_cached_true_count < 0) {
					_cached_true_count = 0;
					
					for (int i = 0; i < _array.Length; ++i) {
						int x = _array [i];
						byte b1 = (byte) (x & 0xff);
						byte b2 = (byte) ((x & (0xff << 8)) >> 8);
						byte b3 = (byte) ((x & (0xff << 16)) >> 16);
						byte b4 = (byte) ((x & (0xff << 24)) >> 24);
						_cached_true_count += true_count [b1];
						_cached_true_count += true_count [b2];
						_cached_true_count += true_count [b3];
						_cached_true_count += true_count [b4];
					}
					
					
					if (_cached_true_count == 0)
						_contains_true = ContainsTrueState.No;
					else
						_contains_true = ContainsTrueState.Yes;
				}
				
				int paranoid_check = 0;
				for (int i = 0; i < Count; ++i)
					if (Get (i))
						++paranoid_check;
				if (paranoid_check  != _cached_true_count)
					Logger.Log.Error ("TrueCount mismatch: {0} vs {1}", _cached_true_count, paranoid_check);
				
				return _cached_true_count;
			}
		}
		
		// Returns the index of the next 'true' value greater than or equal to
		// start.  If there is no such value, we return an index greater than
		// or equal to array.Count.
		// FIXME: It would be slightly nicer to provide some sort of enumerator
		// that returned the indexes of the bits that are set.
		public int GetNextTrueIndex (int start)
		{
			if (start >= _length || _contains_true == ContainsTrueState.No)
				return _length;
			else if (start < 0)
				start = 0;

			int i, offset;
			i = start / 32;
			offset = start % 32;

			while (i < _array.Length) {

				int array_value;
				array_value = _array [i];
				
				if (array_value != 0) {
					if (offset > 0)
						array_value = array_value >> offset;
					while (offset < 32) {
						if ((array_value & 1) != 0) {
							if (i * 32 + offset < _length)
								_contains_true = ContainsTrueState.Yes;
							else if (start == 0)
								_contains_true = ContainsTrueState.No;
							return i * 32 + offset;
						}
						++offset;
						array_value = array_value >> 1;
					}
				}
				
				++i;
				offset = 0;
			}
			
			if (start == 0)
				_contains_true = ContainsTrueState.No;

			// Failed
			return _length;
		}

		// A version of GetNextTrueIndex for walking
		// backwards across the array.
		public int GetPreviousTrueIndex (int start)
		{
			if (start < 0 || _contains_true == ContainsTrueState.No)
				return -1;
			else if (start >= _length)
				start = _length-1;

			int i, offset;
			i = start / 32;
			offset = start % 32;

			while (i >= 0) {

				int array_value;
				array_value = _array [i];
				
				if (array_value != 0) {
					int mask;
					mask = 1 << offset;
					while (offset >= 0) {
						if ((array_value & mask) != 0)
							return i * 32 + offset;
						--offset;
						mask >>= 1;
					}
				}

				--i;
				offset = 31;
			}

			// failed
			return -1;
		}

		public bool ContainsTrue ()
		{
			if (_contains_true == ContainsTrueState.Maybe) {
				if (GetNextTrueIndex (0) < _length)
					_contains_true = ContainsTrueState.Yes;
				else
					_contains_true = ContainsTrueState.No;
			}

			return _contains_true == ContainsTrueState.Yes;
		}
		
#if false
		static void Main ()
		{
			Random rng = new Random ();
			
			for (int trial = 0; trial < 10000; ++trial) {

				bool failed = false;
				
				BetterBitArray ba = new BetterBitArray (rng.Next (100000) + 1);
				
				Hashtable true_hash = new Hashtable ();
				int N = 1 + rng.Next (10000);
				for (int k = 0; k < N; ++k) {
					int j = rng.Next (ba.Count);
					ba [j] = true;
					true_hash [j] = true;
				}

				int i = 0;
				while (i < ba.Count) {
					i = ba.GetNextTrueIndex (i);
					if (i < ba.Count) {
						if (true_hash.Contains (i)) {
							true_hash.Remove (i);
						} else {
							Console.WriteLine ("Spurious true at {0}", i);
							failed = true;
						}
					}
					
					++i;
				}

				if (true_hash.Count > 0) {
					Console.WriteLine ("Missed some trues:");
					foreach (int k in true_hash.Values)
						Console.WriteLine ("  {0}", k);
					failed = true;
				}

				Console.WriteLine ("Trial #{0}: {1}", trial+1,
						   failed ? "FAILED" : "ok");
			}
		}
#endif

	}
}
