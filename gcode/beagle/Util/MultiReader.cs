//
// MultiReader.cs
//
// Copyright (C) 2004 Novell, Inc.
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
using System.IO;
using System.Text;

namespace Beagle.Util {

	public class MultiReader : TextReader {

		Queue readers = new Queue ();
		
		public MultiReader ()
		{ }

		public void Add (TextReader reader)
		{
			readers.Enqueue (reader);
		}

		public int Count {
			get { return readers.Count; }
		}

		private TextReader Current {
			get { 
				if (readers.Count == 0)
					return null;
				return (TextReader) readers.Peek ();
			}
		}

		private void Next ()
		{
			if (readers.Count > 0)
				readers.Dequeue ();
		}

		public override void Close ()
		{
			while (Current != null) {
				Current.Close ();
				Next ();
			}
		}

		public override int Peek ()
		{
			while (Current != null) {
				int peek = Current.Peek ();
				if (peek != -1)
					return peek;
				Next ();
			}

			return -1;
		}

		public override int Read ()
		{
			while (Current != null) {
				int read = Current.Read ();
				if (read != -1)
					return read;
				Next ();
			}

			return -1;
		}

		public override int Read (char[] buffer, int index, int count)
		{
			int total = 0;
			while (Current != null && count > 0) {
				int actual = Current.Read (buffer, index + total, count);
				if (actual < count)
					Next ();
				count -= actual;
				total += actual;
			}
			return total;
		}

		public override int ReadBlock (char[] buffer, int index, int count)
		{
			return Read (buffer, index, count);
		}

		public override string ReadLine ()
		{
			while (Current != null) {
				string str = Current.ReadLine ();
				if (str != null)
					return str;
				Next ();
			}

			return null;
		}

		public override string ReadToEnd ()
		{
			StringBuilder all = new StringBuilder ("");
			
			while (Current != null) {
				string str = Current.ReadToEnd (); 
				if (str != null)
					all.Append (str);
				Next ();
			}

			return all.ToString ();
		}

		

	}
}
