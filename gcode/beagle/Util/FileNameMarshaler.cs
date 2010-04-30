//
// Mono.Unix/FileNameMarshaler.cs
//
// Authors:
//   Jonathan Pryor (jonpryor@vt.edu)
//
// (C) 2005 Jonathan Pryor
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
using System.Text;
using System.Runtime.InteropServices;
using Mono.Unix;

using Beagle.Util;

namespace Mono.Unix.Native {

	class FileNameMarshaler : ICustomMarshaler {

		private static bool local_is_utf8;
		private static Encoding platform_encoding;

		static FileNameMarshaler ()
		{
			if (Encoding.Default != Encoding.UTF8)
				local_is_utf8 = false;
			else
				local_is_utf8 = true;

			if (! local_is_utf8) {
				// We require that MONO_EXTERNAL_ENCODINGS be set !
				// UnixEncoding is nice but does not work for us since I see no way to
				// convert utf8 to unixencoding and vice versa
				string mono_ext_encoding = Environment.GetEnvironmentVariable ("MONO_EXTERNAL_ENCODINGS");
				local_is_utf8 = (mono_ext_encoding == null || String.Compare (mono_ext_encoding, "utf8", true) == 0);
			}

			if (local_is_utf8)
				platform_encoding = Encoding.UTF8;
			else
				platform_encoding = Encoding.Default;

			Log.Debug ("Using {0}utf8 encoding for filenames", local_is_utf8 ? "" : "non-");
		}

		public static bool LocalIsUTF8 {
			get { return local_is_utf8; }
		}

		private static FileNameMarshaler Instance = new FileNameMarshaler ();

		public static ICustomMarshaler GetInstance (string s)
		{
			return Instance;
		}

		public void CleanUpManagedData (object o)
		{
		}

		public void CleanUpNativeData (IntPtr pNativeData)
		{
			// Console.WriteLine ("# FileNameMarshaler.CleanUpManagedData ({0:x})", pNativeData);
			UnixMarshal.FreeHeap (pNativeData);
		}

		public int GetNativeDataSize ()
		{
			return IntPtr.Size;
		}

		public IntPtr MarshalManagedToNative (object obj)
		{
			string s = obj as string;
			if (s == null)
				return IntPtr.Zero;
			IntPtr p = UnixMarshal.StringToHeap (s, platform_encoding);
			// Console.WriteLine ("# FileNameMarshaler.MarshalNativeToManaged for `{0}'={1:x}", s, p);
			return p;
		}

		public object MarshalNativeToManaged (IntPtr pNativeData)
		{
			string s = UnixMarshal.PtrToString (pNativeData, platform_encoding);
			// Console.WriteLine ("# FileNameMarshaler.MarshalNativeToManaged ({0:x})=`{1}'",
			// 		pNativeData, s);
			return s;
		}

		public static string LocalToUTF8 (string local_filename)
		{
			if (LocalIsUTF8)
				return local_filename;

			byte[] bytes = Encoding.Default.GetBytes (local_filename);
			bytes = Encoding.Convert (Encoding.Default, Encoding.UTF8, bytes);
			return Encoding.UTF8.GetString (bytes);
		}

		public static string LocalToUTF8 (byte[] filename_bytes, int begin, int count)
		{
			if (LocalIsUTF8)
				return Encoding.UTF8.GetString (filename_bytes, begin, count);

			filename_bytes = Encoding.Convert (Encoding.Default, Encoding.UTF8, filename_bytes, begin, count);
			return Encoding.UTF8.GetString (filename_bytes);
		}

		public static string UTF8ToLocal (string utf8_filename)
		{
			if (LocalIsUTF8)
				return utf8_filename;

			byte[] bytes = Encoding.UTF8.GetBytes (utf8_filename);
			bytes = Encoding.Convert (Encoding.UTF8, Encoding.Default, bytes);
			return Encoding.Default.GetString (bytes);
		}
	}
}

// vim: noexpandtab
