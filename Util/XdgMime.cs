//#define XDGMIME_DEBUG
//
// XdgMime.cs
//
// Copyright (C) 2006 Debajyoti Bera
//

//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//


using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace Beagle.Util {
	public class XdgMime {

		[DllImport ("libbeagleglue")]
		static extern IntPtr xdg_mime_get_mime_type_from_file_name (string file_name);

		[DllImport ("libbeagleglue")]
		static extern IntPtr xdg_mime_get_mime_type_for_data ([In] byte[] data, IntPtr len);

		[DllImport ("libbeagleglue")]
		static extern bool xdg_mime_mime_type_subclass (string subclass, string superclass);

		public static string GetMimeTypeFromFileName (string file_name)
		{
			return Marshal.PtrToStringAnsi (xdg_mime_get_mime_type_from_file_name (file_name));
		}

		private const string UNKNOWN_MIME_TYPE = "application/octet-stream";

		private static string GetMimeTypeFromXattr (string file_path)
		{
			try {
				// According the shared-mime-info spec, user may specify the
				// mimetype in user.mime_type xattr. This takes preference over
				// any guesses.
				return ExtendedAttribute.Get (file_path, "user.mime_type");
			} catch (IOException) {
				return null;
			}
		}

		public static string GetMimeType (string file_path)
		{
			string mime_type = GetMimeTypeFromXattr (file_path);
			if (mime_type != null)
				return mime_type;

#if XDGMIME_DEBUG
			Console.WriteLine ("From xattr: [{0}]", mime_type);
#endif

			string content_mime_type, extension_mime_type;

			FileStream fs = null;
			byte[] buf = null;
			int len = -1;

			try {
				fs = new FileStream (file_path, FileMode.Open, FileAccess.Read, FileShare.Read, 4096);

				buf = new byte [4096];
				len = fs.Read (buf, 0, 4096);

				fs.Close ();

				if (len == 0)
					content_mime_type = UNKNOWN_MIME_TYPE;
				else
					content_mime_type = Marshal.PtrToStringAnsi (xdg_mime_get_mime_type_for_data (buf, new IntPtr (len)));
				
			} catch (UnauthorizedAccessException) {
				content_mime_type = UNKNOWN_MIME_TYPE;
			} catch (System.Security.SecurityException) {
				content_mime_type = UNKNOWN_MIME_TYPE;
			} catch (FileNotFoundException) {
				content_mime_type = UNKNOWN_MIME_TYPE;
			} catch (IOException) {
				content_mime_type = UNKNOWN_MIME_TYPE;
			}

#if XDGMIME_DEBUG
			Console.WriteLine ("From content: [{0}]", content_mime_type);
#endif

			extension_mime_type = Marshal.PtrToStringAnsi (xdg_mime_get_mime_type_from_file_name (file_path));

#if XDGMIME_DEBUG
			Console.WriteLine ("From extension: [{0}]", extension_mime_type);
#endif

			if (content_mime_type == UNKNOWN_MIME_TYPE)
				mime_type = extension_mime_type;
			else {

				// Fix up some container MIME types that are
				// often wrong.
				switch (content_mime_type) {

				case "application/x-ole-storage": // MS Office
				case "application/x-bzip":
				case "application/x-gzip":
				case "application/zip":
				case "application/xml":
				case "text/xml":
				case "text/x-csrc": // JavaScript, C# others go to this

					if (extension_mime_type != UNKNOWN_MIME_TYPE)
						mime_type = extension_mime_type;
					else
						mime_type = content_mime_type;

					break;

				default:
					
#if XDGMIME_DEBUG
					Console.WriteLine ("extension mimetype subclass of content mimetype ? {0}", xdg_mime_mime_type_subclass (extension_mime_type, content_mime_type));
#endif

					if (xdg_mime_mime_type_subclass (extension_mime_type, content_mime_type))
						mime_type = extension_mime_type;
					else
						mime_type = content_mime_type;
					break;
				}
			}

			// If at the very end we're still application/octet-stream,
			// check the first handful of bytes to see if it's really
			// text.
			if (mime_type == UNKNOWN_MIME_TYPE
			    && buf != null
			    && len > 0
			    && ValidateUTF8 (buf, len))
				mime_type = "text/plain";

#if XDGMIME_DEBUG
			Console.WriteLine ("Detected: [{0}]", mime_type);
#endif

			return mime_type;
		}

		private static UTF8Encoding validating_encoding = new UTF8Encoding (true, true);

		private static bool ValidateUTF8 (byte[] byte_buf, int len)
		{
			int size = Math.Min (len, 256);

			char[] char_buf = new char [size];

			Decoder d = validating_encoding.GetDecoder ();

			try {
				d.GetChars (byte_buf, 0, size, char_buf, 0);

				// FIXME: UTF8 allows control characters in a file.
				// Should we allow control characters in a text file?
			} catch (ArgumentException) {
				return false;
			}

			return true;
		}

#if true
		public static void Main (string[] args)
		{
			if (args.Length != 1)
				Console.WriteLine ("Required full path of file or directory");
			else if (! File.Exists (args [0]) && ! Directory.Exists (args [0]))
				Console.WriteLine ("Invalid path to file or directory: {0}", args [0]);
			else
				Console.WriteLine ("Mimetype for {0} is {1}", args [0], GetMimeType (args [0]));
		}
#endif
	}
}
