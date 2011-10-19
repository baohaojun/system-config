//
// LineReader.cs
//
// Author:
//   D Bera (dbera.web@gmail.com)
//   Dietmar Maurer (dietmar@ximian.com)
//   Miguel de Icaza (miguel@ximian.com) 
//
// Copyright (C) 2006 D Bera (dbera.web@gmail.com)
// (C) Ximian, Inc.  http://www.ximian.com
// Copyright (C) 2004 Novell (http://www.novell.com)
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
using System.Text;
using System.Runtime.InteropServices;

// This file contains an interface that could be used to read lines from files,
// skip lines or seek to a certain position. Similar to the LineInputStreamReader in Java
// This file also contains 3 implementations
//  ( better '>' worse)
// For read: Generic > Reencoding >> Seekable
// For seek: Generic < Reencoding = Seekable
// For skip: Generic > Reencoding >> Seekable

namespace System.IO {

	// A linereader interface
	public interface LineReader {

		// Returns a position marker, which can be used to navigate the lines.
		// Some implementations may only allow moving in the forward direction.
		// Might be different from line number or file offset.
		// Should only be used for traversal.
		long Position {
			get;
			set;
		}

		// Reads and returns the next line, null if EOF
		string ReadLine ();

		// Reads the next line and returns a stringbuilder containing the line
		// The StringBuilder returned could be the same one used while reading,
		// so it should not be modified and its content might change when readline
		// is next called.
		// This is the most worst horriblest API I ever designed, for sake of speed
		// And thats why this should not be a public API.
		StringBuilder ReadLineAsStringBuilder ();

		// Skips the next line, return true if successful
		bool SkipLine ();

		// Skips required number of lines; returns actual number of lines skipped
		long SkipLines (long n);

		// Close the reader
		void Close ();
	}

	public abstract class BaseLineReader : LineReader {

		protected const int DefaultBufferSize = 1024;
		protected const int DefaultFileBufferSize = 4096;
		protected const int MinimumBufferSize = 128;

		protected Encoding encoding;
		protected Decoder decoder;
		protected Stream base_stream;
		protected StringBuilder line_builder;

		// The input buffer
		protected byte [] input_buffer;

		// The decoded buffer from the above input buffer
		protected char [] decoded_buffer;

		// The buffer size that we are using
		protected int buffer_size;

		// Current position in the decoded_buffer
		protected int decoded_pos;

		public abstract long Position {
			get;
			set;
		}

		// to_skip_line is true => Read one line but the line need not be stored
		protected abstract StringBuilder ReadOneLine (bool to_skip_line);

		public virtual string ReadLine ()
		{
			StringBuilder sb = ReadOneLine (false);
			return (sb == null ? null : sb.ToString (0, sb.Length));
		}

		// EULA: By using this method you agree to understand the ugliness and danger
		// of using a shared object without any protection.
		public virtual StringBuilder ReadLineAsStringBuilder ()
		{
			return ReadOneLine (false);
		}

		// Skips the next line, return true if successful
		public virtual bool SkipLine ()
		{
			return (ReadOneLine (true) != null);
		}

		// Skips required number of lines; returns actual number of lines skipped
		virtual public long SkipLines (long n)
		{
			long lines_skipped = 0;

			while (n > 0) {
				if (! SkipLine ())
					return lines_skipped;
				lines_skipped ++;
				n --;
			}

			return lines_skipped;
		}

		virtual protected void DiscardBufferedData ()
		{
			decoded_pos = 0;
			// Discard internal state of the decoder too.
			decoder = encoding.GetDecoder ();
		}

		// Close the reader
		virtual public void Close ()
		{
			if (base_stream != null)
				base_stream.Close ();

			input_buffer = null;
			decoded_buffer = null;
			line_builder.Length = 0;
			line_builder = null;
			encoding = null;
			decoder = null;
			base_stream = null;
		}

		protected BaseLineReader(string path, Encoding encoding, int buffer_size)
		{
			if (null == path)
				throw new ArgumentNullException("path");
			if (String.Empty == path)
				throw new ArgumentException("Empty path not allowed");
			if (path.IndexOfAny (Path.GetInvalidPathChars ()) != -1)
				throw new ArgumentException("path contains invalid characters");
			if (null == encoding)
				throw new ArgumentNullException ("encoding");
			if (buffer_size <= 0)
				throw new ArgumentOutOfRangeException ("buffer_size", "The minimum size of the buffer must be positive");

			string DirName = Path.GetDirectoryName(path);
			if (DirName != String.Empty && !Directory.Exists(DirName))
				throw new DirectoryNotFoundException ("Directory '" + DirName + "' not found.");
			if (!File.Exists(path))
				throw new FileNotFoundException("File not found.", path);

			base_stream = (Stream) File.OpenRead (path);
			if (!base_stream.CanRead)
				throw new ArgumentException ("Cannot read stream");

			if (buffer_size < MinimumBufferSize)
				buffer_size = MinimumBufferSize;

			input_buffer = new byte [buffer_size];
			this.buffer_size = buffer_size;
			this.encoding = encoding;
			decoder = encoding.GetDecoder ();

			decoded_buffer = new char [encoding.GetMaxCharCount (buffer_size)];
			decoded_pos = 0;
			line_builder = new StringBuilder ();
		}
	}

	// A generic linereader, instead of file offsets stores line numbers
	// Fast read, slower seek; seeking basically reads and skips lines
	public class GenericLineReader : BaseLineReader {

		long current_line_number;

		// Decoded bytes in decoded_buffer.
		int decoded_count;

		public GenericLineReader(string path)
			: this (path, Encoding.UTF8, DefaultFileBufferSize) { }

		public GenericLineReader(string path, Encoding encoding)
			: this (path, encoding, DefaultFileBufferSize) { }

		public GenericLineReader(string path, Encoding encoding, int buffer_size)
			: base (path, encoding, buffer_size)
		{
			decoded_count = 0;
			current_line_number = 0;
		}

		// the buffer is empty, fill it again
		private int ReadBuffer ()
		{
			decoded_pos = 0;
			int cbEncoded = 0;

			// keep looping until the decoder gives us some chars
			decoded_count = 0;
			int parse_start = 0;
			do	
			{
				cbEncoded = base_stream.Read (input_buffer, 0, buffer_size);
				
				if (cbEncoded == 0)
					return 0;

				decoded_count = decoder.GetChars (input_buffer, parse_start, cbEncoded, decoded_buffer, 0);
				parse_start = 0;
			} while (decoded_count == 0);

			return decoded_count;
		}

		bool foundCR;
		int FindNextEOL ()
		{
			char c = '\0';
			for (; decoded_pos < decoded_count; decoded_pos++) {
				c = decoded_buffer [decoded_pos];
				if (c == '\n') {
					decoded_pos++;
					int res = (foundCR) ? (decoded_pos - 2) : (decoded_pos - 1);
					if (res < 0)
						res = 0; // if a new buffer starts with a \n and there was a \r at
							// the end of the previous one, we get here.
					foundCR = false;
					return res;
				} else if (foundCR) {
					foundCR = false;
					return decoded_pos - 1;
				}

				foundCR = (c == '\r');
			}

			return -1;
		}

		protected override StringBuilder ReadOneLine (bool to_skip_line)
		{
			if (base_stream == null)
				throw new ObjectDisposedException ("GenericLineReader", "Cannot read from a closed GenericLineReader");

			if (decoded_pos >= decoded_count && ReadBuffer () == 0)
				return null;

			line_builder.Length = 0;

			int begin = decoded_pos;
			int end = FindNextEOL ();
			if (end < decoded_count && end >= begin) {
				if (! to_skip_line)
					line_builder.Append (decoded_buffer, begin, end - begin);
				current_line_number ++;
				return line_builder;
			}

			while (true) {
				if (foundCR) // don't include the trailing CR if present
					decoded_count--;

				if (! to_skip_line)
					line_builder.Append (decoded_buffer, begin, decoded_count - begin);

				if (ReadBuffer () == 0) {
					current_line_number ++;
					return line_builder;//.ToString (0, line_builder.Length);
				}

				begin = decoded_pos;
				end = FindNextEOL ();
				if (end < decoded_count && end >= begin) {
					if (! to_skip_line)
						line_builder.Append (decoded_buffer, begin, end - begin);

					current_line_number ++;
					return line_builder;//.ToString (0, line_builder.Length);
				}
			}
		}

		public override long Position {
			get { return current_line_number; }
			set {
				if (value < current_line_number)
					throw new ArgumentException ("Position cannot be decreased");
				SkipLines (value - current_line_number);
			}
		}
	}

	// Finds out the precise file offsets by re-encoding each line
	// This is definitely better than SeekableLineReader but I am not sure
	// if this works correctly for multibyte encoded files. It should work
	// in theory but I have not tested yet.
	// Slower reading, fast seeking
	// Takes little bit more that GenericLineReader
	public class ReencodingLineReader : BaseLineReader {

		// Decoded bytes in decoded_buffer.
		int decoded_count;

		long last_newline_offset;

		long seek_base;

		static int _0xA_length, _0xD_length, _0xD0xA_length;

		bool foundCR;

		public ReencodingLineReader(string path)
			: this (path, Encoding.UTF8, DefaultFileBufferSize) { }

		public ReencodingLineReader(string path, Encoding encoding)
			: this (path, encoding, DefaultFileBufferSize) { }

		public ReencodingLineReader(string path, Encoding encoding, int buffer_size)
			: base (path, encoding, buffer_size)
		{
			decoded_count = 0;
			last_newline_offset = 0;
			seek_base = 0;
			foundCR = false;

			char[] testing = new char [2];

			// test for \r
			testing [0] = '\r';
			_0xD_length = encoding.GetByteCount (testing, 0, 1);

			// test for \r\n
			testing [1] = '\n';
			_0xD0xA_length = encoding.GetByteCount (testing, 0, 2);

			// test for \n
			_0xA_length = encoding.GetByteCount (testing, 1, 1);
		}

		// the buffer is empty, fill it again
		private int ReadBuffer ()
		{
			decoded_pos = 0;
			int cbEncoded = 0;

			// keep looping until the decoder gives us some chars
			decoded_count = 0;
			int parse_start = 0;
			do	
			{
				cbEncoded = base_stream.Read (input_buffer, 0, buffer_size);
				
				if (cbEncoded == 0)
					return 0;

				decoded_count = decoder.GetChars (input_buffer, parse_start, cbEncoded, decoded_buffer, 0);
				parse_start = 0;
			} while (decoded_count == 0);

			//Console.WriteLine ("Read {0}", decoded_count);
			return decoded_count;
		}

		int FindNextEOL (out int num_bytes_newline)
		{
			char c = '\0';
			num_bytes_newline = 0;

			for (; decoded_pos < decoded_count; decoded_pos++) {
				c = decoded_buffer [decoded_pos];
				if (c == '\n') {
					decoded_pos++;
					num_bytes_newline = (foundCR ? _0xD0xA_length : _0xA_length);
					int res = (foundCR) ? (decoded_pos - 2) : (decoded_pos - 1);
					if (res < 0)
						res = 0; // if a new buffer starts with a \n and there was a \r at
							// the end of the previous one, we get here.
					foundCR = false;
					return res;
				} else if (foundCR) {
					foundCR = false;
					num_bytes_newline = _0xD_length;
					return decoded_pos - 1;
				}

				foundCR = (c == '\r');
			}

			return -1;
		}

		protected override StringBuilder ReadOneLine (bool to_skip_line)
		{
			if (base_stream == null)
				throw new ObjectDisposedException ("GenericLineReader", "Cannot read from a closed GenericLineReader");

			if (decoded_pos >= decoded_count && ReadBuffer () == 0)
				return null;

			line_builder.Length = 0;

			int begin = decoded_pos;
			int num_bytes_newline;
			int end = FindNextEOL (out num_bytes_newline);
			if (end < decoded_count && end >= begin) {
				if (! to_skip_line)
					line_builder.Append (decoded_buffer, begin, end - begin);

				last_newline_offset += encoding.GetByteCount (decoded_buffer, begin, end - begin);
				last_newline_offset += num_bytes_newline;
				return line_builder;
			}

			while (true) {
				if (foundCR) // don't include the trailing CR if present
					decoded_count--;

				if (! to_skip_line)
					line_builder.Append (decoded_buffer, begin, decoded_count - begin);

				last_newline_offset += encoding.GetByteCount (decoded_buffer, begin, decoded_count - begin);
				if (ReadBuffer () == 0) {
					return line_builder;//.ToString (0, line_builder.Length);
				}

				begin = decoded_pos;
				end = FindNextEOL (out num_bytes_newline);
				if (end < decoded_count && end >= begin) {
					if (! to_skip_line)
						line_builder.Append (decoded_buffer, begin, end - begin);

					last_newline_offset += encoding.GetByteCount (decoded_buffer, begin, end - begin);
					last_newline_offset += num_bytes_newline;
					return line_builder;//.ToString (0, line_builder.Length);
				}
			}
		}

		protected override void DiscardBufferedData ()
		{
			decoded_count = 0;
			last_newline_offset = 0;
			foundCR = false;
			base.DiscardBufferedData ();
		}

		public override long Position {
			get { return seek_base + last_newline_offset; }
			set {
				seek_base = value;
				DiscardBufferedData ();
				base_stream.Seek (value, SeekOrigin.Begin);
			}
		}
	}

	// Decodes each byte individually to find EOLs; the slowest of all line readers
	// Very slow read, faster seek
	// Takes about 3 times more time to read than GenericLineReader
	public class SeekableLineReader : BaseLineReader {
		// Number of bytes currently in buffer
		int num_bytes_read;

		// Position in buffer upto which decoding has been done
		int buffer_pos;

		// Number of blocks read
		long count_block;

		// Base value to count position when seeked
		long seek_base;

		bool foundCR;

		// Last position of non-newline character in buffer
		int last_good_pos;

		public SeekableLineReader(string path)
			: this (path, Encoding.UTF8, DefaultFileBufferSize) { }

		public SeekableLineReader(string path, Encoding encoding)
			: this (path, encoding, DefaultFileBufferSize) { }

		public SeekableLineReader(string path, Encoding encoding, int buffer_size)
			: base (path, encoding, buffer_size)
		{
			count_block = -1;
			foundCR = false;
			seek_base = 0;
			buffer_pos = 0;
			num_bytes_read = 0;
			last_good_pos = 0;
		}

		private int ReadBuffer ()
		{
			num_bytes_read = base_stream.Read (input_buffer, 0, buffer_size);
			count_block ++;
			last_good_pos = buffer_pos = decoded_pos = 0;
			return num_bytes_read;
		}

		protected override StringBuilder ReadOneLine (bool to_skip_line)
		{
			if (base_stream == null)
				throw new ObjectDisposedException ("GenericLineReader", "Cannot read from a closed GenericLineReader");

			line_builder.Length = 0;

			last_good_pos = decoded_pos;

			while (true) {
				// Buffer empty, refill
				if (buffer_pos >= num_bytes_read && ReadBuffer () == 0) {
					if (line_builder.Length == 0)
						return null;
					else {
						return line_builder;
					}
				}

				for (; buffer_pos < num_bytes_read; buffer_pos ++) {
					int num_decoded = decoder.GetChars (
					        input_buffer, buffer_pos, 1, decoded_buffer, decoded_pos);

					// Nothing was decoded
					if (num_decoded != 1)
					        continue;

					// We got a character, yippieee!
					char c = decoded_buffer [decoded_pos];

					if (c == '\n') {
						decoded_pos ++;
						// Skip the '\n' and also '\r' if present
						int end = decoded_pos - (foundCR ? 2 : 1);
						if (end < 0)
						    end = 0; // if a new buffer starts with a \n and there was a \r at
							     // the end of the previous one, we get here.
						foundCR = false;

						if (! to_skip_line)
							line_builder.Append (decoded_buffer, last_good_pos, end - last_good_pos); 
						buffer_pos ++;
						return line_builder;
					} else if (foundCR) {
						foundCR = (c == '\r');

						// Found non '\n' after previously seen '\r'
						// Previous '\r' could be in the previous block
						int end = (decoded_pos > 1 ? decoded_pos - 1 : 0);

						// Return whatever is seen
						if (! to_skip_line)
							line_builder.Append (decoded_buffer, last_good_pos, end - last_good_pos);
						buffer_pos ++;
						return line_builder;
					} else {
						foundCR = (c == '\r');
						decoded_pos ++;
					}
				}

				// newline not found in current buffer, need to read again
				// transfer current data to stringbuilder
				if (! to_skip_line)
					line_builder.Append (decoded_buffer, 0, decoded_pos - (foundCR ? 2 : 1) - last_good_pos);
			}
		}

		protected override void DiscardBufferedData ()
		{
			count_block = -1;
			buffer_pos = num_bytes_read = last_good_pos = 0;
			foundCR = false;
			base.DiscardBufferedData ();
		}

		public override long Position {
			get { return seek_base + (buffer_size * count_block) + buffer_pos; }
			set {
				seek_base = value;
				DiscardBufferedData ();
				base_stream.Seek (value, SeekOrigin.Begin);
			}
		}
	}
}
