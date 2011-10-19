//
// OperaHistory.cs: An implementation of the format used by Opera to store web history
//
// Copyright (C) 2006 Pierre Östlund
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
using System.IO;
using System.Text;
using System.Collections;
using System.Globalization;

namespace Beagle.Daemon.OperaQueryable {
	
	public class OperaHistory {
		private ArrayList rows;
		private DateTime lastRead;
		// Details of the cache file format from
		// http://www.opera.com/docs/fileformats/
		public enum Directives : byte {
			RowStart		=	0x01,	// Row start (new entry)
			Address			=	0x03,	// Web address
			LastVisited		=	0x04,	// Last visited
			Length			=	0x08,	// Object length (e.g. image size)
			MimeType		=	0x09,	// Mime type
			Attributes		=	0x10,	// Attributes
			Encoding		=	0x0A,	// Encoding used
			Filename 		=	0x0D,	// Local filename used for this object
			LocalSaveTime		=	0x15,	// Time when an object was saved to the harddrive
			LastChanged		=	0x17,	// Time when the object was last modified on the server
			Compression		=	0x20	// Compression algorithm used (usually gzip)
		}
		
		public class Property {
			private byte directive;
			private byte[] content;
		
			public Property (byte directive, byte[] content)
			{
				this.directive = directive;
				this.content = content;
			}
			
			public byte Directive {
				get { return directive; }
			}
			
			public byte[] Content {
				get { return content; }
			}
		}
		
		public class Row {
			private ArrayList properties;
			private ArrayList attributes;
			private System.Text.Encoding encoding = System.Text.Encoding.Default;
			
			public Row ()
			{
				this.properties = new ArrayList ();
			}
			
			public void AddProperty (Property p)
			{
				if (p != null)
					properties.Add (p);
				
				if (p.Directive == (byte) Directives.Attributes)
					attributes = OperaHistory.ParseRow (p.Content).Properties;
			}
			
			public byte[] GetContent (Directives directive)
			{
				if (properties == null)
					return null;
				
				foreach (Property p in properties) {
					if (p.Directive == (byte) directive)
						return p.Content;
				}
				
				foreach (Property p in attributes) {
					if (p.Directive == (byte) directive)
						return p.Content;
				}
				
				return null;
			}
			
			public ArrayList Properties {
				get { return properties; }
			}
			
			public Uri Address {
				get {
					try {
						return new Uri (encoding.GetString (GetContent (Directives.Address)));
					} catch {
						return null;
					}
				}
			}
			
			public uint Length {
				get {
					return OperaHistory.GetUInt32 (GetContent (Directives.Length));
				}
			}
			
			public string LocalFileName {
				get {
					try {
						return encoding.GetString (GetContent (Directives.Filename));
					} catch {
						return String.Empty;
					}
				}
			}
			
			public DateTime LastVisited {
				get {
					try {
						byte[] content = GetContent (Directives.LastVisited);
						return Beagle.Util.DateTimeUtil.UnixToDateTimeUtc (GetUInt32 (content));
					} catch {
						return DateTime.MinValue;
					}
				}
			}
			
			public DateTime LocalSaveTime {
				get {
					try {
						byte[] content = GetContent (Directives.LocalSaveTime);
						return Beagle.Util.DateTimeUtil.UnixToDateTimeUtc (GetUInt32 (content));
					} catch {
						return DateTime.MinValue;
					}
				}
			}
			
			public DateTime LastChanged {
				get {
					try {
						byte[] content = GetContent (Directives.LastChanged);
						return Beagle.Util.DateTimeUtil.UnixToDateTimeUtc (GetUInt32 (content));
					} catch {
						return DateTime.MinValue;
					}
				}
			}
			
			public string MimeType {
				get {
					try {
						return encoding.GetString (GetContent (Directives.MimeType));
					} catch {
						return string.Empty;
					}
				}
			}
			
			public Encoding Encoding {
				get {
					try {
						byte[] content =GetContent (Directives.Encoding);
						return System.Text.Encoding.GetEncoding (encoding.GetString (content));
					} catch {
						return encoding;
					}
				}
			}
			
			public string Compression {
				get {
					try {
						return encoding.GetString (GetContent (Directives.Compression));
					} catch {
						return string.Empty;
					}
				}
			}
		}
		
		public OperaHistory (string filename)
		{
			this.rows = new ArrayList ();
			this.lastRead = DateTime.MinValue;

			Read (filename);
		}
		
		static uint filepos = 12;
		private void Read (string filename)
		{
			using (StreamReader stream = new StreamReader (filename)) {
				using (BinaryReader binary = new BinaryReader (stream.BaseStream)) {
					this.lastRead = DateTime.Now;
					// Skip first 12 bytes since their purpose is yet unknown
					binary.BaseStream.Seek (12, SeekOrigin.Begin);
					while (binary.ReadByte () == 1) {
						uint length = GetUInt32 (binary.ReadByte (), binary.ReadByte ());
						filepos += 3;

						byte[] line =  binary.ReadBytes ((int)length);
						if (line.Length < length)
							break; // EOF

						try {
							ReadLine (line);
						} catch (EndOfStreamException)  {
							break;
						} catch (IOException) {
							break;
						} catch(Exception e) { 
							Beagle.Util.Logger.Log.Error(e);
						}
						filepos += length;
					}
				}
			}
		}
		
		private void ReadLine (byte[] line)
		{
			Row r = ParseRow (line);
			if (r.Properties.Count > 0)
				rows.Add (r);
		}
		
		public static Row ParseRow (byte[] line)
		{
			int position = 0;
			Row row = new Row ();
			
			while (position <= line.Length) {
				Property prop = NewProperty (line, ref position);
				
				if (prop != null)
					row.AddProperty (prop);
			}
			
			return row;
		}
		
		public static Property NewProperty (byte[] line, ref int position)
		{
			if (position+3 > line.Length) {
				position++;
				return null;
			}
			
			// Tag_id values in which the MSB (Most Significant Bit) is set to 1,
			// are reserved for records with implicit no length.
			// The tag_id field is NOT followed by a length field, nor a payload buffer.
			// Such records are used as Boolean flags: True if present, False if not present. 
			if ((line [position] & (byte) 0x80) == (byte) 0x80) {
#if OPERA_DEBUG
				Console.WriteLine ("Ignoring flag record 0x{0:x} at {1} ({2})", line [position], position, filepos + position);
#endif
				position++;
				return null;
			}

			int start = position+1, length = 0, directive = position;

			// Read the two bytes that follows the directive byte and parse them as an integer.
			// This will be how far we will be reading in the stream
			byte[] length_bytes = new byte [2];
			Array.Copy (line, start, length_bytes, 0, 2);
			length = (int) GetUInt32 (length_bytes);
	
#if OPERA_DEBUG
			Console.WriteLine ("Adding record 0x{0:x2} at {1} of length {2} ({3})", line [position], position, length, filepos + position);
#endif

			// The content is what we really is after. This can be an address, object size or 
			// something else valuable.
			byte[] content = new byte [length];
			Array.Copy (line, start+2, content, 0, length);

			position += 3 + length;
			
			return new Property (line [directive], content);
		}

		public static uint GetUInt32 (params byte[] bytes)
		{
			if (bytes == null || bytes.Length > 4 || bytes.Length == 0)
				return 0;

			byte[] t = bytes;

			if (bytes.Length < 4) {
				t = new byte [4] {0x00, 0x00, 0x00, 0x00};
				Array.Copy (bytes, 0, t, 4 - bytes.Length, bytes.Length);
			}

			return FSpot.BitConverter.ToUInt32 (t, 0, false);
		}
		
		public IEnumerator GetEnumerator ()
		{
			return rows.GetEnumerator ();
		}
		
		public DateTime GetLastRead() 
		{
			return this.lastRead;
		}
#if OPERA_DEBUG
		public static void Main (string[] args)
		{
			if (args.Length != 1)
				return;
			OperaHistory op = new OperaHistory (args [0]);
			IEnumerator iter = op.GetEnumerator ();
			while (iter.MoveNext ()) {
				Row row = (Row) iter.Current;
				Uri uri = row.Address;
				Console.WriteLine (uri);
			}
		}
#endif
	}
}

