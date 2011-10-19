/*
 * Copyright (c) 2006 Novell Inc. 
 *
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

using System;
using System.IO;
using System.Collections;
using FSpot.Xmp;
using FSpot;

#if ENABLE_NUNIT
using NUnit.Framework;
#endif

public class JpegHeader : SemWeb.StatementSource {
	public enum JpegMarker {
		Tem = 0x01,
		Rst0 = 0xd0,  // RstN used for resync, ignore
		Rst1 = 0xd1,
		Rst2 = 0xd2,
		Rst3 = 0xd3,
		Rst4 = 0xd4,
		Rst5 = 0xd5,
		Rst6 = 0xd6,
		Rst7 = 0xd7,
		
		Sof0 = 0xc0, // SOFn Start of frame 0-1 common
		Sof1 = 0xc1,
		Sof2 = 0xc2,
		Sof3 = 0xc3,
		
		Dht = 0xc4,  // Define Huffman Table
		
		Sof5 = 0xc5,
		Sof6 = 0xc6,
		Sof7 = 0xc7,
		
		Jpg = 0xc8, // reserved
		
		Sof9 = 0xc9,
		Sof10 = 0xca,
		Sof11 = 0xcb,
		Sof12 = 0xcc,
		Sof13 = 0xcd,
		Sof14 = 0xce,
		Sof15 = 0xcf,

		// These tags all consist of a marker and then a length.
		
		// These are the major structure tags.
		Soi  = 0xd8,  // Start of Image
		Eoi  = 0xd9,  // End of Image
		Sos  = 0xda,  // Start of Scan
		
		Dnl = 0xdc,
		Dri = 0xdd,  // Define restart interval
		Dhp = 0xde,
		Exp = 0xdf, 
		
		Dqt = 0xdb, // Define Quantization Table
		
		// These are the app marker tags that contain the application metadata
		// in its various forms.
		App0 = 0xe0,  // AppN Markers for application data
		App1 = 0xe1,
		App2 = 0xe2,
		App3 = 0xe3,
		App4 = 0xe4,
		App5 = 0xe5,
		App6 = 0xe6,
		App7 = 0xe7,
		App8 = 0xe8,
		App9 = 0xe9,
		App10 = 0xea,
		App11 = 0xeb,
		App12 = 0xec,
		App13 = 0xed,
		App14 = 0xee,
		App15 = 0xef,
		
		Jpg0 = 0xf0,
		Jpg1 = 0xf1,
		Jpg2 = 0xf2,
		Jpg3 = 0xf3,
		Jpg4 = 0xf4,
		Jpg5 = 0xf5,
		Jpg6 = 0xf6,
		Jpg7 = 0xf7,
		Jpg8 = 0xf8,
		Jpg9 = 0xf9,
		Jpg10 = 0xfa,
		Jpg11 = 0xfb,
		Jpg12 = 0xfc,
		Jpg13 = 0xfd,
		
		Com = 0xfe // Comment
	}	

	private System.Collections.ArrayList marker_list = new System.Collections.ArrayList ();	
	private byte [] image_data;

                // False seems a safe default
                public bool Distinct {
                        get { return false; }
                }


	public class Marker {
		public JpegMarker Type;
		public byte [] Data;
		
		public Marker (JpegMarker type, byte [] data)
		{
			this.Type = type;
			this.Data = data;
		}

		public bool IsApp {
			get {
				return (this.Type >= JpegMarker.App0 && this.Type <= JpegMarker.App15);
			}
		}

		public bool Matches (Signature sig) 
		{
			if (Type == sig.Id) {
				if (sig.Name == null)
					return true;
				
				byte [] name = System.Text.Encoding.ASCII.GetBytes (sig.Name);

				for (int i = 0; i < name.Length; i++)
					if (Data [i] != name [i])
						return false;
				
				return true;
			}
			return false;
		}

		public string GetName ()
		{
			if (!this.IsApp)
				return null;
			
			int j;
			for (j = 0; j < this.Data.Length; j++) {
				if (this.Data [j] == 0x00)
					break;
					
			}
			
			if (j > 0)
				return System.Text.Encoding.ASCII.GetString (this.Data, 0, j);
			else 
				return null;
		}
		
		private static int Read (Stream stream, byte [] dest, int start, int len)
		{
			int pos = 0;

			while (pos < len) {
				int read = stream.Read (dest, pos + start, len - pos);
				if (read <= 0)
					break;

				pos += read;
			}
			return pos;
		}

		public static Marker Load (Stream stream) {
			byte [] raw = new byte [2];
			ushort length;
		       
			if (stream.Length - stream.Position < 2)
				return null;

			// FIXME there is a potential loop here.
			
			int read = Read (stream, raw, 0, 2);
			if (read < 2 || raw [0] != 0xff)
				throw new System.Exception (System.String.Format ("Invalid marker found {0}", raw [0]));
			
			JpegMarker id = (JpegMarker) raw [1];
			switch (id) {
			case JpegMarker.Soi:
			case JpegMarker.Eoi:
			case JpegMarker.Rst0:
			case JpegMarker.Rst1:
			case JpegMarker.Rst2:
			case JpegMarker.Rst3:
			case JpegMarker.Rst4:
			case JpegMarker.Rst5:
			case JpegMarker.Rst6:
			case JpegMarker.Rst7:
			case JpegMarker.Tem: 
			case (JpegMarker) 0:
				return new Marker (id, null);
			default:
				Read (stream, raw, 0, 2);
				length = FSpot.BitConverter.ToUInt16 (raw, 0, false);
				
				byte [] data = new byte [length - 2];
				Read (stream, data, 0, data.Length);
				return new Marker (id, data);
			}
			
		}

		public void Save (System.IO.Stream stream) {
			/* 
			 * It is possible we should just base this choice off the existance
			 * of this.Data, but I'm not sure so I'll do it this way for now
			 */
			
			switch (this.Type) {
			case JpegMarker.Soi:
			case JpegMarker.Eoi:
				stream.WriteByte (0xff);				
				stream.WriteByte ((byte)this.Type);
				break;
			default:
				stream.WriteByte (0xff);				
				stream.WriteByte ((byte)this.Type);
				ushort length = (ushort)(this.Data.Length + 2);
				
				byte [] len = FSpot.BitConverter.GetBytes (length, false);
				stream.Write (len, 0, len.Length);

				//workaround for mono bug: http://bugzilla.ximian.com/show_bug.cgi?id=82836
				if (this.Data.Length > 0)
					stream.Write (this.Data, 0, this.Data.Length);
				break;
			}
		}
	}

	public static Signature JfifSignature = new Signature (JpegMarker.App0, "JFIF\0");
	public static Signature ComSignature = new Signature (JpegMarker.Com, null);
	public static Signature JfxxSignature = new Signature (JpegMarker.App0, "JFXX\0");
	public static Signature XmpSignature = new Signature (JpegMarker.App1, "http://ns.adobe.com/xap/1.0/\0");
	public static Signature ExifSignature = new Signature (JpegMarker.App1, "Exif\0\0");
	public static Signature IccProfileSignature = new Signature (JpegMarker.App2, "ICC_PROFILE\0");
	public static Signature PhotoshopSignature = new Signature (JpegMarker.App13, "Photoshop 3.0\0");
	public static Signature QuantizationSignature = new Signature (JpegMarker.Dqt, null);

	public class Signature {
		public JpegMarker Id;
		public string Name;

		public Signature (JpegMarker marker, string name)
		{
			Id = marker;
			Name = name;
		}

		public int WriteName (Stream stream)
		{
			byte [] sig = System.Text.Encoding.ASCII.GetBytes (Name);
			stream.Write (sig, 0, sig.Length);
			return sig.Length;
		}
	}	

	public Marker FindMarker (Signature sig)
	{
		foreach (Marker m in Markers) {
			if (m.Matches (sig))
				return m;
		}

		return null;
	}


	public Marker FindMarker (JpegMarker id, string name)
	{
		return FindMarker (new Signature (id, name));
	}

#if false
	public Cms.Profile GetProfile ()
	{
		Marker m = FindMarker (IccProfileSignature);
		string name = IccProfileSignature.Name;
		try {
			if (m != null)
				return new Cms.Profile (m.Data, name.Length, m.Data.Length - name.Length); 
		} catch (System.Exception e) {
			//System.Console.WriteLine (e);
		}
		
		FSpot.Tiff.Header exif = GetExifHeader ();
		if (exif != null)
			return exif.Directory.GetProfile ();
		
		return null;
	}
#endif
	public FSpot.Tiff.Header GetExifHeader ()
	{
		string name = ExifSignature.Name;
		Marker marker = FindMarker (ExifSignature);

		if (marker == null)
			return null;
		
		using (System.IO.Stream exifstream = new System.IO.MemoryStream (marker.Data, name.Length, marker.Data.Length - name.Length, false)) {
			FSpot.Tiff.Header exif = new FSpot.Tiff.Header (exifstream);
			return exif;
		}
	}

	public string GetJFIFComment ()
	{
		string name = ComSignature.Name;
		Marker marker = FindMarker (ComSignature);

		if (marker == null)
			return null;

		if (marker.Data != null && marker.Data.Length != 0)
			return System.Text.Encoding.Default.GetString (marker.Data, 0, marker.Data.Length);

		return null;
	}

	public XmpFile GetXmp ()
	{
		string name = XmpSignature.Name;
		Marker marker = FindMarker (XmpSignature);
		if (marker != null) {
			int len = name.Length;
			//System.Console.WriteLine (System.Text.Encoding.ASCII.GetString (marker.Data, len, 
			//								marker.Data.Length - len));
			using (System.IO.Stream xmpstream = new System.IO.MemoryStream (marker.Data, len, 
											marker.Data.Length - len, false)) {
			
				XmpFile xmp = new XmpFile (xmpstream);					
				return xmp;
			}
		}
		return null;
	}

	public void Select (SemWeb.StatementSink sink)
	{
		FSpot.Tiff.Header exif = GetExifHeader ();
		if (exif != null)
			exif.Select (sink);
		
		XmpFile xmp = GetXmp ();
		if (xmp != null)
			xmp.Select (sink);
		
		string name = PhotoshopSignature.Name;
		JpegHeader.Marker marker = FindMarker (PhotoshopSignature);
		if (marker != null) {
			int len = name.Length;
			using (System.IO.Stream bimstream = new System.IO.MemoryStream (marker.Data, len, marker.Data.Length - len, false)) {
				FSpot.Bim.BimFile bim = new FSpot.Bim.BimFile (bimstream);
				bim.Select (sink);
			}
		}
	}

	public FSpot.Iptc.IptcFile GetIptc ()
	{
		string name = PhotoshopSignature.Name;
		JpegHeader.Marker marker = FindMarker (PhotoshopSignature);
		if (marker != null) {
			int len = name.Length;
			using (System.IO.Stream bimstream = new System.IO.MemoryStream (marker.Data, len,
											marker.Data.Length - len, false)) {

				FSpot.Bim.BimFile bim;
				try {
					bim = new FSpot.Bim.BimFile (bimstream);
				} catch {
					// Bim entry with marker "PHUT" is not handled by Bim.cs
					return null;
				}

				// FIXME: What about EntryType.XMP ?
				FSpot.Bim.Entry iptc_entry = bim.FindEntry (FSpot.Bim.EntryType.IPTCNAA);
				if (iptc_entry == null)
					return null;

				using (System.IO.Stream iptcstream = new System.IO.MemoryStream (iptc_entry.Data)) {
					FSpot.Iptc.IptcFile iptc = new FSpot.Iptc.IptcFile (iptcstream);
					return iptc;
				}
			}
		}
		return null;
	}

	public Exif.ExifData Exif {
		get {
			Marker m = FindMarker (ExifSignature);

			if (m  == null)
				return null;

			return new Exif.ExifData (m.Data);
		}
	}

	public void Replace (Signature sig, Marker data)
	{
		bool added = false;
		for (int i = 1; i < Markers.Count; i++) {
			Marker m = (Marker) Markers [i];
			
			if (m.Matches (sig)) {
				
				if (!added) {
					Markers [i] = data;
					added = true;
				} else
					Markers.RemoveAt (i--);
				
			} else if (!m.IsApp || m.Type > sig.Id) {
				if (!added) {
					Markers.Insert (i, data); 
					added = true;
				}
			}
		}

		if (!added)
			throw new System.Exception (String.Format ("unable to replace {0} marker", sig.Name));
	}

	public void SetExif (Exif.ExifData value)
	{
		// Console.WriteLine ("before save");
		byte [] raw_data = value.Save ();
		// Console.WriteLine ("saved");
		Marker exif = new Marker (ExifSignature.Id, raw_data);
		// Console.WriteLine ("new");
		Replace (ExifSignature, exif);
		// Console.WriteLine ("replaced");
	}	
	
	public void SetXmp (XmpFile xmp)
	{
		using (MemoryStream stream = new MemoryStream ()) {
			
			XmpSignature.WriteName (stream);
			xmp.Save (stream);
			
			Marker xmp_marker = new Marker (XmpSignature.Id, stream.ToArray ());
			Replace (XmpSignature, xmp_marker);
		}
	}
	
	public System.Collections.ArrayList Markers {
		get {
			return marker_list;
		}
	}

	public void Save (System.IO.Stream stream)
	{
		foreach (Marker marker in marker_list) {
			// System.Console.WriteLine ("saving marker {0} {1}", marker.Type, 
			//			  (marker.Data != null) ? marker.Data.Length .ToString (): "(null)");
			marker.Save (stream);
			if (marker.Type == JpegMarker.Sos)
				stream.Write (ImageData, 0, ImageData.Length);
		}
	}

	public JpegHeader (System.IO.Stream stream)
	{
		Load (stream, false);
	}

	public JpegHeader (System.IO.Stream stream, bool metadata_only)
	{
		try {
			Load (stream, metadata_only);
		} catch (System.Exception e) {
			Console.WriteLine ("Exeption while reading jpeg headers");
			Console.WriteLine(e);
		}
	}

	private void Load (System.IO.Stream stream, bool metadata_only) 
	{
		marker_list.Clear ();
		image_data = null;
		bool at_image = false;

		Marker marker = Marker.Load (stream);
		if (marker == null || marker.Type != JpegMarker.Soi)
			throw new System.Exception ("This doesn't appear to be a jpeg stream");
		
		this.Markers.Add (marker);
		while (!at_image) {
			marker = Marker.Load (stream);

			if (marker == null)
				break;

			// System.Console.WriteLine ("loaded marker {0} length {1}", marker.Type, marker.Data.Length);

			this.Markers.Add (marker);
			
			if (marker.Type == JpegMarker.Sos) {
				at_image = true;

				if (metadata_only) {
					// System.Console.WriteLine ("read = {0}", stream.Position);
					return;
				}
			}
		}

		long image_data_length = stream.Length - stream.Position;
		this.image_data = new byte [image_data_length];

		if (stream.Read (image_data, 0, (int)image_data_length) != image_data_length)
			throw new System.Exception ("truncated image data or something");
	}

	static int [] StandardLuminanceQuantization = new int [] {
	        16,  11,  12,  14,  12,  10,  16,  14,
		13,  14,  18,  17,  16,  19,  24,  40,
		26,  24,  22,  22,  24,  49,  35,  37,
		29,  40,  58,  51,  61,  60,  57,  51,
		56,  55,  64,  72,  92,  78,  64,  68,
		87,  69,  55,  56,  80, 109,  81,  87,
		95,  98, 103, 104, 103,  62,  77, 113,
		121, 112, 100, 120,  92, 101, 103,  99
	};

	static int [] StandardChrominanceQuantization = new int [] {
		17,  18,  18,  24,  21,  24,  47,  26,
		26,  47,  99,  66,  56,  66,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99,
		99,  99,  99,  99,  99,  99,  99,  99
	};
	
	/* 
	 * GuessQuality is taken from the jpegdump utility
	 * Copyright (c) 1992 Handmade Software, Inc.
	 * by Allan N. Hessenflow licenced as GPL with the authors
	 * permission.  Many Thanks.
	 */
	public int GuessQuality ()
	{
		Marker dqt = FindMarker (QuantizationSignature);
		int quality = 0;
		int position = 0;

		while (position < dqt.Data.Length) {
			int tableindex;
			int [] table = null;
			double cumsf = 0.0;
			double cumsf2 = 0.0;
			bool allones = true;
			int row, col;
			
			tableindex = dqt.Data [position ++];

			switch (tableindex & 0x0f) {
			case 0:
				table = StandardLuminanceQuantization;
				break;
			case 1:
				table = StandardChrominanceQuantization;
				break;
			default:
				table = null;
				break;
			}

			for (row=0; row<8; row++) {
				for (col=0; col<8; col++) {
					uint val;
					
					if ((tableindex >> 4) > 0) {
					        val = FSpot.BitConverter.ToUInt16 (dqt.Data, position, false);
						position += 2;
					} else
						val = (uint) dqt.Data [position ++];

					if (table != null) {
						double x;

						/* scaling factor in percent */
						x = 100.0 * (double)val / (double)table [row*8+col];
						cumsf += x;
						cumsf2 += x * x;

						/* separate check for all-ones table (Q 100) */
						if (val != 1) 
							allones = false;
					}
				}
			}

			if (table != null) {
				double local_quality;
				
				cumsf /= 64.0;	/* mean scale factor */
				cumsf2 /= 64.0;

				//double variance;
				//variance = cumsf2 - (cumsf * cumsf);

				if (allones) /* special case for all-ones table */
					local_quality = 100.0;
				else if (cumsf <= 100.0)
					local_quality = (200.0 - cumsf) / 2.0;
				else
					local_quality = 5000.0 / cumsf;
				
				quality = Math.Max (quality, (int)local_quality);
			}
		}
		return quality;
	}
	
	public byte [] ImageData {
		get {
			return image_data;
		}
	}

#if ENABLE_NUNIT
	[TestFixture]
	public class Tests {
		int quality =  75;

		public string CreateFile ()
		{
			Gdk.Pixbuf test = new Gdk.Pixbuf (null, "f-spot-32.png");
			string path = FSpot.ImageFile.TempPath ("joe.jpg");
			string desc = "\x00a9 Novell Inc.";
			PixbufOrientation orient = PixbufOrientation.TopRight;

			PixbufUtils.SaveJpeg (test, path, quality, new Exif.ExifData ());
			FSpot.JpegFile jimg = new FSpot.JpegFile (path);
			jimg.SetDescription (desc);
			jimg.SetOrientation (orient);
			jimg.SaveMetaData (path);
			
			return path;
		}

		[Test]
		public void Load ()
		{
			string path = CreateFile ();

			using (Stream stream = File.OpenRead (path)) {
				JpegHeader jhead = new JpegHeader (stream);

				Assert.AreEqual (((Marker)jhead.Markers [0]).Type, JpegMarker.Soi);
				Assert.AreEqual (((Marker)jhead.Markers [1]).GetName (), "JFIF");
				Assert.AreEqual (((Marker)jhead.Markers [1]).Type, JpegMarker.App0);
				Assert.AreEqual (((Marker)jhead.Markers [2]).GetName (), "Exif");
				Assert.AreEqual (((Marker)jhead.Markers [2]).Type, JpegMarker.App1);

				// NOTE the currently we don't store the Eoi as the last marker
				Assert.AreEqual (((Marker)jhead.Markers [jhead.Markers.Count -1]).Type, JpegMarker.Sos);

				// NOTE this is kind of sill but it might help
				Assert.IsTrue (Math.Abs (jhead.GuessQuality () - quality) <= 1);

				Assert.IsNotNull (jhead.GetExifHeader ());
			}

			File.Delete (path);
		}

		[Test]
		public void Save ()
		{
			string in_path = CreateFile ();
			string out_path = ImageFile.TempPath ("output.jpg");
			JpegHeader source;
			JpegHeader dest;

			using (Stream orig = File.OpenRead (in_path)) {
				source = new JpegHeader (orig);
				
				using (Stream output = File.OpenWrite (out_path)) {
					source.Save (output);
				}

				using (Stream result = File.OpenRead (out_path)) {
					dest = new JpegHeader (result);
					
					Assert.AreEqual (source.Markers.Count, dest.Markers.Count);
					Assert.AreEqual (source.GuessQuality (), dest.GuessQuality ());
					Assert.AreEqual (orig.Length, result.Length);
					for (int i = 0; i < source.Markers.Count; i++) {
						Marker d = (Marker) dest.Markers [i];
						Marker s = (Marker) source.Markers [i];

						Assert.AreEqual (d.Type, s.Type);
						Assert.AreEqual (d.GetName (), s.GetName ());

						if (d.Data != null) {
							Assert.AreEqual (d.Data.Length, s.Data.Length);
						
							for (int j = 0; j < d.Data.Length; j++) {
								Assert.AreEqual (d.Data [j], s.Data [j]);
							}
						} else {
							Assert.AreEqual (d.Data, s.Data);
						}
					}
				}
			}

			File.Delete (in_path);
			File.Delete (out_path);
		}
	}
#endif

#if false
	public static int Main (string [] args)
	{
		JpegHeader data = new JpegHeader (args [0]);
		byte [] value = data.GetRawXmp ();

		if (value != null) {
			string xml = System.Text.Encoding.UTF8.GetString (value, 29, value.Length - 29);
			//System.Console.WriteLine (xml);
		}
		
		value = data.GetRaw ("ICC_PROFILE");
		if (value != null) {
			System.IO.FileStream stream = new System.IO.FileStream ("profile.icc", System.IO.FileMode.Create);
			stream.Write (value, 12, value.Length - 12);
			stream.Close ();
		}

		value = data.GetRawExif ();
		
		
		//System.IO.Stream ostream = System.IO.File.Open ("/home/lewing/test.jpg", System.IO.FileMode.OpenOrCreate);
		//data.Save (ostream);
		//ostream.Position = 0;
		//data = new JpegHeader (ostream);

		return 0;
	}
#endif
}
