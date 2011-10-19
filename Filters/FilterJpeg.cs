//
// FilterJpeg.cs
//
// Copyright (C) 2004 Novell, Inc.
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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
using System.Text;

using Beagle.Util;
using Exif;
using FSpot.Xmp;
using FSpot.Iptc;
using Tiff = FSpot.Tiff;
using Beagle.Daemon;

using SemWeb;

namespace Beagle.Filters {

	public class FilterJpeg : FilterImage {

		public FilterJpeg ()
		{
			// Store exif model as tokenized word
			// Store exif:Copyright as dc:rights
			SetVersion (1);
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/jpeg"));
		}

		private void AddJfifProperties (JpegHeader header)
		{
			string comment = header.GetJFIFComment ();
			AddProperty (Beagle.Property.New ("jfif:Comment", comment));
		}

		private static string GetExifString (Tiff.ImageDirectory directory, Tiff.TagId id)
		{
			if (directory == null)
				return null;

			Tiff.DirectoryEntry entry = directory.Lookup (id);
			if (entry == null)
				return null;

			return entry.ValueAsString [0];
		}

		private static double GetExifRational (Tiff.ImageDirectory directory, Tiff.TagId id)
		{
			if (directory == null)
				return 0;

			Tiff.DirectoryEntry entry = directory.Lookup (id);
			if (entry == null)
				return -1;

			return entry.RationalValue [0].Value;
		}

		static string GetFlashString (ushort flash_value)
		{
			switch (flash_value) {
			case 0x0000 : return "Flash did not fire";
			case 0x0001 : return "Flash fired";
			case 0x0005 : return "Strobe return light not detected";
			case 0x0007 : return "Strobe return light detected";
			case 0x0009 : return "Flash fired, compulsory flash mode";
			case 0x000D : return "Flash fired, compulsory flash mode, return light not detected";
			case 0x000F : return "Flash fired, compulsory flash mode, return light detected";
			case 0x0010 : return "Flash did not fire, compulsory flash mode";
			case 0x0018 : return "Flash did not fire, auto mode";
			case 0x0019 : return "Flash fired, auto mode";
			case 0x001D : return "Flash fired, auto mode, return light not detected";
			case 0x001F : return "Flash fired, auto mode, return light detected";
			case 0x0020 : return "No flash function";
			case 0x0041 : return "Flash fired, red-eye reduction mode";
			case 0x0045 : return "Flash fired, red-eye reduction mode, return light not detected";
			case 0x0047 : return "Flash fired, red-eye reduction mode, return light detected";
			case 0x0049 : return "Flash fired, compulsory flash mode, red-eye reduction mode";
			case 0x004D : return "Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected";
			case 0x004F : return "Flash fired, compulsory flash mode, red-eye reduction mode, return light detected";
			case 0x0059 : return "Flash fired, auto mode, red-eye reduction mode";
			case 0x005D : return "Flash fired, auto mode, return light not detected, red-eye reduction mode";
			case 0x005F : return "Flash fired, auto mode, return light detected, red-eye reduction mode";
			default     : return null;
			}
		}

		private void AddExifProperties (JpegHeader header)
		{
			Tiff.Header ifd = header.GetExifHeader ();
			if (ifd == null)
				return;
			//ifd.Dump ("foo"); // Uncomment to debug

			string str;
			Tiff.DirectoryEntry entry;

			// Add IFD 0 properies

			str = GetExifString (ifd.Directory, Tiff.TagId.Model);
			AddProperty (Beagle.Property.New ("exif:Model", str));

			str = GetExifString (ifd.Directory, Tiff.TagId.ImageDescription);
			AddProperty (Beagle.Property.New ("exif:ImageDescription", str));

			try {
				entry = ifd.Directory.Lookup (Tiff.TagId.DateTime);
				if (entry != null)
					// Assume datetime stored in the images are local times
					AddProperty (Beagle.Property.NewDate ("exif:DateTime", entry.ValueAsDate.ToUniversalTime ()));
			} catch (FormatException) {
				Logger.Log.Debug ("EXIF DateTime '{0}' is invalid.", GetExifString (ifd.Directory, Tiff.TagId.DateTime));
			} catch (ArgumentOutOfRangeException) {
				Logger.Log.Debug ("EXIF DateTime '{0}' is invalid.", GetExifString (ifd.Directory, Tiff.TagId.DateTime));
			}

			str = GetExifString (ifd.Directory, Tiff.TagId.Copyright);
			AddProperty (Beagle.Property.New ("dc:rights", str));

			// Add IFD 1 properties

			Tiff.SubdirectoryEntry subdir = (Tiff.SubdirectoryEntry) ifd.Directory.Lookup (Tiff.TagId.ExifIfdPointer);
			if (subdir == null)
				return;

			Tiff.ImageDirectory exif = subdir.Directory [0];
			if (exif == null)
				return;

			entry = exif.Lookup (Tiff.TagId.UserComment);
			if (entry != null)
				AddProperty (Beagle.Property.New ("exif:UserComment", entry.UserCommentValue));

			uint val = 0;
			entry = exif.Lookup (Tiff.TagId.PixelXDimension);
			if (entry != null)
				val = entry.ValueAsLong [0];
			if (val > 0) {
				Width = (int) val;
				AddProperty (Beagle.Property.NewUnsearched ("exif:PixelXDimension", val));
			}

			val = 0;
			entry = exif.Lookup (Tiff.TagId.PixelYDimension);
			if (entry != null)
				val = entry.ValueAsLong [0];
			if (val > 0) {
				Height = (int) val;
				AddProperty (Beagle.Property.NewUnsearched ("exif:PixelYDimension", val));
			}

			str = GetExifString (exif, Tiff.TagId.ISOSpeedRatings);
			AddProperty (Beagle.Property.NewUnsearched ("exif:ISOSpeedRatings", str));

			str = GetExifString (exif, Tiff.TagId.ShutterSpeedValue);
			AddProperty (Beagle.Property.NewUnsearched ("exif:ShutterSpeedValue", str));

			str = GetExifString (exif, Tiff.TagId.ExposureTime);
			if (! String.IsNullOrEmpty (str))
				AddProperty (Beagle.Property.NewUnsearched ("exif:ExposureTime", str + " sec."));

			double rational_val;

			rational_val = GetExifRational (exif, Tiff.TagId.FNumber);
			if (rational_val > 0)
				AddProperty (Beagle.Property.NewUnsearched ("exif:FNumber", String.Format ("f/{0}", rational_val)));

			rational_val = GetExifRational (exif, Tiff.TagId.ApertureValue);
			if (rational_val > 0)
				AddProperty (Beagle.Property.NewUnsearched ("exif:ApertureValue", rational_val));

			rational_val = GetExifRational (exif, Tiff.TagId.FocalLength);
			if (rational_val > 0)
				AddProperty (Beagle.Property.NewUnsearched ("exif:FocalLength", String.Format ("{0} mm", rational_val)));

			entry = exif.Lookup (Tiff.TagId.Flash);
			if (entry != null) {
				ushort flash = entry.ShortValue [0];
				AddProperty (Beagle.Property.NewUnsearched ("exif:Flash", GetFlashString (flash)));
			}
		}

		private void AddXmpProperties (JpegHeader header)
		{
			XmpFile xmp = header.GetXmp ();
			if (xmp != null)
				AddXmpProperties (xmp);
		}

		private void AddIptcProperties (JpegHeader header)
		{
			IptcFile iptc = header.GetIptc ();
			if (iptc == null)
				return;

			foreach (DataSet data in iptc.Sets) {
				switch (data.ID) {

				case DataSetID.ContentLocationName:
					AddProperty (Beagle.Property.New ("iptc:location", data.XmpObject));
					break;

				case DataSetID.CaptionAbstract:
					AddProperty (Beagle.Property.New ("iptc:caption", data.XmpObject));
					break;

				case DataSetID.Keywords:
					AddProperty (Beagle.Property.NewKeyword ("iptc:keyword", data.XmpObject));
					AddProperty (Beagle.Property.NewUnstored ("image:tag", data.XmpObject));
					break;

				default:
					// FIXME: Anything else to index ?
					//Log.Debug ("Ignoring {0} = [{1}]", data.ID, data.XmpObject);
					break;
				}
			}
		}

		// FIXME: This is not particularly efficient
		protected override void PullImageProperties ()
		{
			JpegHeader header = new JpegHeader (Stream);

			AddJfifProperties (header);
			AddExifProperties (header);
			AddXmpProperties (header);
			AddIptcProperties (header);

			Finished (); // That's all folks...
		}
	}
}
