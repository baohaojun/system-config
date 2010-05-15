//
// FilterTiff.cs
//
// Copyright (C) 2006 Novell, Inc.
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
using Beagle.Daemon;
using FSpot.Tiff;
using FSpot.Xmp;

using SemWeb;

namespace Beagle.Filters {
	public class FilterTiff : FilterImage {
		public FilterTiff ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/tiff"));
		}

		protected override void PullImageProperties ()
		{
			DirectoryEntry e;
			Header header = new Header (Stream);

			e = header.Directory.Lookup (TagId.ExifIfdPointer);
			if (e != null) {
				ImageDirectory exif = ((SubdirectoryEntry)e).Directory [0];
				AddTiffDirectoryProperties (this, exif);
			}

			e = header.Directory.Lookup (TagId.SubIFDs);
			if (e != null) {
				ImageDirectory [] dirs = ((SubdirectoryEntry)e).Directory;
				 
				foreach (ImageDirectory subdir in dirs)
					AddTiffDirectoryProperties (this, subdir);
			}

			// we filter ifd0 last so that we pick up the
			// width and height from the full resolution
			// version if it exists 
			 
			AddTiffDirectoryProperties (this, header.Directory);
		}

		internal static void AddTiffDirectoryProperties (FilterTiff filter, ImageDirectory directory)
		{
			foreach (DirectoryEntry e in directory.Entries) {
				switch (e.Id) {
				case TagId.ImageDescription:
					filter.AddProperty (Beagle.Property.New ("exif:ImageDescription", e.ValueAsString [0]));
					break;
				case TagId.Model:
					filter.AddProperty (Beagle.Property.New ("exif:Model", e.ValueAsString [0]));
					break;
				case TagId.UserComment:
					filter.AddProperty (Beagle.Property.New ("exif:UserComment", e.ValueAsString [0]));
					break;
				case TagId.DateTimeOriginal:
					AddDateProperty (filter, "exif:DateTimeOriginal", e.ValueAsString [0]);
					break;
				case TagId.DateTimeDigitized:
					AddDateProperty (filter, "exif:DateTimeDigitized", e.ValueAsString [0]);
					break;
				case TagId.DateTime:
					AddDateProperty (filter, "exif:DateTime", e.ValueAsString [0]);
					break;
				case TagId.PixelXDimension:
					filter.Width = (int) e.ValueAsLong [0];
					//filter.AddProperty (Beagle.Property.NewUnsearched ("exif:PixelXDimension", e.ValueAsString [0]));
					break;
				case TagId.PixelYDimension:
					filter.Height = (int) e.ValueAsLong [0];
					//filter.AddProperty (Beagle.Property.NewUnsearched ("exif:PixelYDimension", e.ValueAsString [0]));
					break;
				case TagId.ImageWidth:
					uint wval = e.ValueAsLong [0];
					if (filter.Width < wval)
						filter.Width = (int) wval;
					break;
				case TagId.ImageLength:
					uint hval = e.ValueAsLong [0];
					if (filter.Height < hval)
						filter.Height = (int) hval;
					break;
				case TagId.ExposureTime:
					filter.AddProperty (Beagle.Property.NewUnsearched ("exif:ExposureTime", e.ValueAsString [0]));
					break;
				case TagId.ISOSpeedRatings:
					filter.AddProperty (Beagle.Property.NewUnsearched ("exif:ISOSpeedRatings", e.ValueAsString [0]));
					break;
				case TagId.FNumber:
					filter.AddProperty (Beagle.Property.NewUnsearched ("exif:FNumber", Math.Round ((e.RationalValue [0]).Value, 1)));
					break;
				case TagId.FocalLength:
					filter.AddProperty (Beagle.Property.NewUnsearched ("exif:FocalLength", e.ValueAsString [0]));
					break;
				case TagId.Flash:
					ushort flash_val = e.ShortValue [0];
					filter.AddProperty (Beagle.Property.NewBool ("exif:Flash", (flash_val & 0x1) == 0x1));
					break;
				case TagId.XMP:
					XmpFile xmp = new XmpFile (new System.IO.MemoryStream (e.RawData));
					filter.AddXmpProperties (xmp);
					break;
				}
			}
		}

		private static void AddDateProperty (FilterImage filter, string name, string value) {
			try {
				DateTime time = DirectoryEntry.DateTimeFromString (value);
				filter.AddProperty (Beagle.Property.NewDate (name, time));
			} catch (ArgumentOutOfRangeException) {
				Logger.Log.Debug ("{0} = '{1}' is invalid.", name, value);
			} catch (FormatException) {
				Logger.Log.Debug ("{0} = '{1}' is invalid.", name, value);
			}
		}
	}
}
