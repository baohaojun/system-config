//
// FilterBmp.cs
//
// Copyright (C) 2006 Alexander Macdonald <alex@alexmac.cc>
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
using BitConverter = FSpot.BitConverter;

namespace Beagle.Filters
{
	public class FilterBMP : FilterImage
	{
		enum BitmapCompressionTypes {
			None,
			RunLength8Bit,
			RunLength4Bit,
			RGBBitmapWithMask
		};

		struct BitmapHeader {
			public ushort type;                          // Magic identifier
			public uint size;                            // File size in bytes
			//public ushort reserved1, reserved2;        // unused
			public uint offset;                          // Offset to image data, bytes
		};
		
		struct BitmapInfoHeader {
			public uint size;                            // Header size in bytes
			public int width,height;                     // width and height of image
			public ushort planes;                        // Number of colour planes
			public ushort bits;                          // Bits per pixel
			public BitmapCompressionTypes compression;   // Compression type
			public uint imagesize;                       // Image size in bytes
			public int xresolution,yresolution;          // Pixels per meter
			public uint ncolors;                         // Number of colours
			public uint importantcolors;                 // Important colours
		};

		public FilterBMP () : base ()
		{
			PreLoad = false;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/bmp"));
		}

		protected override void PullImageProperties ()
		{
			byte [] data = new byte [54];
			Stream.Read (data, 0, data.Length);
			
			// Read the Header
			// We're not using it for anything, so we might as well not parse it.
			/*
			BitmapHeader bmh;
			bmh.type        = BitConverter.ToUInt16 (data, 0, true);
			bmh.size        = BitConverter.ToUInt32 (data, 2, true);
			bmh.reserved1   = BitConverter.ToUInt16 (data, 6, true);
			bmh.reserved2   = BitConverter.ToUInt16 (data, 8, true);
			bmh.offset      = BitConverter.ToUInt32 (data, 10, true);
			*/
			
			/* Read the Info Header */
			BitmapInfoHeader bmih;
			bmih.size               = BitConverter.ToUInt32 (data, 14, true);
			bmih.width              = BitConverter.ToInt32  (data, 18, true);
			bmih.height             = BitConverter.ToInt32  (data, 22, true);
			bmih.planes             = BitConverter.ToUInt16 (data, 26, true);
			bmih.bits               = BitConverter.ToUInt16 (data, 28, true);
			bmih.compression        = (BitmapCompressionTypes) BitConverter.ToUInt32 (data, 30, true);
			bmih.imagesize          = BitConverter.ToUInt32 (data, 34, true);
			bmih.xresolution        = BitConverter.ToInt32  (data, 38, true);
			bmih.yresolution        = BitConverter.ToInt32  (data, 42, true);
			bmih.ncolors            = BitConverter.ToUInt32 (data, 46, true);
			bmih.importantcolors    = BitConverter.ToUInt32 (data, 50, true);

			Width = bmih.width;
			Height = bmih.height;
			Depth = bmih.bits;
			
			AddProperty (Beagle.Property.NewUnsearched ("exif:Planes", bmih.planes));
			
			switch	(bmih.compression) {
				case BitmapCompressionTypes.None:
					AddProperty (Beagle.Property.NewUnsearched ("exif:Compression", "none"));
					break;
				case BitmapCompressionTypes.RunLength8Bit:
					AddProperty (Beagle.Property.NewUnsearched ("exif:Compression", "8bit Runlength"));
					break;
				case BitmapCompressionTypes.RunLength4Bit:
					AddProperty (Beagle.Property.NewUnsearched ("exif:Compression", "4bit Runlength"));
					break;
				case BitmapCompressionTypes.RGBBitmapWithMask:
					AddProperty (Beagle.Property.NewUnsearched ("exif:Compression", "RGB bitmap with mask"));
					break;
				default:
					AddProperty (Beagle.Property.NewUnsearched ("exif:Compression", "unknown"));
					break;
			}
			
			AddProperty (Beagle.Property.NewUnsearched ("exif:XResolution",     bmih.xresolution));
			AddProperty (Beagle.Property.NewUnsearched ("exif:YResolution",     bmih.yresolution));
			AddProperty (Beagle.Property.NewUnsearched ("exif:NumberOfColors",  bmih.ncolors));
			AddProperty (Beagle.Property.NewUnsearched ("exif:ImportantColors", bmih.importantcolors));
		}
	}
}
