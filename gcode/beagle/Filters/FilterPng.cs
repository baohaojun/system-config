//
// FilterPng.cs
//
// Copyright (C) 2004 Novell, Inc.
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

using Beagle.Util;
using Beagle.Daemon;
using FSpot.Xmp;
using FSpot.Png;
using PngHeader = FSpot.Png.PngFile.PngHeader;

using SemWeb;

namespace Beagle.Filters {
	
	public class FilterPng : FilterImage {

		public FilterPng () : base ()
		{
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/png"));
		}

		protected override void PullImageProperties ()
		{
			PngHeader png = new PngHeader (Stream);
			
			foreach (PngFile.Chunk chunk in png.Chunks){
				if (chunk is PngFile.IhdrChunk) {
					PngFile.IhdrChunk ihdr = (PngFile.IhdrChunk)chunk;

					Width = (int)ihdr.Width;
					Height = (int)ihdr.Height;
					Depth = ihdr.Depth;

					bool hasAlpha = false;
					string colorType = null;
					
					switch (ihdr.Color) {
					case PngFile.ColorType.Gray:
						colorType = "Greyscale";
						hasAlpha = false;
						break;
					case PngFile.ColorType.Rgb:
						colorType = "Truecolor";
						hasAlpha = false;
						break;
					case PngFile.ColorType.Indexed:
						colorType = "Indexed";
						hasAlpha = false;
						break;
					case PngFile.ColorType.GrayAlpha:
						colorType = "Greyscale";
						hasAlpha = true;
						break;
					case PngFile.ColorType.RgbA:
						colorType = "Truecolor";
						hasAlpha = true;
						break;
					}

					AddProperty (Beagle.Property.NewUnsearched ("fixme:colortype", colorType));
					AddProperty (Beagle.Property.NewBool ("fixme:hasalpha", hasAlpha));
				} else if (chunk is PngFile.TextChunk) {
					ExtractTextProperty ((PngFile.TextChunk) chunk);
				}
			}

			Finished ();
		}

		private void ExtractTextProperty (PngFile.TextChunk tchunk)
		{
			switch (tchunk.Keyword) {
			case "Title":
				AddProperty (Beagle.Property.New ("dc:title", tchunk.Text));
				break;
			case "Author":
				AddProperty (Beagle.Property.New ("dc:creator", tchunk.Text));
				break;
			case "Copyright":
				AddProperty (Beagle.Property.New ("dc:rights", tchunk.Text));
				break;
			case "Description":
				AddProperty (Beagle.Property.New ("png:description", tchunk.Text));
				break;
			case "Comment":
				AddProperty (Beagle.Property.New ("png:comment", tchunk.Text));
				break;
			case "XMP":
			case "XML:com.adobe.xmp":
				XmpFile xmp = new XmpFile (new MemoryStream (tchunk.TextData));
				AddXmpProperties (xmp);
				break;
			case "Disclaimer":
			case "Warning":
			case "Source":
			case "Creation Time":
			case "Software":
				break;
			}
		}
	}
}
