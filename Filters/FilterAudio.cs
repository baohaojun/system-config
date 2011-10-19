//
//  FilterAudio.cs: This is our interface to taglib-sharp's interface.
//
//  Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using Beagle.Daemon;
using Beagle.Util;
using TagLib;

namespace Beagle.Filters {

	public class FilterAudio : Beagle.Daemon.Filter {

		public FilterAudio ()
		{
			// 1: Added duration and bitrate property
			// 2. Use TagLib-Sharp for filtering. Also index lots of new properties provided by TagLib#
			SetVersion (2);
			SetFileType ("audio");
		}

		protected override void RegisterSupportedTypes ()
		{
			foreach (string type in SupportedMimeType.AllMimeTypes) {
				if (! type.StartsWith ("audio/"))
					continue;

				FilterFlavor flavor = FilterFlavor.NewFromMimeType (type);
				flavor.Priority = 1 + FilterTotem.Priority;
				AddSupportedFlavor (flavor);
			}
		}

		private string GetTaglibMimeType ()
		{
			if (Extension != null && Extension.Length > 0)
				return "taglib/" + Extension.Substring (1);
			else
				return MimeType;
		}

		// An IFileAbstraction to work on streams
		// Based on an example by Brian Kerrick Nickel <brian.nickel@gmail.com>
		internal class StreamAbstraction : TagLib.File.IFileAbstraction
		{
			private System.IO.Stream stream;
   
			public StreamAbstraction (System.IO.Stream stream)
			{
				this.stream = stream;
			}
			
			public string Name {
				get { return "[Stream]"; }
			}
			
			public System.IO.Stream ReadStream {
				get { return stream; }
			}
			
			public System.IO.Stream WriteStream {
				get { return stream; }
			}
			
			public void CloseStream (System.IO.Stream stream)
			{
			}
		}

		protected override void DoPullProperties ()
		{
			TagLib.File file;
			
			try {
				file = TagLib.File.Create (new StreamAbstraction (Stream), GetTaglibMimeType (), TagLib.ReadStyle.Average);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Exception filtering music");
				Finished();
				return;
			}

			TagLib.Tag tag = file.Tag;

			AddProperty (Beagle.Property.New ("fixme:album", tag.Album));
			AddProperty (Beagle.Property.New ("dc:title", tag.Title));

			foreach (string artist in tag.AlbumArtists)
				AddProperty (Beagle.Property.New ("fixme:artist", artist));

			foreach (string performer in tag.Performers)
				AddProperty (Beagle.Property.New ("fixme:performer", performer));

			foreach (string composer in tag.Composers)
				AddProperty (Beagle.Property.New ("fixme:composer", composer));

			foreach (string genre in tag.Genres)
				AddProperty (Beagle.Property.New ("fixme:genre", genre));

			AddProperty (Beagle.Property.New ("fixme:comment", tag.Comment));

			if (tag.Track > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:tracknumber", tag.Track));

			if (tag.TrackCount > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:trackcount", tag.TrackCount));

			if (tag.Disc > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:discnumber", tag.Disc));

			if (tag.DiscCount > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:disccount", tag.DiscCount));

			if (tag.Year > 0)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:year", tag.Year));

			foreach (TagLib.ICodec codec in file.Properties.Codecs) {
				TagLib.IAudioCodec acodec = codec as TagLib.IAudioCodec;
				
				if (acodec != null && (acodec.MediaTypes & TagLib.MediaTypes.Audio) != TagLib.MediaTypes.None)
				{
					AddProperty (Beagle.Property.NewUnsearched ("fixme:bitrate", acodec.AudioBitrate));
					AddProperty (Beagle.Property.NewUnsearched ("fixme:samplerate", acodec.AudioSampleRate));
					AddProperty (Beagle.Property.NewUnsearched ("fixme:channels", acodec.AudioChannels));
					// One codec is enough
					break;
				}
				// FIXME: Get data from IVideoCodec too
                	}

                	if (file.Properties.MediaTypes != TagLib.MediaTypes.None)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:duration", file.Properties.Duration));

			// FIXME: Store embedded picture and lyrics

			Finished ();
		}
	}
}
