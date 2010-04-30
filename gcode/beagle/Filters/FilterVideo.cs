//
//  FilterVideo.cs: TagLib# video filter
//
//  Copyright (C) 2007 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.IO;
using Beagle.Daemon;
using Beagle.Util;

using TagLib;

namespace Beagle.Filters {

	public class FilterVideo : Beagle.Daemon.Filter {
	
		public FilterVideo ()
		{
			SetFileType ("video");
		}

		protected override void RegisterSupportedTypes ()
		{
			foreach (string type in SupportedMimeType.AllMimeTypes) {
				if (! type.StartsWith ("video/"))
					continue;

				FilterFlavor flavor = FilterFlavor.NewFromMimeType (type);
				// Use priority = 1 + FilterTotem priority
				flavor.Priority = -1; // disable till the right properties are figured out

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
			TagLib.File file = null;
			
			try {
				file = TagLib.File.Create (new StreamAbstraction (Stream), GetTaglibMimeType (), TagLib.ReadStyle.Average);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Exception filtering video");
				Finished();
				return;
			}

			TagLib.Tag tag = file.Tag;

			// FIXME: Most likely most of these don't make sense
			// for video files.

			AddProperty (Beagle.Property.New ("dc:title", tag.Title));
			AddProperty (Beagle.Property.New ("fixme:album", tag.Album));

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
				TagLib.IVideoCodec vcodec = codec as TagLib.IVideoCodec;
				
				if (vcodec != null && (vcodec.MediaTypes & TagLib.MediaTypes.Video) != TagLib.MediaTypes.None) {
					AddProperty (Beagle.Property.NewUnsearched ("fixme:video:codec", vcodec.Description));
					AddProperty (Beagle.Property.NewUnsearched ("fixme:video:width", vcodec.VideoWidth));
					AddProperty (Beagle.Property.NewUnsearched ("fixme:video:height", vcodec.VideoHeight));

					// One codec is enough
					break;
				}
                	}

                	if (file.Properties.MediaTypes != TagLib.MediaTypes.None)
				AddProperty (Beagle.Property.NewUnsearched ("fixme:duration", file.Properties.Duration));

			Finished ();
		}
	}
}
