//
// FilterTotem.cs
// based on FilterMPlayerVideo.cs
// Copyright (C) 2006 Alexander Macdonald <alex@alexmac.cc>
//
// Copyright (C) 2006 Bastien Nocera <hadess@hadess.net>

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
using System.Diagnostics;
using System.Globalization;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterTotem : Beagle.Daemon.Filter {

		static bool Debug = Beagle.Util.Debug.Enabled ("FilterTotem");

		public FilterTotem ()
		{			
			// 1: Priority update after FilterVideo was added
			SetVersion (1);

			PreLoad = false;
			SetFileType ("video");
		}

		internal static int Priority {
			get { return 1 + FilterMPlayerVideo.Priority; }
		}

		protected override void RegisterSupportedTypes ()
		{
			// Get the list of mime-types from the totem-video-indexer

			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "totem-video-indexer", "--mimetype" };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = true;
			pc.UseLangC = true;

			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				if (Debug)
					Log.Debug (e.Message);

				return;
			}

			StreamReader pout = new StreamReader (pc.StandardOutput);
			string str;

			while ((str = pout.ReadLine ()) != null) {
				FilterFlavor flavor = FilterFlavor.NewFromMimeType (str);
				flavor.Priority = Priority;

				AddSupportedFlavor (flavor);

				if (Debug)
					Log.Debug ("Added {0} as a supported mime type for Totem video filter", str);
			}

			pout.Close ();
			pc.Close ();
		}

		protected override void DoPullProperties ()
		{
			if (FileInfo == null) {
				Log.Error ("FilterTotem: Unable to extract properties for non-file data");
				Error ();
				return;
			}

			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "totem-video-indexer", FileInfo.FullName };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = true;

			// Let totem run for 10 seconds, max.
			pc.CpuLimit = 10;

			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				Error ();
				return;
			}

			StreamReader pout = new StreamReader (pc.StandardOutput);
			string str;

			while ((str = pout.ReadLine ()) != null) {
				if (!str.StartsWith ("TOTEM_INFO_"))
					continue;

				string[] tokens = str.Split ('=');

				if (tokens.Length != 2)
					continue;

				switch (tokens [0]) {
					case "":
						break;
					case "TOTEM_INFO_TITLE":
						AddProperty (Beagle.Property.New ("dc:title", tokens [1]));
						break;
					case "TOTEM_INFO_ARTIST":
						AddProperty (Beagle.Property.New ("fixme:artist", tokens [1]));
						break;
					case "TOTEM_INFO_YEAR":
						AddProperty (Beagle.Property.New ("fixme:year", tokens [1]));
						break;
					case "TOTEM_INFO_ALBUM":
						AddProperty (Beagle.Property.New ("fixme:album", tokens [1]));
						break;
					case "TOTEM_INFO_DURATION":
						//FIXME dc:extent or fixme:duration???
						AddProperty (Beagle.Property.NewUnsearched ("dc:extent", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_TRACK_NUMBER":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:tracknumber", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_HAS_VIDEO":
						break;
					case "TOTEM_INFO_VIDEO_WIDTH":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:video:width", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_VIDEO_HEIGHT":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:video:height", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_VIDEO_CODEC":
						AddProperty (Beagle.Property.NewKeyword ("fixme:video:codec", tokens [1]));
						break;
					case "TOTEM_INFO_FPS":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:video:fps", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_VIDEO_BITRATE":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:video:bitrate", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_HAS_AUDIO":
						break;
					case "TOTEM_INFO_AUDIO_BITRATE":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:audio:bitrate", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_AUDIO_CODEC":
						AddProperty (Beagle.Property.NewKeyword ("fixme:audio:codec", tokens [1]));
						break;
					case "TOTEM_INFO_AUDIO_SAMPLE_RATE":
						AddProperty (Beagle.Property.NewUnsearched ("fixme:audio:samplerate", Convert.ToInt32 (tokens [1])));
						break;
					case "TOTEM_INFO_AUDIO_CHANNELS":
						//FIXME this is very broken, needs fixing in Totem
						break;
					default:
						// Mismatching version of Totem with more information, possibly
						break;
				}
			}
			
			pout.Close ();
			pc.Close ();

			Finished ();
		}
	}
}

