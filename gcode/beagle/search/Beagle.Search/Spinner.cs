//
// Spinner.cs
//
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>

using System;

using Gtk;
using Gdk;

namespace Beagle.Search {

	public class Spinner : Gtk.Image {

		private IconTheme theme;

        	private Pixbuf idle_pixbuf;
        	private Pixbuf[] frames;

        	private int current_frame;
        	private uint timeout_id;

		private const int target_size = 24;
		private const int refresh_rate = 125;

		public Spinner ()
		{
		}

		~Spinner ()
		{
			Stop ();
		}

		public void Start ()
		{
			if (!IsRealized)
				return;

			if (frames == null || frames.Length == 0)
				return;

			if (timeout_id > 0)
				return;

			timeout_id = GLib.Timeout.Add (refresh_rate, TimeoutHandler);
		}

		public void Stop ()
		{
			if (timeout_id == 0)
				return;

			GLib.Source.Remove (timeout_id);
			timeout_id = 0;
			
			Pixbuf = idle_pixbuf;
		}

        	private bool TimeoutHandler ()
        	{
        		Pixbuf = frames [current_frame];
			current_frame = (current_frame + 1) % frames.Length;

        		return true;
        	}

		protected override void OnRealized ()
		{
			base.OnRealized ();

			theme = Gtk.IconTheme.GetForScreen (Screen);
			theme.Changed += ThemeChanged;

			LoadImages ();
		}

		private void ThemeChanged (object o, EventArgs args)
		{
			LoadImages ();
		}

        	private void LoadImages ()
        	{
			int icon_size = target_size;

			// Find available spinner sizes and use the appropriate one

			foreach (int size in theme.GetIconSizes ("gnome-spinner-rest")) {
				if (size >= target_size) {
					icon_size = size;
					break;
				}
			}

			try {
				idle_pixbuf = theme.LoadIcon ("gnome-spinner-rest", icon_size, 0);
			} catch {
				Console.Error.WriteLine ("Could not load spinner image");
				frames = null;
				Pixbuf = null;
				return;
			}

			Gdk.Pixbuf frames_pixbuf = null;

			try {
				frames_pixbuf = theme.LoadIcon ("gnome-spinner", icon_size, 0);
			} catch {
				Console.Error.WriteLine ("Could not load spinner image");
				frames = null;
				Pixbuf = idle_pixbuf;
				return;
			}

			int frame_width = idle_pixbuf.Width, frame_height = idle_pixbuf.Height;
        		int width = frames_pixbuf.Width, height = frames_pixbuf.Height;

        		if (width % frame_width != 0 || height % frame_height != 0) {
				Console.Error.WriteLine ("Spinner images are wrong size");
				frames = null;
				Pixbuf = idle_pixbuf;
				return;
			}

			int rows = height / frame_height, cols = width / frame_width;

        		frames = new Pixbuf [rows * cols];

        		for (int y = 0, n = 0; y < rows; y++) {
        			for (int x = 0; x < cols; x++) {
        				frames [n++] = new Pixbuf (frames_pixbuf,
								   x * frame_width,
								   y * frame_height,
								   frame_width,
								   frame_height);
        			}
        		}

        		current_frame = 0;

			if (timeout_id != 0)
				Pixbuf = frames [current_frame];
			else
				Pixbuf = idle_pixbuf;
        	}
	}
}
