using System.Collections;
using System.Runtime.InteropServices;
using System;
using System.IO;
using FSpot;

/**
  1        2       3      4         5            6           7          8

888888  888888      88  88      8888888888  88                  88  8888888888
88          88      88  88      88  88      88  88          88  88      88  88
8888      8888    8888  8888    88          8888888888  8888888888          88
88          88      88  88
88          88  888888  888888

t-l     t-r     b-r     b-l     l-t         r-t         r-b             l-b

**/

public enum PixbufOrientation {
	TopLeft = 1,
	TopRight = 2,
	BottomRight = 3,
	BottomLeft = 4,
	LeftTop = 5,
	RightTop = 6,
	RightBottom = 7,
	LeftBottom = 8
}

public class PixbufUtils {
		
	static public PixbufOrientation Rotate270 (PixbufOrientation orientation)
	{
		PixbufOrientation [] rot = new PixbufOrientation [] {
			PixbufOrientation.LeftBottom, 
			PixbufOrientation.LeftTop,
			PixbufOrientation.RightTop,
			PixbufOrientation.RightBottom, 
			PixbufOrientation.BottomLeft,
			PixbufOrientation.TopLeft,
			PixbufOrientation.TopRight,
			PixbufOrientation.BottomRight
		};

		orientation = rot [((int)orientation) -1];
		return orientation;
	}

	static public PixbufOrientation Rotate90 (PixbufOrientation orientation)
	{
		orientation = Rotate270 (orientation);
		orientation = Rotate270 (orientation);
		orientation = Rotate270 (orientation);
		return orientation;
	}

#if false
	static Pixbuf error_pixbuf = null;
	public static Pixbuf ErrorPixbuf {
		get {
			if (error_pixbuf == null)
				error_pixbuf = GtkUtil.TryLoadIcon (FSpot.Global.IconTheme, "f-spot-question-mark", 256, (Gtk.IconLookupFlags)0);
			return error_pixbuf;
		}
	}
	public static Pixbuf LoadingPixbuf = PixbufUtils.LoadFromAssembly ("f-spot-loading.png");

	public static int GetSize (Pixbuf pixbuf)
	{
		return Math.Max (pixbuf.Width, pixbuf.Height);
	}

	public static double Fit (Pixbuf pixbuf,
				  int dest_width, int dest_height,
				  bool upscale_smaller,
				  out int fit_width, out int fit_height)
	{
		return Fit (pixbuf.Width, pixbuf.Height, 
			    dest_width, dest_height, 
			    upscale_smaller, 
			    out fit_width, out fit_height);
	}

	public static double Fit (int orig_width, int orig_height,
				  int dest_width, int dest_height,
				  bool upscale_smaller,
				  out int fit_width, out int fit_height)
	{
		if (orig_width == 0 || orig_height == 0) {
			fit_width = 0;
			fit_height = 0;
			return 0.0;
		}

		double scale = Math.Min (dest_width / (double)orig_width,
					 dest_height / (double)orig_height);
		
		if (scale > 1.0 && !upscale_smaller)
			scale = 1.0;

		fit_width = (int)(scale * orig_width);
		fit_height = (int)(scale * orig_height);
		
		return scale;
	}


	// FIXME: These should be in GTK#.  When my patch is committed, these LoadFrom* methods will
	// go away.

	public class AspectLoader {
		Gdk.PixbufLoader loader = new Gdk.PixbufLoader ();
		int max_width;
		int max_height;
		PixbufOrientation orientation;
		int orig_width;

		public AspectLoader (int max_width, int max_height) 
		{
			this.max_height = max_height;
			this.max_width = max_width;
			loader.SizePrepared += HandleSizePrepared;
		}

		private void HandleSizePrepared (object obj, SizePreparedArgs args)
		{
			switch (orientation) {
			case PixbufOrientation.LeftTop:
			case PixbufOrientation.LeftBottom:
			case PixbufOrientation.RightTop:
			case PixbufOrientation.RightBottom:	
				int tmp = max_width;
				max_width = max_height;
				max_height = tmp;
				break;
			default:
				break;
			}

			double scale = Math.Min (max_width / (double)args.Width,
						 max_height / (double)args.Height);

			
			int scale_width = (int)(scale * args.Width);
			int scale_height = (int)(scale * args.Height);

			if (scale < 1.0)
				loader.SetSize (scale_width, scale_height);
		}

		public Pixbuf Load (System.IO.Stream stream, PixbufOrientation orientation)
		{
			int count;
			byte [] data = new byte [8192];
			while (((count = stream.Read (data, 0, data.Length)) > 0) && loader.Write (data, (ulong)count))
				;
			
			loader.Close ();
			Pixbuf orig = loader.Pixbuf;
			Gdk.Pixbuf rotated = TransformOrientation (orig, orientation, true);
			
			if (orig != rotated) {
				CopyThumbnailOptions (orig, rotated);
				orig.Dispose ();
			}
			loader.Dispose ();
			return rotated;
		}
		
		public Pixbuf LoadFromFile (string path)
		{
			try {
				orientation = GetOrientation (path);
				using (FileStream fs = File.OpenRead (path)) {
					return Load (fs, orientation);
				}
			} catch (Exception) {
				System.Console.WriteLine ("Error loading photo {0}", path);
				return null;
			} 
		}
	}

	public static Pixbuf ShallowCopy (Pixbuf pixbuf)
	{
		Pixbuf result = new Pixbuf (pixbuf, 0, 0, pixbuf.Width, pixbuf.Height);
		CopyThumbnailOptions (pixbuf, result);
		return result;
	}

	public static Pixbuf ScaleToMaxSize (Pixbuf pixbuf, int width, int height)
	{
		return ScaleToMaxSize (pixbuf, width, height, true);
	}	

	public static Pixbuf ScaleToMaxSize (Pixbuf pixbuf, int width, int height, bool upscale)
	{
		double scale = Math.Min  (width / (double)pixbuf.Width, height / (double)pixbuf.Height);
		int scale_width = (int)(scale * pixbuf.Width);
		int scale_height = (int)(scale * pixbuf.Height);

		Gdk.Pixbuf result;
		if (upscale || (scale < 1.0))
			result = pixbuf.ScaleSimple (scale_width, scale_height, (scale_width > 20) ? Gdk.InterpType.Bilinear : Gdk.InterpType.Nearest);
		else
			result = pixbuf.Copy ();

		CopyThumbnailOptions (pixbuf, result);

		return result;
	}
		
	static public void GetSize (string path, out int width, out int height)
	{
		Gdk.PixbufLoader loader = new Gdk.PixbufLoader ();
		int orig_width = 0;
		int orig_height = 0;
		bool done = false;

		loader.SizePrepared += delegate (object obj, SizePreparedArgs args) {
			orig_width = args.Width;
			orig_height = args.Height;
			done = true;
		};
		
		using (Stream stream = File.OpenRead (path)) {
			byte [] data = new byte [4096];
			int count;

			while (((count = stream.Read (data, 0, data.Length)) > 0) && loader.Write (data, (ulong)count)) {
				if (done)
					break;
			}
		}
		
		width = orig_width;
		height = orig_height;
	}

	static public Pixbuf LoadAtMaxSize (string path, int max_width, int max_height)
	{
#if true
		PixbufUtils.AspectLoader loader = new AspectLoader (max_width, max_height);
		return loader.LoadFromFile (path);
#else
		int width, height;
		JpegUtils.GetSize (path, out width, out height);
		PixbufUtils.Fit (width, height, max_width, max_height, false, out width, out height);
		Gdk.Pixbuf image = JpegUtils.LoadScaled (path, width, height);
		
		return image;
#endif
	}

	static public Pixbuf LoadFromStream (System.IO.Stream input)
	{
		Gdk.PixbufLoader loader = new Gdk.PixbufLoader ();
		byte [] buffer = new byte [8192];
		int n;

		while ((n = input.Read (buffer, 0, 8192)) != 0)
			loader.Write (buffer, (ulong) n);
		
		loader.Close ();
		
		return loader.Pixbuf;
		
	}
	

	// 
	// FIXME this is actually not public api and we should do a verison check,
	// but frankly I'm irritated that it isn't public so I don't much care.
	//
	[DllImport("libgdk_pixbuf-2.0-0.dll")]
	static extern bool gdk_pixbuf_set_option(IntPtr raw, string key, string value);
	
	public static bool SetOption(Gdk.Pixbuf pixbuf, string key, string value)
	{
		
		if (value != null)
			return gdk_pixbuf_set_option(pixbuf.Handle, key, value);
		else
			return false;
	}
	
	public static void CopyThumbnailOptions (Gdk.Pixbuf src, Gdk.Pixbuf dest)
	{
		if (src != null && dest != null) {
			PixbufUtils.SetOption (dest, "tEXt::Thumb::URI", src.GetOption ("tEXt::Thumb::URI"));
			PixbufUtils.SetOption (dest, "tEXt::Thumb::MTime", src.GetOption ("tEXt::Thumb::MTime"));
		}
	}

	public static void Save (Gdk.Pixbuf pixbuf, System.IO.Stream stream, string type, string [] options, string [] values)
	{
		byte [] data;

		data = PixbufUtils.Save (pixbuf, type, options, values);
		stream.Write (data, 0, data.Length);
	}

	static string [] NullTerminateArray (string [] options)
	{
		string [] terminated_options = options;

		if (options != null && options [ options.Length - 1 ] != null) {
			terminated_options = new string [options.Length + 1];
			Array.Copy (options, terminated_options, options.Length);
		}
		
		return terminated_options;
	}

	[DllImport("libgdk_pixbuf-2.0-0.dll")]
	static extern bool gdk_pixbuf_save_to_bufferv (IntPtr raw, out IntPtr data, out IntPtr length, 
						       string type, 
						       string [] keys, string [] values, out IntPtr error);

					
	public static byte [] Save (Gdk.Pixbuf pixbuf, string type, string [] options, string [] values)
	{
		IntPtr error = IntPtr.Zero;
		IntPtr data;
		IntPtr length;

		bool success = gdk_pixbuf_save_to_bufferv (pixbuf.Handle, 
							   out data, 
							   out length, 
							   type,
							   NullTerminateArray (options),
							   NullTerminateArray (values),
							   out error);
		
		if (error != IntPtr.Zero) 
			throw new GLib.GException (error);

		if (!success)
			throw new ApplicationException ("Unknown error while saving file");

		byte [] content = new byte [(int)length];
		Marshal.Copy (data, content, 0, (int)length);

		GLib.Marshaller.Free (data);

		return content;
	}
	
	public static Pixbuf TagIconFromPixbuf (Pixbuf source)
	{
		return IconFromPixbuf (source, (int) Tag.IconSize.Large);
	}

	public static Pixbuf IconFromPixbuf (Pixbuf source, int size)
	{
		Pixbuf tmp = null;
		Pixbuf icon = null;

		if (source.Width > source.Height)
			source = tmp = new Pixbuf (source, (source.Width - source.Height) /2, 0, source.Height, source.Height);
		else if (source.Width < source.Height) 
			source = tmp = new Pixbuf (source, 0, (source.Height - source.Width) /2, source.Width, source.Width);

		if (source.Width == source.Height)
			icon = source.ScaleSimple (size, size, InterpType.Bilinear);
		else
			throw new Exception ("Bad logic leads to bad accidents");

		if (tmp != null)
			tmp.Dispose ();
		
		return icon;
	}
		
	static public Pixbuf LoadFromScreen (Gdk.Window win) {
		Gdk.Screen screen = win.Screen;
		Drawable d = screen.RootWindow;
		int monitor = screen.GetMonitorAtWindow (win);
		Gdk.Rectangle geom = screen.GetMonitorGeometry (monitor);
		
		//
		// We use the screen width and height because that reflects
		// the current resolution, the RootWindow can actually be different.
		//

		Pixbuf buf = new Pixbuf (Colorspace.Rgb, false, 8, geom.Width, geom.Height);
		
		return buf.GetFromDrawable (d,
					    d.Colormap, geom.X, geom.Y, 0, 0, 
					    geom.Width, geom.Height);
	}

	static public Pixbuf LoadFromScreen () {
		Screen screen = Display.Default.GetScreen (0);
		Drawable d = screen.RootWindow;
		int width = screen.Width;
		int height = screen.Height;
		
		//
		// We use the screen width and height because that reflects
		// the current resolution, the RootWindow can actually be different.
		//

		Pixbuf buf = new Pixbuf (Colorspace.Rgb, false, 8, width, height);
		
		return buf.GetFromDrawable (d,
					    d.Colormap, 0, 0, 0, 0, 
					    width, height);
	}

	static public Pixbuf LoadFromAssembly (string resource)
	{
		try {
			return new Pixbuf (System.Reflection.Assembly.GetEntryAssembly (), resource);
		} catch {
			return null;
		}
	}

	[DllImport ("libc")]
	static extern int rename (string oldpath, string newpath);

	public static void SaveAtomic (Gdk.Pixbuf src, string filename, string type, string [] keys, string [] values)
	{
			string tmpname = filename + ".tmp";
			src.Savev (tmpname, type, NullTerminateArray (keys), NullTerminateArray (values));
			if (rename (tmpname, filename) < 0)
				throw new Exception ("Error renaming file");
	}

	public static Gdk.Pixbuf ScaleToAspect (Gdk.Pixbuf orig, int width, int height)
	{
		Gdk.Rectangle pos;
		double scale = Fit (orig, width, height, false, out pos.Width, out pos.Height);
		pos.X = (width - pos.Width) / 2;
		pos.Y = (height - pos.Height) / 2;

		Pixbuf scaled = new Pixbuf (Colorspace.Rgb, false, 8, width, height);
		scaled.Fill (0x000000); 

		orig.Composite (scaled, pos.X, pos.Y, 
				pos.Width, pos.Height,
				pos.X, pos.Y, scale, scale,
				Gdk.InterpType.Bilinear,
				255);

		return scaled;
	}

	public static string Resize (string orig_path, int size, bool copy_meta)
	{
		string version_path = System.IO.Path.GetTempFileName ();
		Resize (orig_path, version_path, size, copy_meta);
		return version_path;
	}

	public static void Resize (string orig_path, string dest_path, int size, bool copy_meta)
	{
		Exif.ExifData exif_data;
		if (copy_meta)
			exif_data = new Exif.ExifData (orig_path);
		else 
			exif_data = new Exif.ExifData ();

		Gdk.Pixbuf image = PixbufUtils.LoadAtMaxSize (orig_path, size, size);

		PixbufUtils.SaveJpeg (image, dest_path, 95, exif_data);
		image.Dispose ();
	}
	

	public static Pixbuf Flatten (Pixbuf pixbuf)
	{
		if (!pixbuf.HasAlpha)
			return null;

		Pixbuf flattened = new Pixbuf (Colorspace.Rgb, false, 8, pixbuf.Width, pixbuf.Height);
		pixbuf.CompositeColor (flattened, 0, 0, 
				       pixbuf.Width, pixbuf.Height, 
				       0, 0, 1, 1, 
				       InterpType.Bilinear,
				       255, 0, 0, 2000, 0xffffff, 0xffffff);

		return flattened;
	}

	[StructLayout(LayoutKind.Sequential)]
	public unsafe struct FPixbufJpegMarker {
		public int type;
		public byte *data;
		public int length;
	}

	[DllImport ("libfspot")]
	static extern bool f_pixbuf_save_jpeg (IntPtr src, string path, int quality, FPixbufJpegMarker [] markers, int num_markers);

	public static void SaveJpeg (Pixbuf pixbuf, string path, int quality, Exif.ExifData exif_data)
	{
		Pixbuf temp = null;
		if (pixbuf.HasAlpha) {
			temp = Flatten (pixbuf);
			pixbuf = temp;
		}

		// The DCF spec says thumbnails should be 160x120 always
		Pixbuf thumbnail = ScaleToAspect (pixbuf, 160, 120);
		byte [] thumb_data = Save (thumbnail, "jpeg", null, null);
		exif_data.Data = thumb_data;
		thumbnail.Dispose ();

		// Most of the things we will set will be in the 0th ifd
		Exif.ExifContent content = exif_data.GetContents (Exif.Ifd.Zero);

		// reset the orientation tag the default is top/left
		content.GetEntry (Exif.Tag.Orientation).Reset ();

		// set the write time in the datetime tag
		content.GetEntry (Exif.Tag.DateTime).Reset ();

		// set the software tag
		content.GetEntry (Exif.Tag.Software).SetData (FSpot.Defines.PACKAGE + " version " + FSpot.Defines.VERSION);

		byte [] data = exif_data.Save ();
		FPixbufJpegMarker [] marker = new FPixbufJpegMarker [0];
		bool result = false;

		unsafe {
			if (data.Length > 0) {
				
				fixed (byte *p = data) {
					marker = new FPixbufJpegMarker [1];
					marker [0].type = 0xe1; // APP1 marker
					marker [0].data = p;
					marker [0].length = data.Length;
					
					result = f_pixbuf_save_jpeg (pixbuf.Handle, path, quality, marker, marker.Length);
				}					
			} else
				result = f_pixbuf_save_jpeg (pixbuf.Handle, path, quality, marker, marker.Length);
			
		}

		if (temp != null)
			temp.Dispose ();
		
		if (result == false)
			throw new System.Exception ("Error Saving File");
	}


	[DllImport ("libfspot")]
	static extern IntPtr f_pixbuf_unsharp_mask (IntPtr src, double radius, double amount, double threshold);

	public static Pixbuf UnsharpMask (Pixbuf src, double radius, double amount, double threshold)
	{
		IntPtr raw_ret = f_pixbuf_unsharp_mask (src.Handle, radius, amount, threshold);
 		Gdk.Pixbuf ret = (Gdk.Pixbuf) GLib.Object.GetObject(raw_ret, true);
		return ret;
	}	
	
	[DllImport ("libfspot")]
	static extern IntPtr f_pixbuf_blur (IntPtr src, double radius);

	public static Pixbuf Blur (Pixbuf src, double radius)
	{
		IntPtr raw_ret = f_pixbuf_blur (src.Handle, radius);
 		Gdk.Pixbuf ret = (Gdk.Pixbuf) GLib.Object.GetObject(raw_ret, true);
		return ret;
	}	

#if OLDREDEYE
	[DllImport ("libfspot")]
	static extern void f_pixbuf_remove_redeye (IntPtr src);

	public static Gdk.Pixbuf RemoveRedeye (Gdk.Pixbuf src, Gdk.Rectangle area)
#else
	public unsafe static Gdk.Pixbuf RemoveRedeye (Gdk.Pixbuf src, Gdk.Rectangle area)
	{
		return RemoveRedeye (src, area, -15);
	}

	public unsafe static Gdk.Pixbuf RemoveRedeye (Gdk.Pixbuf src, Gdk.Rectangle area, int threshold)
	//threshold, factors and comparisons borrowed from the gimp plugin 'redeye.c' by Robert Merkel
#endif
	{
		Gdk.Pixbuf copy = src.Copy ();
		Gdk.Pixbuf selection = new Gdk.Pixbuf (copy, area.X, area.Y, area.Width, area.Height);
#if OLREDEYE
		f_pixbuf_remove_redeye (selection.Handle);
		selection.Dispose ();
#else
		byte *spix = (byte *)selection.Pixels;
		int h = selection.Height;
		int w = selection.Width;
		int channels = src.NChannels;

		double RED_FACTOR = 0.5133333;
		double GREEN_FACTOR = 1;
		double BLUE_FACTOR = 0.1933333;

		for (int j = 0; j < h; j++) {
			byte *s = spix;
			for (int i = 0; i < w; i++) {
				int adjusted_red = (int)(s[0] * RED_FACTOR);
				int adjusted_green = (int)(s[1] * GREEN_FACTOR);
				int adjusted_blue = (int)(s[2] * BLUE_FACTOR);

				if (adjusted_red >= adjusted_green - threshold
				    && adjusted_red >= adjusted_blue - threshold)
					s[0] = (byte)(((double)(adjusted_green + adjusted_blue)) / (2.0 * RED_FACTOR));
				s += channels;
			}
			spix += selection.Rowstride;
		}

#endif
		return copy;
	}

	public static unsafe Pixbuf ColorAdjust (Pixbuf src, double brightness, double contrast,
					  double hue, double saturation, int src_color, int dest_color)
	{
		Pixbuf adjusted = new Pixbuf (Colorspace.Rgb, src.HasAlpha, 8, src.Width, src.Height);
		PixbufUtils.ColorAdjust (src, adjusted, brightness, contrast, hue, saturation, src_color, dest_color);
		return adjusted;
	}

	public static Cms.Format PixbufCmsFormat (Pixbuf buf)
	{
		return buf.HasAlpha ? Cms.Format.Rgba8Planar : Cms.Format.Rgb8;
	}

	public static unsafe void ColorAdjust (Pixbuf src, Pixbuf dest, 
					       double brightness, double contrast,
					       double hue, double saturation, 
					       int src_color, int dest_color)
	{
		if (src.Width != dest.Width || src.Height != dest.Height)
			throw new Exception ("Invalid Dimensions");

		//Cms.Profile eos10d = new Cms.Profile ("/home/lewing/ICCProfiles/EOS-10D-True-Color-Non-Linear.icm");
		Cms.Profile srgb = Cms.Profile.CreateStandardRgb ();

		Cms.Profile bchsw = Cms.Profile.CreateAbstract (256, 
								0.0, 
								brightness, contrast,
								hue, saturation, src_color, 
								dest_color);

		Cms.Profile [] list = new Cms.Profile [] { srgb, bchsw, srgb };
		Cms.Transform trans = new Cms.Transform (list, 
							 PixbufCmsFormat (src),
							 PixbufCmsFormat (dest),
							 Cms.Intent.Perceptual, 0x0100);

		ColorAdjust (src, dest, trans);

		trans.Dispose ();
		srgb.Dispose ();
		bchsw.Dispose ();
	}


	public static unsafe void ColorAdjust (Gdk.Pixbuf src, Gdk.Pixbuf dest, Cms.Transform trans)
	{
		int width = src.Width;
		byte * srcpix  = (byte *) src.Pixels;
		byte * destpix = (byte *) dest.Pixels;

		for (int row = 0; row < src.Height; row++) {
			trans.Apply ((IntPtr) (srcpix + row * src.Rowstride),
				     (IntPtr) (destpix + row * dest.Rowstride), 
				     (uint)width);
		}
		
	}

	public static unsafe bool IsGray (Gdk.Pixbuf pixbuf, int max_difference)
	{
		int chan = pixbuf.NChannels;

		byte *pix = (byte *)pixbuf.Pixels;
		int h = pixbuf.Height;
		int w = pixbuf.Width;
		int stride = pixbuf.Rowstride;

		for (int j = 0; j < h; j++) {
			byte *p = pix;
			for (int i = 0; i < w; i++) {
				if (Math.Abs (p[0] - p[1]) > max_difference || Math.Abs (p[0] - p [2]) > max_difference) {
					goto Found;
				}
				p += chan;
			}
			pix += stride;
		}

		return true;

	Found:
		return false;
	}

	public static unsafe void ReplaceColor (Gdk.Pixbuf src, Gdk.Pixbuf dest)
	{
		if (src.HasAlpha || !dest.HasAlpha || (src.Width != dest.Width) || (src.Height != dest.Height))
			throw new ApplicationException ("invalid pixbufs");

		byte *dpix = (byte *)dest.Pixels;
		byte *spix = (byte *)src.Pixels;
		int h = src.Height;
		int w = src.Width;
		for (int j = 0; j < h; j++) {
			byte *d = dpix;
			byte *s = spix;
			for (int i = 0; i < w; i++) {
				d[0] = s[0];
				d[1] = s[1];
				d[2] = s[2];
				d += 4;
				s += 3;
			}
			dpix += dest.Rowstride;
			spix += src.Rowstride;
		}
	}

	public static Gdk.Pixbuf GetThumbnail (Exif.ExifData data)
	{
		byte [] thumb_data = data.Data;
		if (thumb_data.Length > 0) {
			PixbufOrientation orientation = GetOrientation (data);
			
			using (MemoryStream mem = new MemoryStream (thumb_data)) {
				Gdk.Pixbuf thumb = new Gdk.Pixbuf (mem);

				Gdk.Pixbuf rotated = PixbufUtils.TransformOrientation (thumb, orientation);
				
				if (rotated != thumb)
					thumb.Dispose ();
				return rotated;
			}			
		}
		return null;
	}

	public static PixbufOrientation GetOrientation (Exif.ExifData data)
	{
		PixbufOrientation orientation = PixbufOrientation.TopLeft;
		
		Exif.ExifEntry e = data.GetContents (Exif.Ifd.Zero).Lookup (Exif.Tag.Orientation);

		if (e != null) {
			ushort [] value = e.GetDataUShort ();
			orientation = (PixbufOrientation) value [0];
		}

		return orientation;
	}
	
	public static PixbufOrientation GetOrientation (string path)
	{
		using (FSpot.ImageFile img = FSpot.ImageFile.Create (path)) {
			return img.Orientation;
		}
	}

	[DllImport("libgnomeui-2-0.dll")]
	static extern IntPtr gnome_thumbnail_scale_down_pixbuf(IntPtr pixbuf, int dest_width, int dest_height);

	public static Gdk.Pixbuf ScaleDown (Gdk.Pixbuf src, int width, int height)
	{
		IntPtr raw_ret = gnome_thumbnail_scale_down_pixbuf(src.Handle, width, height);
		Gdk.Pixbuf ret;
		if (raw_ret == IntPtr.Zero)
			ret = null;
		else
			ret = (Gdk.Pixbuf) GLib.Object.GetObject(raw_ret, true);
		return ret;
	}

	public static Gdk.Pixbuf TransformOrientation (Gdk.Pixbuf src, PixbufOrientation orientation, bool copy_data)
	{
		Gdk.Pixbuf pixbuf;
		if (src == null)
			return null;
		
		switch (orientation) {
		case PixbufOrientation.LeftTop:
		case PixbufOrientation.LeftBottom:
		case PixbufOrientation.RightTop:
		case PixbufOrientation.RightBottom:	
			pixbuf = new Gdk.Pixbuf (src.Colorspace, src.HasAlpha, 
						 src.BitsPerSample,
						 src.Height, src.Width);
			break;
		case PixbufOrientation.TopRight:
		case PixbufOrientation.BottomRight:
		case PixbufOrientation.BottomLeft:
			pixbuf = new Gdk.Pixbuf (src.Colorspace, src.HasAlpha, 
						 src.BitsPerSample,
						 src.Width, src.Height);
			break;
		default:
			pixbuf = src;
			break;
		}

		if (copy_data && src != pixbuf) 
			TransformAndCopy (src, pixbuf, orientation, new Gdk.Rectangle (0, 0, src.Width, src.Height));

		return pixbuf;
	}

	public static Gdk.Pixbuf TransformOrientation (Gdk.Pixbuf src, PixbufOrientation orientation)
	{
		return TransformOrientation (src, orientation, true);
	}

	public static Gdk.Rectangle TransformOrientation (Gdk.Pixbuf src, Gdk.Rectangle args, PixbufOrientation orientation)
	{
		return TransformOrientation (src.Width, src.Height, args, orientation);
	}
	
	public static Gdk.Rectangle TransformOrientation (int total_width, int total_height, Gdk.Rectangle args, PixbufOrientation orientation)
	{
		Gdk.Rectangle area = args;
		
		switch (orientation) {
		case PixbufOrientation.BottomRight:
			area.X = total_width - args.X - args.Width;
			area.Y = total_height - args.Y - args.Height;
			break;
		case PixbufOrientation.TopRight:
			area.X = total_width - args.X - args.Width;
			break;
		case PixbufOrientation.BottomLeft:
			area.Y = total_height - args.Y - args.Height;
			break;
		case PixbufOrientation.LeftTop:
			area.X = args.Y;
			area.Y = args.X;
			area.Width = args.Height;
			area.Height = args.Width;
			break;
		case PixbufOrientation.RightBottom:
			area.X = total_height - args.Y - args.Height;
			area.Y = total_width - args.X - args.Width;
			area.Width = args.Height;
			area.Height = args.Width;
			break;
		case PixbufOrientation.RightTop:
			area.X = total_height - args.Y - args.Height;
			area.Y = args.X;
			area.Width = args.Height;
			area.Height = args.Width;
			break;
		case PixbufOrientation.LeftBottom:
			area.X = args.Y;
			area.Y = total_width - args.X - args.Width;
			area.Width = args.Height;
			area.Height = args.Width;
			break;
		default:
			break;
		}
		
		return area;
	}
	
	public static Gdk.Rectangle TransformAndCopy (Gdk.Pixbuf src, Gdk.Pixbuf dest, PixbufOrientation orientation, Gdk.Rectangle args)
	{
		Gdk.Rectangle area = TransformOrientation (src, args, orientation);

		int step = 256;

		Gdk.Rectangle rect = new Gdk.Rectangle (args.X, args.Y, 
							Math.Min (step, args.Width),
							Math.Min (step, args.Height));

		Gdk.Rectangle trect = TransformOrientation (src, rect, orientation);
		Gdk.Pixbuf tmp = new Gdk.Pixbuf (src.Colorspace, src.HasAlpha, 
						 src.BitsPerSample,
						 trect.Width, trect.Height);

		Gdk.Rectangle subarea;
		BlockProcessor proc = new BlockProcessor (args, 256);
		while (proc.Step (out subarea)) {
			Gdk.Rectangle trans = TransformOrientation (src, subarea, orientation);
			Gdk.Pixbuf ssub = new Gdk.Pixbuf (src, subarea.X, subarea.Y,
							  subarea.Width, subarea.Height);

			Gdk.Pixbuf tsub = new Gdk.Pixbuf (tmp, 0, 0, trans.Width, trans.Height);
			CopyWithOrientation (ssub, tsub, orientation);

			tsub.CopyArea (0, 0, trans.Width, trans.Height, dest, trans.X, trans.Y);
			ssub.Dispose ();
			tsub.Dispose ();
		}

		tmp.Dispose ();
		return area;
	}
	// Bindings from libf.

	[DllImport ("libfspot")]
	static extern IntPtr f_pixbuf_copy_apply_brightness_and_contrast (IntPtr src, float brightness, float contrast);

	public static Pixbuf ApplyBrightnessAndContrast (Pixbuf src, float brightness, float contrast)
	{
		return new Pixbuf (f_pixbuf_copy_apply_brightness_and_contrast (src.Handle, brightness, contrast));
	}

	[DllImport ("libfspot")]
	static extern bool f_pixbuf_save_jpeg_atomic (IntPtr pixbuf, string filename, int quality, out IntPtr error);

	public static void SaveAsJpegAtomically (Pixbuf pixbuf, string filename, int quality)
	{
		IntPtr error = IntPtr.Zero;

		if (! f_pixbuf_save_jpeg_atomic (pixbuf.Handle, filename, quality, out error)) {
			throw new GLib.GException (error);
		}
	}

	[DllImport ("libfspot")]
	static extern void f_pixbuf_copy_with_orientation (IntPtr src, IntPtr dest, int orientation);

	public static void CopyWithOrientation (Gdk.Pixbuf src, Gdk.Pixbuf dest, PixbufOrientation orientation)
	{
		f_pixbuf_copy_with_orientation (src.Handle, dest.Handle, (int)orientation);
	}

#if false
	[DllImport("glibsharpglue")]
	static extern int gtksharp_object_get_ref_count (IntPtr obj);
	
	public static int RefCount (GLib.Object obj) {
		return gtksharp_object_get_ref_count (obj.Handle);
	}
#endif
#endif
}
