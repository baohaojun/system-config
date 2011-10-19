using System;
using System.IO;
using UriUtils = Beagle.Util.UriFu;

namespace FSpot {
	public class ImageFormatException : ApplicationException {
		public ImageFormatException (string msg) : base (msg)
		{
		}
	}

	public class ImageFile : IDisposable {
		protected Uri uri;

		static System.Collections.Hashtable name_table;

		public ImageFile (string path) 
		{
			this.uri = UriUtils.PathToFileUri (path);
		}
		
		public ImageFile (Uri uri)
		{
			this.uri = uri;
		}
		
		protected Stream Open ()
		{
			//Gnome.Vfs.Uri vfs = new Gnome.Vfs.Uri (uri.ToString ());
			// FIXME, this seems like the sane thing to do, but vfs streams seem to 
			// actually be faster and they need more testing.
			//if (vfs.IsLocal)
			return File.OpenRead (uri.LocalPath);

#if false
			//System.Console.WriteLine ("open uri = {0}", uri.ToString ());
			return new Gnome.Vfs.VfsStream (uri.ToString (), FileMode.Open);
#endif
		}

		public virtual Stream PixbufStream ()
		{
			return Open ();
		}

		static ImageFile ()
		{
			name_table = new System.Collections.Hashtable ();
			name_table [".svg"] = typeof (FSpot.Svg.SvgFile);
			name_table [".gif"] = typeof (ImageFile);
			name_table [".bmp"] = typeof (ImageFile);
			name_table [".jpeg"] = typeof (JpegFile);
			name_table [".jpg"] = typeof (JpegFile);
			name_table [".png"] = typeof (FSpot.Png.PngFile);
			name_table [".cr2"] = typeof (FSpot.Tiff.Cr2File);
			name_table [".nef"] = typeof (FSpot.Tiff.NefFile);
			name_table [".pef"] = typeof (FSpot.Tiff.NefFile);
			name_table [".raw"] = typeof (FSpot.Tiff.NefFile);
			name_table [".kdc"] = typeof (FSpot.Tiff.NefFile);
			name_table [".arw"] = typeof (FSpot.Tiff.NefFile);
			name_table [".tiff"] = typeof (FSpot.Tiff.TiffFile);
			name_table [".tif"] = typeof (FSpot.Tiff.TiffFile);
			name_table [".orf"] =  typeof (FSpot.Tiff.NefFile);
			name_table [".srf"] = typeof (FSpot.Tiff.NefFile);
			name_table [".dng"] = typeof (FSpot.Tiff.DngFile);
			name_table [".crw"] = typeof (FSpot.Ciff.CiffFile);
			name_table [".ppm"] = typeof (FSpot.Pnm.PnmFile);
			name_table [".mrw"] = typeof (FSpot.Mrw.MrwFile);
			name_table [".raf"] = typeof (FSpot.Raf.RafFile);
			name_table [".x3f"] = typeof (FSpot.X3f.X3fFile);
		}

		public Uri Uri {
			get { return this.uri; }
		}

		public PixbufOrientation Orientation {
			get { return GetOrientation (); }
		}

		public virtual string Description
		{
			get { return null; }
		}
		
#if false
		public virtual void Save (Gdk.Pixbuf pixbuf, System.IO.Stream stream)
		{
			throw new NotImplementedException ();
		}

		protected Gdk.Pixbuf TransformAndDispose (Gdk.Pixbuf orig)
		{
			if (orig == null)
				return null;

			Gdk.Pixbuf rotated = PixbufUtils.TransformOrientation (orig, this.Orientation, true);
			//ValidateThumbnail (photo, rotated);
			if (rotated != orig)
				orig.Dispose ();
			
			return rotated;
		}
		
		public virtual Gdk.Pixbuf Load ()
		{
			using (Stream stream = PixbufStream ()) {
				Gdk.Pixbuf orig = new Gdk.Pixbuf (stream);
				return TransformAndDispose (orig);
			}
		}
		
		public virtual Gdk.Pixbuf Load (int max_width, int max_height)
		{
			System.IO.Stream stream = PixbufStream ();
			if (stream == null) {
				Gdk.Pixbuf orig = this.Load ();
				Gdk.Pixbuf scaled = PixbufUtils.ScaleToMaxSize (orig,  max_width, max_height);	
				orig.Dispose ();
				return scaled;
			}

			using (stream) {
				PixbufUtils.AspectLoader aspect = new PixbufUtils.AspectLoader (max_width, max_height);
				return aspect.Load (stream, Orientation);
			}	
		}
#endif
		public virtual PixbufOrientation GetOrientation () 
		{
			return PixbufOrientation.TopLeft;
		}
#if false		
		// FIXME this need to have an intent just like the loading stuff.
		public virtual Cms.Profile GetProfile ()
		{
			return null;
		}
#endif	
		public virtual System.DateTime Date 
		{
			get {
				return System.IO.File.GetCreationTimeUtc (uri.LocalPath);
#if false
				// FIXME mono uses the file change time (ctime) incorrectly
				// as the creation time so we try to work around that slightly
				Gnome.Vfs.FileInfo info = new Gnome.Vfs.FileInfo (uri.ToString ());

				DateTime create = info.Ctime;
				DateTime write = info.Mtime;

				if (create < write)
					return create;
				else 
					return write;
#endif
			}
		}

		[Obsolete ("use HasLoader (System.Uri) instead")]
		public static bool HasLoader (string path)
		{
			return HasLoader (UriUtils.PathToFileUri (path));
		}
		
		public static bool HasLoader (Uri uri)
		{
			string path = uri.AbsolutePath;
			string extension = System.IO.Path.GetExtension (path).ToLower ();
			System.Type t = (System.Type) name_table [extension];
			
			return (t != null);
		}

		[Obsolete ("use Create (System.Uri) instead")]
		public static ImageFile Create (string path)
		{
			return Create (UriUtils.PathToFileUri (path));
		}

		public static ImageFile Create (Uri uri)
		{
			string path = uri.AbsolutePath;
			string extension = System.IO.Path.GetExtension (path).ToLower ();
			System.Type t = (System.Type) name_table [extension];
			ImageFile img;

			if (t != null)
				img = (ImageFile) System.Activator.CreateInstance (t, new object[] { uri });
			else 
				img = new ImageFile (uri);

			return img;
		}
		
		// FIXME these are horrible hacks to get a temporary name
		// with the right extension for ImageFile until we use the mime data
		// properly.  It is here to make sure we can find the places that use
		// this hack
		public static string TempPath (string name)
		{
			return TempPath (name, System.IO.Path.GetExtension (name));
		}
		
		public static string TempPath (string name, string extension)
		{
			string temp = System.IO.Path.GetTempFileName ();
			string imgtemp = temp + "." + extension;

			System.IO.File.Move (temp, imgtemp);

			return imgtemp;
		}

		public void Dispose ()
		{
			Close ();
			GC.SuppressFinalize (this);
		}

		protected virtual void Close ()
		{
		}

		public static bool IsRaw (string name)
		{
			string [] raw_extensions = {".nef", ".crw", ".cr2", ".arw", ".mrw", ".pef", ".dng"};
			foreach (string ext in raw_extensions)
				if (ext == System.IO.Path.GetExtension (name).ToLower ())
					return true;
			return false;
		}

		public static bool IsJpeg (string name)
		{
			string [] jpg_extensions = {".jpg", ".jpeg"};
			foreach (string ext in jpg_extensions)
				if (ext == System.IO.Path.GetExtension (name).ToLower ())
					return true;
			return false;
		}
	} 
}
