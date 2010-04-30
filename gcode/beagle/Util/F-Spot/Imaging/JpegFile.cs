#define USE_TIFF
using System;
using System.IO;
using FSpot.Xmp;
using FSpot.Tiff;

#if ENABLE_NUNIT
using NUnit.Framework;
#endif

namespace FSpot {
	public interface IThumbnailContainer {
#if false
		Gdk.Pixbuf GetEmbeddedThumbnail ();
#endif
	}

	public class JpegFile : ImageFile, IThumbnailContainer, SemWeb.StatementSource {
		private Exif.ExifData exif_data;
		private XmpFile xmp;
		private JpegHeader header;
		private FSpot.Tiff.Header exif_header;

                // False seems a safe default
                public bool Distinct {
                        get { return false; }
                }
		
		public JpegFile (Uri uri) : base (uri)
		{
			try {
				// Console.WriteLine ("approximate quality = {0}", Header.GuessQuality ());
			} catch (Exception e) {
				Beagle.Util.Log.Error (e, "Error opening JPEG file {0}", uri);
			}
		}
		
		public JpegFile (string path) : base (path) 
		{
		}

		public JpegHeader Header {
			get {
				if (header == null) {
					using (Stream stream = this.Open ()) {
						header = new JpegHeader (stream, true);
					}
				}
				return header;
			}
		}

		public FSpot.Tiff.Header ExifHeader {
			get {
				if (exif_header == null) {
					exif_header = Header.GetExifHeader ();
				}
				return exif_header;
			}
		}

		public void Select (SemWeb.StatementSink sink)
		{
			Header.Select (sink);
		}

#if false
		public override Cms.Profile GetProfile ()
		{
			return Header.GetProfile ();
		}
#endif
		public override string Description {
			get {
#if USE_TIFF
				try {
					SubdirectoryEntry sub = (SubdirectoryEntry) ExifHeader.Directory.Lookup (TagId.ExifIfdPointer);
					if (sub != null) {
						DirectoryEntry entry = sub.Directory [0].Lookup (TagId.UserComment);
						if (entry != null)
							return entry.ValueAsString [0];
					}
				} catch (System.Exception e) {
					Console.WriteLine (e);
				}
				return null;
#else
				try {
					Exif.ExifContent exif_content = this.ExifData.GetContents (Exif.Ifd.Exif);
					Exif.ExifEntry entry = exif_content.Lookup (Exif.Tag.UserComment);
					
					if (entry == null)
						return null;
					
					UserComment comment = new UserComment (entry.Data, entry.ByteOrder == Exif.ByteOrder.Intel);
					return comment.Value;
				} catch (Exception e) {
					// errors here shouldn't be fatal
					return null;
				}
#endif				
			}
		}

		public void SetDescription (string value)
		{
			Exif.ExifContent exif_content = this.ExifData.GetContents (Exif.Ifd.Exif);			
			Exif.ExifEntry entry = exif_content.GetEntry (Exif.Tag.UserComment);

			UserComment comment = new UserComment (value);
			byte [] data = comment.GetBytes (entry.ByteOrder == Exif.ByteOrder.Intel);
			entry.SetData (data);
		}
		
		public void SetXmp (XmpFile xmp)
		{
			this.xmp = xmp;
		}

		private void UpdateMeta ()
		{
			Exif.ExifContent image_content = this.ExifData.GetContents (Exif.Ifd.Zero);
			image_content.GetEntry (Exif.Tag.Software).SetData ("F-Spot" + " version " + "r3823");

			// set the write time in the datetime tag
			image_content.GetEntry (Exif.Tag.DateTime).Reset ();
		}

		private void SaveMetaData (System.IO.Stream input, System.IO.Stream output)
		{
			JpegHeader header = new JpegHeader (input);
			UpdateMeta ();
			
			// Console.WriteLine ("updated metadata");
			header.SetExif (this.ExifData);
			// Console.WriteLine ("set exif");
			if (xmp != null)
				header.SetXmp (xmp);
			// Console.WriteLine ("set xmp");
			header.Save (output);
			// Console.WriteLine ("saved");
		}
		
#if false
		public void SaveMetaData (string path)
		{
			UpdateMeta ();

			//Get file permissions... (mkstemp does not keep permissions or ownership)
			Mono.Unix.Native.Stat stat;
			int stat_err = Mono.Unix.Native.Syscall.stat (path, out stat);

			string  temp_path = path;
			using (System.IO.FileStream stream = System.IO.File.OpenRead (path)) {
				using (System.IO.Stream output = FSpot.Utils.Unix.MakeSafeTemp (ref temp_path)) {
					SaveMetaData (stream, output);
				}
			}

			File.SetAttributes (temp_path, File.GetAttributes (path));

			if (FSpot.Utils.Unix.Rename (temp_path, path) < 0) {
				System.IO.File.Delete (temp_path);
				throw new System.Exception (System.String.Format ("Unable to rename {0} to {1}",
										  temp_path, path));
			}

			//Set file permissions and gid...
			if (stat_err == 0) 
				try {
					Mono.Unix.Native.Syscall.chmod (path, stat.st_mode |
									      Mono.Unix.Native.FilePermissions.S_IRUSR | 
									      Mono.Unix.Native.FilePermissions.S_IWUSR);
					Mono.Unix.Native.Syscall.chown(path, Mono.Unix.Native.Syscall.getuid (), stat.st_gid);
				} catch (Exception) {}
		}

		public void SetThumbnail (Gdk.Pixbuf source)
		{
			// Then create the thumbnail
			// The DCF spec says thumbnails should be 160x120 always
			Gdk.Pixbuf thumbnail = PixbufUtils.ScaleToAspect (source, 160, 120);
			byte [] thumb_data = PixbufUtils.Save (thumbnail, "jpeg", null, null);
			
			// System.Console.WriteLine ("saving thumbnail");				

			// now update the exif data
			ExifData.Data = thumb_data;
		}
#endif

		public void SetDimensions (int width, int height)
		{
			Exif.ExifEntry e;
			Exif.ExifContent thumb_content;
			
			// update the thumbnail related image fields if they exist.
			thumb_content = this.ExifData.GetContents (Exif.Ifd.One);
			e = thumb_content.Lookup (Exif.Tag.RelatedImageWidth);
			if (e != null)
				e.SetData ((uint)width);

			e = thumb_content.Lookup (Exif.Tag.RelatedImageHeight);
			if (e != null)
				e.SetData ((uint)height);
			
			Exif.ExifContent image_content;
			image_content = this.ExifData.GetContents (Exif.Ifd.Zero);
			image_content.GetEntry (Exif.Tag.Orientation).SetData ((ushort)PixbufOrientation.TopLeft);
			//image_content.GetEntry (Exif.Tag.ImageWidth).SetData ((uint)pixbuf.Width);
			//image_content.GetEntry (Exif.Tag.ImageHeight).SetData ((uint)pixbuf.Height);
			image_content.GetEntry (Exif.Tag.PixelXDimension).SetData ((uint)width);
			image_content.GetEntry (Exif.Tag.PixelYDimension).SetData ((uint)height);
		}

#if false
		public override void Save (Gdk.Pixbuf pixbuf, System.IO.Stream stream)
		{

			// Console.WriteLine ("starting save");
			// First save the imagedata
			int quality = Header.GuessQuality ();
			quality = quality == 0 ? 75 : quality;
			byte [] image_data = PixbufUtils.Save (pixbuf, "jpeg", new string [] {"quality" }, new string [] { quality.ToString () });
			System.IO.MemoryStream buffer = new System.IO.MemoryStream ();
			buffer.Write (image_data, 0, image_data.Length);
			buffer.Position = 0;
			
			// Console.WriteLine ("setting thumbnail");
			SetThumbnail (pixbuf);
			SetDimensions (pixbuf.Width, pixbuf.Height);
			pixbuf.Dispose ();
			
			// Console.WriteLine ("saving metatdata");
			SaveMetaData (buffer, stream);
			// Console.WriteLine ("done");
			buffer.Close ();
		}
		
		public Gdk.Pixbuf GetEmbeddedThumbnail ()
		{
			if (this.ExifData.Data.Length > 0) {
				MemoryStream mem = new MemoryStream (this.ExifData.Data);
				Gdk.Pixbuf thumb = new Gdk.Pixbuf (mem);
				Gdk.Pixbuf rotated = PixbufUtils.TransformOrientation (thumb, this.Orientation);
				
				if (rotated != thumb)
					thumb.Dispose ();
				
				mem.Close ();
				return rotated;
			}
			return null;
		}
#endif	
		public Exif.ExifData ExifData {
			get {
				if (exif_data == null) {
					exif_data = Header.Exif;

					if (exif_data == null || exif_data.Handle.Handle == System.IntPtr.Zero)
						exif_data = new Exif.ExifData ();
				}
				// System.Console.WriteLine ("loading exif data");
				return exif_data;
			}
			set {
				this.exif_data = value;
			}
		}

		public override PixbufOrientation GetOrientation () 
		{
			PixbufOrientation orientation = PixbufOrientation.TopLeft;
#if USE_TIFF
			try {
				DirectoryEntry e = ExifHeader.Directory.Lookup (TagId.Orientation);
				orientation = (PixbufOrientation)e.ValueAsLong [0];
			} catch {
				//System.Console.WriteLine ("error checking orientation");
			}
#else						     
			Exif.ExifEntry e = this.ExifData.GetContents (Exif.Ifd.Zero).Lookup (Exif.Tag.Orientation);
			
			if (e != null) {
				ushort [] value = e.GetDataUShort ();
				orientation = (PixbufOrientation) value [0];
			}
#endif			
			if (orientation < PixbufOrientation.TopLeft || orientation > PixbufOrientation.LeftBottom)
				orientation = PixbufOrientation.TopLeft;

			return orientation;
		}
		
		public void SetOrientation (PixbufOrientation orientation)
		{
			Exif.ExifEntry e = this.ExifData.GetContents (Exif.Ifd.Zero).GetEntry (Exif.Tag.Orientation);
			// System.Console.WriteLine ("Saving orientation as {0}", orientation);
			e.SetData ((ushort)orientation);
		       
			e = this.ExifData.GetContents (Exif.Ifd.One).Lookup (Exif.Tag.Orientation);
			if (e != null)
				e.SetData ((ushort)orientation);
		}
		
		public void SetDateTimeOriginal (DateTime time)
		{
			Exif.ExifEntry e = ExifData.LookupFirst (Exif.Tag.DateTimeOriginal);
			if (e != null)
				e.SetData (time);
			else {
				Exif.ExifContent exif_content = this.ExifData.GetContents (Exif.Ifd.Exif);
				Exif.ExifEntry entry = exif_content.GetEntry (Exif.Tag.DateTimeOriginal);
				entry.SetData (time);
			}		
		}

		public override System.DateTime Date {
			get {
				System.DateTime time;
				try {
#if USE_TIFF
					SubdirectoryEntry sub = (SubdirectoryEntry) ExifHeader.Directory.Lookup (TagId.ExifIfdPointer);
					DirectoryEntry e;
					
					if (sub != null) {
						e = sub.Directory [0].Lookup (TagId.DateTimeOriginal);
						
						if (e != null)
							return DirectoryEntry.DateTimeFromString (e.StringValue).ToUniversalTime ();
					}
					
					e = ExifHeader.Directory.Lookup (TagId.DateTime);
					
					if (e != null)
						return DirectoryEntry.DateTimeFromString (e.StringValue).ToUniversalTime ();
					
					return base.Date;
#else
					string time_str = "";				
					time_str = ExifData.LookupFirstValue (Exif.Tag.DateTimeOriginal);
					
					if (time_str == null || time_str == "") 
						time_str = ExifData.LookupFirstValue (Exif.Tag.DateTime);
					
					time = Exif.ExifUtil.DateTimeFromString (time_str).ToUniversalTime (); 
#endif
				} catch (System.Exception e) {
					Console.WriteLine (e);
					time = base.Date;
				}
				return time;
			}
		}

#if ENABLE_NUNIT
		[TestFixture]
		public class Tests {
			public Tests ()
			{
				Gnome.Vfs.Vfs.Initialize ();
				Gtk.Application.Init ();
			}
			
#if false
			[Test]
			public void TestLoad ()
			{
				JpegFile jimg = new JpegFile ("/home/lewing/start.swe.jpeg");
				Assert.AreEqual (PixbufOrientation.TopLeft, jimg.Orientation);
			}
#endif
			[Test]
			public void TestSave ()
			{
				string desc = "this is an example description";
				string desc2 = "\x00a9 Novell Inc.";
				PixbufOrientation orient = PixbufOrientation.TopRight;
				Gdk.Pixbuf test = new Gdk.Pixbuf (null, "f-spot-32.png");
				string path = ImageFile.TempPath ("joe.jpg");
				
				PixbufUtils.SaveJpeg (test, path, 75, new Exif.ExifData ());
				JpegFile jimg = new JpegFile (path);
				jimg.SetDescription (desc);
				jimg.SetOrientation (orient);
				jimg.SaveMetaData (path);
				JpegFile mod = new JpegFile (path);
				Assert.AreEqual (mod.Orientation, orient);
				Assert.AreEqual (mod.Description, desc);
				jimg.SetDescription (desc2);
				jimg.SaveMetaData (path);
				mod = new JpegFile (path);
				Assert.AreEqual (mod.Description, desc2);
				
				File.Delete (path);
			}
		}
#endif
	}
}
