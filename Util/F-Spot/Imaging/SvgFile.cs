using System;

namespace FSpot.Svg {
	public class SvgFile : ImageFile // SemWeb.StatementSource 
	{
		MetadataStore store;

                // false seems a safe default
                public bool Distinct {
                        get { return false; }
                }

		public SvgFile (Uri uri) : base (uri)
		{
		}

		public SvgFile (string path) : base (path) 
		{
		}

		public MetadataStore Store {
			get {
				if (store == null) {
					store = new MetadataStore ();
					using (System.IO.Stream input = Open ()) {
						Load (input);
					}
				}
				return store;
			}
		}

		public void Load (System.IO.Stream stream)
		{
			try {
				store.Import (new SemWeb.RdfXmlReader (stream));
				store.Dump ();

			} catch (System.Exception e) {
				Beagle.Util.Log.Error (e, "Error loading SVG file");
			}
		}

#if BROKEN_RSVG
		public override Gdk.Pixbuf Load (int max_width, int max_height)
		{
			// FIXME this is a hack to work around a crash in the scaled
			// gdk pixbuf rsvg loader.  We load it without scaling it then scale the image
			using (System.IO.Stream stream = Open ()) {
				using (Gdk.Pixbuf pixbuf = new Gdk.Pixbuf (stream)) {
					Gdk.Pixbuf scaled = PixbufUtils.ScaleToMaxSize (pixbuf, max_width, max_height);
					return scaled;
				}
			}
		}
#endif
		public void Select (SemWeb.StatementSink sink)
		{
			Store.Select (sink);
		}
	}
}
