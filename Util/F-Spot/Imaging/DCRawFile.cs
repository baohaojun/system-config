using System.Diagnostics;
using System.IO;
using System;

namespace FSpot {
	public class Pipe : System.IO.Stream {
		// This class is a hack to make sure mono doesn't dispose the process
		// and by extension the stream from the pipe when we are still using the
		// the stream.
		Process process;
		Stream stream;

		public override bool CanRead {
			get { return stream.CanRead; }
		}

		public override bool CanSeek {
			get { return stream.CanSeek; }
		}

		public override bool CanWrite {
			get { return stream.CanWrite; }
		}
		
		public override long Length {
			get { return stream.Length; }
		}

		public override long Position {
			get { return stream.Position; }
			set { stream.Position = value; }
		}

		public Pipe (Process p, Stream stream)
		{
			this.process = p;
			this.stream = stream;
		}
		
		public override void Flush ()
		{
			stream.Flush ();
		}

		public override int Read (byte [] b, int s, int l)
		{
			return stream.Read (b, s, l);
		}

		public override long Seek (long l, SeekOrigin origin)
		{
			return stream.Seek(l, origin);
		}
		
		public override void SetLength (long l)
		{
			stream.SetLength (l);
		}

		public override void Write (byte [] b, int s, int l)
		{
			stream.Write (b, s, l);
		}
		
		public override void Close ()
		{
			stream.Close ();
			stream = null;
			process.Dispose ();
			process = null;
		}
	}

	public class DCRawFile : ImageFile {
		const string dcraw_command = "dcraw";

		public DCRawFile (string path) : base (path)
		{
		}

		public DCRawFile (Uri uri) : base (uri)
		{
		}

		public override System.IO.Stream PixbufStream ()
		{
			return RawPixbufStream (uri);
		}

		internal static System.IO.Stream RawPixbufStream (Uri location)
		{
#if false
			string path = location.LocalPath;
			string [] args = new string [] { dcraw_command, "-h", "-w", "-c", "-t", "0", path };
			
			InternalProcess proc = new InternalProcess (System.IO.Path.GetDirectoryName (path), args);
			proc.StandardInput.Close ();
			return proc.StandardOutput;
#else
			return null;
#endif
		}
	
		public static System.IO.Stream RawPixbufStreamOld (string path)
		{
			// FIXME this filename quoting is super lame
			string args = System.String.Format ("-h -w -c -t 0 \"{0}\"", path);

			System.Diagnostics.Process process = new System.Diagnostics.Process ();
			process.StartInfo = new System.Diagnostics.ProcessStartInfo (dcraw_command, args);
			process.StartInfo.RedirectStandardOutput = true;
			process.StartInfo.UseShellExecute = false;
			process.Start ();
			return new Pipe (process, process.StandardOutput.BaseStream);
		}
		
#if false
		public static Gdk.Pixbuf Load (string path, string args)
		{
			// FIXME this filename quoting is super lame
			args = System.String.Format ("-h -w -c \"{0}\"", path);

			//System.Console.WriteLine ("path = {0}, args = \"{1}\"", path, args);
			 
			using (System.Diagnostics.Process process = new System.Diagnostics.Process ()) {
				process.StartInfo = new System.Diagnostics.ProcessStartInfo (dcraw_command, args);
				process.StartInfo.RedirectStandardOutput = true;
				process.StartInfo.UseShellExecute = false;
				process.Start ();
				return PixbufUtils.LoadFromStream (process.StandardOutput.BaseStream);
			}
		}
#endif
	}
}
