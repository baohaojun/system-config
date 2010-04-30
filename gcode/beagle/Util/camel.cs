//
// camel.cs: Parser for Evolution mbox summary files.
//
// Authors:
//    Miguel de Icaza <miguel@ximian.com>
//
//    Imap support by Erik Bågfors <erik@bagfors.nu>
//

using System.IO;
using System;
using System.Collections;
using System.Globalization;
using System.Text;

namespace Beagle.Util {
namespace Camel {
	
	public enum CamelFlags : uint {
		Answered     = 1 << 0,
		Deleted      = 1 << 1,
		Draft        = 1 << 2,
		Flagged      = 1 << 3,
		Seen         = 1 << 4,
		Attachments  = 1 << 5,
		AnsweredAll  = 1 << 6,
		Junk         = 1 << 7,
		Secure       = 1 << 8
	}
	
public abstract class Summary : IEnumerable { 
	public SummaryHeader header;
	internal string filename;

	public static Summary LoadMBoxSummary (string file)
	{
		Summary s = new MBoxSummary ();
		s.Load (file);

		return s;
	}

	public static Summary LoadImapSummary (string file)
	{
		Summary s = new ImapSummary ();
		s.Load (file);

		return s;
	}

	public static Summary LoadImap4Summary (string file)
	{
		Summary s = new Imap4Summary ();
		s.Load (file);

		return s;
	}

	private void Load (string file)
	{
		this.filename = file;
		FileStream f = File.OpenRead (file);
		this.header = this.ReadHeader (f);
		f.Close ();
	}

	public override string ToString ()
	{
		if (header == null)
			return "No header read";
		else
			return header.ToString ();
	}
	
	public IEnumerator GetEnumerator () {
		return new SummaryEnumerator (this);
	}
	
	protected abstract SummaryHeader ReadHeader (FileStream f);
	protected abstract MessageInfo ReadMessageInfo (FileStream f);
	
	private class SummaryEnumerator : IEnumerator, IDisposable {
		FileStream f = null;
		SummaryHeader header;
		Summary s;
		int index;
		MessageInfo info;
		public SummaryEnumerator (Summary s) {
			this.s = s;
			index = -1;
		}
		
		public void Dispose () 
		{
			if (f != null) {
				f.Close ();
				f = null;
			}
			GC.SuppressFinalize (this);
		}
		
		~SummaryEnumerator () 
		{
			if (f != null) {
				f.Close ();
			}
		}
		
		public bool MoveNext () {
			++index;
			if (index == 0) {
				f = File.OpenRead (s.filename);
				header = s.ReadHeader (f);
			}
			
			if (index >= header.count) {
				if (f != null) {
					f.Close ();
					f = null;
				}
				return false;
			} else {
				info = null;
				while (info == null && index < header.count) {
					try {
						info = s.ReadMessageInfo (f);
					} catch (IOException) {
						Log.Warn ("Unexpected end of file. Skipping rest of messages in {0}", s.filename);
						info = null;
						index = header.count; // just in case
						if (f != null) {
							f.Close ();
							f = null;
						}
						break;
					} catch (Exception e) {
						Log.Warn ("Skipping bogus message " +
							  "[file={0}, index={1}, error={2}]",
							  s.filename, index, e.ToString());
						
						info = null;
						++index;
					}
				}
				
				return (info != null);
			}
		}
			
		public object Current {
			get { return info; }
		}
		
		public void Reset () 
		{
			f.Close ();
			f = null;
			index = -1;
		}
	}
}

	public class MBoxSummary: Summary {
		public MBoxSummary ()
		{
		}

		protected override SummaryHeader ReadHeader (FileStream f)
		{
			return new MBoxSummaryHeader (f);
		}

		protected override MessageInfo ReadMessageInfo (FileStream f)
		{
			return new MBoxMessageInfo (f);
		}
	}

	public class ImapSummary: Summary {
		public ImapSummary ()
		{
		}

		protected override SummaryHeader ReadHeader (FileStream f)
		{
			return new ImapSummaryHeader (f);
		}

		protected override MessageInfo ReadMessageInfo (FileStream f)
		{
			return new ImapMessageInfo (f, true);
		}
 	}

	public class Imap4Summary : Summary {
		public Imap4Summary ()
		{
		}

		protected override SummaryHeader ReadHeader (FileStream f)
		{
			return new ImapSummaryHeader (f);
		}

		protected override MessageInfo ReadMessageInfo (FileStream f)
		{
			return new ImapMessageInfo (f, false);
		}
	}

	public class MessageInfo {
		public string uid, subject, from, to, cc, mlist;
		public uint size, flags;
		public DateTime sent, received;

		private void SkipContentInfo (FileStream f)
		{
			Decode.SkipToken (f); // type
			Decode.SkipToken (f); // subtype
			uint count = Decode.UInt (f); // count
			for (int i = 0; i < count; ++i) {
				Decode.SkipToken (f); // name
				Decode.SkipToken (f); // value
			}
			Decode.SkipToken (f); // id
			Decode.SkipToken (f); // description
			Decode.SkipToken (f); // encoding
			Decode.UInt (f); // size

			count = Decode.UInt (f); // child count
			for (int i = 0; i < count; ++i) // recursively skip children
				SkipContentInfo (f);
		}
		
		public MessageInfo (FileStream f)
		{
			uid      = Decode.String (f);
			flags    = Decode.UInt (f);
			size     = Decode.UInt (f);
			sent     = Decode.Time (f);
			received = Decode.Time (f);
			subject  = Decode.String (f);
			from     = Decode.String (f);
			to       = Decode.String (f);
			cc       = Decode.String (f);
			mlist    = Decode.String (f);

			Decode.SkipFixedInt (f);
			Decode.SkipFixedInt (f);

			uint count;

			// references
			count = Decode.UInt (f);
			if (count > 0) {
				for (int i = 0; i < count; i++) {
					Decode.SkipFixedInt (f);
					Decode.SkipFixedInt (f);
				}
			}

			// user flags
			count = Decode.UInt (f);
			if (count > 0) {
				for (int i = 0; i < count; i++) {
					Decode.SkipString (f);
				}
			}

			// user tags
			count = Decode.UInt (f);
			if (count > 0){
				for (int i = 0; i < count; i++){
					Decode.SkipString (f);
					Decode.SkipString (f);
				}
			}

			// FIXME: How do we know if there is content info in there?
			// SkipContentInfo (f);
		}

		public override string ToString ()
		{
			return String.Format ("From: {0}\nTo: {1}\nSubject: {2}\nUID: {3}\n", from, to, subject, uid);
		}

		public DateTime SentDate {
			get { return sent; }
		}
		
		public DateTime ReceivedDate {
			get { return received; }
		}
		
		private bool CheckFlag (CamelFlags test)
		{
			return (flags & (uint) test) == (uint) test;
		}
		
		public bool IsAnswered {
			get { return CheckFlag (CamelFlags.Answered); }
		}

		public bool IsDeleted {
			get { return CheckFlag (CamelFlags.Deleted); }
		}

		public bool IsDraft {
			get { return CheckFlag (CamelFlags.Draft); }
		}

		public bool IsFlagged {
			get { return CheckFlag (CamelFlags.Flagged); }
		}

		public bool IsSeen {
			get { return CheckFlag (CamelFlags.Seen); }
		}

		public bool HasAttachments {
			get { return CheckFlag (CamelFlags.Attachments); }
		}

		public bool IsAnsweredAll {
			get { return CheckFlag (CamelFlags.AnsweredAll); }
		}

		public bool IsJunk {
			get { return CheckFlag (CamelFlags.Junk); }
		}

		public bool IsSecure {
			get { return CheckFlag (CamelFlags.Secure); }
		}
	}

	public class MBoxMessageInfo : MessageInfo {
		public uint from_pos;
		
		public MBoxMessageInfo (FileStream f) : base (f)
		{
			from_pos = Decode.Offset (f);
		}

		public override string ToString ()
		{
			return String.Format ("From: {0}\nTo: {1}\nSubject: {2}\nPos: {3} Size: {4}\n", from, to, subject,
					      from_pos, size);
		}
	}

	public class ImapMessageInfo : MessageInfo {
		public uint server_flags;
		
		public ImapMessageInfo (FileStream f, bool content_info_load) : base (f)
		{
			server_flags = Decode.UInt (f);

			if (content_info_load)
				PerformContentInfoLoad (f);
		}

		public override string ToString ()
		{
			return String.Format ("From: {0}\nTo: {1}\nSubject: {2}\nSize: {3}\n", from, to, subject, size);
		}

		private bool PerformContentInfoLoad (FileStream f)
		{
		    bool ci = ContentInfoLoad (f);
		    if (!ci) 
			return false;

		    uint count = Decode.UInt (f);

		    if (count > 500) {
			return false;
		    }
		    
		    for (int i = 0; i < count; i++) {

			bool part = PerformContentInfoLoad (f);
			if (!part) 
			    throw new Exception ();
		    }
		    return true;
		}

		private bool ContentInfoLoad (FileStream f)
		{
		    if (f.ReadByte () == 0) 
			return true;

		    // type
		    Decode.SkipToken (f);
		    // subtype
		    Decode.SkipToken (f);

		    uint count;
		    count = Decode.UInt (f);
		    if (count > 500)
			return false;

		    for (int i = 0; i < count; i++) {
			// Name
			Decode.SkipToken (f);
			// Value
			Decode.SkipToken (f);
		    }
		    
		    // id
		    Decode.SkipToken (f);

		    // description
		    Decode.SkipToken (f);

		    // encoding
		    Decode.SkipToken (f);

		    // size
		    Decode.UInt (f);
		    return true;
		}
	}
	
	public class SummaryHeader {
		public int      version;
		public int      flags;
		public int      nextuid;
		public DateTime time;
		public int      count;
		public int      unread;
		public int      deleted;
		public int      junk;
		
		public SummaryHeader (FileStream f)
		{
			bool legacy;

			version = Decode.FixedInt (f);

			if (version > 0xff && (version & 0xff) < 12)
				throw new Exception ("Summary header version too low");

			if (version < 0x100 && version >= 13)
				legacy = false;
			else
				legacy = true;

			flags   = Decode.FixedInt (f);
			nextuid = Decode.FixedInt (f);
			time    = Decode.Time (f);
			count   = Decode.FixedInt (f);

			if (!legacy) {
				unread  = Decode.FixedInt (f);
				deleted = Decode.FixedInt (f);
				junk    = Decode.FixedInt (f);
			}
		}

		public override string ToString ()
		{
			return String.Format ("version={0} flags={1} nextuid={2} time={3} count={4} unread={5} deleted={6} junk={7}",
					      version, flags, nextuid, time, count, unread, deleted, junk);
		}
	}

	public class MBoxSummaryHeader : SummaryHeader {
		public int local_version;
		public int mbox_version;
		public int folder_size;
		
		public MBoxSummaryHeader (FileStream f) : base (f)
		{
			local_version = Decode.FixedInt (f);
			mbox_version = Decode.FixedInt (f);
			folder_size = Decode.FixedInt (f);
		}

		public override string ToString ()
		{
			return String.Format ("{0} local_version={1} mbox_version={2} folder_size={3}",
					      base.ToString (), local_version, mbox_version, folder_size);
		}
	}

	public class ImapSummaryHeader : SummaryHeader {
		private int imap_version;
		
		public ImapSummaryHeader (FileStream f) : base (f)
		{
			// Check for legacy version
			if (base.version != 0x30c) { // 780
				imap_version = Decode.FixedInt (f);

				if (imap_version < 0)
					throw new Exception ("IMAP summary version too low");

				// Right now we only support summary versions 1 through 3
				if (imap_version > 3)
					throw new Exception (String.Format ("Reported summary version ({0}) is too new", imap_version));

				if (imap_version == 2)
					Decode.FixedInt (f);

				// validity
				Decode.SkipFixedInt (f);
			} else {
				// validity
				Decode.UInt (f);
			}
		}

		public override string ToString ()
		{
			return String.Format ("{0} imap_version={1}", base.ToString (), imap_version);
		}
	}
	
	public class Decode {
		static Encoding e = Encoding.UTF8;
		static long UnixBaseTicks;

		static Decode ()
		{
			UnixBaseTicks = DateTimeUtil.UnixToDateTimeUtc (0).Ticks;
			//UnixBaseTicks = new DateTime (1970, 1, 1, 0, 0, 0).Ticks;
		}

		public static string Token (FileStream f) 
		{
		    int len = (int) UInt (f);
		    if (len < 32) {
			if (len <= 0) 
			    return "NULL"; 
			
			// Ok, this is a token from the list, we can ignore it
			return "token_from_list";
		    } else if (len < 0 || len > 10240) {
			throw new Exception ();
		    } else {
			len -= 32;
			byte [] buffer = new byte [len];
			f.Read (buffer, 0, (int) len);
			return new System.String (e.GetChars (buffer, 0, len));
		    }
		}

		public static void SkipToken (FileStream f)
		{
			int len = (int) UInt (f);
			len -= 32;
			if (len > 0)
				f.Seek (len, SeekOrigin.Current);
		}
	       
		public static string String (FileStream f)
		{
			int len = (int) UInt (f);
			len--;

			if (len < 0 || len > 65535)
				throw new Exception ();
			byte [] buffer = new byte [len];
			f.Read (buffer, 0, (int) len);
			return new System.String (e.GetChars (buffer, 0, len));
		}

		public static void SkipString (FileStream f)
		{
			int len = (int) UInt (f);
			--len;
			if (len > 0)
				f.Seek (len, SeekOrigin.Current);
		}

		public static uint UInt (FileStream f)
		{
			uint value = 0;
			int v;
			
			while (((v = f.ReadByte ()) & 0x80) == 0 && v != -1) {
				value |= (byte) v;
				value <<= 7;
			}

			if (v == -1)
				throw new IOException ("Unexpected end of file");

			return value | ((byte)(v & 0x7f));
		}
		
		public static int FixedInt (FileStream f)
		{
			byte [] b = new byte [4];

			f.Read (b, 0, 4);

			return (b [0] << 24) | (b [1] << 16) | (b [2] << 8) | b [3];
		}

		public static void SkipFixedInt (FileStream f)
		{
			f.Seek (4, SeekOrigin.Current);
		}

		public static DateTime Time (FileStream f)
		{
			long seconds = 0;

			// FIXME: Is it safe to assume that sizeof (time_t) == IntPtr.Size?  Probably not.
			for (int i = IntPtr.Size - 1; i >= 0; i--) {
				int v = f.ReadByte ();

				if (v == -1)
					throw new IOException ("Unexpected end of file");

				seconds |= (uint) v << (i * 8);
			}

			if (seconds == 0)
				return new DateTime (0);

			return new DateTime (UnixBaseTicks).AddSeconds (seconds);
		}

		public static uint Offset (FileStream f)
		{
			byte [] b = new byte [4];

			f.Read (b, 0, 4);

			return (uint)((b [0] << 24) | (b [1] << 16) | (b [2] << 8) | b [3]);
		}
	}


#if false
	class Test {
		public static void Main (string [] args)
		{
			string file;
			
			if (args.Length == 0)
				file = "./summary";
			else
				file = args [0];
			
			Summary s = Summary.LoadImapSummary (file);
			Console.WriteLine (s);
			Console.WriteLine ();
			foreach (MessageInfo m in s) {
				Console.WriteLine(m);
			}
			
		}
		
	}
#endif
}
}
