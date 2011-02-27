
//
// EvolutionMailIndexableGenerator.cs
//
// Copyright (C) 2004 Novell, Inc.
//
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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using System.Xml;

using Beagle.Util;
using Beagle.Daemon;

using B_U_Camel = Beagle.Util.Camel;

namespace Beagle.Daemon.EvolutionMailQueryable {

	public abstract class EvolutionMailIndexableGenerator : IIndexableGenerator {

		protected static bool Debug = false;

		private static bool gmime_initialized = false;

		private static ArrayList excludes = new ArrayList ();

		static EvolutionMailIndexableGenerator () {
			List<string[]> values = Conf.Daemon.GetListOptionValues (Conf.Names.ExcludeMailfolder);
			if (values == null)
				return;

			foreach (string[] item in values)
				excludes.Add (item [0].ToLower ());
		}

		protected EvolutionMailQueryable queryable;

		protected string account_name, folder_name;
		protected int count, indexed_count;

		protected EvolutionMailIndexableGenerator (EvolutionMailQueryable queryable)
		{
			this.queryable = queryable;
		}

		protected abstract string GetFolderName (FileSystemInfo info);
		protected abstract bool Setup ();
		protected abstract FileInfo CrawlFile { get; }
		public abstract string GetTarget ();
		public abstract bool HasNextIndexable ();
		public abstract Indexable GetNextIndexable ();
		public abstract void Checkpoint ();


		protected double progress_percent;
		public double ProgressPercent {
			get { return progress_percent; }
		}

		public void PostFlushHook ()
		{
			Checkpoint ();
		}

		protected static void InitializeGMime ()
		{
			if (!gmime_initialized) {
				GMime.Global.Init ();
				gmime_initialized = true;
			}
		}

		protected bool IsSpamFolder (string name)
		{
			if (name.ToLower () == "spam" || name.ToLower () == "junk")
				return true;
			else
				return false;
		}

		protected bool IgnoreFolder (string path)
		{
			// FIXME: Use System.IO.Path
			foreach (string exclude in excludes) {
				if (path.StartsWith (exclude))
					return true;
			}
			return false;
		}
					
		protected virtual void CrawlFinished ()
		{
			this.queryable.RemoveGenerator (this);
		}

		public string StatusName {
			get { return this.CrawlFile.FullName; }
		}

		public override bool Equals (object o)
		{
			EvolutionMailIndexableGenerator generator = o as EvolutionMailIndexableGenerator;

			if (generator == null)
				return false;

			if (Object.ReferenceEquals (this, generator))
				return true;

			if (this.CrawlFile.FullName == generator.CrawlFile.FullName)
				return true;
			else
				return false;
		}

		public override int GetHashCode ()
		{
			return this.CrawlFile.FullName.GetHashCode ();
		}
	}

	public class EvolutionMailIndexableGeneratorMbox : EvolutionMailIndexableGenerator {
		private FileInfo mbox_info;
		private int mbox_fd = -1;
		private GMime.StreamFs mbox_stream;
		private GMime.Parser mbox_parser;
		private long file_size;

		public EvolutionMailIndexableGeneratorMbox (EvolutionMailQueryable queryable, FileInfo mbox_info) : base (queryable)
		{
			this.mbox_info = mbox_info;
		}

		protected override string GetFolderName (FileSystemInfo info)
		{
			FileInfo file_info = (FileInfo) info;
			DirectoryInfo di;
			string folder_name = "";

			di = file_info.Directory;
			while (di != null) {
				// Evo uses ".sbd" as the extension on a folder
				if (di.Extension == ".sbd")
					folder_name = Path.Combine (Path.GetFileNameWithoutExtension (di.Name), folder_name);
				else
					break;

				di = di.Parent;
			}

			return Path.Combine (folder_name, file_info.Name);
		}

		protected override bool Setup ()
		{
			this.account_name = "local@local";
			this.folder_name = this.GetFolderName (this.mbox_info);

			if (this.IsSpamFolder (this.folder_name))
				return false;

			if (this.IgnoreFolder (this.mbox_info.FullName))
				return false;

			return true;
		}

		private long MboxLastOffset {
			get {
				string offset_str = this.queryable.ReadDataLine ("offset-" + this.folder_name.Replace ('/', '-'));
				long offset = Convert.ToInt64 (offset_str);

				return offset;
			}

			set {
				this.queryable.WriteDataLine ("offset-" + this.folder_name.Replace ('/', '-'), value.ToString ());
			}
		}

		public override bool HasNextIndexable ()
		{
			if (this.account_name == null) {
				if (!Setup ()) {
					this.queryable.RemoveGenerator (this);
					return false;
				}
			}

			if (this.mbox_fd < 0) {
				Logger.Log.Debug ("Opening mbox {0}", this.mbox_info.Name);

				try {
					InitializeGMime ();
				} catch (Exception e) {
					Logger.Log.Warn (e, "Caught exception trying to initalize gmime:");
					return false;
				}

				this.mbox_fd = Mono.Unix.Native.Syscall.open (this.mbox_info.FullName, Mono.Unix.Native.OpenFlags.O_RDONLY);

				if (this.mbox_fd < 0) {
					Log.Error ("Unable to open {0}: {1}", this.mbox_info.FullName, Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError ()));
					return false;
				}

				this.mbox_stream = new GMime.StreamFs (this.mbox_fd);
				this.mbox_stream.Seek (this.MboxLastOffset);
				this.mbox_parser = new GMime.Parser (this.mbox_stream);
				this.mbox_parser.ScanFrom = true;

				FileInfo info = new FileInfo (this.mbox_info.FullName);
				this.file_size = info.Length;
			}

			if (this.mbox_parser.Eos ()) {
				long offset = this.mbox_parser.FromOffset;

				this.mbox_stream.Close ();

				this.mbox_fd = -1;
				this.mbox_stream.Dispose ();
				this.mbox_stream = null;
				this.mbox_parser.Dispose ();
				this.mbox_parser = null;
				
				Logger.Log.Debug ("{0}: Finished indexing {1} messages", this.folder_name, this.indexed_count);

				if (offset >= 0)
					this.MboxLastOffset = offset;
				this.CrawlFinished ();

				return false;
			} else
				return true;
		}

		public override Indexable GetNextIndexable ()
		{
			using (GMime.Message message = this.mbox_parser.ConstructMessage ()) {
				// Work around what I think is a bug in GMime: If you
				// have a zero-byte file or seek to the end of a
				// file, parser.Eos () will return true until it
				// actually tries to read something off the wire.
				// Since parser.ConstructMessage() always returns a
				// message (which may also be a bug), we'll often get
				// one empty message which we need to deal with here.
				//
				// Check if its empty by seeing if the Headers
				// property is null or empty.
				if (message == null || message.Headers == null || message.Headers == "")
					return null;
				
				++this.count;

				string x_evolution = message.GetHeader ("X-Evolution");
				if (x_evolution == null || x_evolution == "") {
					Logger.Log.Info ("{0}: Message at offset {1} has no X-Evolution header!",
							 this.folder_name, this.mbox_parser.FromOffset);
					return null;
				}
				
				// This extracts the UID and flags from the X-Evolution header.
				// It may also contain user-defined flags and tags, but we don't
				// support those right now.
				int separator_idx = x_evolution.IndexOf ('-');

				string uid_str = x_evolution.Substring (0, separator_idx);
				string uid = Convert.ToUInt32 (uid_str, 16).ToString (); // ugh.
				uint flags = Convert.ToUInt32 (x_evolution.Substring (separator_idx + 1, 4), 16);

				Indexable indexable = this.GMimeMessageToIndexable (uid, message, flags);

				if (Debug) {
					Logger.Log.Debug ("Constructed message {0} with uid {1}, flags {2}.  Indexable {3} null",
							  this.count, uid, flags, indexable == null ? "" : "not");
				}
				
				if (indexable == null)
					return null;

				++this.indexed_count;

				return indexable;
			}
		}

		private static bool CheckFlags (uint flags, B_U_Camel.CamelFlags test)
		{
			return (flags & (uint) test) == (uint) test;
		}

		private Indexable GMimeMessageToIndexable (string uid, GMime.Message message, uint flags)
		{
			// Don't index messages flagged as junk
			if (CheckFlags (flags, B_U_Camel.CamelFlags.Junk))
				return null;

			System.Uri uri = EvolutionMailQueryable.EmailUri (this.account_name, this.folder_name, uid);
			Indexable indexable = new Indexable (uri);

			indexable.Timestamp = message.Date.ToUniversalTime ();
			indexable.HitType = "MailMessage";
			indexable.MimeType = "message/rfc822";
			indexable.CacheContent = true;

			indexable.AddProperty (Property.NewUnsearched ("fixme:client", "evolution"));
			indexable.AddProperty (Property.NewUnsearched ("fixme:account", "Local"));
                        indexable.AddProperty (Property.NewUnsearched ("fixme:folder", this.folder_name));

			GMime.InternetAddressList addrs;

			addrs = message.GetRecipients (GMime.Message.RecipientType.To);
			foreach (GMime.InternetAddress ia in addrs) {
				if (this.folder_name == "Sent" && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = message.GetRecipients (GMime.Message.RecipientType.Cc);
			foreach (GMime.InternetAddress ia in addrs) {
				if (this.folder_name == "Sent"  && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = GMime.InternetAddressList.ParseString (GMime.Utils.HeaderDecodePhrase (message.Sender));
			foreach (GMime.InternetAddress ia in addrs) {
				if (this.folder_name != "Sent"  && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:gotFrom", ia.Addr));
			}
			addrs.Dispose ();

			if (this.folder_name == "Sent")
				indexable.AddProperty (Property.NewFlag ("fixme:isSent"));

	                Property flag_prop = Property.NewUnsearched ("fixme:flags", flags);
			flag_prop.IsMutable = true;
			indexable.AddProperty (flag_prop);

			if (CheckFlags (flags, B_U_Camel.CamelFlags.Answered))
				indexable.AddProperty (Property.NewFlag ("fixme:isAnswered"));

			if (CheckFlags (flags, B_U_Camel.CamelFlags.Deleted))
				indexable.AddProperty (Property.NewFlag ("fixme:isDeleted"));

			if (CheckFlags (flags, B_U_Camel.CamelFlags.Draft))
				indexable.AddProperty (Property.NewFlag ("fixme:isDraft"));

			if (CheckFlags (flags, B_U_Camel.CamelFlags.Flagged))
				indexable.AddProperty (Property.NewFlag ("fixme:isFlagged"));

			if (CheckFlags (flags, B_U_Camel.CamelFlags.Seen))
				indexable.AddProperty (Property.NewFlag ("fixme:isSeen"));

			if (CheckFlags (flags, B_U_Camel.CamelFlags.AnsweredAll))
				indexable.AddProperty (Property.NewFlag ("fixme:isAnsweredAll"));

			indexable.SetBinaryStream (message.Stream);

			return indexable;
		}

		public override void Checkpoint ()
		{
			long offset = -1;

			if (this.mbox_parser != null) {
				offset = this.mbox_parser.FromOffset;

				if (offset >= 0)
					this.MboxLastOffset = offset;
			}

			string progress = "";
			if (this.file_size > 0 && offset > 0) {
				progress = String.Format (" ({0}/{1} bytes {2:###.0}%)",
							  offset, this.file_size, 100.0 * offset / this.file_size);

				this.progress_percent = 100.0 * offset / this.file_size;
			}

			Logger.Log.Debug ("{0}: indexed {1} messages{2}",
					  this.folder_name, this.indexed_count, progress);
		}

		public override string GetTarget ()
		{
			return "mbox-file:" + mbox_info.FullName;
		}

		protected override FileInfo CrawlFile {
			get { return this.mbox_info; }
		}
	}

	public class EvolutionMailIndexableGeneratorImap : EvolutionMailIndexableGenerator {
		private enum ImapBackendType {
			Imap,
			Imap4
		};

		private FileInfo summary_info;
		private string imap_name;
		private ImapBackendType backend_type;
		private B_U_Camel.Summary summary;
		private IEnumerator summary_enumerator;
		private EvolutionSummaryTracker tracker;
		private DateTime start_crawl_time;
		private ArrayList deleted_list;
		private bool delete_mode;
		private int delete_count;

		public EvolutionMailIndexableGeneratorImap (EvolutionMailQueryable queryable, FileInfo summary_info) : base (queryable)
		{
			this.summary_info = summary_info;
		}

		protected override string GetFolderName (FileSystemInfo info)
		{
			DirectoryInfo dir_info = (DirectoryInfo) info;
			string folder_name = "";

			while (dir_info != null) {
				folder_name = Path.Combine (dir_info.Name, folder_name);

				dir_info = dir_info.Parent;

				if (dir_info.Name != "subfolders")
					break;
				else
					dir_info = dir_info.Parent;
			}

			return folder_name;
		}

		protected override bool Setup ()
		{
			string dir_name = summary_info.DirectoryName;
			int imap_start_idx;

			int idx = dir_name.IndexOf (".evolution/mail/imap4/");

			if (idx >= 0) {
				this.backend_type = ImapBackendType.Imap4;
				imap_start_idx = idx + 22;
			} else {
				this.backend_type = ImapBackendType.Imap;
				imap_start_idx = dir_name.IndexOf (".evolution/mail/imap/") + 21;
			}

			string imap_start = dir_name.Substring (imap_start_idx);
			this.imap_name = imap_start.Substring (0, imap_start.IndexOf ('/'));

			ICollection accounts = null;

			try {
				accounts = (ICollection) GConfThreadHelper.Get ("/apps/evolution/mail/accounts");
			} catch (Exception ex) {
				Logger.Log.Warn ("Caught exception in Setup(): " + ex.Message);
				Logger.Log.Warn ("There are no configured evolution accounts, ignoring {0}", this.imap_name);
				return false;
			}

			// This should only happen if we shut down while waiting for the GConf results to come back.
			if (accounts == null)
				return false;

			foreach (string xml in accounts) {
				XmlDocument xmlDoc = new XmlDocument ();

				xmlDoc.LoadXml (xml);
				
				XmlNode account = xmlDoc.SelectSingleNode ("//account");

				if (account == null)
					continue;

				string uid = null;

				foreach (XmlAttribute attr in account.Attributes) {
					if (attr.Name == "uid") {
						uid = attr.InnerText;
						break;
					}
				}

				if (uid == null)
					continue;

				XmlNode imap_url_node = xmlDoc.SelectSingleNode ("//source/url");

				if (imap_url_node == null)
					continue;

				string imap_url = imap_url_node.InnerText;
				// If there is a semicolon in the username part of the URL, it
				// indicates that there's an auth scheme there.  We don't care
				// about that, so remove it.
				int user_end = imap_url.IndexOf ('@');
				int semicolon = imap_url.IndexOf (';', 0, user_end + 1);

				if (semicolon != -1)
					imap_url = imap_url.Substring (0, semicolon) + imap_url.Substring (user_end);

				// Escape backslashes, which frequently appear when using IMAP against Exchange servers
				this.imap_name = this.imap_name.Replace ("\\", "%5c");

				// Escape out additional @s in the name.  I hate the class libs so much.
				int lastIdx = this.imap_name.LastIndexOf ('@');
				if (this.imap_name.IndexOf ('@') != lastIdx) {
					string toEscape = this.imap_name.Substring (0, lastIdx);
					this.imap_name = toEscape.Replace ("@", "%40") + this.imap_name.Substring (lastIdx);
				}

				string backend_url_prefix;
				if (this.backend_type == ImapBackendType.Imap)
					backend_url_prefix = "imap";
				else
					backend_url_prefix = "imap4";

				if (imap_url.StartsWith (backend_url_prefix + "://" + this.imap_name + "/")) {
					this.account_name = uid;
					break;
				}
			}

			if (this.account_name == null) {
				Logger.Log.Info ("Unable to determine account name for {0}", this.imap_name);
				return false;
			}

			// Need to check the directory on disk to see if it's a junk/spam folder,
			// since the folder name will be "foo/spam" and not match the check below.
			DirectoryInfo dir_info = new DirectoryInfo (dir_name);
			if (this.IsSpamFolder (dir_info.Name))
				return false;

			// Check if the folder is listed in the configuration as to be excluded from indexing
			if (this.IgnoreFolder (dir_info.FullName))
				return false;
					
			this.folder_name = GetFolderName (new DirectoryInfo (dir_name));

			return true;
		}

		public override bool HasNextIndexable ()
		{
			if (this.account_name == null) {
				if (!Setup ()) {
					this.queryable.RemoveGenerator (this);
					return false;
				}
			}

			if (this.tracker == null) {
				if (this.queryable.FileAttributesStore.IsUpToDate (this.CrawlFile.FullName)) {
					Logger.Log.Debug ("{0}: summary has not been updated; crawl unncessary", this.folder_name);
					this.queryable.RemoveGenerator (this);
					return false;
				}

				this.tracker = new EvolutionSummaryTracker (this.queryable.IndexDirectory, this.account_name, this.folder_name);
				this.start_crawl_time = DateTime.UtcNow;
			}

			if (this.summary == null) {
				try {
					if (this.backend_type == ImapBackendType.Imap)
						this.summary = B_U_Camel.Summary.LoadImapSummary (this.summary_info.FullName);
					else
						this.summary = B_U_Camel.Summary.LoadImap4Summary (this.summary_info.FullName);
				} catch (Exception e) {
					Logger.Log.Warn (e, "Unable to index {0}:", this.folder_name);
					this.queryable.RemoveGenerator (this);
					return false;
				}
			}

			if (this.summary_enumerator == null)
				this.summary_enumerator = this.summary.GetEnumerator ();

			if (this.summary_enumerator.MoveNext ())
				return true;

			this.delete_mode = true;
			this.deleted_list = this.tracker.GetOlderThan (this.start_crawl_time);

			if (this.deleted_list.Count > 0)
				return true;

			this.deleted_list = null;

			string progress = "";
			if (this.count > 0 && this.summary.header.count > 0) {
				progress = String.Format ("({0}/{1} {2:###.0}%)",
							  this.count,
							  this.summary.header.count,
							  100.0 * this.count / this.summary.header.count);
			}

			Logger.Log.Debug ("{0}: Finished indexing {1} messages {2}, {3} messages deleted", this.folder_name, this.indexed_count, progress, this.delete_count);

			this.tracker.Close ();
			this.tracker = null;
			this.CrawlFinished ();

			return false;
		}

		// Kind of nasty, but we need the function.
		[System.Runtime.InteropServices.DllImport("libglib-2.0.so.0")]
		static extern int g_str_hash (string str);

		// Stolen from deep within e-d-s's camel-data-cache.c  Very evil.
		private const int CAMEL_DATA_CACHE_MASK = ((1 << 6) - 1);

		public override Indexable GetNextIndexable ()
		{
			Indexable indexable = null;

			// No more new messages to index, so start on the removals.
			if (this.delete_mode) {
				string uid = (string) this.deleted_list [0];
				Uri uri = EvolutionMailQueryable.EmailUri (this.account_name, this.folder_name, uid);
				
				indexable = new Indexable (IndexableType.Remove, uri);

				this.deleted_list.RemoveAt (0);
				this.tracker.Remove (uid);

				this.delete_count++;

				return indexable;
			}

			B_U_Camel.MessageInfo mi = (B_U_Camel.MessageInfo) this.summary_enumerator.Current;

			++this.count;

			if (Debug) {
				Logger.Log.Debug ("Constructed message {0} with uid {1}, flags {2}.",
						  this.count, mi.uid, mi.flags);
			}

			uint flags;
			bool found = this.tracker.Get (mi.uid, out flags);

			if (! found) {
				// New, previously unseen message
				string msg_file;

				if (this.backend_type == ImapBackendType.Imap)
					msg_file = Path.Combine (summary_info.DirectoryName, mi.uid + ".");
				else {
					// This is taken from e-d-s's camel-data-cache.c.  No doubt
					// NotZed would scream bloody murder if he saw this here.
					int hash = (g_str_hash (mi.uid) >> 5) & CAMEL_DATA_CACHE_MASK;
					string cache_path = String.Format ("cache/{0:x}/{1}", hash, mi.uid);
					msg_file = Path.Combine (summary_info.DirectoryName, cache_path);
				}

				indexable = this.CamelMessageToIndexable (mi, msg_file);

				if (Debug)
					Logger.Log.Debug ("Unseen message, indexable {0} null", indexable == null ? "" : "not");

				if (indexable != null)
					++this.indexed_count;
			} else if (found && flags != mi.flags) {
				// Previously seen message, but flags have changed.
				Uri uri = CamelMessageUri (mi);
				indexable = new Indexable (uri);
				indexable.Type = IndexableType.PropertyChange;

				Property flag_prop = Property.NewUnsearched ("fixme:flags", mi.flags);
				flag_prop.IsMutable = true;
				indexable.AddProperty (flag_prop);

				if (Debug)
					Logger.Log.Debug ("Previously seen message, flags changed: {0} -> {1}", flags, mi.flags);

				++this.indexed_count;
			} else {
				if (Debug)
					Logger.Log.Debug ("Previously seen message, unchanged.");
			}

			this.tracker.Update (mi.uid, mi.flags);

			return indexable;
		}

		private Uri CamelMessageUri (B_U_Camel.MessageInfo message_info)
		{
			return EvolutionMailQueryable.EmailUri (this.account_name, this.folder_name, message_info.uid);
		}

		private Indexable CamelMessageToIndexable (B_U_Camel.MessageInfo messageInfo, string msg_file)
		{
			// Don't index messages flagged as junk
			if (messageInfo.IsJunk)
				return null;

			// Many properties will be set by the filter when
			// processing the cached data, if it's there.  So
			// don't set a number of properties in that case.
			bool have_content = File.Exists (msg_file);

			Uri uri = CamelMessageUri (messageInfo);
			Indexable indexable = new Indexable (uri);

			indexable.Timestamp = messageInfo.SentDate;
			indexable.MimeType = "message/rfc822";
			indexable.HitType = "MailMessage";

			indexable.AddProperty (Property.NewUnsearched ("fixme:account",  this.imap_name));
                        indexable.AddProperty (Property.NewUnsearched ("fixme:folder",   this.folder_name));
			indexable.AddProperty (Property.NewUnsearched ("fixme:client", "evolution"));
			
			if (!have_content) {
				indexable.AddProperty (Property.New ("dc:title", GMime.Utils.HeaderDecodePhrase (messageInfo.subject)));
				indexable.AddProperty (Property.NewDate ("fixme:date", messageInfo.SentDate));
			}

			GMime.InternetAddressList addrs;
			addrs = GMime.InternetAddressList.ParseString (messageInfo.to);
			foreach (GMime.InternetAddress ia in addrs) {
				if (!have_content) {
					indexable.AddProperty (Property.NewUnsearched ("fixme:to", ia.ToString (false)));
					if (ia.AddressType != GMime.InternetAddressType.Group)
						indexable.AddProperty (Property.New ("fixme:to_address", ia.Addr));

					indexable.AddProperty (Property.New ("fixme:to_name", ia.Name));
				}

				if (this.folder_name == "Sent" && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = GMime.InternetAddressList.ParseString (messageInfo.cc);
			foreach (GMime.InternetAddress ia in addrs) {
				if (!have_content) {
					indexable.AddProperty (Property.NewUnsearched ("fixme:cc", ia.ToString (false)));
					if (ia.AddressType != GMime.InternetAddressType.Group)
						indexable.AddProperty (Property.New ("fixme:cc_address", ia.Addr));

					indexable.AddProperty (Property.New ("fixme:cc_name", ia.Name));
				}

				if (this.folder_name == "Sent" && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:sentTo", ia.Addr));
			}
			addrs.Dispose ();

			addrs = GMime.InternetAddressList.ParseString (messageInfo.from);
			foreach (GMime.InternetAddress ia in addrs) {
				if (!have_content) {
					indexable.AddProperty (Property.NewUnsearched ("fixme:from", ia.ToString (false)));
					if (ia.AddressType != GMime.InternetAddressType.Group)
						indexable.AddProperty (Property.New ("fixme:from_address", ia.Addr));

					indexable.AddProperty (Property.New ("fixme:from_name", ia.Name));
				}

				if (this.folder_name != "Sent" && ia.AddressType != GMime.InternetAddressType.Group)
					indexable.AddProperty (Property.NewUnsearched ("fixme:gotFrom", ia.Addr));
			}
			addrs.Dispose ();

                        indexable.AddProperty (Property.NewKeyword ("fixme:mlist", messageInfo.mlist));

			Property flag_prop = Property.NewUnsearched ("fixme:flags", messageInfo.flags);
			flag_prop.IsMutable = true;
			indexable.AddProperty (flag_prop);

			if (this.folder_name == "Sent")
				indexable.AddProperty (Property.NewFlag ("fixme:isSent"));

			if (messageInfo.IsAnswered)
				indexable.AddProperty (Property.NewFlag ("fixme:isAnswered"));

			if (messageInfo.IsDeleted)
				indexable.AddProperty (Property.NewFlag ("fixme:isDeleted"));

			if (messageInfo.IsDraft)
				indexable.AddProperty (Property.NewFlag ("fixme:isDraft"));

			if (messageInfo.IsFlagged)
				indexable.AddProperty (Property.NewFlag ("fixme:isFlagged"));

			if (messageInfo.IsSeen)
				indexable.AddProperty (Property.NewFlag ("fixme:isSeen"));

			if (messageInfo.HasAttachments && !have_content)
				indexable.AddProperty (Property.NewFlag ("fixme:hasAttachments"));

			if (messageInfo.IsAnsweredAll)
				indexable.AddProperty (Property.NewFlag ("fixme:isAnsweredAll"));

			if (have_content)
				indexable.ContentUri = UriFu.PathToFileUri (msg_file);
			else 
				indexable.NoContent = true;

			return indexable;
		}

		public override void Checkpoint ()
		{
			if (this.summary != null) {

				string progress = "";
				if (this.count > 0 && this.summary.header.count > 0) {
					progress = String.Format (" ({0}/{1} {2:###.0}%)",
								  this.count,
								  this.summary.header.count,
								  100.0 * this.count / this.summary.header.count);

					this.progress_percent = 100.0 * this.count / this.summary.header.count;

				}
				
				Logger.Log.Debug ("{0}: indexed {1} messages{2}",
						  this.folder_name, this.indexed_count, progress);
			}
			
			if (this.tracker != null)
				this.tracker.Checkpoint ();
		}

		protected override void CrawlFinished ()
		{
			this.queryable.FileAttributesStore.AttachLastWriteTime (this.CrawlFile.FullName, this.start_crawl_time);
			base.CrawlFinished ();
		}

		public override string GetTarget ()
		{
			return "summary-file:" + summary_info.FullName;
		}

		protected override FileInfo CrawlFile {
			get { return this.summary_info; }
		}
	}
}
