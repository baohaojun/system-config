//
// FilterArchive.cs
//
// Copyright (C) 2004-2006 Novell, Inc.
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
using System.IO;

using ICSharpCode.SharpZipLib;
using ICSharpCode.SharpZipLib.Zip;
using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpZipLib.BZip2;
using ICSharpCode.SharpZipLib.Tar;

using Beagle;
using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {

	public class FilterArchive : Beagle.Daemon.Filter {

		internal delegate ArchiveEntry GetNextEntry ();

		private Stream archive_stream;
		private GetNextEntry get_next_entry;
		private int count = 0;
		private	long total_size = 0;

		// Fairly arbitrary limits
		private const int  MAX_CHILDREN    = 30;
		private const long MAX_SINGLE_FILE = 10*1024*1024;
		private const long MAX_ALL_FILES   = 25*1024*1024;

		public FilterArchive ()
		{
			// 1: Store entry names as text content
			SetVersion (1);
			SetFileType ("archive");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/zip"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-bzip-compressed-tar"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-compressed-tar"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-tar"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-tgz"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-gzip"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-bzip"));
		}
		
		public override bool HasGeneratedIndexable {
			get { return true; }
		}

		bool setup_done = false;
		string archive_type = "zip";

		private void SetupArchiveStream ()
		{
			this.count = 0;
			this.total_size = 0;

			switch (MimeType) {
			case "application/zip":
				archive_stream = new ZipInputStream (Stream);
				get_next_entry = GetNextEntryZip;
				archive_type = "zip";
				break;

			case "application/x-bzip-compressed-tar":
				archive_stream = new TarInputStream (new BZip2InputStream (Stream));
				get_next_entry = GetNextEntryTar;
				archive_type = "tar-bz2";
				break;

			case "application/x-compressed-tar":
			case "application/x-tgz":
				archive_stream = new TarInputStream (new GZipInputStream (Stream));
				get_next_entry = GetNextEntryTar;
				archive_type = "tar-gz";
				break;

			case "application/x-tar":
				archive_stream = new TarInputStream (Stream);
				get_next_entry = GetNextEntryTar;
				archive_type = "tar";
				break;

			case "application/x-gzip":
				archive_stream = new GZipInputStream (Stream);
				get_next_entry = GetNextEntrySingle;
				archive_type = "gzip";
				break;

			case "application/x-bzip":
				archive_stream = new BZip2InputStream (Stream);
				get_next_entry = GetNextEntrySingle;
				archive_type = "bz2";
				break;

			default:
				throw new ArgumentException ("Invalid or unsupported mime type.");
			}

			setup_done = true;
		}
				
		protected override void DoPullProperties ()
		{
			// FIXME: Fetch the archive properties.
		}

		public override bool GenerateNextIndexable (out Indexable child)
		{
			ArchiveEntry a_entry;
			child = null;

			if (! setup_done)
				SetupArchiveStream ();

			if (count >= MAX_CHILDREN) {
				Log.Debug ("Archive {0} contains more than {1} files.  Only {1} files indexed.", Indexable.DisplayUri.ToString (), count);
				Close ();
				return false;
			}

			if (total_size > MAX_ALL_FILES) {
				Log.Debug ("Archive {0} crossed our max uncompressed size threshold.  Only {1} files extracted", Indexable.DisplayUri.ToString (), count);
				Close ();
				return false;
			}

			a_entry = DoGetNextEntry ();
			if (a_entry == null) {
				Close ();
				return false;
			}

			// Store file names in the archive
			AppendText (Path.GetFileName (a_entry.Name));
			AppendWhiteSpace ();

			// If this is an invalid or oversized entry, skip it.
			if (a_entry.TempFile == null)
				return true;

			++count;
			total_size += a_entry.Size;

			// Add "#<escaped-path-to-entry>" to the end of the Indexable Uri
			// So, file b#c in archive foo.zip becomes file:///foo.zip#b%23c
			// And file c in archive b in archive foo.zip becomes file:///foo.zip#b#c
			child = new Indexable (UriFu.AddFragment (Indexable.Uri, a_entry.Name, false));

			child.CacheContent = true;
			child.MimeType = a_entry.MimeType;

			child.DisplayUri = new Uri (Indexable.DisplayUri.ToString () + "#" + a_entry.Name);
			child.ContentUri = UriFu.PathToFileUri (a_entry.TempFile);
			child.DeleteContent = true;

			// FIXME Remove fixme:inside_archive during Property Hack Week
			// Replace most flag properties by value properties
			child.AddProperty (Property.NewBool ("fixme:inside_archive", true));
			// Use this instead of fixme:inside_archive
			child.AddProperty (Property.NewKeyword ("archive:type", archive_type));

			child.AddProperty (Property.NewKeyword ("fixme:relativeuri", a_entry.Name));
			child.AddProperty (Property.New ("fixme:comment", a_entry.Comment));
			child.AddProperty (Property.NewUnsearched ("fixme:filesize", a_entry.Size));

			foreach (Property prop in Property.StandardFileProperties (Path.GetFileName (a_entry.Name), false))
				child.AddProperty (prop);

			child.SetChildOf (Indexable);

			return true;
		}

		private void Close ()
		{
			// Cleanup
			archive_stream.Close ();
			archive_stream = null;
			get_next_entry = null;

			Finished ();
		}

		internal class ArchiveEntry {
			public string Name;
			public string MimeType;
			public DateTime Modified;
			public string Comment;
			public long Size;

			public string TempFile;
		}

		private string StoreStreamInTempFile (Stream stream, string extension, DateTime mtime)
		{
			if (stream == null)
				return null;

			string filename = FileSystem.GetTempFileName (extension);
			FileStream file_stream = new FileStream (filename, FileMode.OpenOrCreate, FileAccess.Write, FileShare.ReadWrite); // FileShare.ReadWrite needed for setting the mtime

			// When we dump the contents of an indexable into a file, we
			// expect to use it again soon.
			FileAdvise.PreLoad (file_stream);

			//Log.Debug ("Storing archive contents in {0}", filename);

			File.SetLastWriteTimeUtc (filename, mtime); // change this before making read-only
			Mono.Unix.Native.Syscall.chmod (filename, Mono.Unix.Native.FilePermissions.S_IRUSR);

			BufferedStream buffered_stream = new BufferedStream (file_stream);

			byte [] buffer = new byte [8192];
			long prev_pos = -1;
			int read, total_bytes_read = 0;
			bool skip_file = false;
			int stuck_count = 0;

			do {
				try {
					read = stream.Read (buffer, 0, buffer.Length);
				} catch (Exception e) {
					Log.Error (e, "Caught exception extracting data from archive {0}", Indexable.DisplayUri.ToString ());
					skip_file = true;
					break;
				}

				total_bytes_read += read;

				// Don't extract big files, to avoid filling up /tmp
				if (total_bytes_read > MAX_SINGLE_FILE) {
					Log.Debug ("10 meg threshold hit, skipping over {0}", Indexable.DisplayUri.ToString ());
					skip_file = true;
					break;
				}

				if (read > 0)
					buffered_stream.Write (buffer, 0, read);

				// Lame workaround for some gzip files which loop
				// forever with SharpZipLib.  We have to check for
				// the parser getting stuck on a certain stream
				// position.
				if (stream is GZipInputStream && read == buffer.Length) {
					if (stream.Position == prev_pos) {
						stuck_count++;

						// 20 is a fairly arbitrary value
						if (stuck_count == 20) {
							Log.Debug ("{0} appears to be broken, skipping", Indexable.DisplayUri.ToString ());
							skip_file = true;
							break;
						}
					} else {
						stuck_count = 0;
						prev_pos = stream.Position;
					}
				}
			} while (read > 0);

			buffered_stream.Close ();

			if (skip_file) {
				FileSystem.PosixDelete (filename);
				return null;
			}

			return filename;
		}

		// Special method to get ".tar.gz" and ".tar.bz2" instead of
		// just ".gz" and ".bz2"
		private string GetExtension (string filename)
		{
			string ext = Path.GetExtension (filename);
			if (ext != ".gz" && ext != ".bz2")
				return ext;

			if (filename.EndsWith (".tar.gz"))
				return ".tar.gz";
			else if (filename.EndsWith (".tar.bz2"))
				return ".tar.bz2";
			else
				return ext;
		}

		private ArchiveEntry DoGetNextEntry ()
		{
			try {
				return this.get_next_entry ();
			} catch (Exception e) {
				Log.Warn (e, "Unable to extract file from {0}", Indexable.DisplayUri.ToString ());
				return null;
			}
		}

		private ArchiveEntry GetNextEntryZip ()
		{
			ZipInputStream zip_stream = (ZipInputStream) archive_stream;
			ZipEntry zip_entry;

			do {
				zip_entry = zip_stream.GetNextEntry ();
			} while (zip_entry != null && zip_entry.IsDirectory);

			// End of the entries.
			if (zip_entry == null)
				return null;

			ArchiveEntry entry = new ArchiveEntry ();
			entry.Name = zip_entry.Name;
			entry.Modified = zip_entry.DateTime; // FIXME: Not sure zip_entry.DateTime is UTC.
			entry.Comment = zip_entry.Comment;
			entry.Size = zip_entry.Size;

			// Only index smaller subfiles, to avoid filling /tmp
			if (entry.Size > MAX_SINGLE_FILE) {
				Log.Debug ("Skipping over large file {0} in {1}", entry.Name, Indexable.DisplayUri.ToString ());
				return entry;
			}

			entry.TempFile = StoreStreamInTempFile (archive_stream, GetExtension (entry.Name), entry.Modified);
			if (entry.TempFile != null)
				entry.MimeType = XdgMime.GetMimeType (entry.TempFile);

			return entry;
		}

		private ArchiveEntry GetNextEntryTar ()
		{
			TarInputStream tar_stream = (TarInputStream) archive_stream;
			TarEntry tar_entry;

			do {
				tar_entry = tar_stream.GetNextEntry ();
			} while (tar_entry != null && tar_entry.IsDirectory);

			// End of the entries;
			if (tar_entry == null)
				return null;

			ArchiveEntry entry = new ArchiveEntry ();
			entry.Name = tar_entry.Name;
			entry.Modified = tar_entry.ModTime;
			entry.Size = tar_entry.Size;

			// Only index smaller subfiles, to avoid filling /tmp
			if (entry.Size > MAX_SINGLE_FILE) {
				Log.Debug ("Skipping over large file {0} in {1}", entry.Name, Indexable.DisplayUri.ToString ());
				return entry;
			}

			entry.TempFile = StoreStreamInTempFile (archive_stream, GetExtension (entry.Name), entry.Modified);
			if (entry.TempFile != null)
				entry.MimeType = XdgMime.GetMimeType (entry.TempFile);

			return entry;
		}

		private bool handled_single = false;

		private ArchiveEntry GetNextEntrySingle ()
		{
			if (handled_single)
				return null;

			ArchiveEntry entry = new ArchiveEntry ();

			// If there is a ExactFilename property, get name using that,
			// else use the FileInfo name
			string exact_filename = null;
			foreach (Property p in Indexable.Properties)
				if (p.Key == Property.ExactFilenamePropKey) {
					exact_filename = p.Value;
					break;
				}

			if (exact_filename != null)
				entry.Name = Path.GetFileNameWithoutExtension (exact_filename);
			else
				entry.Name = Path.GetFileNameWithoutExtension (FileInfo.Name);

			entry.Modified = FileInfo.LastWriteTimeUtc;

			entry.TempFile = StoreStreamInTempFile (archive_stream, GetExtension (entry.Name), entry.Modified);

			if (entry.TempFile != null) {
				entry.Size = new FileInfo (entry.TempFile).Length;
				entry.MimeType = XdgMime.GetMimeType (entry.TempFile);
			}

			handled_single = true;

			return entry;
		}
	}	
}
