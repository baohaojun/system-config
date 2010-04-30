//
// FileAttributesStore_ExtendedAttribute.cs
//
// Copyright (C) 2004 Novell, Inc.
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.IO;

using Beagle.Util;

namespace Beagle.Daemon {
	
	public class FileAttributesStore_ExtendedAttribute : IFileAttributesStore {

		// Version history:
		// 1: Original
		// 2: Replace LastIndexedTime with LastAttrTime
		// 3: Store EA's using a CSV format
		//    Format: "Version[2] Fingerprint,Uid,LastWriteTime,LastAttrTime,FilterVersion[3] FilterName"
		private const int EA_VERSION = 3;

		public static bool Disable = false;

		private string index_fingerprint;

		public FileAttributesStore_ExtendedAttribute (string index_fingerprint)
		{
			this.index_fingerprint = index_fingerprint;
		}

		public void Dispose () { }

		public FileAttributes Read (string path)
		{
			if (Disable)
				return null;

			try {
				string tmp = ExtendedAttribute.Get (path);

				if (tmp == null)
					return null;

				string[] csv = tmp.Split (',');

				if (int.Parse (csv [0].Substring (0, 2)) != EA_VERSION
				    || (index_fingerprint != null && csv [0].Substring (3) != index_fingerprint))
					return null;

				FileAttributes attr = new FileAttributes ();
				attr.UniqueId = GuidFu.FromShortString (csv [1]);
				attr.Path = path;
				attr.LastWriteTime = StringFu.StringToDateTime (csv [2]);
				attr.LastAttrTime = StringFu.StringToDateTime (csv [3]);

				if (! String.IsNullOrEmpty (csv [4])) {
					attr.FilterVersion = int.Parse (csv [4].Substring (0, 3));
					attr.FilterName = csv [4].Substring (4);
				}
				
				return attr;

			} catch (Exception e) {
				//Logger.Log.Debug ("Caught exception reading EAs from {0}", path);
				//Logger.Log.Debug (e);
				// FIXME: Do something smarter with the exception.
				return null;
			}
		}

		public bool Write (FileAttributes attr)
		{
			if (Disable)
				return false;

			try {
				if (ExtendedAttribute.OldExists (attr.Path, "Fingerprint"))
					DropObsoleteAttributes (attr.Path);

				string fingerprint = String.Format ("{0:00} {1}", EA_VERSION, index_fingerprint);
				string uid = GuidFu.ToShortString (attr.UniqueId);
				string mtime = StringFu.DateTimeToString (attr.LastWriteTime);

				string filter = String.Empty;

				if (attr.HasFilterInfo)
					filter = String.Format ("{0:000} {1}", attr.FilterVersion, attr.FilterName);

				attr.LastAttrTime = DateTime.UtcNow;
				string attrtime = StringFu.DateTimeToString (attr.LastAttrTime);

				string [] csv = {fingerprint, uid, mtime, attrtime, filter};
				ExtendedAttribute.Set (attr.Path, String.Join (",", csv));
							
				return true;
			} catch (IOException e) {
				// An IOException here probably means that we don't have the right
				// permissions to set the EAs.  We just fail silently and return false rather
				// than spewing a bunch of scary exceptions.
				//Logger.Log.Debug (e);
				return false;
			} catch (Exception e) {
				//Logger.Log.Debug (e, "Caught exception writing EAs to {0}", attr.Path);
				// FIXME: Do something smarter with the exception.
				return false;
			}
		}

		public void Drop (string path)
		{
			if (Disable)
				return;

			try {
				ExtendedAttribute.Remove (path);
			} catch {
				// FIXME: Do something smarter with the exception.
			}
		}

		// IMPORTANT: Remove this post 0.3.3 release!
		private void DropObsoleteAttributes (string path)
		{
			try {
				ExtendedAttribute.RemoveOld (path, "Fingerprint");
				ExtendedAttribute.RemoveOld (path, "Uid");
				ExtendedAttribute.RemoveOld (path, "MTime");
				ExtendedAttribute.RemoveOld (path, "AttrTime");
				ExtendedAttribute.RemoveOld (path, "Filter");

				// And some others from Joe's favorite list :-)
				ExtendedAttribute.RemoveOld (path, "Name");
				ExtendedAttribute.RemoveOld (path, "IndexTime");
			} catch { }
		}

		// There are no transactions for EAs
		
		public void BeginTransaction ()
		{ }

		public void CommitTransaction ()
		{ }

	}
}
