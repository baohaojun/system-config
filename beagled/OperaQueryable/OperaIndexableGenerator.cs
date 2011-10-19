//
// OperaIndexableGenerator.cs: Generates objects that beagle can index
//
// Copyright (C) 2007 Kevin Kubasik <Kevin@Kubasik.net>
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
using System.Collections;

using Beagle;
using Beagle.Util;
using Beagle.Daemon;
using FSQ = Beagle.Daemon.FileSystemQueryable;
using ICSharpCode.SharpZipLib.GZip;

namespace Beagle.Daemon.OperaQueryable {
	
	public class OperaIndexableGenerator : IIndexableGenerator {
		private string cache_dir;
		private OperaIndexer indexer;
		private OperaHistory history;
		
		private IEnumerator history_enumerator;
		
		// Just in case we want to index more types in the future
		private readonly string[] indexed_mimetypes = new string [] {
			"text/html", "text/plain", "application/pdf"
		};
		
		public OperaIndexableGenerator(OperaIndexer indexer, string cache_dir)
		{
			this.cache_dir = cache_dir;
			this.indexer = indexer;
			if(history != null && history.GetLastRead() >= Directory.GetLastWriteTime(cache_dir)){
				history_enumerator = history.GetEnumerator();
				return;
			}
			try {
				history = new OperaHistory (Path.Combine (cache_dir, "dcache4.url"));

				history_enumerator = history.GetEnumerator ();
				
			} catch (Exception e) {
				Logger.Log.Error (e, "Failed to list cache objects in {0}", 
					Path.Combine (cache_dir, "dcache4.url"));
			}
		}
		
		public bool HasNextIndexable ()
		{
			if (history_enumerator == null)
				return false;

			while (history_enumerator.MoveNext ()) {
				OperaHistory.Row row = (OperaHistory.Row) history_enumerator.Current;
				if (Allowed (row) && ! IsUpToDate (row))
					return true;
			}

			history_enumerator = null;
			history = null; // Help the GC here
			return false;
		}

		public Indexable GetNextIndexable ()
		{
			OperaHistory.Row row = (OperaHistory.Row) history_enumerator.Current;

			try {
				return OperaRowToIndexable (row);
			} catch (Exception ex) {
				Log.Error (ex, "Unable to index {0} ({1})", row.Address, row.LocalFileName);
				return null;
			}
		}
		
		private Indexable OperaRowToIndexable (OperaHistory.Row row)
		{
			// It's unsafe to index secure content since it may contain sensitive data
			if (row.Address.Scheme == Uri.UriSchemeHttps)
				return null;
			
			Indexable indexable = new Indexable (row.Address);
			
			indexable.HitType = "WebHistory";
			indexable.MimeType = row.MimeType;
			indexable.Timestamp = row.LastVisited;
			indexable.AddProperty(Beagle.Property.New ("fixme:host",row.Address.Host));
			indexable.AddProperty (Beagle.Property.NewUnsearched ("fixme:size", row.Length));
			// hint for the filter about the charset
			indexable.AddProperty (Property.NewUnsearched (StringFu.UnindexedNamespace + "charset", row.Encoding.ToString ()));

			if(row.Compression == "gzip")	
				indexable.SetBinaryStream (new GZipInputStream (File.OpenRead (Path.Combine (cache_dir, row.LocalFileName))));
			else
				indexable.ContentUri = new Uri (Path.Combine (cache_dir, row.LocalFileName));
			
			indexer.AttributeStore.AttachLastWriteTime (Path.Combine (cache_dir, row.LocalFileName), DateTime.UtcNow);
			
			return indexable;
		}
		
		public void PostFlushHook ()
		{
		}
		
		public bool IsUpToDate (OperaHistory.Row row)
		{
			return indexer.AttributeStore.IsUpToDate (Path.Combine (cache_dir, row.LocalFileName));
		}
		
		public bool Allowed (OperaHistory.Row row)
		{
			foreach (string mime in indexed_mimetypes) {
				if (row.MimeType == mime)
					return true;
			}
			
			return false;
		}
		
		public string StatusName {
			get { return cache_dir; }
		}
	}
	
}
