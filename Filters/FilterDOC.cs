//
// FilterDOC.cs: MS Word filter.  Uses an external extractor utility to
//               actually get the text.
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.IO;
using System.Runtime.InteropServices;

using Beagle.Util;
using Beagle.Daemon;

using Gsf;

namespace Beagle.Filters {
    
	public class FilterDOC : FilterOle {

		//////////////////////////////////////////////////////////

		public FilterDOC () 
		{
			SnippetMode = true;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/msword"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.ms-word"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-msword"));
		}
		
		override protected void OpenStorage (FileInfo info)
		{
			FileName = info.FullName;
		}

		override protected void ExtractMetaData (Gsf.Input sumStream, Gsf.Input docSumStream)
		{
			int count = 0;
			DocProp prop = null;

			if (sumMeta != null) {
				prop = sumMeta.Lookup ("gsf:word-count");
				if (prop != null)
					count = (int) prop.Val;
				if (count > 0)
					AddProperty (Beagle.Property.NewUnsearched ("fixme:word-count", count));

				count = 0;
				prop = sumMeta.Lookup ("gsf:page-count");		
				if (prop != null)
					count = (int) prop.Val;
				if (count > 0)
					AddProperty (Beagle.Property.NewUnsearched ("fixme:page-count", count));
			}
		}

		private bool pull_started = false;
		private SafeProcess pc;
		private StreamReader pout;

		private bool RunExtractor ()
		{
			string extractor_path, exe;

			// Hack, along with magic in beagled-index-helper.in
			// and tools/wrapper.in to make this work in the
			// uninstalled case.
			extractor_path = Environment.GetEnvironmentVariable ("BEAGLE_TOOL_PATH");

			if (extractor_path != null)
				exe = Path.Combine (extractor_path, "beagle-doc-extractor");
			else
				exe = "beagle-doc-extractor";

			pc = new SafeProcess ();
			pc.Arguments = new string [] { exe, FileInfo.FullName };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = true;
			pc.UseLangC = true;

			// Let beagle-doc-extractor run for 90 CPU seconds, max.
			pc.CpuLimit = 90;

			// Some documents make wv1 go crazy with memory.  Limit
			// it to 100 megs of address space, too.
			pc.MemLimit = 100*1024*1024;

			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e);
				Error ();
				return false;
			}

			pout = new StreamReader (pc.StandardOutput);
			pull_started = true;

			return true;
		}

		override protected void DoPull ()
		{
			// RunExtractor() calls Error() if it fails
			if (! pull_started && ! RunExtractor ())
				return;

			string line = pout.ReadLine ();
			if (line == null) {
				Finished ();
				return;
			}

			if (line.StartsWith ("**BREAK**"))
				AppendStructuralBreak ();
			else if (line.StartsWith ("**HOT**")) {
				string l = line.Substring (7);
				AppendText (l, l);
			} else
				AppendText (line);
		}

		override protected void DoClose ()
		{
			base.DoClose ();

			if (! pull_started)
				return;

			pout.Close ();

			pout = new StreamReader (pc.StandardError);
			
			string line;
			while ((line = pout.ReadLine ()) != null)
				Log.Warn ("doc extractor [{0}]: {1}", Indexable.Uri, line);

			pout.Close ();
			pc.Close ();
		}
	}
}
