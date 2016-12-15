//
// FilterIgnore.cs
// Files to ignore!
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using Beagrep.Daemon;

namespace Beagrep.Filters {

	// This is a dummy filter which does "nothing" for files which are not handled. This is done to:
	// * While crawling, if FilterIgnore is set, then do not resubmit the file for indexing. 
	//   Otherwise, nothing would be set and the file would be re-indexed everytime.
	// * Instead of adding the mimetypes to conf.ignore, add it here so that users can easily add
	//   filters for these ignored types if they want
	// * Executables can set the property filetype=application

	public class FilterIgnore : Filter {

		private int version = 0;

		public FilterIgnore ()
		{
			PreLoad = false;
			SnippetMode = false;

			//base.SetVersion (version);
		}

		protected new void SetVersion (int version)
		{
			this.version += version;
			base.SetVersion (version);
		}

		protected override void RegisterSupportedTypes ()
		{
			// Add mimetypes that beagrep surely will not filter
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-object"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-sharedlib"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-java")); // .class files
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-special")); // .class files
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/x-eps"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-ole-storage")); // .db files
//			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-patch")); // patch files
		}

		protected override void DoPullProperties ()
		{
			Finished ();
		}
	}

	public class FilterApplication : FilterIgnore {
	
		public FilterApplication ()
		{
			SetFileType ("application");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-ms-dos-executable"));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-executable"));
		}
	}
}

