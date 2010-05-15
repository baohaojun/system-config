//
// FilterChm.cs : Trivial implementation of a CHM filter.
//
// Copyright (C) 2005,2006 Miguel Cabrera <mfcabrera@gmail.com>
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
using System.Text;
using HtmlAgilityPack;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterChm : FilterHtml  {
		
		ChmFile chmFile;
	
		public FilterChm () : base()
		{
			SnippetMode = true;
			SetFileType ("documentation");
		}
		
		override protected void DoOpen (FileInfo info) 
		{
					
			chmFile = new ChmFile ();
			

			try {
				chmFile.Load(info.FullName);
			} catch (Exception e) {
				Logger.Log.Warn (e, "Could not load {0}:", info.Name);
				Finished ();
				return;
			}

			AddProperty (Beagle.Property.New ("dc:title", chmFile.Title));
			chmFile.ParseContents (FilterFileContents);
			Finished ();
		}

		public void FilterFileContents(TextReader text) {
			
			HtmlDocument doc = new HtmlDocument ();
			doc.StreamMode = true;
			doc.ReportNode += HandleNodeEvent;
			
			try {
				doc.Load (text);
				
			} 
			catch (Exception e) {
				Logger.Log.Warn (e, "Error parsing file contents");
				//Console.WriteLine (e.Message);
				//Console.WriteLine (e.StackTrace);
			}
					

		}

		override protected void  DoClose() 
		{
			chmFile.Dispose ();
		}
		
		override protected  void  RegisterSupportedTypes()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-chm"));
		}

	}
}
