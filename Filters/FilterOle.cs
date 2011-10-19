//
// FilterOle.cs
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
using System.Collections;
using System.IO;
using System.Text;
using Gsf;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {
    
	public abstract class FilterOle : Beagle.Daemon.Filter {

		public FilterOle () 
		{
		}

		protected virtual void ExtractMetaData (Gsf.Input sum_stream, 
							Gsf.Input doc_stream) 
		{ 
		}
		
		protected virtual void OpenStorage (FileInfo info) {}

		protected Infile file;
		protected DocMetaData sumMeta = null;
		protected DocMetaData docSumMeta = null;
		protected string FileName;

		override protected void DoOpen (FileInfo info)
		{
			try {
				Gsf.Global.Init ();

				Input input = new InputStdio (info.FullName);
				
				if (input != null) {
					input = input.Uncompress();
					file = new InfileMSOle (input);
					input.Dispose ();
				}

				if (input == null || file == null) {
					Log.Warn ("Unable to open '{0}': input/file is null", info.FullName);
					Error ();
					return;
				}
				
				OpenStorage (info);
			} catch (Exception e) {
				Log.Warn (e, "Unable to open " + info.FullName);
				Error ();
				return;
			}
		}

		void PullMetaData (Gsf.Input sum_stream, Gsf.Input doc_stream) 
		{ 
			
			DocProp prop = null;

			sumMeta = new DocMetaData ();
			if (sum_stream != null)
				Msole.MetadataRead (sum_stream, sumMeta);
			else
				Logger.Log.Warn ("SummaryInformationStream not found in {0}", FileName);

			docSumMeta = new DocMetaData ();
			if (doc_stream != null)
				Msole.MetadataRead (doc_stream, docSumMeta);
			else
				Logger.Log.Warn ("DocumentSummaryInformationStream not found in {0}", FileName);

			if (sumMeta != null) {
				prop = sumMeta.Lookup ("dc:title");
				if (prop != null)
					AddProperty (Beagle.Property.New ("dc:title", prop.Val as string));

				prop = sumMeta.Lookup ("dc:subject");			
				if (prop != null)
					AddProperty (Beagle.Property.New ("dc:subject", prop.Val as string));

				prop = sumMeta.Lookup ("dc:description");		
				if (prop != null)
					AddProperty (Beagle.Property.New ("dc:description", prop.Val as string));

				prop = sumMeta.Lookup ("gsf:keywords");
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:keywords", prop.Val as string));

				prop = sumMeta.Lookup ("gsf:creator");
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:author", prop.Val as string));

				prop = sumMeta.Lookup ("gsf:last-saved-by");		
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:last-saved-by", prop.Val as string));

				prop = sumMeta.Lookup ("gsf:generator");		
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:generator", prop.Val as string));

				prop = sumMeta.Lookup ("gsf:template");		
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:template", prop.Val as string));
			}
			
			if (docSumMeta != null) {
				prop = docSumMeta.Lookup ("gsf:company");
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:company", prop.Val as string));

				prop = docSumMeta.Lookup ("gsf:category");
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:category", prop.Val as string));
				
				prop = docSumMeta.Lookup ("CreativeCommons_LicenseURL");
				if (prop != null)
					AddProperty (Beagle.Property.New ("fixme:license", prop.Val as string));
			}

			ExtractMetaData (sum_stream, doc_stream);
			
			if (sumMeta != null)
				sumMeta.Dispose ();
			
			if (docSumMeta != null)
				docSumMeta.Dispose ();
		}

		override protected void DoPullProperties ()
		{
			Input sum_stream = null;
			Input doc_stream = null;
			
			if (file == null) {
				Finished ();
				return;
			}
			
			try {
				sum_stream = file.ChildByName ("\u0005SummaryInformation");
				doc_stream = file.ChildByName ("\u0005DocumentSummaryInformation");

				PullMetaData (sum_stream, doc_stream);
			} catch (Exception e) {
				Logger.Log.Error (e, "Exception occurred duing DoPullProperties.");
				Error ();
			} finally {
				if (sum_stream != null)
					sum_stream.Dispose ();
				if (doc_stream != null)
					doc_stream.Dispose ();
			}
		}

		override protected void DoClose ()
		{
			if (file != null)
				file.Dispose ();
			
			// FIXME: Uncomment this when Shutdown() is available in gsf#
			// Gsf.Global.Shutdown ();
		}

		// FIXME: These are utility functions and can be useful 
		// outside this filter as well.
		public static uint GetInt32 (byte [] data, int offset) {
			return (uint)(data[offset] + (data[offset + 1] << 8) + (data[offset + 2] << 16) + (data[offset + 3] << 24));
		}
		public static ushort GetInt16 (byte [] data, int offset) {
			return (ushort)(data[offset] + (data[offset + 1] << 8));
		}

	}
}
