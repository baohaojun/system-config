//
// FilterKopeteLog.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.Xml;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterKopeteLog : Beagle.Daemon.Filter {

		public FilterKopeteLog ()
		{
			SnippetMode = true;
			OriginalIsText = true;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("beagle/x-kopete-log"));
		}

		protected override void DoPullProperties ()
		{
			AddProperty (Beagle.Property.NewUnsearched ("fixme:client", "Kopete"));

			string protocol = GetProtocol (FileInfo);
			AddProperty (Beagle.Property.NewUnsearched ("fixme:protocol", protocol));

			XmlTextReader reader = new XmlTextReader (base.TextReader);

			while (reader.Read ()) {
				if (reader.NodeType != XmlNodeType.Element)
					continue;
				
				// Extract our identity and speaking to from the beginning
				// of the log
				if (reader.Name == "contact") {				
					if (reader ["type"] != null && reader ["type"] == "myself")
						AddProperty (Beagle.Property.NewUnsearched ("fixme:identity", reader ["contactId"]));
					else
						AddProperty (Beagle.Property.NewKeyword ("fixme:speakingto", reader ["contactId"]));
				}

				// Extract the actual message text
				if (reader.Name == "msg") {
					// Advance to the text node for the actual message
					reader.Read ();
				
					AppendText (reader.Value);
					AppendWhiteSpace ();
				}
			}

			// FIXME: We are dropping endtime and startime in favor of performance
			// for the time being, until we can figure out how to get
			// it out of the log file more efficiently.
			//AddProperty (Beagle.Property.NewDate ("fixme:starttime", log.StartTime));
			//AddProperty (Beagle.Property.NewDate ("fixme:endtime", log.EndTime)); 
		}

		private static string GetProtocol (FileInfo file)
		{
			// Figure out the protocol from the parent.parent or parent foldername
			if (file.Directory.Parent.Name.EndsWith ("Protocol"))
				return file.Directory.Parent.Name.Substring (0, file.Directory.Parent.Name.Length - 8).ToLower ();

			if (file.Directory.Name.EndsWith ("Protocol"))
				return file.Directory.Name.Substring (0, file.Directory.Name.Length - 8).ToLower ();

			return file.Directory.Name;
		}
	}
}
