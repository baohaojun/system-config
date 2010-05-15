//
// FilterEmpathyLog.cs
//
// Copyright (C) 2007 Kevin Kubasik <kevin@kubasik.net>
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
using System.Xml.XPath;
using System.Collections.Generic;

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Filters {

	public class FilterEmpathyLog : Beagle.Daemon.Filter {

		public FilterEmpathyLog ()
		{
			SnippetMode = true;
			OriginalIsText = true;
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("beagle/x-empathy-log"));
		}

		protected override void DoPullProperties ()
		{
			AddProperty (Beagle.Property.NewUnsearched ("fixme:client", "Empathy"));


			XmlTextReader reader = new XmlTextReader (base.TextReader);
			XPathDocument xdoc = new XPathDocument(reader);
			XPathNavigator xnav = xdoc.CreateNavigator();
			XPathNodeIterator xiter =  xnav.Select("//message");
			bool have_identity = false;
			List<string> speakingtoids = new List<string>();
			//FIXME: GetStarts and Finishs
//			DateTime start = DateTime.MinValue;
//			DateTime end = DateTime.MinValue;
			xiter.MoveNext();
			while(xiter.MoveNext()){

				this.AppendText (xiter.Current.Value.ToString());
				AppendWhiteSpace ();
				
				if(xiter.Current.GetAttribute ("isuser",String.Empty) == "true"&& !have_identity){
					AddProperty (Beagle.Property.NewUnsearched ("fixme:identity",xiter.Current.GetAttribute("id",String.Empty)));
					have_identity = true;
				}else{
					if(!speakingtoids.Contains (xiter.Current.GetAttribute("id",String.Empty))){ 
						AddProperty (Beagle.Property.New ("fixme:speakingto", xiter.Current.GetAttribute ("id", String.Empty)));
						AddProperty (Beagle.Property.New ("fixme:speakingto_alias", xiter.Current.GetAttribute ("name", String.Empty)));
						speakingtoids.Add (xiter.Current.GetAttribute("id",String.Empty));
					}
				}
			}
			
		}

		
	}
}
