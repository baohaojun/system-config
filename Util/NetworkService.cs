//
// NetworkService.cs
//
// Copyright (C) 2006 Kyle Ambroff <kwa@icculus.org>
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
using System.Xml.Serialization;

namespace Beagle.Util
{
        public class NetworkService
        {
                private string name = null;
                private string uri = null;
                private string cookie = null;
                private bool is_protected;
                
                [XmlAttribute ("Name")]
                public string Name {
                        get { return name; }
                        set { name = value; }
                }
                
                [XmlAttribute ("Uri")]
                public string UriString {
                        get { return uri; }
                        set { uri = value; }
                }

                [XmlAttribute ("Password")]
                public bool IsProtected {
                        get { return is_protected; }
                        set { is_protected = value; }
                }

                [XmlAttribute ("Cookie")]
                public string Cookie {
                        get { return cookie; }
                        set { cookie = value; }
                }

                public NetworkService ()
                {                        
                }

                public NetworkService (string name, Uri uri, bool is_protected, string cookie)
                {
                        this.name = name;
                        this.uri = UriFu.UriToEscapedString (uri);
                        this.is_protected = is_protected;
                        this.cookie = cookie;
                }

                public System.Uri GetUri ()
                {
                        return UriFu.EscapedStringToUri (uri);
                }

                public void SetUri (System.Uri uri)
                {
                        this.uri = UriFu.UriToEscapedString (uri);
                }

                public override string ToString ()
                {
                        return String.Format ("{1} ({0})", uri, name);
                }
        }
}
