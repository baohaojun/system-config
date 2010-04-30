//
// QueryKeywordMapping.cs
//
// Copyright (C) 2006,2007 Debajyoti Bera
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
using System.Xml.Serialization;

namespace Beagle.Util {

	// FIXME handle i18n issues... user might use an i18n-ised string
	public class QueryKeywordMapping {
		private string keyword, propertyname, description = null;
		private bool is_keyword = false;

		[XmlAttribute ("QueryKeyword")]
		public string Keyword {
			get { return keyword; }
			set { keyword = value; }
		}

		[XmlAttribute ("BeagleProperty")]
		public string PropertyName {
			get { return propertyname; }
			set { propertyname = value; }
		}

		[XmlText]
		public string Description {
			get { return description; }
			set { description = value; }
		}

		[XmlAttribute ("Tokenize")]
		public bool Tokenize {
			get { return ! is_keyword; }
			set { is_keyword = ! value; }
		}

		[XmlIgnore]
		public bool IsKeyword {
			get { return is_keyword; }
		}
	}
}
