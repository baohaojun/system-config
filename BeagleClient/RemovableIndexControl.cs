//
// RemovableIndexControl.cs
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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

using System.Collections;
using System.Text;
using System.Xml;
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

	public class RemovableIndexRequest : RequestMessage {
		// To Mount or Unmount
		public bool Mount;

		// 1. path to the config file and the index
		public string IndexDir;

		// 2. path where the removable index is mounted
		// Separate #3 is needed in case users want to search unmounted media
		public string MountDir;
	}

	public class RemovableIndexResponse : ResponseMessage {
		// If success, return the name of Static source.
		// Otherwise return null.
		public string Source;
	}
}
