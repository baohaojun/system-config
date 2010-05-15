//
// Versioned.cs
//
// Copyright (C) 2004 Novell, Inc.
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
using System.Xml.Serialization;

using Beagle.Util;

namespace Beagle {

	public class Versioned {

		protected DateTime timestamp = DateTime.MinValue;

		public bool ValidTimestamp {
			get { return timestamp.Ticks > 0; }
		}

		[XmlIgnore]
		public DateTime Timestamp {
			get { return timestamp; }
			set { timestamp = value; }
		}

		[XmlAttribute ("Timestamp")]
		public string TimestampAsString {
			get { return StringFu.DateTimeToString (timestamp); }
			set { timestamp = StringFu.StringToDateTime (value); }
		}

		public bool IsObsoletedBy (DateTime timestamp)
		{
			return !ValidTimestamp || Timestamp < timestamp;
		}

		public bool IsObsoletedBy (Versioned other)
		{
			// We are never obsoleted by null.
			if (other == null)
				return false;

			// Anything with a valid timestamp always is
			// more recent than something w/o a timestamp.
			if (ValidTimestamp || other.ValidTimestamp) {
				if (other.ValidTimestamp)
					return IsObsoletedBy (other.Timestamp);
				else
					return false;
			}

			return false;
		}

		public bool IsNewerThan (DateTime timestamp)
		{
			return ValidTimestamp && Timestamp > timestamp;
		}

		public bool IsNewerThan (Versioned other)
		{
			// We are always newer than null.
			if (other == null)
				return true;

			// Anything with a valid timestamp always is
			// more recent than something w/o a timestamp.
			if (ValidTimestamp || other.ValidTimestamp) {
				if (other.ValidTimestamp)
					return IsNewerThan (other.Timestamp);
				else
					return true;
			}

			return false;
		}
	}
}
