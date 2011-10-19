//
// RemoteControl.cs
//
// Copyright (C) 2004-2005 Novell, Inc.
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

	public class DaemonInformationRequest : RequestMessage {
		/* User can request one or more of the four information. */
		public bool GetVersion, GetSchedInfo, GetIndexStatus, GetIsIndexing;

		// For backward compatibility
		public DaemonInformationRequest () : this (true, true, true, true) { }

		public DaemonInformationRequest (
			bool get_version,
			bool get_scheduler_info,
			bool get_index_status,
			bool get_is_indexing)
		{
			this.GetVersion = get_version;
			this.GetSchedInfo = get_scheduler_info;
			this.GetIndexStatus = get_index_status;
			this.GetIsIndexing = get_is_indexing;
		}
	}

	// These requests have no interesting client-side state
	public class ShutdownRequest : RequestMessage { }

	public class ReloadConfigRequest : RequestMessage { }

	public class OptimizeIndexesRequest : RequestMessage { }

	public class DaemonInformationResponse : ResponseMessage {
		public string Version = null;

		public SchedulerInformation SchedulerInformation = null;

		[XmlArray]
		[XmlArrayItem (ElementName = "QueryableStatus", Type = typeof (QueryableStatus))]
		public ArrayList IndexStatus = null;

		public bool IsIndexing = false;

		// Methods and properties for backward compatibility and general utility
		// The names of the properties dont match the corresponding method names,
		// this is to not break clients out there.

		[XmlIgnore]
		public string HumanReadableStatus {
			get {
				if (SchedulerInformation == null)
					return null;

				return SchedulerInformation.ToHumanReadableString ();
			}
		}
		
		[XmlIgnore]
		public string IndexInformation {
			get {
				if (IndexStatus == null)
					return null;

				StringBuilder builder = new StringBuilder ('\n');

				foreach (QueryableStatus status in IndexStatus) {
					builder.Append ("Name: ").Append (status.Name).Append ('\n');
					builder.Append ("Count: ").Append (status.ItemCount).Append ('\n');
					builder.Append ("Crawling: ").Append (status.IsIndexing);

					if (status.ProgressPercent != -1)
						builder.Append (" (").Append (status.ProgressPercent).Append ("%)");

					builder.Append ("\n\n");
				}

				return builder.ToString ();
			}
		}

	}
}
