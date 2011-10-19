using System;

using Beagle;
using Beagle.Util;

namespace Beagle.Search.Tiles {

	public class HitFlavor {
		
		private string uri;
		private string type;
		private string mimetype;

		public HitFlavor (string uri)
		{
			this.uri = uri;
		}

		public HitFlavor (string uri, string type, string mimetype)
		{
			this.uri = uri;
			this.type = type;
			this.mimetype = mimetype;
		}

		public string Uri { 
			get { return uri; }
		}

		public string Type {
			get { return type; }
		}

		public string MimeType {
			get { return mimetype; }
		}

		public bool IsMatch (Hit hit)
		{
			return (uri == null || StringFu.GlobMatch (uri, hit.EscapedUri))
				&& (type == null || StringFu.GlobMatch (type, hit.Type))
				&& (mimetype == null || StringFu.GlobMatch (mimetype, hit.MimeType));
		}
		
		public int Weight {
                        get {
                                int weight = 0;
				
                                if (mimetype != null)
                                        weight += 4;
                                if (type != null)
                                        weight += 2;
                                if (uri != null)
                                        weight += 1;

				return weight;
                        }
                }
	}
}
