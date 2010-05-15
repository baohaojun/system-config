//
// WebServer.cs
//
// This class knows the logic behind handling all the static web pages for webbeagle.
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Collections.Generic;
using System.Net;
using System.Text;

using Beagle.Util;

namespace Beagle.Daemon {

	class WebServer {

		struct PageMapping {
			public string Filename;
			public string ContentType;

			public PageMapping (string filename, string content_type)
			{
				this.Filename = filename;
				this.ContentType = content_type;
			}
		}

		private static Dictionary<string, PageMapping> mappings;

		static string webserver_dir;

		static WebServer ()
		{
			mappings = new Dictionary<string, PageMapping> ();

			mappings.Add ("/", new PageMapping ("index.xml", "text/xml; charset=utf-8"));
			mappings.Add ("/mappings.xml", new PageMapping ("mappings.xml", "text/xml; charset=utf-8"));
			mappings.Add ("/opensearch.xml", new PageMapping ("opensearch.xml", "text/xml; charset=utf-8"));
			mappings.Add ("/index.xsl", new PageMapping ("index.xsl", "text/xml; charset=utf-8"));
			mappings.Add ("/statusresult.xsl", new PageMapping ("statusresult.xsl", "text/xml; charset=utf-8"));
			mappings.Add ("/hitresult.xsl", new PageMapping ("hitresult.xsl", "text/xml; charset=utf-8"));
			mappings.Add ("/help.html", new PageMapping ("help.html", "text/html; charset=utf-8"));
			mappings.Add ("/default.css", new PageMapping ("default.css", "text/css"));
			// If E4X is needed, change the content-type here
			mappings.Add ("/default.js", new PageMapping ("default.js", "text/javascript"));
			mappings.Add ("/propname-table.js", new PageMapping ("propname-table.js", "text/javascript"));
			mappings.Add ("/images/title_bg.png", new PageMapping ("images/title_bg.png", "image/png"));
			mappings.Add ("/images/beagle-logo.png", new PageMapping ("images/beagle-logo.png", "image/png"));
			mappings.Add ("/images/busy-animation.gif", new PageMapping ("images/busy-animation.gif", "image/gif"));
			mappings.Add ("/images/favicon.png", new PageMapping ("images/favicon.png", "image/png"));
			mappings.Add ("/images/system-search.png", new PageMapping ("images/system-search.png", "image/png"));

			webserver_dir = Environment.GetEnvironmentVariable ("BEAGLE_WEBSERVER_DIR");
			if (webserver_dir != null) {
				webserver_dir = Path.GetFullPath (webserver_dir);
				if (! Directory.Exists (webserver_dir))
					Log.Error ("BEAGLE_WEBSERVER_DIR ({0}) does not exist. Web interface disabled", webserver_dir);
				else
					Log.Info ("Web interface root: {0}", webserver_dir);
				return;
			}

			webserver_dir = Path.Combine (ExternalStringsHack.DataDir, "beagle");
			webserver_dir = Path.Combine (webserver_dir, "webinterface");
			if (! Directory.Exists (webserver_dir))
				webserver_dir = null;

			if (webserver_dir == null)
				Log.Error ("No BEAGLE_WEBSERVER_DIR found. Web interface disabled");
			else
				Log.Info ("Web interface root: {0}", webserver_dir);
		}

		static byte[] buffer = new byte [1024];

#if false

		internal static void HandleStaticPages (HttpListenerContext context)
		{
			context.Response.StatusCode = 404;
			context.Response.Close ();
			return;
		}
# else
		internal static void HandleStaticPages (HttpListenerContext context)
		{
			if (webserver_dir == null) {
				context.Response.StatusCode = 404;
				context.Response.Close ();
				return;
			}

			string request_path = context.Request.Url.LocalPath;
			//Log.Debug ("GET request:" + request_path);

			context.Response.KeepAlive = false;
			context.Response.StatusCode = (int) HttpStatusCode.OK;

			if (! mappings.ContainsKey (request_path)) {
				context.Response.StatusCode = 404;
				context.Response.Close ();
				return;
			}

			// Else serve the page
			PageMapping mapping = mappings [request_path];
			context.Response.ContentType = mapping.ContentType;

			string path = Path.Combine (webserver_dir, mapping.Filename);

			using (BinaryReader r = new BinaryReader (new FileStream (path, FileMode.Open, FileAccess.Read))) {
				using (BinaryWriter w = new BinaryWriter (context.Response.OutputStream)) {

					int count = 1024;
					while (count == 1024) {
						count = r.Read (buffer, 0, count);
						if (count == 0)
							break;
						w.Write (buffer, 0, count);
					}
				}
			}

			context.Response.Close ();
		}
#endif
	}
}
