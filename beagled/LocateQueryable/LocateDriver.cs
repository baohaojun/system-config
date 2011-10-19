//
// LocateDriver.cs
//
// A backend which returns results of 'locate'.
// This is really used as a supplement to the FileSystem backend
// for say, when FSQ crawling is not finised yet or the user
// wants usual locate style searching of files everywhere.
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
//
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
using System.Collections.Generic;
using System.IO;
using System.Text;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.LocateQueryable {

	[QueryableFlavor (Name="Locate", Domain=QueryDomain.System, RequireInotify=false, DependsOn="Files")]
	public class LocateDriver : IQueryable {

		public LocateDriver ()
		{
		}

		public virtual void Start ()
		{
		}

		public bool AcceptQuery (Query query)
		{
			// FIXME Process [-/OR] 'source:Locate' if specified

			bool has_text = false;
			foreach (QueryPart qp in query.Parts) {
				if (! (qp is QueryPart_Text))
					continue;

				if (! has_text)
					has_text = true;
				else {
					Log.Error ("LocateDriver does not support searching for multiple words");
					return false;
				}
			}

			if (! has_text) {
				Log.Error ("LocateDriver can only search for text and does not support 'OR', 'NOT' queries.");
				return false;
			}
			    
			return true;
		}

		public void DoQuery (Query query, IQueryResult result, IQueryableChangeData data)
		{
			string search = null;
			foreach (QueryPart qp in query.Parts) {
				if (qp is QueryPart_Text) {
					search = ((QueryPart_Text) qp).Text;
					break;
				}
			}

			if (String.IsNullOrEmpty (search))
				return;

			SafeProcess pc = new SafeProcess ();
			// Double the max-hits since it is hard to tell locate to ignore
			// hidden files and directories; so we prune them later.
			// So if hidden files are returned first, you are doomed
			pc.Arguments = new string[] { "locate", "-P", "-e", "-l", (2 * query.MaxHits).ToString (), search };
			pc.RedirectStandardOutput = true;
			pc.RedirectStandardError = false;
			pc.UseLangC = true;

			try {
				pc.Start ();
			} catch (Beagle.Util.SafeProcessException e) {
				Log.Error (e, "Error while running 'locate -P -e -l {0} {1}'", (2 * query.MaxHits), search);
				return;
			}

			string match = null;
			ArrayList result_batch = new ArrayList ();
			const int MAX_QUEUED_HITS = 25;
			Hit hit;
			int count = 0;

			using (StreamReader pout = new StreamReader (pc.StandardOutput)) {
				while (count < query.MaxHits && ! pout.EndOfStream) {
					match = pout.ReadLine ();
					hit = PathToHit (match);
					if (hit == null)
						continue;

					result_batch.Add (hit);

					if (result_batch.Count >= MAX_QUEUED_HITS) {
						result.Add (result_batch);
						result_batch.Clear ();
					}
					count ++;
				}
			}

			result.Add (result_batch, count);

			pc.Close ();
		}

#if ENABLE_RDF_ADAPTER
		public ICollection DoRDFQuery (Query query)
		{
			return null;
		}
#endif

		public int DoCountMatchQuery (Query query)
		{
			return 0;
		}

		public ISnippetReader GetSnippet (string[] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			return null;
		}

		public QueryableStatus GetQueryableStatus ()
		{
			QueryableStatus status = new QueryableStatus ();

			// FIXME Add Updatedb stats
			status.ItemCount = -1;

			status.ProgressPercent = -1;
			status.IsIndexing = false;

			return status;
		}

		//////////////////////////////////////////////////
		// Convert matches to Hits

		private Hit PathToHit (string path)
		{
			// Check if hidden
			if (path.IndexOf ("/.") != -1)
				return null;

			Hit hit = new Hit ();
			hit.Uri = UriFu.PathToFileUri (path);
			hit.Timestamp = File.GetLastWriteTimeUtc (path);
                        hit.AddProperty (Property.NewUnsearched ("beagle:HitType", "File"));
			// Prevent any mimetype matching
                        hit.AddProperty (Property.NewUnsearched ("beagle:MimeType", "beagle/x-locate-result"));
                        hit.AddProperty (Property.NewUnsearched ("beagle:Source", "Locate"));
			// Use a generic enough filetype to hint there is _no_ special properties
			// for this hit
                        hit.AddProperty (Property.NewUnsearched ("beagle:FileType", "document"));
			hit.Score = 1.0;

			foreach (Property std_prop in Property.StandardFileProperties (Path.GetFileName (path), true))
				hit.AddProperty (std_prop);

			hit.AddProperty (Property.NewUnsearched (
					 Property.ParentDirUriPropKey,
					 UriFu.PathToFileUri (Path.GetDirectoryName (path))));

			return hit;
		}

	}
}

