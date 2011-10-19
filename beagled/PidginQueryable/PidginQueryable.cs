//
// PidginQueryable.cs
//
// Copyright (C) 2004 Novell, Inc.
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
using System.Text;
using System.Threading;
using System.Collections;
using System.Collections.Generic;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.PidginQueryable {

	[QueryableFlavor (Name="Pidgin", Domain=QueryDomain.Local, RequireInotify=false)]
	public class PidginQueryable : LuceneFileQueryable {

		private PidginBuddyListReader list = new PidginBuddyListReader ();
		private const uint polling_interval = 60000;

		// 1: Add the speaking to persons alias as a keyword
		const int MINOR_VERSION = 1;

		public PidginQueryable () : base ("PidginIndex", MINOR_VERSION)
		{
		}

		/////////////////////////////////////////////////
					
		private void StartWorker() 
		{
			bool gaim_exists = Directory.Exists (Path.Combine (PathFinder.HomeDir, ".gaim"));
			gaim_exists = gaim_exists && Directory.Exists (Path.Combine (Path.Combine (PathFinder.HomeDir, ".gaim"), "logs"));
			bool pidgin_exists = Directory.Exists (Path.Combine (PathFinder.HomeDir, ".purple"));
			pidgin_exists = pidgin_exists && Directory.Exists (Path.Combine (Path.Combine (PathFinder.HomeDir, ".purple"), "logs"));

			if (!pidgin_exists && !gaim_exists) {
				GLib.Timeout.Add (polling_interval, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			Log.Info ("Starting Pidgin IM log backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();
			
			if (gaim_exists) {
				new PidginIndexableGenerator (this, Path.Combine (PathFinder.HomeDir, ".gaim"));
			}

			if (pidgin_exists) {
				new PidginIndexableGenerator (this, Path.Combine (PathFinder.HomeDir, ".purple"));
			}

			stopwatch.Stop ();
			
			Log.Info ("Pidgin log backend worker started in {0}", stopwatch); 
		}
		
		public override void Start () 
		{
			base.Start ();
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		private bool CheckForExistence ()
		{
			bool gaim_exists = Directory.Exists (Path.Combine (PathFinder.HomeDir, ".gaim"));
			gaim_exists = gaim_exists && Directory.Exists (Path.Combine (Path.Combine (PathFinder.HomeDir, ".gaim"), "logs"));
			bool pidgin_exists = Directory.Exists (Path.Combine (PathFinder.HomeDir, ".purple"));
			pidgin_exists = pidgin_exists && Directory.Exists (Path.Combine (Path.Combine (PathFinder.HomeDir, ".purple"), "logs"));

			if (gaim_exists || pidgin_exists) {
				this.Start ();
				return false;
			}

			return true;
		}

		/////////////////////////////////////////////////

		protected override bool HitFilter (Hit hit) 
		{
			string speakingto = hit ["fixme:speakingto"];

			// We have no idea who we're speaking to.  Bad, but we
			// still want to present it.
			if (String.IsNullOrEmpty (speakingto))
				return true;

			ImBuddy buddy = list.Search (speakingto);
			
			// We might still want to see a chat even if someone's
			// not on our buddy list.
			if (buddy == null) 
				return true;
			
			if (! String.IsNullOrEmpty (buddy.Alias))
 				hit.AddProperty (Property.NewKeyword ("fixme:speakingto_alias", buddy.Alias));
			
 			if (! String.IsNullOrEmpty (buddy.BuddyIconLocation))
 				hit.AddProperty (Property.NewUnsearched ("fixme:speakingto_icon", buddy.BuddyIconLocation));
			
			return true;
		}

		public override ISnippetReader GetSnippet (string [] query_terms, Hit hit, bool full_text, int ctx_length, int snp_length)
		{
			TextReader reader = TextCache.UserCache.GetReader (hit.Uri);

			if (reader == null)
				return null;

			string line = reader.ReadLine ();

			if (line[0] == '<')
				reader = new HtmlRemovingReader (reader);

			return SnippetFu.GetSnippet (query_terms, reader, full_text, ctx_length, snp_length);
		}

		public ImBuddyListReader ImBuddyListReader {
			get { return list; }
		}
	}
}

