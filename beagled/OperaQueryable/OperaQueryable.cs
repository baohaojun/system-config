//
// OperaQueryable.cs: Everything starts from here
//
// Copyright (C) 2007 Kevin Kubasik <Kevin@Kubasik.net>
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
using System.IO;
using System.Threading;

using Beagle;
using Beagle.Util;

namespace Beagle.Daemon.OperaQueryable {

	[QueryableFlavor (Name = "Opera",  Domain = QueryDomain.Local, RequireInotify = false)]
	public class OperaQueryable : LuceneQueryable {
		private string root_dir;
		private OperaIndexer indexer;
		
		public OperaQueryable () :
			base ("OperaIndex")
		{
			this.root_dir = Path.Combine (PathFinder.HomeDir, ".opera");
		}
		
		public override void Start ()
		{
			base.Start ();
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}
		
		private void StartWorker ()
		{
			if (!Directory.Exists (root_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}
			
			Logger.Log.Info ("Starting Opera history backend");
			Stopwatch watch = new Stopwatch ();
			watch.Start ();
			
			//State = QueryableState.Crawling;
			indexer = new OperaIndexer (this, this.FileAttributesStore, root_dir);
			indexer.Crawl ();
			//State = QueryableState.Idle;
			
			watch.Stop ();
			Logger.Log.Info ("Opera history backend done in {0}s", watch.ElapsedTime);
		}
		
		private bool CheckForExistence ()
		{
			if (!Directory.Exists (root_dir))
				return true;
			
			StartWorker ();
			return false;
		}
	}

}
