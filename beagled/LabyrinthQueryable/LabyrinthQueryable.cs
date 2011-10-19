//
// LabyrithQueryable.cs
//
// Copyright (C) 2006 Kevin Kubasik <kevin@kubasik.net>
// Copyright (C) 2004-2006 Novell, Inc.
// Copyright (C) 2004 Christopher Orr
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
using System.IO;
using System.Threading;
using System.Text;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Daemon.LabyrinthQueryable {

	[QueryableFlavor (Name="Labyrinth", Domain=QueryDomain.Local, RequireInotify=false)]
	public class LabyrinthQueryable  : LuceneFileQueryable, IIndexableGenerator {

		string lab_dir;

		public LabyrinthQueryable () : base ("LabyrinthIndex")
		{
			lab_dir = Path.Combine (PathFinder.HomeDir, ".gnome2");
			lab_dir = Path.Combine (lab_dir, "labyrinth");
		}

		/////////////////////////////////////////////////
					
		private void StartWorker() 
		{	
			if (! Directory.Exists (lab_dir)) {
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForExistence));
				return;
			}

			Log.Info ("Starting Labyrinth backend");

			Stopwatch stopwatch = new Stopwatch ();
			stopwatch.Start ();

			
			
			if (Inotify.Enabled)
				Inotify.Subscribe (lab_dir, OnInotifyNewNote, Inotify.EventType.CloseWrite | Inotify.EventType.Modify);

			Scheduler.Task task;
			task = NewAddTask (this);
			task.Tag = "Crawling Labyrinth Notes";
			task.Source = this;

			ThisScheduler.Add (task);
			
			stopwatch.Stop ();

			Log.Info ("labyrinth backend worker thread done in {0}", stopwatch); 
		}
		
		public override void Start () 
		{
			base.Start ();
			
			ExceptionHandlingThread.Start (new ThreadStart (StartWorker));
		}

		/////////////////////////////////////////////////

		
		public string StatusName {
			get { return "LabyrinthQueryable"; }
		}

		private IEnumerator map_files = null;

		public void PostFlushHook () { }

		public bool HasNextIndexable ()
		{
			if (map_files == null)
				map_files = DirectoryWalker.GetFileInfos (lab_dir).GetEnumerator ();

			return map_files.MoveNext ();
		}

		public Indexable GetNextIndexable ()
		{
			FileInfo file = (FileInfo) map_files.Current;

			if (! file.Exists)
				return null;

			if (IsUpToDate (file.FullName))
				return null;

			Indexable indexable = NoteToIndexable (file);
			
			return indexable;
		}
		private bool CheckForExistence ()
		{
			if (!Directory.Exists (lab_dir))
				return true;

			this.Start ();

			return false;
		}
		/////////////////////////////////////////////////

		private static Indexable NoteToIndexable (FileInfo file) 
		{
			Indexable indexable = new Indexable (UriFu.PathToFileUri(file.FullName));
			indexable.Timestamp = file.LastWriteTimeUtc;
			indexable.HitType = "Note";
			indexable.MimeType = "x-beagle/x-labyrinth-note";
			indexable.AddProperty (Property.NewUnsearched ("fixme:application","labyrinth"));
			
			return indexable;
		
		}
		private void IndexNote (FileInfo file, Scheduler.Priority priority)
		{
			
			if (! File.Exists (file.FullName))
				return;

			if (IsUpToDate (file.FullName))
				return;

			Indexable indexable = NoteToIndexable (file);
			Scheduler.Task task = NewAddTask (indexable);
			task.Priority = priority;
			task.SubPriority = 0;
			ThisScheduler.Add (task);
			
		}


		/////////////////////////////////////////////////


		private void OnInotifyNewNote(Inotify.Watch watch,
						string path, string subitem, string srcpath,
						Inotify.EventType type)
		{
			if (subitem.Length == 0 || (type & Inotify.EventType.IsDirectory) != 0)
				return;

			if ( subitem.EndsWith("map"))
				IndexNote (new FileInfo(Path.Combine (path, subitem)), Scheduler.Priority.Immediate);			
		}

		/////////////////////////////////////////////////
		

	}
}

