//
// InotifyEventBackend.cs
//
// Copyright (C) 2005 Novell, Inc.
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

using Beagle.Util;
using Beagle.Daemon;

namespace Beagle.Daemon.FileSystemQueryable {

	public class InotifyBackend : IFileEventBackend {
		
		FileSystemQueryable queryable;
		Inotify.InotifyCallback inotify_callback;

		public InotifyBackend ()
		{
			inotify_callback = new Inotify.InotifyCallback (OnInotifyEvent);
		}

		public object CreateWatch (string path)
		{
			object watch = null;
			try {
				watch = Inotify.Subscribe (path, inotify_callback,
							   Inotify.EventType.Create
							   | Inotify.EventType.Delete
							   | Inotify.EventType.CloseWrite
							   | Inotify.EventType.MovedFrom
							   | Inotify.EventType.MovedTo
							   | Inotify.EventType.Attrib);

			}
			catch (IOException) {
				// We can race and files can disappear.  No big deal.
			}
			return watch;
		}

		public bool ForgetWatch (object watch_handle)
		{
			try {
				((Inotify.Watch) watch_handle).Unsubscribe ();
			} catch (Exception ex) {
				Logger.Log.Error (ex, "Caught exception while doing ForgetWatch");
				return false;
			}
			return true;
		}

		public void Start (FileSystemQueryable queryable)
		{
			this.queryable = queryable;
		}

		private void OnInotifyEvent (Inotify.Watch     watch,
					     string            path,
					     string            subitem,
					     string            srcpath,
					     Inotify.EventType type)
		{
			bool is_directory;
			is_directory = (type & Inotify.EventType.IsDirectory) != 0;

			queryable.ReportEventInDirectory (path);

			// The case of matched move events
			if ((type & Inotify.EventType.MovedTo) != 0 && srcpath != null) {
				queryable.HandleMoveEvent (Path.GetDirectoryName (srcpath),
							   Path.GetFileName (srcpath),
							   path, subitem, is_directory);
				return;
			}

			// Then this must be an unmatched moveto
			// An unmatched MovedTo is like a create
			if ((type & Inotify.EventType.MovedTo) != 0) { 

				// Synthesize the appropriate Create event.  Note that we could check for the
				// IsDirectory event here, but this also shrinks the race window.
				if (is_directory)
					type |= Inotify.EventType.Create;
				else
					type |= Inotify.EventType.CloseWrite;
				Logger.Log.Debug ("Synthesizing event on unpaired MoveTo", type);
			}

			// An unmatched MovedFrom is like a delete
			if ((type & Inotify.EventType.MovedFrom) != 0) {
				type |= Inotify.EventType.Delete;
				Logger.Log.Debug ("Synthesizing event on unpaired MoveFrom", type);
			}

			if ((type & Inotify.EventType.Delete) != 0) {
				queryable.HandleRemoveEvent (path, subitem, is_directory);
				return;
			}

			if ((type & Inotify.EventType.Create) != 0) {
				if (is_directory)
					queryable.HandleAddEvent (path, subitem, is_directory);
				return;
			}

			if ((type & Inotify.EventType.CloseWrite) != 0) {
				queryable.HandleAddEvent (path, subitem, is_directory);
				return;
			}

			if ((type & Inotify.EventType.Attrib) != 0) {
				queryable.HandleAttribEvent (path, subitem, is_directory);
				return;
			}

			if ((type & Inotify.EventType.QueueOverflow) != 0) {
				Logger.Log.Warn ("Inotify queue overflowed: file system is in an unknown state");
				queryable.HandleOverflowEvent ();
				return;
			}
		}
	}
}
