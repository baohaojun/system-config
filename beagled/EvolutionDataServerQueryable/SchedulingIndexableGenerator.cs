//
// SchedulingIndexableGenerator.cs
//
// Copyright (C) 2007 Novell, Inc.
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
using System.Collections.Generic;

using Beagle.Util;

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	public class SchedulingIndexableGenerator : IIndexableGenerator {

		private LuceneQueryable queryable;
		private Scheduler.Task self_task;
		private Scheduler.Priority highest_prio = Scheduler.Priority.Idle;
		private string name;
		private Queue <Indexable> indexables = new Queue <Indexable> ();

		public SchedulingIndexableGenerator (LuceneQueryable queryable, string name)
		{
			this.queryable = queryable;
			this.name = name;
		}

		public void Add (Indexable indexable, Scheduler.Priority priority)
		{
			lock (indexables) {
				indexables.Enqueue (indexable);

				if (priority > highest_prio)
					highest_prio = priority;

				if (self_task == null) {
					self_task = queryable.NewAddTask (this);
					self_task.Priority = highest_prio;
					queryable.ThisScheduler.Add (self_task);
				} else {
					self_task.Priority = highest_prio;
				}
			}
		}

		public bool HasNextIndexable ()
		{
			lock (indexables)
				return (indexables.Count > 0);
		}

		public Indexable GetNextIndexable ()
		{
			Indexable indexable;

			lock (indexables)
				indexable = indexables.Dequeue ();

			return indexable;
		}

		public string StatusName {
			get { return name; }
		}

		public void PostFlushHook ()
		{
			lock (indexables) {
				self_task.Description = String.Format ("{0} indexables to process", indexables.Count);

				if (indexables.Count > 0)
					self_task.Reschedule = true;
				else {
					// Instruct the scheduler to not reschedule
					// this task. Otherwise AddIndexableGenerator
					// might reschedule this task once more
					// to figure out there is no more
					// indexables.
					self_task.Reschedule = false;
					self_task = null;
				}
			}
		}
	}
}
