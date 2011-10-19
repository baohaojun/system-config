//
// Scheduler.cs
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

using System;
using System.Collections;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;

namespace Beagle.Util {

	public class Scheduler {

		// Fire an event if there are no tasks left to execute.
		public delegate void EmptyQueueDelegate ();
		public event EmptyQueueDelegate EmptyQueueEvent;

		private static bool Debug = Beagle.Util.Debug.Enabled ("Scheduler");

		public enum Priority {

			Shutdown    = 0, // Do it on shutdown 

			Idle        = 1, // Execute only when the whole machine is idle
			                 // Probably should be reserved for computationally-expensive stuff
			                 // FIXME: These are not properly scheduled right now

			Maintenance = 2, // Only execute when there are no lower-priority
			                 // tasks from the same source to execute instead

			Delayed     = 3, // Do it soon

			Immediate   = 4, // Do it right now
		}

		public delegate void Hook ();
		public delegate void TaskHook (Task task);

		//////////////////////////////////////////////////////////////////////////////

		public abstract class Task : IComparable {

			private string tag = null;
			private Priority priority = Priority.Delayed;
			private int sub_priority = 0;
			private DateTime trigger_time = DateTime.MinValue;
			private DateTime timestamp; // when added to the scheduler

			// Some metadata
			public string Creator;
			public string Description;

			// Current status, to be implemented by instances
			protected virtual string StatusName {
				get { return null; }
			}

			public object Source = null; // this is just an opaque identifier

			public ITaskCollector Collector = null;
			public double Weight = 1.0;

			public bool Reschedule = false;

			private ArrayList task_groups = null;
			private TaskGroupPrivate child_task_group = null;

			///////////////////////////////


			// The tag is the task's unique identifier
			public string Tag {

				get { return tag; }

				set {
					// Don't allow us to change the tag of a scheduled task
					if (tag == null || scheduler == null)
						tag = value;
					else
						throw new Exception ("Can't change tag of " + tag + "!");
				}
			}

			public Priority Priority {

				get { return priority; }

				set { 
					if (priority != value) {
						priority = value;
						Recompute ();
					}
				}
			}

			public int SubPriority {

				get { return sub_priority; }

				set {
					if (sub_priority != value) {
						sub_priority = value;
						Recompute ();
					}
				}
			}

			public DateTime TriggerTime {

				get { return trigger_time; }

				set {
					if (trigger_time != value) {
						trigger_time = value;
						Recompute ();
					}
				}
			}

			public DateTime Timestamp {
				get { return timestamp; }
			}
			
			///////////////////////////////

			public void AddTaskGroup (TaskGroup group)
			{
				if (task_groups == null)
					task_groups = new ArrayList ();
				task_groups.Add (group);
			}

			private void IncrementAllTaskGroups ()
			{
				if (task_groups != null) {
					foreach (TaskGroupPrivate group in task_groups) {
						if (! group.Finished)
							group.Increment ();
					}
				}
			}

			private void DecrementAllTaskGroups ()
			{
				if (task_groups != null) {
					foreach (TaskGroupPrivate group in task_groups) {
						if (! group.Finished)
							group.Decrement ();
					}
				}
			}

			private void TouchAllTaskGroups ()
			{
				if (task_groups != null) {
					foreach (TaskGroupPrivate group in task_groups) {
						if (! group.Finished)
							group.Touch ();
					}
				}
			}

			///////////////////////////////

			private Scheduler scheduler = null;

			public void Schedule (Scheduler scheduler)
			{
				if (this.cancelled)
					return; // do not schedule a cancelled task

				// Increment the task groups the first
				// time a task is scheduled.
				if (this.scheduler == null)
					IncrementAllTaskGroups ();
				this.timestamp = DateTime.Now;
				this.scheduler = scheduler;
				this.cancelled = false;
			}

			private void Recompute ()
			{
				if (scheduler != null)
					scheduler.Recompute ();
			}

			///////////////////////////////

			// A blocked task will not execute.
			public bool Blocked {
				get {
					// Block the task if we have unexecuted children
					return child_task_group != null && ! child_task_group.Finished;
				}
			}
			
			///////////////////////////////

			private bool cancelled = false;

			public bool Cancelled {
				get { return cancelled; }
			}

			public void Cancel ()
			{
				Cancel (null);
			}

			public void Cancel (string reason)
			{
				if (! cancelled) {
					AddToCancelledTaskList (reason);
					DecrementAllTaskGroups ();
					Cleanup (); // clean up after cancelled tasks
				}
				cancelled = true;
			}

			private void AddToCancelledTaskList (string reason)
			{
				string task_desc = String.Format ("Cancelled task: {5}\n"  +
								  "        Tag: {0}\n" +
								  "    Creator: {1}\n" +
								  "Description: {2}\n" +
								  "   Priority: {3} ({4})\n", 
								  Tag, Creator, Description, Priority, SubPriority,
								  (reason == null ? String.Empty : reason));
				if (scheduler != null)
					scheduler.ReportCancelledTask (task_desc);
			}

			///////////////////////////////

			// The Task's count keeps track of how many
			// times it has been executed.

			private int count = 0;
			
			public int Count {
				get { return count; }
			}

			// Keeps track of how many times the task was executed but
			// there was some exception
			private int misfires = 0;

			// Allow at most this many exceptions per task
			// This is a bit high since some tasks like the generators are
			// long running tasks. This should be a high enough number
			// to tolerate some exceptional cases but stop the task
			// in case it went into an infinite loop with exceptions.
			// Ideally all exceptions should be caught and handled downstream.
			const int MAX_TASK_EXCEPTION = 200;

			///////////////////////////////
			
			public void SpawnChild (Task child_task)
			{
				if (child_task_group == null)
					child_task_group = new TaskGroupPrivate ("Children of " + Tag, null, null);
				child_task.AddTaskGroup (child_task_group);
				child_task.Source = this.Source;
				scheduler.Add (child_task);
			}

			///////////////////////////////
			
			public void DoTask ()
			{
				if (! cancelled) {
					if (Debug)
						Logger.Log.Debug ("Starting task {0}", Tag);
					child_task_group = null;
					Reschedule = false;
					TouchAllTaskGroups ();

					Stopwatch sw = new Stopwatch ();
					sw.Start ();
						
					try {
						DoTaskReal ();
					} catch (Exception ex) {
						misfires ++;
						Logger.Log.Warn (ex,
								 "Caught exception in DoTaskReal\n" +
								 "        Tag: {0}\n" +
								 "    Creator: {1}\n" +
								 "Description: {2}\n" +
								 "   Priority: {3} ({4})", 
								 Tag, Creator, Description, Priority, SubPriority);
						if (misfires >= MAX_TASK_EXCEPTION) {
							Log.Warn ("More than {5} exceptions in DoTaskReal. Disabling further execution of task:\n" +
								 "        Tag: {0}\n" +
								 "    Creator: {1}\n" +
								 "Description: {2}\n" +
								 "   Priority: {3} ({4})", 
								 Tag, Creator, Description, Priority, SubPriority, MAX_TASK_EXCEPTION);
							Cancel ("Exceptions in DoTaskReal");
						}
					}
					sw.Stop ();
					if (Debug)
						Logger.Log.Debug ("Finished task {0} in {1}", Tag, sw);

					if (cancelled) {
						return;
					} else if (Reschedule) {
						++count;
						if (Debug)
							Log.Debug ("Rescheduling task {0}", Tag);
						scheduler.Add (this); // re-add ourselves
					} else {
						DecrementAllTaskGroups ();
						scheduler = null;
					}
				}
			}

			protected abstract void DoTaskReal ();

			///////////////////////////////

			// Clean-up is called whenever we know that a task will never
			// be executed.  It is never called on tasks for who DoTaskReal
			// has been called (except when rescheduled).  Cleanup is also
			// called when a task is cancelled.

			public void Cleanup ()
			{
				try {
					DoCleanup ();
				} catch (Exception ex) {
					Logger.Log.Warn (ex, "Caught exception cleaning up task '{0}'", Tag);
				} finally {
					Reschedule = false;
					scheduler = null;
				}
			}

			protected virtual void DoCleanup ()
			{
				// Do nothing by default
			}

			///////////////////////////////

			// Sort from lowest to highest priority
			// FIXME: This does not define a total ordering
			// on the set of all tasks, so use it with care.
			public int CompareTo (object obj)
			{
				Task other = obj as Task;
				if (other == null)
					return 1;

				Priority this_priority;
				Priority other_priority;

				this_priority = this.Priority;
				other_priority = other.Priority;

				// To other sources, Maintenance tasks looks like
				// Delayed tasks.
				if (this.Source != other.Source) {
					if (this_priority == Priority.Maintenance)
						this_priority = Priority.Delayed;
					if (other_priority == Priority.Maintenance)
						other_priority = Priority.Delayed;
				} 
				
				int cmp;
				cmp = (int)this_priority - (int)other_priority;
				if (cmp != 0)
					return cmp;
				
				cmp = this.SubPriority - other.SubPriority;
				if (cmp != 0)
					return cmp;

				// Tasks that were added to the scheduler earlier take
				// precedence over those that were added later.
				cmp = DateTime.Compare (other.Timestamp, this.Timestamp);
				if (cmp != 0)
					return cmp;
				
				// Try to break any ties
				return this.GetHashCode () - other.GetHashCode ();
			}

			public void AppendToStringBuilder (StringBuilder sb)
			{
				sb.Append (Priority).Append (' ').Append (SubPriority);
				sb.Append (" (").Append (Timestamp).Append (")\n");
					
				sb.Append (Tag).Append ('\n');

				double t = (TriggerTime - DateTime.Now).TotalSeconds;
				if (t > 0) {
					if (t < 120)
						sb.AppendFormat ("Hold for {0:0.00} seconds\n", t);
					else {
						sb.Append ("Hold until ").Append (TriggerTime);
						sb.Append ('\n');
					}
				}

				if (Creator != null)
					sb.Append ("Creator: ").Append (Creator).Append ('\n');

				if (Description != null)
					sb.Append (Description).Append ('\n');

				if (StatusName != null)
					sb.Append ("Status: ").Append (StatusName).Append ('\n');
			}
		}

		private class TaskHookWrapper : Task {

			TaskHook hook;
			
			public TaskHookWrapper (TaskHook hook) 
			{
				this.hook = hook;
			}

			protected override void DoTaskReal ()
			{
				if (hook != null)
					hook (this);
			}
		}

		public static Task TaskFromHook (TaskHook hook)
		{
			return new TaskHookWrapper (hook);
		}

		//////////////////////////////////////////////////////////////////////////////

		//
		// Task Groups
		//

		public static TaskGroup NewTaskGroup (string name, Hook pre_hook, Hook post_hook)
		{
			return new TaskGroupPrivate (name, pre_hook, post_hook);
		}

		// The TaskGroup we hand back to the user is an interface that
		// exposes minimal functionality.
		public interface TaskGroup {
			string Name { get; }
			bool Finished { get; }
		}

		private class TaskGroupPrivate : TaskGroup {
			private string name;
			private int task_count = 0;
			private bool touched = false;
			private bool finished = false;
			private Hook pre_hook;
			private Hook post_hook;

			public TaskGroupPrivate (string name,
						 Hook   pre_hook,
						 Hook   post_hook)
			{
				this.name = name;
				this.pre_hook = pre_hook;
				this.post_hook = post_hook;
			}

			public string Name {
				get { return name; }
			}

			public bool Finished {
				get { return finished; }
			}

			// Call this when a task is added to the task group.
			public void Increment ()
			{
				if (finished)
					throw new Exception ("Tried to increment a finished TaskGroup");
				++task_count;
			}

			// Call this when we execute a task in the task group.
			public void Touch ()
			{
				if (finished)
					throw new Exception ("Tried to touch a finished TaskGroup");

				if (! touched) {
					if (pre_hook != null) {
						try {
							pre_hook ();
						} catch (Exception ex) {
							Logger.Log.Warn (ex, "Caught exception in pre_hook of task group '{0}'", Name);
						}
					}
					touched = true;
				}
			}

			// Call this after a task in the task group is complete.
			public void Decrement ()
			{
				if (finished)
					throw new Exception ("Tried to decrement a finished TaskGroup");

				--task_count;
				// Only fire our post-hook if the pre-hook fired
				// (or would have fired, had it been non-null)
				if (task_count == 0 && touched) {
					if (post_hook != null) {
						try {
							post_hook ();
						} catch (Exception ex) {
							Logger.Log.Warn (ex, "Caught exception in post_hook of task group '{0}'", Name);
						}
					}
					finished = true;
				}
			}
		}

		//////////////////////////////////////////////////////////////////////////////

		//
		// Task Collector
		//
		// This is a mechanism for executing tasks in sets, possibly outside of
		// priority order.
		//

		public interface ITaskCollector {

			double GetMaximumWeight ();

			void PreTaskHook ();
			void PostTaskHook ();
		}

		//////////////////////////////////////////////////////////////////////////////

		private static double global_delay = -1.0;

		static Scheduler ()
		{
			string exercise;
			exercise = Environment.GetEnvironmentVariable ("BEAGLE_EXERCISE_THE_DOG");

			if (exercise != null) {
				Log.Always ("BEAGLE_EXERCISE_THE_DOG is set.");

				if (exercise.Length > 2 && exercise [0] == 't')
					global_delay = Double.Parse (exercise.Substring (1));
				else
					global_delay = 0.0;
			}
		}

		//////////////////////////////////////////////////////////////////////////////

		private static Scheduler global = new Scheduler ();

		public static Scheduler Global {
			get { return global; }
		}

		//////////////////////////////////////////////////////////////////////////////

		private object big_lock = new object ();

		// FIXME: shutdown tasks should probably be ordered by something
		private Queue shutdown_task_queue = new Queue ();

		private Hashtable tasks_by_tag = new Hashtable ();
		private int total_executed_task_count = 0;

		public void Add (Task task)
		{
			if (task == null)
				return;

			if (task.Source == null)
				throw new Exception ("Attempting to add Task with no source!");

			Task old_task = null;

			lock (big_lock) {
				
				// Keep track of when immediate priority tasks are
				// added so that we can throttle if the scheduler
				// is being slammed with them.
				if (task.Priority == Priority.Immediate) {
					// Shift our times down by one
					Array.Copy (last_immediate_times, 1, last_immediate_times, 0, immediate_throttle_count - 1);
					last_immediate_times [immediate_throttle_count - 1] = DateTime.Now;
				}
				
				old_task = tasks_by_tag [task.Tag] as Task;

				task.Schedule (this);

				// Re-adding the same task is basically a no-op --- we
				// just update the timestamp and return.
				if (old_task == task)
					return;

				if (Debug) {
					Logger.Log.Debug ("Adding task {0}", task.Tag);
					if (task.Description != null)
						Logger.Log.Debug ("  Desc: {0}", task.Description);
				}

				if (task.Priority == Priority.Shutdown)
					shutdown_task_queue.Enqueue (task);
				else
					tasks_by_tag [task.Tag] = task;
				
				Monitor.Pulse (big_lock);
			}

			// If we clobbered another task, call cancel on it.
			// This happens after we release the lock, since
			// cancellation could result in a task group post-hook
			// being run.
			if (old_task != null)
				old_task.Cancel ();
		}

		public Task GetByTag (string tag)
		{
			lock (big_lock)
				return tasks_by_tag [tag] as Task;
		}

		public bool ContainsByTag (string tag)
		{
			Task task = GetByTag (tag);
			return task != null && !task.Cancelled;
		}

		public void Recompute ()
		{
			lock (big_lock)
				Monitor.Pulse (big_lock);
		}

		//////////////////////////////////////////////////////////////////////////////

		private Thread thread = null;
		public bool running = false;
		private static bool shutdown_requested = false;

		public void Start ()
		{
			lock (this) {
				if (shutdown_requested || thread != null)
					return;
				running = true;
				thread = ExceptionHandlingThread.Start (new ThreadStart (Worker));
			}
		}

		public void Stop (bool to_shutdown)
		{
			lock (big_lock) {
				shutdown_requested = to_shutdown;

				if (running) {
					running = false;
					thread = null;
					status_str = "Stopped";
					Monitor.Pulse (big_lock);
				}
			}
		}

		public void Stop ()
		{
			Stop (false);
		}

		//
		// Delay Computations
		//
		// This code controls how we space out tasks
		//

		// FIXME: random magic constants
		const double idle_threshold               = 5.314159 * 60;  // probably should be longer
		const double idle_ramp_up_time            = 5.271828 * 60;  // probably should be longer
		const double default_delayed_rate_factor  = 9.03;           // work about 1/10th of the time
		const double default_throttle_rate_factor = 3.042;          // work about 1/4th of the time
		const double default_idle_rate_factor     = 2.097;          // work about 1/3rd of the time
		const double maximum_delay                = 20;             // never wait for more than 20s
		const double min_throttled_delay          = 1.5;            // never wait less than this when throttled
		const double min_overloaded_delay         = 2.2;            // never wait less than this when there are many tasks
		const int    task_overload_threshold      = 15;             // number of tasks to process before delaying
		const int    immediate_throttle_count     = 5;              // number of immediate tasks to consider before throttling
		const double immediate_throttle_delta     = 4.1;            // amount of time between first and last tracked task

		DateTime[] last_immediate_times = new DateTime [immediate_throttle_count];

		// The return value and duration_of_previous_task are both measured in seconds.
		private double ComputeDelay (Priority priority_of_next_task,
					     double   duration_of_previous_task,
					     int      executed_task_count)
		{
			if (global_delay >= 0.0)
				return global_delay;

			double rate_factor;
			
			rate_factor = 2.0;

			// Do everything faster the longer we are idle.
			double idle_time = SystemInformation.InputIdleTime;
			double idle_scale = 1.0;
			bool is_idle = false;
			bool need_throttle = false;

			// Never speed up if we are using the battery.
			if (idle_time > idle_threshold && BatteryMonitor.UsingAC) {
				is_idle = true;
				double t = (idle_time - idle_threshold) / idle_ramp_up_time;				     
				idle_scale = (1 - Math.Min (t, 1.0));
			} 

			switch (priority_of_next_task) {
				
			case Priority.Immediate:
				rate_factor = 0;

				if (last_immediate_times [0] != DateTime.MinValue) {
					TimeSpan last_add_delta = DateTime.Now.Subtract (last_immediate_times [immediate_throttle_count - 1]);

					// If less than a second has gone by since the
					// last immediate task was added, there is
					// still a torrent of events coming in, and we
					// may need to throttle.
					if (last_add_delta.TotalSeconds <= 1) {
						TimeSpan between_add_delta = last_immediate_times [immediate_throttle_count - 1].Subtract (last_immediate_times [0]);

						// At least immediate_throttle_count tasks have been
						// added in the last immediate_throttle_delta seconds.
						// We definitely need to throttle.
						if (between_add_delta.TotalSeconds <= immediate_throttle_delta) {
							need_throttle = true;
							rate_factor = idle_scale * default_throttle_rate_factor;
						}
					}
				}

				// If we've processed many tasks since the last
				// time we took a break, ignore the priority and set a
				// delay equivalent to Priority.Delayed.
				if (!is_idle && executed_task_count >= task_overload_threshold)
					rate_factor = idle_scale * default_delayed_rate_factor;
				
				break;

			case Priority.Delayed:
				rate_factor = idle_scale * default_delayed_rate_factor;
				break;

			case Priority.Idle:
				rate_factor = idle_scale * default_idle_rate_factor;
				break;
			}


			// FIXME: we should do something more sophisticated than this
			// with the load average.
			// Random numbers galore!
			double load_average = SystemInformation.LoadAverageOneMinute;
			if (load_average > 3.001)
				rate_factor *= 5.002;
			else if (load_average > 1.5003)
				rate_factor *= 2.004;

			double delay = rate_factor * duration_of_previous_task;

			// space out delayed tasks a bit when we aren't idle
			if (! is_idle
			    && priority_of_next_task == Priority.Delayed
			    && delay < 0.5)
				delay = 0.5;

			if (delay > maximum_delay)
				delay = maximum_delay;

			// If we need to throttle, make sure we don't delay less than
			// a second and some.
			if (need_throttle && delay < min_throttled_delay)
				delay = min_throttled_delay;

			// If we're not idle and we've just processed more
			// than a certain number of events, take a break.
			if (! is_idle
			    && executed_task_count >= task_overload_threshold
			    && delay < min_overloaded_delay)
				delay = min_overloaded_delay;

			return delay;
		}

		//
		// The main loop
		//

		// A convenience function.  There should be a 
		// constructor to TimeSpan that does this.
		private static TimeSpan TimeSpanFromSeconds (double t)
		{
			// Wait barfs if you hand it a negative TimeSpan,
			// so we are paranoid;
			if (t < 0.001)
				t = 0;

			// 1 tick = 100 nanoseconds
			long ticks = (long) (t * 1.0e+7);
			return new TimeSpan (ticks);
		}

		private string status_str = null;

		private void Worker ()
		{
			DateTime end_time_of_previous_task = DateTime.MinValue;
			double duration_of_previous_task = 0.0;

			Hook pre_hook = null;
			Hook post_hook = null;
			ArrayList to_be_executed = new ArrayList ();
			Hashtable max_priority_by_source = new Hashtable ();
			int executed_task_count = 0;
			StringBuilder status_builder = new StringBuilder ();

			while (running) {

				status_str = "Finding next task to execute";

				lock (big_lock) {

					// If there are no pending tasks, wait
					// on our lock and then re-start our
					// while loop
					if (tasks_by_tag.Count == 0) {
						if (EmptyQueueEvent != null)
							EmptyQueueEvent ();
						status_str = "Waiting on empty queue";
						Monitor.Wait (big_lock);
						executed_task_count = 0;
						continue;
					}

					if (Debug)
						Log.Debug ("Running Scheduler inner loop.  Pending tasks: {0}", tasks_by_tag.Count);

					// Walk across our list of tasks and find
					// the next one to execute.
					DateTime now = DateTime.Now;
					DateTime next_trigger_time = DateTime.MaxValue;

					// Make a first pass over our tasks, finding the
					// highest-priority item per source.
					max_priority_by_source.Clear ();
					foreach (Task task in tasks_by_tag.Values) {
						if (task.Blocked || task.TriggerTime >= now)
							continue;
						if (max_priority_by_source.Contains (task.Source)) {
							Priority p = (Priority) max_priority_by_source [task.Source];
							if (p < task.Priority)
								max_priority_by_source [task.Source] = task.Priority;
						} else {
							max_priority_by_source [task.Source] = task.Priority;
						}
					}
					
					// Now make a second pass over the tasks and find
					// the highest-priority item.  We use the information
					// from the first pass to correctly prioritize maintenance tasks.
					Task next_task = null;
					foreach (Task task in tasks_by_tag.Values) {
						if (task.Blocked)
							continue;
						if (task.TriggerTime >= now) {
							if (task.TriggerTime < next_trigger_time)
								next_trigger_time = task.TriggerTime;
							continue;
						}
						
						// If this is a maintenance task and there is a high-priority
						// task from the same source, skip it.
						if (task.Priority == Priority.Maintenance) {
							Priority p = (Priority) max_priority_by_source [task.Source];
							if (p > task.Priority)
								continue;
						}

						if (task.TriggerTime < now) {
							if (next_task == null || next_task.CompareTo (task) < 0)
								next_task = task;
						}
					}

					// If we didn't find a task, wait for the next trigger-time
					// and then re-start our while loop.
					if (next_task == null) {
						if (next_trigger_time == DateTime.MaxValue) {
							status_str = "Waiting for an unblocked task";
							Monitor.Wait (big_lock);
						} else {
							status_str = "Waiting for the next trigger time";
							Monitor.Wait (big_lock, next_trigger_time - now);
						}
						executed_task_count = 0;
						continue;
					}

					// If we did find a task, do we want to execute it right now?
					// Or should we wait a bit?

					// How should we space things out?
					double delay = 0;
					delay = ComputeDelay (next_task.Priority, duration_of_previous_task, executed_task_count);
					delay = Math.Min (delay, (next_trigger_time - now).TotalSeconds);

					if (Debug)
						Log.Debug ("Computed a delay of {0:.00}s for next task ({1}: {2})", delay, next_task.Tag, next_task.Priority);

					// Adjust by the time that has actually elapsed since the
					// last task.
					delay -= (now - end_time_of_previous_task).TotalSeconds;

					// If we still need to wait a bit longer, wait for the appropriate
					// amount of time and then re-start our while loop.
					if (delay > 0.001) {
						TimeSpan span = TimeSpanFromSeconds (delay);

						if (Debug)
							Log.Debug ("Waiting {0:.00}s until the next task at {1}", span.TotalSeconds, now + span);

						status_str = String.Format ("Waiting for next task at {0}", now + span);
						Monitor.Wait (big_lock, span);
						executed_task_count = 0;
						continue;
					}

					//
					// If we've made it to this point, it is time to start
					// executing our selected task.
					//

					to_be_executed.Clear ();

					if (next_task.Collector == null) {

						to_be_executed.Add (next_task);

					} else {

						pre_hook = new Hook (next_task.Collector.PreTaskHook);
						post_hook = new Hook (next_task.Collector.PostTaskHook);

						// Find all eligible tasks with the same collector,
						// and add them to the collection list.
						now = DateTime.Now;
						foreach (Task task in tasks_by_tag.Values)
							if (task != next_task
							    && task.Collector == next_task.Collector
							    && !task.Blocked
							    && task.TriggerTime < now)
								to_be_executed.Add (task);

						// Order the tasks from highest to lowest priority.
						// Our original task will always be the first item
						// in the resulting array.
						to_be_executed.Sort ();
						to_be_executed.Add (next_task);
						to_be_executed.Reverse ();

						// Now find how many tasks can be executed before we
						// exceed the collector's maximum weight.  If necessary,
						// prune the list of tasks.
						double remaining_weight;
						remaining_weight = next_task.Collector.GetMaximumWeight ();
						int i = 0;
						while (i < to_be_executed.Count && remaining_weight > 0) {
							Task task;
							task = to_be_executed [i] as Task;
							remaining_weight -= task.Weight;
							++i;
						}
						if (i < to_be_executed.Count)
							to_be_executed.RemoveRange (i, to_be_executed.Count - i);
					}

					// Remove the tasks we are about to execute from our 
					// master list.
					foreach (Task task in to_be_executed)
						tasks_by_tag.Remove (task.Tag);

					// Pulse our lock, in case anyone is waiting for it.
					Monitor.Pulse (big_lock);
				}

				// Now actually execute the set of tasks we found.

				status_builder.Length = 0;
				status_builder.Append ("Executing task");
				if (to_be_executed.Count > 1)
					status_builder.Append ('s');
				status_builder.Append ('\n');
				foreach (Task task in to_be_executed) {
					task.AppendToStringBuilder (status_builder);
					status_builder.Append ('\n');
				}
				status_str = status_builder.ToString ();

				DateTime start_time = DateTime.Now;
				if (pre_hook != null) {
					try {
						pre_hook ();
					} catch (Exception ex) {
						Logger.Log.Error (ex, "Caught exception in pre_hook '{0}'", pre_hook);
					}
				}
				foreach (Task task in to_be_executed) {
					task.DoTask ();
					++total_executed_task_count;
					++executed_task_count;
				}
				if (post_hook != null) {
					try {
						post_hook ();
					} catch (Exception ex) {
						Logger.Log.Error (ex, "Caught exception in post_hook '{0}'", post_hook);
					}
				}

				end_time_of_previous_task = DateTime.Now;
				duration_of_previous_task = (end_time_of_previous_task - start_time).TotalSeconds;
			}

			// Execute all shutdown tasks
			foreach (Task task in shutdown_task_queue)
				if (! task.Cancelled && ! task.Blocked)
					task.DoTask ();

			// Call Cleanup on all of our unexecuted tasks
			foreach (Task task in tasks_by_tag.Values)
				task.Cleanup ();

			if (Debug)
				Logger.Log.Debug ("Scheduler.Worker finished");
		}
		
		//////////////////////////////////////////////////////////////////////////////

		// A list of descriptions of cancelled tasks
		// Kept for display purposes; it is useful to know
		// which tasks were cancelled and why.
		private ArrayList cancelled_tasks = new ArrayList ();

		internal void ReportCancelledTask (string description)
		{
			// The number of cancelled tasks will be low,
			// so there is no harm in checking for duplicates
			// by doing a linear search.
			if (cancelled_tasks.Contains (description))
				return;

			cancelled_tasks.Add (description);
		}

		//////////////////////////////////////////////////////////////////////////////

		private static StringBuilder cached_sb = new StringBuilder ();
		
		public SchedulerInformation GetCurrentStatus ()
		{
		    SchedulerInformation current_status = new SchedulerInformation ();

			lock (big_lock) {

				ArrayList blocked_tasks = new ArrayList ();
				ArrayList future_tasks = new ArrayList ();
				ArrayList pending_tasks = new ArrayList ();

				DateTime now = DateTime.Now;
				foreach (Task task in tasks_by_tag.Values) {
					if (task.Blocked)
						blocked_tasks.Add (task);
					else if (task.TriggerTime > now)
						future_tasks.Add (task);
					else
						pending_tasks.Add (task);
				}

				blocked_tasks.Sort ();
				blocked_tasks.Reverse ();
				
				future_tasks.Sort ();
				future_tasks.Reverse ();

				pending_tasks.Sort ();
				pending_tasks.Reverse ();

				foreach (Task task in pending_tasks) {
					cached_sb.Length = 0;
					task.AppendToStringBuilder (cached_sb);
					current_status.PendingTasks.Add (cached_sb.ToString ());
				}

				foreach (Task task in future_tasks) {
					cached_sb.Length = 0;
					task.AppendToStringBuilder (cached_sb);
					current_status.FutureTasks.Add (cached_sb.ToString ());
				}

				foreach (Task task in blocked_tasks) {
					cached_sb.Length = 0;
					task.AppendToStringBuilder (cached_sb);
					current_status.BlockedTasks.Add (cached_sb.ToString ());
				}

				// Add the cancelled tasks to blocked_task list for the time being
				// This is avoid ABI change in libbeagle
				foreach (string description in cancelled_tasks)
					current_status.BlockedTasks.Add (description);

				current_status.TotalTaskCount = total_executed_task_count;
				current_status.StatusString = status_str;

			}

			return current_status;
		}

	}

	public class SchedulerInformation {
		[XmlAttribute]
		public int TotalTaskCount = -1;

		[XmlAttribute]
		public string StatusString;

		[XmlArray]
		[XmlArrayItem (ElementName="PendingTask", Type=typeof (string))]
		public ArrayList PendingTasks = new ArrayList ();

		[XmlArray]
		[XmlArrayItem (ElementName="FutureTask", Type=typeof (string))]
		public ArrayList FutureTasks = new ArrayList ();

		[XmlArray]
		[XmlArrayItem (ElementName="BlockedTask", Type=typeof (string))]
		public ArrayList BlockedTasks = new ArrayList ();

		private static StringBuilder sb = new StringBuilder ();

		public string ToHumanReadableString ()
		{
			sb.Length = 0;

			sb.Append ("Scheduler:\n");
			sb.Append ("Count: ").Append (TotalTaskCount);
			sb.Append ('\n');

			if (StatusString != null)
				sb.Append ("Status: ").Append (StatusString).Append ('\n');

			int pos = 1;
			sb.Append ("\nPending Tasks:\n");
			if (PendingTasks != null && PendingTasks.Count > 0) {
				foreach (string task in PendingTasks) {
					sb.Append (pos).Append (' ').Append (task).Append ('\n');
					++pos;
				}
			} else
				sb.Append ("Scheduler queue is empty.\n");


			if (FutureTasks != null && FutureTasks.Count > 0) {
				sb.Append ("\nFuture Tasks:\n");
				foreach (string task in FutureTasks)
					sb.Append (task).Append ('\n');
			}

			if (BlockedTasks != null && BlockedTasks.Count > 0) {
				sb.Append ("\nBlocked Tasks:\n");
				foreach (string task in BlockedTasks)
					sb.Append (task).Append ('\n');
			}

			return sb.ToString ();
		}
	}

#if false
	class TestTask : Scheduler.Task {

		private class TestCollector : Scheduler.ITaskCollector {
			
			public double GetMinimumWeight ()
			{
				return 0;
			}

			public double GetMaximumWeight ()
			{
				return 5;
			}

			public void PreTaskHook ()
			{
				Console.WriteLine ("+++ Pre-Task Hook");
			}

			public void PostTaskHook ()
			{
				Console.WriteLine ("+++ Post-Task Hook");
			}
		}

		protected override void DoTaskReal ()
		{
			Console.WriteLine ("Doing task '{0}' at {1}", Tag, DateTime.Now);
			Thread.Sleep (200);
			if (Tag == "Bar")
				Reschedule = true;
		}

		private static void BeginTaskGroup ()
		{
			Console.WriteLine ("--- Begin Task Group!");
		}

		private static void EndTaskGroup ()
		{
			Console.WriteLine ("--- End Task Group!");
		}

		public static void Main ()
		{
			Scheduler sched = Scheduler.Global;

			Scheduler.TaskGroup tg = Scheduler.NewTaskGroup ("foo",
									 new Scheduler.Hook (BeginTaskGroup),
									 new Scheduler.Hook (EndTaskGroup));

			sched.Start ();

			Scheduler.Task task;

			task = new TestTask ();
			task.Tag = "Foo";
			task.AddTaskGroup (tg);
			task.Priority = Scheduler.Priority.Delayed;
			task.TriggerTime = DateTime.Now.AddSeconds (7);
			sched.Add (task);

			task = new TestTask ();
			task.Tag = "Bar";			
			task.AddTaskGroup (tg);
			task.Priority = Scheduler.Priority.Delayed;
			sched.Add (task);

			Scheduler.ITaskCollector collector = null;
			for (int i = 0; i < 20; ++i) {
				if ((i % 10) == 0)
					collector = new TestCollector ();
				task = new TestTask ();
				task.Tag = String.Format ("Baboon {0}", i);
				task.Collector = collector;
				task.Priority = Scheduler.Priority.Delayed;
				sched.Add (task);
			}

			while (true) {
				Thread.Sleep (1000);
			}
		}
	}
#endif
}

