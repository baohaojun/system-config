//
// FrequencyStatistics.cs
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
using System.Collections;
using System.Threading;

namespace Beagle.Util {

	public class FrequencyStatistics {
		
		// In this code, all time spans represented
		// by doubles are measured in seconds.

		// OK, this is crack.  We fix a very large
		// constant that is returned as the gap before
		// the second event comes in.  This is a hack
		// that lets us pretty much ignore the special
		// case of the initial state in our backoff code.
		private const double first_gap = 1.0e+8;

		// decay is between 0.0 and 1.0.
		// A lower number implies a longer memory.
		private const double decay = 0.317732; // a magic constant

		private int count = 0;
		private double expma_gap;
		private DateTime previous_time;

		public int Count {
			get { return count; }
		}

		public double TimeSinceLastEvent {
			get {
				if (count == 0)
					return first_gap;
				else
					return (DateTime.Now - previous_time).TotalSeconds;
			}
		}

		public double EstimatedFrequency {
			get {
				if (count == 0)
					return 1 / first_gap;

				// expma_gap will always be > 0
				// It might be very small, though.
				return 1 / expma_gap;
			}
		}

		public double ImpliedFrequency {
			get {
				return ComputeExpMaGap (DateTime.Now, expma_gap);
			}
		}

		// How many seconds would be have to be idle for our exmpa
		// frequency to reach the required level.
		public double TimeToReachFrequency (double target_frequency)
		{
			if (target_frequency < 0.001)
				return 0;

			double target_gap = 1 / target_frequency;

			if (count < 2 || (target_gap < expma_gap))
				return 0;

			// Compute the size of gap that would get us to the
			// target level.
			double t;
			t = (target_gap - (1 - decay) * expma_gap) / decay;

			// Adjust by the amount of time that has actually passed.
			t -= (DateTime.Now - previous_time).TotalSeconds;
			
			return Math.Max (t, 0);
		}

		private double ComputeExpMaGap (DateTime now, double prev)
		{
			if (count <= 1)
				return first_gap;
			
			double gap = (now - previous_time).TotalSeconds;
			
			if (count == 2)
				return gap;

			return decay * gap + (1 - decay) * prev;
		}

		public void AddEvent ()
		{
			DateTime now = DateTime.Now;
			expma_gap = ComputeExpMaGap (now, expma_gap);
			previous_time = now;
			++count;
		}
	}
}
