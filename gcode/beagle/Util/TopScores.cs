//
// TopScores.cs
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

namespace Beagle.Util {

	public class TopScores {

		private static bool Debug = Beagle.Util.Debug.Enabled ("TopScores");

		private struct Node {
			public long Score;
			public object Obj;
			public int LtEq;
			public int Gt;

			public void Clear ()
			{
				LtEq = -1;
				Gt = -1;
			}
		}

		int count;
		bool at_capacity = false;
		int free_node = 0;
		int root_node = -1;
		Node [] nodes;
		long cutoff_score;

		public TopScores (int count)
		{
			this.count = count;
			this.nodes = new Node [count];
			for (int i = 0; i < count; ++i)
				this.nodes [i].Clear ();
		}

		public int Count {
			get { return count; }
		}

		public long MinimumScore {
			get {
				if (at_capacity)
					return cutoff_score;

				long score = 0;
				int i = root_node;
				while (i != -1) {
					score = nodes [i].Score;
					i = nodes [i].LtEq;
				}
				return score;
			}
		}

		public bool WillAccept (long score)
		{
			return (! at_capacity) || score > cutoff_score;
		}

		public void Add (long score, object obj)
		{
			if (at_capacity && score <= cutoff_score) {
				if (Debug)
					Console.WriteLine ("Skipping {0} ({1}), since threshold is {2}",
							   obj, score, cutoff_score);
				return;
			}

			int node, next_node, prev_node;

			// If this is the first thing we are adding,
			// make it the root node.
			if (root_node == -1) {
				if (Debug)
					Console.WriteLine ("Added {0} ({1}) as root node {2}", obj, score, free_node);
				root_node = free_node;
				nodes [free_node].Score = score;
				nodes [free_node].Obj = obj;
				UpdateAtCapacity ();
				return;
			}

			// If we are at capacity, find the leftmost node
			// and detatch it.
			if (at_capacity) {
				prev_node = -1;
				node = root_node;
				
				// Find the leftmost node
				while (true) {
					next_node = nodes [node].LtEq;
					if (next_node == -1)
						break;
					prev_node = node;
					node = next_node;
				}
				
				// Detatch the node, and store its index
				// in free_node so we can re-use it immediately.
				if (prev_node != -1) {
					// Detatching a non-root node
					if (Debug)
						Console.WriteLine ("Detatching {0} ({1}) from node {2}",
								   nodes [node].Obj, nodes [node].Score, node);
					nodes [prev_node].LtEq = nodes [node].Gt;
					free_node = node;

					node = prev_node; // where to start looking for the new cut-off
				} else {
					// Detatching the root node
					if (Debug)
						Console.WriteLine ("Detatching {0} ({1}) from root node {2}",
								   nodes [root_node].Obj, nodes [root_node].Score, root_node);
					free_node = root_node;
					root_node = nodes [root_node].Gt;

					node = root_node; // where to start looking for the new cut-off
				}

				// Find the new cut-off
				while (node != -1) {
					cutoff_score = nodes [node].Score;
					node = nodes [node].LtEq;
				}

				nodes [free_node].Clear ();

				if (Debug)
					Console.WriteLine ("New cutoff is {0}", cutoff_score);
			}
			

			// Find where we want to attach this node
			bool is_lteq = false;
			node = -1;
			next_node = root_node;
			while (next_node != -1) {
				node = next_node;
				if (score <= nodes [node].Score) {
					next_node = nodes [node].LtEq;
					is_lteq = true;
				} else {
					next_node = nodes [node].Gt;
					is_lteq = false;
				}
			}

			nodes [free_node].Score = score;
			nodes [free_node].Obj = obj;
			if (Debug)
				Console.WriteLine ("Added {0} ({1}) as node {2}, under and {3} of {4}",
						   obj, score, free_node, is_lteq ? "left" : "right", node);

			if (at_capacity && score < cutoff_score) {
				cutoff_score = score;
				if (Debug)
					Console.WriteLine ("New cutoff is {0}", cutoff_score);
			}

			
			if (is_lteq)
				nodes [node].LtEq = free_node;
			else
				nodes [node].Gt = free_node;

			UpdateAtCapacity ();
		}

		private void UpdateAtCapacity ()
		{
			if (at_capacity)
				return;

			++free_node;
			if (free_node >= count) {
				cutoff_score = MinimumScore;
				at_capacity = true;
				if (Debug)
					Console.WriteLine ("Hit capacity: set cutoff to {0}", cutoff_score);
			
			}
		}

		private int ComputeMaxDepth (int i)
		{
			if (i == -1)
				return 0;
			int a = 1 + ComputeMaxDepth (nodes [i].LtEq);
			int b = 1 + ComputeMaxDepth (nodes [i].Gt);
			return Math.Max (a, b);
		}

		public int MaxDepth {
			get { return ComputeMaxDepth (root_node); }
		}

		private void BuildArray (ArrayList array, int i)
		{
			if (i != -1) {
				BuildArray (array, nodes [i].Gt);
				array.Add (nodes [i].Obj);
				BuildArray (array, nodes [i].LtEq);
			}
		}

		// Returns objects sorted from high to low scores
		public ArrayList TopScoringObjects {
			get {
				ArrayList array = new ArrayList ();
				BuildArray (array, root_node);
				return array;
			}
		}

#if false
		static void Main ()
		{
			int trial = 0;
			Random rng = new Random ();

			for (int j = 0; j < 1000; ++j) {
				++trial;
				Console.WriteLine ("Trial #{0}", trial);

				TopScores top;
				top = new TopScores (6 + rng.Next (100));

				ArrayList all = new ArrayList ();
				int i;
				int N = 100 + rng.Next (100);
				for (i = 0; i < N; ++i) {
					int n = rng.Next (200);
					all.Add (n);
					top.Add (n, n);
				}

				all.Sort ();
				i = all.Count - 1;
				foreach (object obj in top.TopScoringObjects) {
					if (! obj.Equals (all [i])) {
						Console.WriteLine ("Mismatch!");
						i = all.Count - 1;
						foreach (object x in top.TopScoringObjects) {
							Console.WriteLine ("{0} {1} {2}",
									   x, all [i],
									   x.Equals (all [i]) ? "" : "***");
							--i;
						}
						return;
					}
					--i;
				}
			}
		}
#endif

	}
}
