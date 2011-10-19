//
// Thunderbird.cs: Basic Thunderbird routines
//
// Copyright (C) 2006 Novell, Inc.
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
using Beagle.Util;

namespace Beagle.Util {

	public static class Thunderbird {
		private static readonly string [] exec_names = new string [] {
			"thunderbird",
			"mozilla-thunderbird"
		};
		
		public static string ExecutableName {
			get {
				foreach (string name in exec_names) {
					foreach (string path in PathFinder.Paths) {
						string executable = Path.Combine (path, name);
						
						if (File.Exists (executable))
							return executable;
							
					}
				}
				
				return string.Empty;
			}
		}
		
		public static SafeProcess GetSafeProcess (params string[] args)
		{
			SafeProcess p = new SafeProcess ();
			
			p.Arguments = new string [1 + args.Length];
			p.Arguments [0] = ExecutableName;
			Array.Copy (args, 0, p.Arguments, 1, args.Length);
			
			return p;
		}
	}
}
