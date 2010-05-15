//
// Logger.cs
//
// Copyright (C) 2004-2005 Novell, Inc.
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
using System.Collections;
using System.Text;
using System.IO;
using System.Diagnostics;

namespace Beagle.Util {
		
	public class Logger {

		public Logger () { }

		static Logger static_logger = new Logger ();

		static public Logger Log { get { return static_logger; } }
		
		static public Logger Get (string old_style_log_name) { return static_logger; }

		/////////////////////////////////////////////////////////////////////////////////////////

		public void Debug (string message, params object [] args)
		{
			Beagle.Util.Log.Debug (message, args);
		}

		public void Debug (Exception ex, string message, params object [] args)
		{
			Beagle.Util.Log.Debug (ex, message, args);
		}

		public void Debug (Exception ex)
		{
			Beagle.Util.Log.Debug (ex);
		}

		public void Info (string message, params object [] args)
		{
			Beagle.Util.Log.Info (message, args);
		}

		public void Info (Exception ex, string message, params object [] args)
		{
			Beagle.Util.Log.Info (ex, message, args);
		}

		public void Info (Exception ex)
		{
			Beagle.Util.Log.Info (ex);
		}

		public void Warn (string message, params object [] args)
		{
			Beagle.Util.Log.Warn (message, args);
		}

		public void Warn (Exception ex, string message, params object [] args)
		{
			Beagle.Util.Log.Warn (ex, message, args);
		}

		public void Warn (Exception ex)
		{
			Beagle.Util.Log.Warn (ex);
		}

		public void Error (string message, params object [] args)
		{
			Beagle.Util.Log.Error (message, args);
		}
		
		public void Error (Exception ex, string message, params object [] args)
		{
			Beagle.Util.Log.Error (ex, message, args);
		}

		public void Error (Exception ex)
		{
			Beagle.Util.Log.Error (ex);
		}
	}
}

