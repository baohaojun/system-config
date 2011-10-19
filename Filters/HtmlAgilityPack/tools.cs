// HtmlAgilityPack V1.0 

/*
Copyright (C) 2003 Simon Mourier <simonm@microsoft.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

using System;
using System.Diagnostics;
using System.IO;

namespace HtmlAgilityPack
{
	internal struct IOLibrary
	{
		internal static void MakeWritable(string path)
		{
			if (!File.Exists(path))
				return;
			File.SetAttributes(path, File.GetAttributes(path) & ~FileAttributes.ReadOnly);
		}

		internal static void CopyAlways(string source, string target)
		{
			if (!File.Exists(source))
				return;
			Directory.CreateDirectory(Path.GetDirectoryName(target));
			MakeWritable(target);
			File.Copy(source, target, true);
		}
	}

	internal struct HtmlLibrary
	{
		[Conditional("DEBUG")]
		internal static void GetVersion(ref string version)
		{
			System.Diagnostics.StackFrame sf = new System.Diagnostics.StackFrame(1, true);
			version = sf.GetMethod().DeclaringType.Assembly.GetName().Version.ToString();
		}

		[Conditional("DEBUG")]
		[Conditional("TRACE")]
		internal static void Trace(object Value)
		{
			// category is the method
			string name = null;
			GetCurrentMethodName(2, ref name);
			System.Diagnostics.Trace.WriteLine(Value, name);
		}

		[Conditional("DEBUG")]
		[Conditional("TRACE")]
		internal static void TraceStackFrame(int steps)
		{
			string name = null;
			GetCurrentMethodName(2, ref name);
			string trace = "";
			for(int i=1;i<steps;i++)
			{
				System.Diagnostics.StackFrame sf = new System.Diagnostics.StackFrame(i, true);
				trace += sf.ToString();
			}
			System.Diagnostics.Trace.WriteLine(trace, name);
			System.Diagnostics.Trace.WriteLine("");
		}

		[Conditional("DEBUG")]
		internal static void GetCurrentMethodName(ref string name)
		{
			name = null;
			GetCurrentMethodName(2, ref name);
		}

		[Conditional("DEBUG")]
		internal static void GetCurrentMethodName(int skipframe, ref string name)
		{
			StackFrame sf = new StackFrame(skipframe, true);
			name = sf.GetMethod().DeclaringType.Name + "." + sf.GetMethod().Name;
		}

	}

	internal class HtmlCmdLine
	{
		static internal bool Help;

		static HtmlCmdLine()
		{
			Help = false;
			ParseArgs();
		}

		internal static string GetOption(string name, string def)
		{
			string p = def;
			string[] args = Environment.GetCommandLineArgs();
			for (int i=1;i<args.Length;i++)
			{
				GetStringArg(args[i], name, ref p);
			}
			return p;
		}

		internal static string GetOption(int index, string def)
		{
			string p = def;
			string[] args = Environment.GetCommandLineArgs();
			int j = 0;
			for (int i=1;i<args.Length;i++)
			{
				if (GetStringArg(args[i], ref p))
				{
					if (index==j)
						return p;
					else
						p = def;
					j++;
				}
			}
			return p;
		}

		internal static bool GetOption(string name, bool def)
		{
			bool p = def;
			string[] args = Environment.GetCommandLineArgs();
			for (int i=1;i<args.Length;i++)
			{
				GetBoolArg(args[i], name, ref p);
			}
			return p;
		}

		internal static int GetOption(string name, int def)
		{
			int p = def;
			string[] args = Environment.GetCommandLineArgs();
			for (int i=1;i<args.Length;i++)
			{
				GetIntArg(args[i], name, ref p);
			}
			return p;
		}

		private static void ParseArgs()
		{
			string[] args = Environment.GetCommandLineArgs();
			for (int i=1;i<args.Length;i++)
			{
				// help
				GetBoolArg(args[i], "?", ref Help);
				GetBoolArg(args[i], "h", ref Help);
				GetBoolArg(args[i], "help", ref Help);
			}
		}

		private static bool GetStringArg(string Arg, ref string ArgValue)
		{
			if (('/'==Arg[0]) || ('-'==Arg[0]))
				return false;
			ArgValue = Arg;
			return true;
		}

		private static void GetStringArg(string Arg, string Name, ref string ArgValue)
		{
			if (Arg.Length<(Name.Length+3)) // -name:x is 3 more than name
				return;
			if (('/'!=Arg[0]) && ('-'!=Arg[0]))	// not a param
				return;
			if (Arg.Substring(1, Name.Length).ToLower()==Name.ToLower())
				ArgValue = Arg.Substring(Name.Length+2, Arg.Length-Name.Length-2);
		}

		private static void GetBoolArg(string Arg, string Name, ref bool ArgValue)
		{
			if (Arg.Length<(Name.Length+1)) // -name is 1 more than name
				return;
			if (('/'!=Arg[0]) && ('-'!=Arg[0]))	// not a param
				return;
			if (Arg.Substring(1, Name.Length).ToLower()==Name.ToLower())
				ArgValue = true;
		}

		private static void GetIntArg(string Arg, string Name, ref int ArgValue)
		{
			if (Arg.Length<(Name.Length+3)) // -name:12 is 3 more than name
				return;
			if (('/'!=Arg[0]) && ('-'!=Arg[0]))	// not a param
				return;
			if (Arg.Substring(1, Name.Length).ToLower()==Name.ToLower())
			{
				try
				{
					ArgValue = Convert.ToInt32(Arg.Substring(Name.Length+2, Arg.Length-Name.Length-2));
				}
				catch
				{
				}
				
			}
		}
	}

	internal class HtmlConsoleListener: System.Diagnostics.TraceListener
	{
		public override void WriteLine(string Message)
		{
			Write(Message + "\n");
		}

		public override void Write(string Message)
		{
			Write(Message, "");
		}
	
		public override void Write(string Message, string Category)
		{
			Console.Write("T:" + Category + ": " + Message);
		}

		public override void WriteLine(string Message, string Category)
		{
			Write(Message + "\n", Category);
		}
	}

}
