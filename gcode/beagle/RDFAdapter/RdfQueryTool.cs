//
// RdfQueryTool.cs
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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

// compile as
// $ gmcs  -debug RdfQueryTool.cs -r:../Util/Util.dll -r:../BeagleClient/Beagle.dll
// run as
// $ MONO_PATH=../Util:../BeagleClient mono --debug RdfQueryTool.exe --help
//

using System;
using System.IO;
using System.Text;

using Beagle;

public class RdfQueryTool {
	public static void Main (string[] args)
	{

		if (args.Length != 3) {
			Console.WriteLine ("Usage: program-name <subject> <predicate> <object>");
			Console.WriteLine ("      <subject>         : URI or path");
			Console.WriteLine ("      <predicate>       : property name (string)");
			Console.WriteLine ("      <object>          : object (string)");
			Console.WriteLine ("      Use \"\" (empty string) for unspecified subject, predicate, type or object");
			return;
		}

		RDFQueryResult result;

		Console.WriteLine ("subject:'{0}' predicate:'{1}' object:'{2}'", args [0], args [1], args [2]);

		Uri subject = null;
		if (args [0] != String.Empty)
			subject = new Uri (args [0]);

		RDFQuery query = new RDFQuery (subject, args [1], args [2]);
		result = (RDFQueryResult) query.Send ();

		if (result == null) {
			Console.WriteLine ("null.......");
			return;
		}

		foreach (Hit hit in result.Hits) {
			foreach (Property prop in hit.Properties)
				Console.WriteLine ("<{0}> <{1}> \"{2}\" .",
						hit.Uri,
						prop.Key,
						prop.Value);
		}
	
	}
}
