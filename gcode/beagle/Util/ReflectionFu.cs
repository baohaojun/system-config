//
// ReflectionFu.cs
//
// Copyright (C) 2004-2006 Novell, Inc.
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
using System.Reflection;

namespace Beagle.Util {

	public class ReflectionFu {

		public delegate void ForeachDirMethod (Assembly a);

		static public ArrayList ScanEnvironmentForAssemblies (string env_var, string fallback_path)
		{
			return ScanEnvironmentForAssemblies (env_var, fallback_path, null);
		}

		static public ArrayList ScanEnvironmentForAssemblies (string env_var, string fallback_path, ForeachDirMethod method)
		{
			ArrayList assemblies = new ArrayList ();

			string path = Environment.GetEnvironmentVariable (env_var);

			if (path == null || path == "")
				path = fallback_path;
			else if (path [path.Length-1] == ':')
				path += fallback_path;

			Hashtable seen = new Hashtable ();

			foreach (string dir in path.Split (':')) {
				if (! seen.Contains (dir)) {
					foreach (Assembly a in ScanDirectoryForAssemblies (dir)) {
						assemblies.Add (a);

						if (method != null)
							method (a);
					}
				}

				seen [dir] = true;
			}

			return assemblies;
		}

		static public ArrayList ScanDirectoryForAssemblies (string dir)
		{
			ArrayList assemblies = new ArrayList ();

			if (dir == null || dir == "")
				return assemblies;

			if (! Directory.Exists (dir)) {
				Logger.Log.Debug ("'{0}' is not a directory: Nothing loaded from here", dir);
				return assemblies;
			}
			
			DirectoryInfo dir_info = new DirectoryInfo (dir);
			foreach (FileInfo file_info in dir_info.GetFiles ()) {
				if (file_info.Extension == ".dll") {
					Assembly a = Assembly.LoadFrom (file_info.FullName);
					assemblies.Add (a);
				}
			}

			return assemblies;
		}

		static public ArrayList GetTypesFromAssemblyAttribute (Assembly assembly, Type attr_type)
		{
			ArrayList types = new ArrayList ();

			TypeCacheAttribute attr = (TypeCacheAttribute) Attribute.GetCustomAttribute (assembly, attr_type);

			if (attr != null)
				types.AddRange (attr.Types);

			return types;
		}

		static public ArrayList ScanTypeForAttribute (Type type, Type attribute_type)
		{
			ArrayList attrs = new ArrayList ();

			object[] attributes = Attribute.GetCustomAttributes (type);
			foreach (object obj in attributes) {
				Type obj_type = obj.GetType ();

				if (obj_type == attribute_type || obj_type.IsSubclassOf (attribute_type))
					attrs.Add (obj);
			}

			return attrs;
		}
	}
}