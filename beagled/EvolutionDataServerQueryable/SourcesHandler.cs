//
// SourcesHandler.cs
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

using Beagle.Util;

using Evolution;

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	public class SourcesHandler {

		Type container_type;
		EvolutionDataServerQueryable queryable;
		string fingerprint;
		object[] ctor_args;
		SourceList source_list;
		Hashtable src_to_cont_map = new Hashtable ();

		public SourcesHandler (string gconf_key, Type container_type, EvolutionDataServerQueryable queryable, string fingerprint, params object[] ctor_args)
		{
			this.container_type = container_type;
			this.queryable = queryable;
			this.fingerprint = fingerprint;
			this.ctor_args = ctor_args;

			// This is the first code to hit e-d-s, so we might
			// get a DllNotFoundException exception if things
			// aren't configured correctly.  However, since
			// we're instantiating an object, it might be
			// wrapped in a TypeInitializationException, so
			// catch that and rethrow the inner exception.
			try {
				this.source_list = new SourceList (gconf_key);
			} catch (TypeInitializationException ex) {
				throw ex.InnerException;
			}

			if (this.source_list == null) {
				// FIXME: We may want to watch for the creation
				// of the sources GConf key
				Logger.Log.Info ("No sources found at {0}", gconf_key);
				return;
			}

			this.source_list.GroupAdded += OnGroupAdded;
			this.source_list.GroupRemoved += OnGroupRemoved;

			foreach (SourceGroup group in this.source_list.Groups)
				IndexSourceGroup (group, false);
		}

		private void IndexSourceGroup (SourceGroup group, bool all_items)
		{
			group.SourceAdded += OnSourceAdded;
			group.SourceRemoved += OnSourceRemoved;

			foreach (Evolution.Source src in group.Sources) {
				object[] args = new object [this.ctor_args.Length + 3];
				args [0] = src;
				args [1] = this.queryable;
				args [2] = this.fingerprint;
				Array.Copy (this.ctor_args, 0, args, 3, this.ctor_args.Length);

				Container cont = (Container) Activator.CreateInstance (this.container_type, args);
				if (!cont.OpenClient ())
					continue;

				src_to_cont_map [src] = cont;

				if (all_items)
					cont.IndexAll ();
				else
					cont.IndexChanges ();

				cont.OpenView ();
			}
		}

		private void RemoveSourceGroup (SourceGroup group)
		{
			foreach (Evolution.Source src in group.Sources) {
				Container cont = (Container) src_to_cont_map [src];
				cont.Remove ();
				src_to_cont_map.Remove (src);
			}
		}

		private void OnGroupAdded (object o, GroupAddedArgs args)
		{
			IndexSourceGroup (args.Group, true);
		}

		private void OnGroupRemoved (object o, GroupRemovedArgs args)
		{
			RemoveSourceGroup (args.Group);
		}

		private void OnSourceAdded (object o, SourceAddedArgs args)
		{
			object[] new_args = new object [this.ctor_args.Length + 3];
			new_args [0] = args.Source;
			new_args [1] = this.queryable;
			new_args [2] = this.fingerprint;
			Array.Copy (this.ctor_args, 0, new_args, 3, this.ctor_args.Length);

			Container cont = (Container) Activator.CreateInstance (this.container_type, new_args);
			if (!cont.OpenClient ())
				return;
			cont.IndexAll ();
			cont.OpenView ();
		}

		private void OnSourceRemoved (object o, SourceRemovedArgs args)
		{
			Container cont = (Container) src_to_cont_map [args.Source];
			cont.Remove ();
		}
	}
}
