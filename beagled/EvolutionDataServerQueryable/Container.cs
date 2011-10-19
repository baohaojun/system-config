//
// Container.cs
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

namespace Beagle.Daemon.EvolutionDataServerQueryable {

	public abstract class Container {

		protected Evolution.Source source;
		protected EvolutionDataServerQueryable queryable;
		protected string fingerprint;

		public Container (Evolution.Source source, EvolutionDataServerQueryable queryable, string fingerprint)
		{
			this.source = source;
			this.queryable = queryable;
			this.fingerprint = fingerprint;
		}

		public abstract bool OpenClient ();
		public abstract void OpenView ();
		public abstract void IndexAll ();
		public abstract void IndexChanges ();
		public abstract void Remove ();
	}
}
