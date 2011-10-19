/*
 * FlagCollection.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;

namespace System.Net.Imap {
  public class Flags:System.Collections.CollectionBase {
		public void Add(Flag flag){
	    this.List.Add(flag);
	  }
  }
}
