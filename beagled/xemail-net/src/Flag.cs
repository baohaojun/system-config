/*
 * Flag.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;

namespace System.Net.Imap {

  public class Flag {
    string _name;
    public Flag(){}

    public Flag(string name) {
      this._name = name;
    }
    public string Name {
      get { return this._name;}
      set {this._name = value;}
    }
		public override string ToString() {
			return this._name;
		}
  }
}
