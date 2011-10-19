/*
 * Mailbox.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;

namespace System.Net.Imap {

  public class Mailbox {
    private string _name;
    private int _nummsg = 0;
    private int _recent = 0;
    private int _numunseen = 0;
    private bool _rw = false;
    private Flags _flags= new Flags();

    public Mailbox() {}
    public Mailbox(string name) {
      this._name = name;
    }
    public string Name {
            get { return this._name;}
      set { this._name = value;}
        }
    public int NumNewMsg {
      get { return this._recent;}
      set { this._recent = value;}
    }
    public int NumMsg {
      get { return this._nummsg;}
      set { this._nummsg = value;}
    }
    public int NumUnSeen {
      get { return this._numunseen;}
      set { this._numunseen = value;}
    }
    public Flags Flags {
      get {return this._flags;}
      set {this._flags = value;}
    }
    public bool Rw {
      get {return this._rw;}
      set {this._rw = value;}
    }
    public void SetFlags(string flags) {
      string[] f = flags.Split(' ');
      this._flags.Clear();
      foreach(string s in f){
  	    this._flags.Add(new Flag(s));
      }
		}
	}
}

