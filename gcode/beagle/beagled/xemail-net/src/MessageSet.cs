/*
 * MessageSet.cs.
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;


namespace System.Net.Imap {

  public class MessageSet {

    private ArrayList   _messagesids;
  
    public MessageSet(){
			_messagesids = new ArrayList();
		}
    public ArrayList Messages {
	    set { this._messagesids = value;}
      get { return this._messagesids;}
    }
		public void AddMessage(int Id) {
			this._messagesids.Add(Id);
		}
  }
}
