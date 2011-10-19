/*
 * MailCollection.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;

namespace System.Net.Imap {

  public class MailCollection:System.Collections.CollectionBase {
		public void Add(Mail mail){
	 		this.List.Add(mail);
		}
  }
}

