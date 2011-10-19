/*
 * MailboxCollection.cs
 * Copyright (C) 2006 COLIN Cyrille.
 *
 */

using System;
using System.Collections;

namespace System.Net.Imap {

  public class MailboxCollection:System.Collections.CollectionBase {
		public void Add(Mailbox mailbox){
			this.List.Add(mailbox);
		}
  }
}

