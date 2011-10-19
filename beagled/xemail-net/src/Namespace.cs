/*
 * NameSpace.cs
 * Copyright (C) 2005 COLIN Cyrille.
 *
 */

using System;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.Serialization;
using System.Web.Mail;

namespace System.Net.Imap {
	public class Namespaces {

		private ArrayList		_servernamespace;
		private ArrayList		_usernamespace;   
		private ArrayList   	_sharednamespace;  
		
		public Namespaces(){
			this. _servernamespace = new ArrayList();
			this. _usernamespace = new ArrayList();
			this. _sharednamespace = new ArrayList();
		}
		public void AddServerNamespace(string key,string name) {
			this. _servernamespace.Add(new Namespace(key,name));
		}
		public void AddUserNamespace(string key,string name) {
			this. _usernamespace.Add(new Namespace(key,name));
		}
		public void AddSharedNamespace(string key,string name) {
			this. _sharednamespace.Add(new Namespace(key,name));
		}
		public ArrayList ServerNamespace {
			set { this._servernamespace = value;}
			get { return this._servernamespace;}
		}
		public ArrayList UserNamespace {
			set { this._usernamespace = value;}
			get { return this._usernamespace;}
		}
		public ArrayList SharedNamespace {
			set { this._sharednamespace = value;}
			get { return this._sharednamespace;}
		}
	}
	public class Namespace {
		private string _prefix;
		private string _delimiter;
		public Namespace(string prefix, string delimiter){
			this._prefix = prefix;
			this._delimiter = delimiter;
		}
		public Namespace(){}
		public string Prefix {
	        set { this._prefix = value;}
			get { return this._prefix;}
		}
		public string Delimiter {
 	       set { this._delimiter = value;}
           get { return this._delimiter;}
        }
	}
}
