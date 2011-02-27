/*
Copyright (C) 2003 Simon Mourier <simonm@microsoft.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

using System;
using System.Collections;

namespace HtmlAgilityPack
{
	internal class NameValuePair
	{
		internal readonly string Name;
		internal string Value;

		internal NameValuePair()
		{
		}

		internal NameValuePair(string name):
			this()
		{
			Name = name;
		}

		internal NameValuePair(string name, string value):
			this(name)
		{
			Value = value;
		}
	}

	internal class NameValuePairList
	{
		internal readonly string Text;
		private ArrayList _allPairs;
		private Hashtable _pairsWithName;

		internal NameValuePairList():
			this(null)
		{
		}

		internal NameValuePairList(string text)
		{
			Text = text;
			_allPairs = new ArrayList();
			_pairsWithName = new Hashtable();

			Parse(text);
		}

		internal string GetNameValuePairValue(string name)
		{
			if (name==null)
				throw new ArgumentNullException();
			ArrayList al = GetNameValuePairs(name);
			if (al==null)
				return null;

			// return first item
			NameValuePair nvp = al[0] as NameValuePair;
			return nvp.Value;
		}

		internal ArrayList GetNameValuePairs(string name)
		{
			if (name==null)
				return _allPairs;
			return _pairsWithName[name] as ArrayList;
		}

		private void Parse(string text)
		{
			_allPairs.Clear();
			_pairsWithName.Clear();
			if (text==null)
				return;

			string[] p = text.Split(';');
			if (p==null)
				return;
			foreach(string pv in p)
			{
				if (pv.Length==0)
					continue;
				string[] onep = pv.Split(new char[]{'='}, 2);
				if (onep==null)
					continue;
				NameValuePair nvp = new NameValuePair(onep[0].Trim().ToLower());
				if (onep.Length<2)
					nvp.Value = "";
				else
					nvp.Value = onep[1];

				_allPairs.Add(nvp);

				// index by name
				ArrayList al = _pairsWithName[nvp.Name] as ArrayList;
				if (al==null)
				{
					al = new ArrayList();
					_pairsWithName[nvp.Name] = al;
				}
				al.Add(nvp);
			}
		}

		internal static string GetNameValuePairsValue(string text, string name)
		{
			NameValuePairList l = new NameValuePairList(text);
			return l.GetNameValuePairValue(name);
		}
	}
}
