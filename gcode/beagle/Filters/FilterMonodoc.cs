//
// FilterMonodoc.cs
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
using System.IO;
using System.Xml;
using System.Text;
using System.Threading;
using System.Collections;

using Beagle.Daemon;
using Beagle.Util;

using ICSharpCode.SharpZipLib.Zip;

namespace Beagle.Filters {

	public class FilterMonodoc : Filter {

		ZipFile archive = null;

		public FilterMonodoc ()
		{
			SetFileType ("documentation");
		}

		protected override void RegisterSupportedTypes ()
		{
			// 1: Change to indexable generator
			SetVersion (1);

			// FIXME: Autoconf to find the monodoc prefix
			AddSupportedFlavor (new FilterFlavor ("file:///usr/lib/monodoc/sources/*", ".zip", null, 0));
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/monodoc"));
		}

		public override bool HasGeneratedIndexable {
			get {
				try {
					archive = new ZipFile (Stream);
					entry_enumerator = GetNextEntryIndexable ().GetEnumerator ();
					return true;
				} catch (Exception e) {
					Log.Error ("Error while filtering Monodoc archive: {0}", e);
					// If we cannot scan archived entries, we take that as a failure
					Error ();
					return false;
				}
			}
		}

		private IEnumerator entry_enumerator = null;

		public override bool GenerateNextIndexable (out Indexable child)
		{
			child = null;

			if (entry_enumerator == null || ! entry_enumerator.MoveNext ())
				return false;

			child = (Indexable) entry_enumerator.Current;
			
			return true;
		}

		private IEnumerable GetNextEntryIndexable ()
		{
			foreach (ZipEntry entry in archive) {
				if (entry.Name.IndexOf (".") != -1)
					continue;
				
				XmlDocument document = new XmlDocument ();
				document.Load (archive.GetInputStream (entry));
				
				XmlNode type = document.SelectSingleNode ("/Type");
				
				if (type == null)
					continue;
				
				Indexable type_indexable = TypeNodeToIndexable (type, Indexable.Uri);
				type_indexable.SetChildOf (this.Indexable);
				type_indexable.StoreStream ();
				type_indexable.CloseStreams ();
				yield return type_indexable;
				
				foreach(XmlNode member in type.SelectNodes ("Members/Member")) {
					Indexable member_indexable = MemberNodeToIndexable (member,
											    Indexable.Uri,
											    type.Attributes ["FullName"].Value);
					member_indexable.SetChildOf (this.Indexable);
					member_indexable.StoreStream ();
					member_indexable.CloseStreams ();
					yield return member_indexable;
				}
			}

			Finished ();
		}

		static private Indexable TypeNodeToIndexable (XmlNode node, Uri base_uri)
		{
			string fragment = "T:" + node.Attributes ["FullName"].Value;
			Indexable indexable = new Indexable (UriFu.AddFragment (base_uri, fragment, false));
			
			indexable.MimeType = "text/html";
			indexable.HitType = "MonodocEntry";
			
			indexable.AddProperty (Property.New ("dc:title", "T:" + node.Attributes["FullName"].Value));
			indexable.AddProperty (Property.NewUnsearched ("fixme:name", "T:" + node.Attributes["FullName"].Value));
			indexable.AddProperty (Property.NewUnsearched ("fixme:type", "type"));
			
			StringReader reader = new StringReader (node.SelectSingleNode ("Docs").InnerXml); 
                        indexable.SetTextReader (reader);
			
			return indexable;
		}
		
		static private Indexable MemberNodeToIndexable(XmlNode node, Uri base_uri, string parentName)
		{
			char memberType = MemberTypeToChar (node.SelectSingleNode ("MemberType").InnerText);
			StringBuilder memberFullName = new StringBuilder ();
			
			memberFullName.Append (memberType + ":"+ parentName);
			
			if (memberType != 'C')
				memberFullName.Append ("." + node.Attributes["MemberName"].Value);
			
			if (memberType == 'C' || memberType == 'M' || memberType == 'E') {
				memberFullName.Append ("(");
				bool inside = false;
				
				foreach (XmlNode parameter in node.SelectNodes ("Parameters/Parameter")) {
					if (!inside) inside = true; else memberFullName.Append(",");
					memberFullName.Append (parameter.Attributes["Type"].Value);
				}
				
				memberFullName.Append (")");
			}

			Indexable indexable = new Indexable (UriFu.AddFragment (base_uri, memberFullName.ToString (), false));

			indexable.MimeType = "text/html";
			indexable.HitType = "MonodocEntry";

			indexable.AddProperty (Property.New ("dc:title",  memberFullName.ToString ()));
			indexable.AddProperty (Property.New ("fixme:name", memberFullName.ToString ()));
			indexable.AddProperty (Property.NewUnsearched ("fixme:type", node.SelectSingleNode ("MemberType").InnerText.ToLower ()));

			StringReader reader = new StringReader (node.SelectSingleNode ("Docs").InnerXml); 
                        indexable.SetTextReader (reader);

			return indexable;		
		}

		static private char MemberTypeToChar (string memberType)
		{
			switch (memberType) {
			case "Constructor":
				return 'C';
			case "Event":
				return 'E';
			case "Property":
				return 'P';
			case "Field":
				return 'F';
			case "Method":
				return 'M';
			default:
				return 'U';
			}
		}
	}
}
