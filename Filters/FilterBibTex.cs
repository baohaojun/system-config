//
// FilterBibTex.cs : Simple BibTex .bib filter, based on the external tool bibparse.
//
// As it turns out, bibtex format is pretty tough to parse and most of the parsers
// out there use some kind of lexical generator. I decided to use the tried and tested
// bibparse to do the basic parsing.
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
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
using System.Collections;
using System.Diagnostics;

using Beagle.Util;

namespace Beagle.Filters {
	public class FilterBibTex : Beagle.Daemon.Filter {
		public FilterBibTex ()
		{
			SnippetMode = false;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
//			AddSupportedFlavor (Beagle.Daemon.FilterFlavor.NewFromMimeType ("text/x-bibtex"));
		}

		private static bool bibparse_installed = false;

		static FilterBibTex ()
		{
			// Check if bibparse is present
			SafeProcess pc = new SafeProcess ();
			pc.Arguments = new string [] { "bibparse"};
			pc.RedirectStandardOutput = false;
			pc.RedirectStandardError = false;

			try {
				pc.Start ();
				bibparse_installed = true;
			} catch (SafeProcessException) {
				Log.Warn ("bibparse is not found; bibtex files will not be indexed");
				bibparse_installed = false;
			}

			pc.Close ();
		}

		protected override void DoPullProperties ()
		{
			if (! bibparse_installed)
				Error ();
			return;
		}

		public override bool HasGeneratedIndexable {
			get { return bibparse_installed; }
		}

		private SafeProcess bib_process = null;
		private StreamReader reader;

		private bool InitBibparse ()
		{
			bib_process = new SafeProcess ();
			bib_process.Arguments = new string[] { "bibparse", FileInfo.FullName};
			bib_process.RedirectStandardError = false;
			bib_process.RedirectStandardOutput = true;
			bib_process.CpuLimit = 180; // 3 minutes
			bib_process.MemLimit = 50*1024*1024; // 50 MB

			try {
				bib_process.Start ();
			} catch (SafeProcessException e) {
				Log.Error (e, "Error running 'bibparse {0}'", FileInfo.FullName);
				return false;
			}

			reader = new StreamReader (bib_process.StandardOutput);
			return true;
		}

		public override bool GenerateNextIndexable (out Indexable child)
		{
			child = null;

			if (bib_process == null && ! InitBibparse ())
				return false;

			string line = null;
			string type = null, name = null;
			while ((line = reader.ReadLine ()) != null) {
				if (line == String.Empty || line [0] != '@')
					continue;

				int i = line.IndexOf (' ');
				if (i == -1 || line.Length == i + 1)
					continue;
				type = line.Substring (1, i - 1).ToLower ();
				name = line.Substring (i + 1);
				break;
			}

			if (line == null)
				return false;

			child = new Indexable (UriFu.AddFragment (Indexable.Uri, name, false));
			child.CacheContent = false;
			child.MimeType = "text/x-bibtex";
			child.DisplayUri = child.Uri;
			child.NoContent = true;
			child.AddProperty (Property.NewKeyword ("bibtex:type", type));

			string key, value;
			// Now fill in properties from the key=value lines
			while ((line = reader.ReadLine ()) != null) {
				// Entries are separated by empty lines
				if (line == String.Empty)
					break;

				int i = line.IndexOf ('=');
				// ensure non-empty key
				if (i < 1 || line.Length == i + i)
					continue;
				key = line.Substring (0, i).ToLower ();
				value = line.Substring (i + 1);
				foreach (Property prop in EntryLineToProperty (key, value))
					child.AddProperty (prop);
			}

			child.SetChildOf (Indexable);
			return true;
		}

		protected override void DoClose ()
		{
			if (reader != null)
				reader.Close ();

			if (bib_process != null)
				bib_process.Close ();
		}

		private static readonly string[] AND = new string[] { "and", "AND"};
		private IEnumerable EntryLineToProperty (string key, string value)
		{
			if (key != "author") {
				if (IsKeyword (key))
					yield return Property.NewKeyword (KeyToPropName (key), value);
				else
					yield return Property.New (KeyToPropName (key), CleanupValue (value));
#if ENABLE_RDF_ADAPTER
				if (key == "url")
					AddLink (value);
#endif
				yield break;
			}

			// for author, split the values into multiple authors if possible
			string[] authors = value.Split (AND, StringSplitOptions.None);
			foreach (string a in authors) {
				string author = a.Trim ();
				if (author == "and" || author == String.Empty)
					continue;
				yield return Property.New (KeyToPropName (key), CleanupValue (author));
			}
		}

		private static bool IsKeyword (string key)
		{
			return key == "pages" || key == "url" || key == "identifier";
		}

		private static string KeyToPropName (string key)
		{
			// bibtex to rdf mapping
			switch (key) {
			case "title":
				return "dc:title";
			case "author":
				return "dc:creator";
			case "address":
				return "vcard:ADR";
			case "location":
				return "vcard:ADR";
			case "dc:publisher":
				return "dc:publisher";
			case "url":
				return "dc:identifier";
			default:
				return key;
			}
		}

		private static string CleanupValue (string value)
		{
			return value.Replace ("\\'", String.Empty); // FIXME better processing of latex excape symbols
		}
	}
}
