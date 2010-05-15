//
// FilterPdf.cs: Very simplistic PDF filter
//
// Author:
//   Christopher Orr <dashboard@protactin.co.uk>
//
// Copyright 2004 by Christopher Orr
//

using System;
using System.IO;
using System.Diagnostics;

using Beagle.Util;
using Beagle.Daemon;
using FSpot;
using FSpot.Xmp;
using SemWeb;

namespace Beagle.Filters {

	public class FilterPdf : Beagle.Daemon.Filter {

		public FilterPdf ()
		{
			SnippetMode = true;
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/pdf"));
		}

		// FIXME: we should have a reasonable failure mode if pdftotext is
		// not installed.

		SafeProcess pc = null;
		StreamReader pout = null;

		protected override void DoPullProperties ()
		{
			// create new external process
			pc = new SafeProcess ();
			pc.Arguments = new string [] { "pdfinfo", "-meta", FileInfo.FullName };
			pc.RedirectStandardOutput = true;
			// See FIXME below for why this is false.
			pc.RedirectStandardError = false;

			// Let pdfinfo run for at most 10 CPU seconds, and not
			// use more than 100 megs memory.
			pc.CpuLimit = 90;
			pc.MemLimit = 100*1024*1024;
			pc.UseLangC = true;

			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				Error ();
				return;
			}

			// add pdfinfo's output to pool
			pout = new StreamReader (pc.StandardOutput);
			string str = null;
			int idx = -1;
			string strMetaTag = null;
			bool bKeyword = false;
			string prop = null;
			string val = null;

			while ((str = pout.ReadLine ()) != null) {
				bKeyword = false;
				strMetaTag = null;
				idx = str.IndexOf (':');
				if (idx > 0) {
					prop = str.Substring (0, idx);
					val = str.Substring (idx + 1);
					switch (prop) {
					case "Title":
						strMetaTag = "dc:title";
						break;
					case "Author":
						strMetaTag = "dc:author";
						break;
					case "Pages":
						strMetaTag = "fixme:page-count";
						bKeyword = true;
						break;
					case "Creator":
						strMetaTag = "dc:creator";
						break;
					case "Keywords":
						strMetaTag = "dc:keyword";
						break;
					case "Producer":
						strMetaTag = "dc:appname";
						break;
					case "Metadata":
						string xmpString = pout.ReadToEnd();
						XmpFile xmp = new XmpFile (new MemoryStream(System.Text.Encoding.ASCII.GetBytes(xmpString)));
						AddXmpProperties(xmp);
						break;
					}
					if (strMetaTag != null) {
						if (bKeyword)
							AddProperty (Beagle.Property.NewUnsearched (strMetaTag, 
												 val.Trim ()));
						else
							AddProperty (Beagle.Property.New (strMetaTag, 
											  val.Trim ()));
					}
						
				}
			}
			pout.Close ();

#if false
			// Log any errors or warnings from stderr
			pout = new StreamReader (pc.StandardError);
			while ((str = pout.ReadLine ()) != null)
				Log.Warn ("pdfinfo [{0}]: {1}", Indexable.Uri, str);

			pout.Close ();
#endif
			pc.Close ();
		}
		
		bool pull_started = false;

		private bool InitDoPull ()
		{
			// create new external process
			pc = new SafeProcess ();
			pc.Arguments = new string [] { "pdftotext", "-q", "-nopgbrk", "-enc", "UTF-8", FileInfo.FullName, "-" };
			pc.RedirectStandardOutput = true;

			// FIXME: This should really be true, and we should
			// process the output.  But we can deadlock when
			// pdftotext is blocked writing to stderr because of a
			// full buffer and we're blocking while reading from
			// stdout.
			pc.RedirectStandardError = false;

			// Let pdftotext run for at most 90 CPU seconds, and not
			// use more than 100 megs memory.
			pc.CpuLimit = 90;
			pc.MemLimit = 100*1024*1024;

			try {
				pc.Start ();
			} catch (SafeProcessException e) {
				Log.Warn (e.Message);
				Error ();
				return false;
			}

			// add pdftotext's output to pool
			pout = new StreamReader (pc.StandardOutput);
			pull_started = true;

			return true;
		}

		protected override void DoPull ()
		{
			// InitDoPull() calls Error() if it fails
			if (! pull_started && ! InitDoPull ())
				return;

			int n = 0;

			// Using internal information: Lucene currently asks for char[2048] data
			while (n <= 2048) {

				// FIXME:  I don't think this is really required
				// Line by line parsing, however, we have to make
				// sure, that "pdftotext" doesn't output any "New-lines".
				string str = pout.ReadLine ();
				if (str == null) {
					Finished ();
					return;
				} else {
					AppendLine (str);
					AppendStructuralBreak ();
					// If we have added 2048 chars, stop
					// DoPull is called repeatedly till the buffer is full,
					// so stop after the buffer is full (and possibly overflown)
					// to reduce number of function calls
					n += str.Length;
					n ++; // for the structural break
				}
			}
		}

		override protected void DoClose ()
		{
			if (! pull_started)
				return;

			pout.Close ();
#if false
			// FIXME: See FIXME above.
			pout = new StreamReader (pc.StandardError);

			string str;
			while ((str = pout.ReadLine ()) != null)
				Log.Warn ("pdftotext [{0}]: {1}", Uri, str);

			pout.Close ();
#endif
			pc.Close ();
		}
		
		private void AddXmpProperties (XmpFile xmp)
		{
			Resource subject_anon = null;
			Resource creator_anon = null;
			Resource rights_anon = null;
			Resource title_anon = null;

			foreach (Statement stmt in xmp.Store) {
				if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:subject")) {
					//Console.WriteLine ("found subject");
					subject_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:creator")) {
					//Console.WriteLine ("found creator");
					creator_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:rights")) {
					rights_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("dc:title")) {
					if (stmt.Object is Literal)
						AddProperty (Beagle.Property.New ("dc:title", ((Literal)stmt.Object).Value));
					else if (stmt.Object is BNode)
						title_anon = stmt.Object;
				} else if (stmt.Predicate == MetadataStore.Namespaces.Resolve ("cc:license")) {
					AddProperty (Beagle.Property.NewKeyword ("fixme:license", ((Literal)stmt.Object).Value));
				}
			}
			
			foreach (Statement stmt in xmp.Store) {
				if (stmt.Subject == subject_anon && 
				    stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:subject", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == creator_anon &&  
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:creator", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == rights_anon &&  
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:rights", ((Literal)stmt.Object).Value));
				} else if (stmt.Subject == title_anon &&
					   stmt.Predicate != MetadataStore.Namespaces.Resolve ("rdf:type")) {
					AddProperty (Beagle.Property.New ("dc:title", ((Literal)stmt.Object).Value));
				}
			}
		}
	}
}

