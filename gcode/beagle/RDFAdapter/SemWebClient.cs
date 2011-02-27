//
// SemWebClient.cs
//
// Copyright (C) 2007 Enrico Minack <minack@l3s.de>
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.IO;
using Beagle;
using Beagle.Util;
using SemWeb;
using SemWeb.Query;
using SemWeb.Inference;

public class SemWebClient {
	public static void Main (string[] args)
	{
		BeagleSource source = new BeagleSource ();

		TestSource (source);
		//TestEmailThreads (source);
		//TestReceipients (source);
	}

	private static void TestSource (BeagleSource source)
	{
		source.RDFToBeagle = new BeagleSource.RDFToBeagleHook (RDFToBeagle);
		source.BeagleToRDF = new BeagleSource.BeagleToRDFHook (EmailToEntity);

		System.Console.Out.WriteLine ();
		System.Console.Out.WriteLine ("Querying for all Triples with MimeType:");
		query (source, new Statement (null, new Entity ("prop:k:beagle:MimeType"), null));

		System.Console.Out.WriteLine ();
		System.Console.Out.WriteLine ("Querying for all Triples with FileSize:");
		query (source, new Statement (null, BeagleSource.BeaglePropertyToEntity ("prop:k:fixme:filesize"), null));
		
		System.Console.Out.WriteLine ();
		System.Console.Out.WriteLine ("Querying for all Triples:");
		query (source, Statement.All);
	}
	
	public static void query (SelectableSource source, Statement filter) {
		using (RdfWriter writer = new N3Writer (System.Console.Out))
			source.Select (filter, writer);
	}

	// WARNING! This will generate all threads, pretty expensive
	private static void TestEmailThreads (BeagleSource source)
	{
		source.BeagleToRDF = new BeagleSource.BeagleToRDFHook (MsgIdToEntity);
		source.RDFToBeagle = new BeagleSource.RDFToBeagleHook (RDFToBeagle);
		Entity inthread = BeagleSource.BeaglePropertyToEntity ("inthread");

		/* NOTE: change the subject to whatever email subject you wish */
		const string Subject = "beagle";

		string rules = "@prefix : <http://beagle-project.org/property#>.\n" +
			"\n" +
			/* every message is in its own thread */
			"{ ?email :prop:t:dc:title \"" + Subject + "\" . ?email :prop:k:fixme:msgid ?msg . } => { ?msg :inthread ?msg } .\n" +
			/* if any email refers to some email in thread, then this email is also in thread */
			"{ ?ref :inthread ?parent . ?email1 :prop:k:fixme:reference ?ref . ?email1 :prop:k:fixme:msgid ?msg .} => {?msg :inthread ?parent} .\n";

		Statement msg_in_thread = new Statement (new Variable ("msg"), inthread, new Variable ("parent"));
		Console.WriteLine ("Proof of 'threads in email with subject :{0}'", Subject);
		EulerQuery (source, rules, new Statement[] {msg_in_thread});
	}

	private static void TestReceipients (BeagleSource source)
	{
		source.BeagleToRDF = new BeagleSource.BeagleToRDFHook (EmailToEntity);
		source.RDFToBeagle = new BeagleSource.RDFToBeagleHook (RDFToBeagle);

		Entity recvd = BeagleSource.BeaglePropertyToEntity ("recvd");

		string rules = "@prefix : <http://beagle-project.org/property#>.\n" +
			"\n" +
			"{ ?email :prop:t:fixme:from_address ?from . ?email :prop:t:fixme:to_address ?to . } => {?from :recvd ?to .}.\n" +
			"{ ?email :prop:t:fixme:from_address ?from . ?email :prop:t:fixme:cc_address ?to . } => {?from :recvd ?to .}.\n";

		/* NOTE: change the sender to whatever email address you wish */
		const string Sender = "dbera.web@gmail.com";

		Entity from = new Entity ("mailto://" + Sender);
		Variable to = new Variable ("recpt");
		Statement q = new Statement(from, recvd, to);

		Console.WriteLine ("Proof of 'receipients of emails from :{0}'", Sender);
		EulerQuery (source, rules, new Statement[] {q});
	}

	private static void EulerQuery (SelectableSource source, string rules, Statement[] queries)
	{
		Store store = new Store (source);

		Euler engine = new Euler(new N3Reader(new StringReader(rules)));

		foreach (Proof p in engine.Prove (store, queries)) {
			foreach (Statement s in p.Proved)
				Console.WriteLine(s);
		}
	}

	// Make URIs out of certain objects
	private static void RDFToBeagle (Entity subj, Entity pred, Resource obj, out Uri s, out string p, out string o)
	{
		s = (subj == null || String.IsNullOrEmpty (subj.Uri)) ? null : new Uri (subj.Uri);
		p = (pred == null || String.IsNullOrEmpty (pred.Uri)) ? null : pred.Uri.Substring (BeagleSource.Prefix.Length);
		o = null;

		if (obj != null) {
			if (obj is Literal) {
				Literal l = (Literal) obj;
				o = l.Value;
			} else {
				o = obj.Uri;
				if (o.StartsWith ("mailto://"))
					o = o.Substring (9);
				else if (o.StartsWith ("email://"))
					o = o.Substring (8);
			}
		}
	}

	private static void EmailToEntity (Property prop, out Resource _object)
	{
		_object = null;

		// Create URIs for email addresses
		if (prop.Key == "fixme:from_address" || prop.Key == "fixme:cc_address" || prop.Key == "fixme:to_address")
			_object = new Entity ("mailto://" + prop.Value);
		else
			_object = new Literal (prop.Value);
	}

	private static void MsgIdToEntity (Property prop, out Resource _object)
	{
		_object = null;

		// Create URIs for email msg ids
		if (prop.Key == "fixme:msgid" || prop.Key == "fixme:reference")
			_object = new Entity ("email://" + prop.Value);
		else
			_object = new Literal (prop.Value);
	}
}

