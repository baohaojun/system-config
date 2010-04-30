//
// BeagleSource.cs
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

using Beagle;
using SemWeb;
using System;

public class BeagleSource : SelectableSource {

	public delegate void RDFToBeagleHook (Entity subject, Entity predicate, Resource obj, out Uri s, out string p, out string o);
	public delegate void BeagleToRDFHook (Property property, out Resource resource); // Convert a property to the correct RDF resouce to be used as an object

	private RDFToBeagleHook rdf_to_beagle_hook;
	private BeagleToRDFHook property_to_rdf_hook;

	public RDFToBeagleHook RDFToBeagle {
		set { rdf_to_beagle_hook = value; }
	}

	public BeagleToRDFHook BeagleToRDF {
		set { property_to_rdf_hook = value; }
	}

	public BeagleSource () : base ()
	{
		rdf_to_beagle_hook = new RDFToBeagleHook (DefaultRDFToBeagle);
		property_to_rdf_hook = new BeagleToRDFHook (DefaultPropertyToRDF);
	}

	// counts the statements that match the template and returns if this number > 0
	public bool Contains (Statement template)
	{
		StatementCounterSink sink = new StatementCounterSink ();
		this.Select (template, sink);
		return sink.StatementCount > 0;
	}

	public bool Contains (Resource resource)
	{
		// FIXME !
		throw new NotImplementedException ("BeagleSource.Contains (Resource)");
	}

	// we provide only distinct statements
	public bool Distinct {
		get { return true; }
	}

	// this simply forwards to a select all
	public void Select (StatementSink sink)
	{
		this.Select (Statement.All, sink);
	}

	public void Select (Statement template, StatementSink sink)
	{
		// extract the fields for easy access
		Entity subj = template.Subject;
		Entity pred = template.Predicate;
		Resource obj = template.Object;

		// convert the SemWeb fields to the RDFQuery fields
		Uri s;
		string p, o;
		rdf_to_beagle_hook (subj, pred, obj, out s, out p, out o);

		RDFQuery query = new RDFQuery (s, p, o);
		RDFQueryResult result = (RDFQueryResult) query.Send ();
		
		foreach (Hit hit in result.Hits) {
			Entity subject = new Entity (hit.Uri.ToString ()); //FIXME: Do we have to use strings here?
			foreach (Property prop in hit.Properties) {
				Entity predicate = BeaglePropertyToEntity (prop.Type, prop.Key);
				Resource _object;
				property_to_rdf_hook (prop, out _object);
			
				// now create a the statement and add it to the result
				Statement st = new Statement (subject, predicate, _object);
				sink.Add (st);
			}
		}
	}

	public void Select (SelectFilter filter, StatementSink sink)
	{
		throw new NotImplementedException ("Select");
		// FIXME: not implemented yet, SelectFilter are a little more complex
		// than Statements with wildcards
	}

	// copied from SemWeb/Store.cs
	internal class StatementCounterSink : StatementSink {
		int counter = 0;

		public int StatementCount {
			get { return counter; }
		}

		public bool Add (Statement statement) {
			counter ++;
			return true;
		}
	}

	public const string Prefix = "http://beagle-project.org/property#";

	public static Entity BeaglePropertyToEntity (string propname)
	{
		return BeaglePropertyToEntity (PropertyType.Internal, propname);
	}

	public static Entity BeaglePropertyToEntity (PropertyType type, string propname)
	{
		if (String.IsNullOrEmpty (propname))
			throw new Exception ("bad property name");

		return new Entity (Prefix + PropertyToFieldName (type, propname));
	}

	//////////// Internal knowledge (LuceneCommon.cs)

	static private char TypeToCode (PropertyType type)
	{
		switch (type) {
		case PropertyType.Text:    return 't';
		case PropertyType.Keyword: return 'k';
		case PropertyType.Date:    return 'd';
		}
		throw new Exception ("Bad property type: " + type);
	}

	private static string PropertyToFieldName (PropertyType type, string key)
	{
		if (type == PropertyType.Internal)
			return key;
		return String.Format ("prop:{0}:{1}", TypeToCode (type), key);

	}

	//////////////////////////////////////////////////

	private static void DefaultRDFToBeagle (Entity subj, Entity pred, Resource obj, out Uri s, out string p, out string o)
	{
		s = (subj == null || String.IsNullOrEmpty (subj.Uri)) ? null : new Uri (subj.Uri);
		p = (pred == null || String.IsNullOrEmpty (pred.Uri)) ? null : pred.Uri.Substring (Prefix.Length);
		o = null;

		if (obj != null) {
			if (obj is Literal) {
				Literal l = (Literal) obj;
				o = l.Value;
			} else {
				o = obj.Uri;
			}
		}
	}

	private static void DefaultPropertyToRDF (Property prop, out Resource _object)
	{
		_object = null;

		// for some properties the object is actually an URI (Entity)
		if (prop.Key == "ParentDirUri")
			_object = new Entity (prop.Value);
		else
			_object = new Literal (prop.Value);
	}
}
