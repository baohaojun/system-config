//
// FilterKCal.cs
// WARNING: This is not a general purpose ical/vcard filter!
// It could be modified to serve that purpose, but for the time
// being, it is just a base class to filter kcal (knotes, korganizer and 
// kaddressbook) files.
//
// Copyright (C) 2006 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Collections;
using System.IO;
using System.Text;
using System.Globalization;

using Beagle.Daemon;
using Beagle.Util;

namespace Beagle.Filters {

	public abstract class FilterKCal : Beagle.Daemon.Filter {
		protected Hashtable KCalNamePropertyMapping;

		protected enum KCalType {
			Special, // Special handling
			Text,
			Date
		}

		protected struct KCalProperty {
			public string property_name;
			public bool is_comma_sep;
			public bool is_keyword;
			public KCalType property_type;

			public KCalProperty (string name, bool comma_sep, bool keyword, KCalType type)
			{
				property_name = name;
				is_comma_sep = comma_sep;
				is_keyword = keyword;
				property_type = type;
			}

			public KCalProperty (KCalType type) : this (null, false, false, type)
			{
			}
		}

		protected struct KCalPropertyParameter {
			public string param_name;
			public string param_value;

			public KCalPropertyParameter (string name, string value)
			{
				param_name = name;
				param_value = value;
			}
		}

		virtual protected Hashtable KCalPropertyMapping {
			get { return null;}
		}

		override protected void DoPullProperties ()
		{
			ICalParser parser = new ICalParser (TextReader);
			string current_property = null;
			string current_parameter = null;
			ArrayList paramlist = new ArrayList ();

			while (parser.MoveNext()) {
				ICalParser.Token token = (ICalParser.Token) parser.Current;
				//Console.WriteLine (token);
				
				if (token.Type == ICalParser.TokenType.Property)
					current_property = token.Value;

				else if (token.Type == ICalParser.TokenType.PropertyParameter)
					current_parameter = token.Value;

				else if (token.Type == ICalParser.TokenType.PropertyParameterValue) {
					paramlist.Add (new KCalPropertyParameter (
						current_parameter,
						token.Value));

				} else if (token.Type == ICalParser.TokenType.PropertyValue) {
					ProcessProperty (current_property,
							 paramlist,
							 token.Value);
				}
			}
		}

		override protected void DoPull ()
		{
			Finished ();
		}

		private void ProcessProperty (string prop_name,
					      ArrayList paramlist,
					      string prop_value)
		{
			if (prop_name == null || paramlist == null)
				return;

			if (prop_value == null)
				prop_value = String.Empty;

			if (! KCalPropertyMapping.Contains (prop_name))
				return;

			KCalProperty vcp = (KCalProperty) KCalPropertyMapping [prop_name];
			if (vcp.property_type == KCalType.Special)
				ProcessPropertySpecial (prop_name, paramlist, prop_value);

			string beagle_prop_name = GetPropertyName (prop_name, paramlist);

			if (vcp.property_type == KCalType.Date) {
				DateTime dt = ProcessKCalDate (prop_value);
				AddProperty (Beagle.Property.NewDate (beagle_prop_name, dt));
			} else if (vcp.property_type == KCalType.Text) {
				prop_value = prop_value.Replace ("\\,", ",");
				prop_value = prop_value.Replace ("\\\\", "\\");
				prop_value = prop_value.Replace ("\\n", "\n");

				if (vcp.is_comma_sep) {
					foreach (string sub_value in prop_value.Split (',')) {
						if (vcp.is_keyword)
							AddProperty (Beagle.Property.NewKeyword (
								beagle_prop_name,
								sub_value));
						else
							AddProperty (Beagle.Property.New (
								beagle_prop_name,
								sub_value));
					}
				} else {
					if (vcp.is_keyword)
						AddProperty (Beagle.Property.NewKeyword (beagle_prop_name, prop_value));
					else
						AddProperty (Beagle.Property.New (beagle_prop_name, prop_value));
				}
			}

			paramlist.Clear ();

		}

		protected DateTime ProcessKCalDate (string date_string)
		{
			DateTime dt = DateTime.MinValue;

			try {
				date_string = date_string.Replace ("Z", "+00:00");
				string[] fmts =
				{
					// ISO 8601 formats
					"yyyy-MM-ddTHH:mm:sszzz",
					"yyyyMMddTHHmmsszzz"
				};
				
				dt = DateTime.ParseExact (
					date_string,
					fmts,
					DateTimeFormatInfo.InvariantInfo,
					DateTimeStyles.AdjustToUniversal);
			} catch (Exception e) {
				Log.Debug (e, "Error while parsing date string [{0}]", date_string);
			}
			return dt;
		}
		virtual protected string GetPropertyName (string prop_name, ArrayList paramlist)
		{
			string mapped_prop_name =
				((KCalProperty)KCalPropertyMapping [prop_name]).property_name;
			return mapped_prop_name;
		}

		virtual protected void ProcessPropertySpecial (string prop_name,
							       ArrayList paramlist,
							       string prop_value)
		{
		}
	}
}
