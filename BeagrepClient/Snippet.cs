//
// Snippet.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Collections;
using System.Text;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

using Beagrep.Util;

namespace Beagrep {

        public class SnippetRequest : RequestMessage {

                public Hit Hit;
                public string[] QueryTerms;

                // If fulltext is true, returns the full text instead of snippet.
                // FIXME: Highlight fulltext with query words. It will be expensive and clients
                // should know that.
                public bool FullText = false; // default, not fulltext

                public int ContextLength = -1; // Use system default = 6 if not specified

                public int SnippetLength = -1; // 200, if not specified

                public SnippetRequest () : base (false) { }

                public SnippetRequest (Query query, Hit hit) : base (false)
                {
                        this.QueryTerms = new string [query.StemmedText.Count];
                        int i = 0;
                        foreach (string term in query.StemmedText) {
                                this.QueryTerms [i] = term;
                                ++i;
                        }

                        this.Hit = hit;
                }
        }

        public class SnippetResponse : ResponseMessage {
                [XmlElement ("Snippets")]
                public SnippetList SnippetList;

                [XmlIgnore]
                public string Snippet {
                        // A handy method for lazy people
                        get { return SnippetList.ToString (); }
                }

                public SnippetResponse () { }

                public SnippetResponse (SnippetList snippet_list)
                {
                        this.SnippetList = snippet_list;
                }
        }

        ///////////// How to send a snippet to a client ? /////////////////

        public class Fragment {
                [XmlAttribute ("QueryTermIndex")]
                public int QueryTermIndex;

                [XmlText]
                public string Text;

                public Fragment ()
                {
                }

                public Fragment (int index, string text)
                {
                        this.QueryTermIndex = index;
                        this.Text = text;
                }
        }

        [System.Xml.Serialization.XmlRoot("SnippetLine", Namespace="", IsNullable=false)]
        [System.Xml.Serialization.XmlType("SnippetLine", Namespace="")]
        public class SnippetLine {
                /// <value>
                /// Line number of the snippet
                /// </value>
                [XmlAttribute]
                public ulong Line;

                /// <value>
                /// List of fragments
                /// </value>
                [XmlArrayItem (ElementName="Fragment", Type=typeof (Fragment))]
                public ArrayList Fragments;

                /// <value>
                /// Number of fragments
                /// </value>
                [XmlIgnore]
                public int Count {
                        get {
                                if (Fragments == null)
                                        return 0;
                                return Fragments.Count;
                        }
                }

                public SnippetLine ()
                {
                        this.Line = 0;
                }

                public SnippetLine (ulong line)
                {
                        this.Line = line;
                }

                public void AddNonMatchFragment (string text)
                {
                        AddMatchFragment (-1, text);
                }

                public void AddMatchFragment (int query_term_index, string text)
                {
                        text = StringFu.CleanupInvalidXmlCharacters (text);
                        if (String.IsNullOrEmpty (text))
                                return;

                        if (Fragments == null)
                                Fragments = new ArrayList (3); // mostly will be 3 fragments

                        // Before we send a snippet over the wire, clean up any
                        // characters that would be invalid in XML.
                        Fragments.Add (new Fragment (query_term_index, text));
                }

                [XmlIgnore]
                public int Length {
                        get {
                                if (Fragments == null)
                                        return 0;
                                int i = 0;
                                foreach (Fragment fragment in Fragments)
                                        i += fragment.Text.Length;
                                return i;
                        }
                }

                static string[] colors = new string [] {"red", "blue", "green", "orange", "purple", "brown"};

                public override string ToString ()
                {
                        if (Fragments == null)
                                return null;

                        string line = null;
                        foreach (Fragment fragment in Fragments) {
                                if (fragment.QueryTermIndex == -1)
                                        line += fragment.Text;
                                else
                                        line = String.Concat (line,
                                                            "<font color=\"",
                                                            colors [fragment.QueryTermIndex % colors.Length],
                                                            "\"><b>",
                                                            fragment.Text,
                                                            "</b></font>");
                        }
                        return line;
                }
        }

        // SnippetReader needs Lucene.Net, so cannot be moved here!
        public interface ISnippetReader {
                IEnumerable GetSnippet ();
                string ReadLine ();
                void Close ();
        }

        /* Custom serialized Snippet class. Consists of a list of lines with matches.
         * Each match consists of the line number and a sequence of fragments showing the contextual
         * matchings in the line. Each fragment specifies if it a mon-match contextual string, or
         * if it a match to a query term (in which case it also gives the index to the query.stemmed_text[]
         * of the query term which matched). Pretty complicated, but contains all information to
         * make everyone happy.
         * e.g.
         * <Snippets FullText="false">
         *      <SnippetLine Line="1234">
         *              <Fragment QueryTermIndex="-1">non match word</Fragment>
         *              <Fragment QueryTermIndex="2">matched</Fragment>
         *      </Snippet>
         *      <Snippet Line="1244">
         *              <Fragment QueryTermIndex=1">one</Fragment>
         *      </Snippet>
         * </Snippets>
         * or
         * <Snippets FullText="true">
         *      <SnippetLine Line="1">
         *              <Fragment QueryTermIndex="-1">full text</Fragment>
         *      </Snippet>
         * <Snippets>
         * The actual snippets are fetched at runtime.
         */
        // Serialized as "Snippets"
        public class SnippetList : IXmlSerializable {
                private ArrayList snippets; // List of SnippetLine objects
                private ISnippetReader snippet_reader = null;

                [XmlAttribute]
                public bool FullText = false;

                [XmlIgnore]
                public IEnumerable Snippets {
                        get { return snippets; }
                }

                // Returns a coloured snippet string as before
                public override string ToString ()
                {
                        if (snippets == null)
                                return null;

                        StringBuilder sb = new StringBuilder ();
                        foreach (SnippetLine snippet_line in snippets) {
                                sb.Append (snippet_line.ToString ());
                                sb.Append (" ... ");
                        }

                        return sb.ToString ();
                }

                private static XmlSerializer snippet_line_ser = new XmlSerializer (typeof (SnippetLine));

                public SnippetList ()
                {
                }

                public SnippetList (bool full_text, ISnippetReader snippet_reader)
                {
                        this.FullText = full_text;
                        this.snippet_reader = snippet_reader;
                }

                public void WriteXml (XmlWriter writer)
                {
                        writer.WriteAttributeString ("FullText", (FullText ? "true" : "false"));

                        if (snippet_reader == null)
                                return;

                        if (FullText) {
                                writer.WriteStartElement ("SnippetLine");
                                writer.WriteAttributeString ("Line", "1");
                                writer.WriteStartElement ("Fragment");
                                writer.WriteAttributeString ("QueryTermIndex", "-1");

                                string line;
                                // Read data from snippet_reader and write
                                while ((line = snippet_reader.ReadLine ()) != null) {
                                        writer.WriteString (StringFu.CleanupInvalidXmlCharacters (line));
                                        writer.WriteString ("\n");
                                }

                                writer.WriteEndElement ();
                                writer.WriteEndElement ();
                        } else if (snippet_reader.GetSnippet() != null) {
                        // If fulltext is false, read lines from snippet reader
                                foreach (SnippetLine snippet_line in snippet_reader.GetSnippet ()) {
                                        writer.WriteStartElement ("SnippetLine");
                                        writer.WriteAttributeString ("Line", Convert.ToString (snippet_line.Line));
                                        foreach (Fragment fragment in snippet_line.Fragments) {
                                                writer.WriteStartElement ("Fragment");
                                                writer.WriteAttributeString ("QueryTermIndex", Convert.ToString (fragment.QueryTermIndex));
                                                writer.WriteString (fragment.Text);
                                                writer.WriteEndElement ();
                                        }
                                        writer.WriteEndElement ();
                                }
                        }
                        snippet_reader.Close ();
                }

                public void ReadXml (XmlReader reader)
                {
                        FullText = Convert.ToBoolean (reader.GetAttribute ("FullText"));
                        reader.MoveToContent ();

                        if (reader.IsEmptyElement) // no <snippetline>...</snippetline>
                                return;

                        reader.Read ();
                        reader.MoveToContent (); // Keep doing this, to skip over the whitespaces
                        while (reader.Name == "SnippetLine" && reader.NodeType == XmlNodeType.Element) {
                                string snippet_line_string = reader.ReadOuterXml (); // could be really huge for full text
                                reader.MoveToContent ();
                                SnippetLine snippet_line = (SnippetLine) snippet_line_ser.Deserialize (new StringReader (snippet_line_string));
                                if (snippets == null)
                                        snippets = new ArrayList ();
                                snippets.Add (snippet_line);
                        }
                        reader.ReadEndElement ();
                }

                public XmlSchema GetSchema ()
                {
                        return null;
                }
        }

}
