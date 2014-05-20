//
// LuceneCommon.cs
//
// Copyright (C) 2004-2007 Novell, Inc.
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
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using System.Threading;
using System.Xml;
using System.Xml.Serialization;

using Lucene.Net.Analysis;
using Lucene.Net.Analysis.Standard;
using Lucene.Net.Documents;
using Lucene.Net.Index;
using Lucene.Net.QueryParsers;
using LNS = Lucene.Net.Search;

using Beagrep.Util;

namespace Beagrep.Daemon {

        public class LuceneCommon {

                public delegate bool HitFilter (Hit hit);
                public delegate QueryPart QueryPartHook (QueryPart query_part);

                // VERSION HISTORY
                // ---------------
                //
                //  1: Original
                //  2: Changed format of timestamp strings
                //  3: Schema changed to be more Dashboard-Match-like
                //  4: Schema changed for files to include _Directory property
                //  5: Changed analyzer to support stemming.  Bumped version # to
                //     force everyone to re-index.
                //  6: lots of schema changes as part of the general refactoring
                //  7: incremented to force a re-index after our upgrade to lucene 1.4
                //     (in theory the file formats are compatible, we are seeing 'term
                //     out of order' exceptions in some cases)
                //  8: another forced re-index, this time because of massive changes
                //     in the file system backend (it would be nice to have per-backend
                //     versioning so that we didn't have to purge all indexes just
                //     because one changed)
                //  9: changed the way properties are stored, changed in conjunction
                //     with sane handling of multiple properties on hits.
                // 10: changed to support typed and mutable properties
                // 11: moved mime type and hit type into properties
                // 12: added year-month and year-month-day resolutions for all
                //     date properties
                // 13: moved source into a property
                // 14: allow wildcard queries to also match keywords
                // 15: analyze PropertyKeyword field, and store all properties as
                //     lower case so that we're truly case insensitive.
                // 16: add inverted timestamp to make querying substantially faster
                // 17: add boolean property to denote a child indexable
                // 18: add IsPersistent to properties, and adjust coded values
                //     in AddPropertyToDocument() and GetPropertyFromDocument();
                //     changed subdate field format rules for better readability
                // 19: Update lucene and analyzer (lucene.net-2.1)
                private const int MAJOR_VERSION = 19;
                private int minor_version = 0;

                private string index_name;
                private string top_dir;

                private string fingerprint;
                private int last_item_count = -1;

                // This is the big index, containing document full-texts and
                // data that is expensive to index.
                private Lucene.Net.Store.Directory primary_store = null;

                // This is the small index, containing document info that we
                // expect to have change.  Canonical example: file names.
                private Lucene.Net.Store.Directory secondary_store = null;

                // Flush if more than this number of requests
                public const int RequestFlushThreshold = Lucene.Net.Index.IndexWriter.DEFAULT_MAX_BUFFERED_DOCS; // Use same value as Lucene's flush threshold

                //////////////////////////////////////////////////////////////////////////////

                protected LuceneCommon (string index_name, int minor_version)
                {
                        this.index_name = index_name;
                        this.minor_version = minor_version;

                        this.top_dir = (Path.IsPathRooted (index_name)) ? index_name : Path.Combine (PathFinder.IndexDir, index_name);
                }

                //////////////////////////////////////////////////////////////////////////////

                public string IndexName { get { return index_name; } }

                public Lucene.Net.Store.Directory PrimaryStore { get { return primary_store; } }

                public Lucene.Net.Store.Directory SecondaryStore { get { return secondary_store; } }

                public string Fingerprint { get { return fingerprint; } }

                public string TopDirectory { get { return top_dir; } }

                //////////////////////////////////////////////////////////////////////////////

                protected TextCache text_cache = null;

                public TextCache TextCache {
                        get { return text_cache; }
                        set { text_cache = value; }
                }

                //////////////////////////////////////////////////////////////////////////////

                private string VersionFile {
                        get { return Path.Combine (top_dir, "version"); }
                }

                private string FingerprintFile {
                        get { return Path.Combine (top_dir, "fingerprint"); }
                }

                // Shouldn't really be public
                public string PrimaryIndexDirectory {
                        get { return Path.Combine (top_dir, "PrimaryIndex"); }
                }

                // Shouldn't really be public
                public string SecondaryIndexDirectory {
                        get { return Path.Combine (top_dir, "SecondaryIndex"); }
                }

                public string LockDirectory {
                        get { return Path.Combine (top_dir, "Locks"); }
                }

                //////////////////////////////////////////////////////////////////////////////

                // Deal with dangling locks

                private bool IsDanglingLock (FileInfo info)
                {
                        Log.Debug ("Checking for dangling locks...");

                        // It isn't even a lock file
                        if (! info.Name.EndsWith (".lock"))
                                return false;

                        StreamReader reader;
                        string pid = null;

                        try {
                                reader = new StreamReader (info.FullName);
                                pid = reader.ReadLine ();
                                reader.Close ();

                        } catch {
                                // We couldn't read the lockfile, so it probably went away.
                                return false;
                        }


                        if (pid == null) {
                                // Looks like the lock file was empty, which really
                                // shouldn't happen.  It should contain the PID of
                                // the process which locked it.  Lets be on the safe
                                // side and assume it's a dangling lock.
                                Log.Warn ("Found an empty lock file, that shouldn't happen: {0}", info.FullName);
                                return true;
                        }

                        string cmdline_file;
                        cmdline_file = String.Format ("/proc/{0}/cmdline", pid);

                        string cmdline = "";
                        try {
                                reader = new StreamReader (cmdline_file);
                                cmdline = reader.ReadLine ();
                                reader.Close ();
                        } catch {
                                // If we can't open that file, either:
                                // (1) The process doesn't exist
                                // (2) It does exist, but it doesn't belong to us.
                                //     Thus it isn't an IndexHelper
                                // In either case, the lock is dangling --- if it
                                // still exists.
                                return info.Exists;
                        }

                        // The process exists, but isn't an IndexHelper.
                        // If the lock file is still there, it is dangling.
                        // FIXME: During one run of bludgeon I got a null reference
                        // exception here, so I added the cmdline == null check.
                        // Why exactly would that happen?  Is this logic correct
                        // in that (odd and presumably rare) case?
                        if (cmdline == null || cmdline.IndexOf ("IndexHelper.exe") == -1)
                                return info.Exists;

                        // If we reach this point, we know:
                        // (1) The process still exists
                        // (2) We own it
                        // (3) It is an IndexHelper process
                        // Thus it almost certainly isn't a dangling lock.
                        // The process might be wedged, but that is
                        // another issue...
                        return false;
                }

                protected bool Exists ()
                {
                        if (! (Directory.Exists (top_dir)
                               && File.Exists (VersionFile)
                               && File.Exists (FingerprintFile)
                               && Directory.Exists (PrimaryIndexDirectory)
                               && IndexReader.IndexExists (PrimaryIndexDirectory)
                               && Directory.Exists (SecondaryIndexDirectory)
                               && IndexReader.IndexExists (SecondaryIndexDirectory)
                               && Directory.Exists (LockDirectory)))
                                return false;

                        // Check the index's version number.  If it is wrong,
                        // declare the index non-existent.

                        StreamReader version_reader;
                        string version_str;
                        version_reader = new StreamReader (VersionFile);
                        version_str = version_reader.ReadLine ();
                        version_reader.Close ();

                        int current_major_version, current_minor_version;
                        int i = version_str.IndexOf ('.');

                        try {
                                if (i != -1) {
                                        current_major_version = Convert.ToInt32 (version_str.Substring (0, i));
                                        current_minor_version = Convert.ToInt32 (version_str.Substring (i+1));
                                } else {
                                        current_minor_version = Convert.ToInt32 (version_str);
                                        current_major_version = 0;
                                }
                        } catch (FormatException) {
                                // Something wrong with the version file.
                                return false;
                        }

                        if (current_major_version != MAJOR_VERSION
                            || (minor_version >= 0 && current_minor_version != minor_version)) {
                                Logger.Log.Debug ("Version mismatch in {0}", index_name);
                                Logger.Log.Debug ("Index has version {0}.{1}, expected {2}.{3}",
                                                  current_major_version, current_minor_version,
                                                  MAJOR_VERSION, minor_version);
                                return false;
                        }

                        // Check the lock directory: If there is a dangling write lock,
                        // assume that the index is corrupted and declare it non-existent.
                        DirectoryInfo lock_dir_info;
                        lock_dir_info = new DirectoryInfo (LockDirectory);
                        bool dangling_lock = false;

                        foreach (FileInfo info in lock_dir_info.GetFiles ()) {
                                if (IsDanglingLock (info)) {
                                        Logger.Log.Warn ("Found a dangling index lock on {0}.", info.FullName);
                                        dangling_lock = true;
                                }
                        }

                        if (dangling_lock) {
                                Beagrep.Util.Stopwatch w = new Beagrep.Util.Stopwatch ();
                                w.Start ();

                                if (VerifyLuceneIndex (PrimaryIndexDirectory) &&
                                    VerifyLuceneIndex (SecondaryIndexDirectory)) {
                                        w.Stop ();

                                        Log.Warn ("Indexes verified in {0}.  Deleting stale lock files.", w);

                                        try {
                                                foreach (FileInfo info in lock_dir_info.GetFiles ())
                                                        info.Delete ();
                                        } catch {
                                                Log.Warn ("Could not delete lock files.");
                                                return false;
                                        }
                                        return true;
                                } else
                                        return false;
                        }

                        return true;
                }

                private bool VerifyLuceneIndex (string path)
                {
                        if (! Directory.Exists (path))
                                return true;

                        Log.Debug ("Verifying index {0}", path);

                        IndexReader reader = null;
                        TermEnum enumerator = null;
                        TermPositions positions = null;

                        try {
                                reader = IndexReader.Open (path);

                                // Crawl all of the terms in the index, and get the
                                // term positions and crawl those as well.  This
                                // method is suggested as a way to verify the index
                                // here:
                                //
                                // http://mail-archives.apache.org/mod_mbox/lucene-java-user/200504.mbox/%3c4265767B.5090307@getopt.org%3e
                                enumerator = reader.Terms ();

                                do {
                                        Term term = enumerator.Term ();
                                        if (term == null)
                                                break;

                                        positions = reader.TermPositions (term);

                                        while (positions.Next ()) {
                                                int freq = positions.Freq ();

                                                for (int i = 0; i < freq; i++)
                                                        positions.NextPosition ();
                                        }
                                        positions.Close ();
                                        positions = null;
                                } while (enumerator.Next ());

                                enumerator.Close ();
                                enumerator = null;

                                reader.Close ();
                                reader = null;
                        } catch (Exception e) {
                                Log.Warn ("Lucene index {0} is corrupted ({1})", path, e.Message);
                                return false;
                        } finally {
                                if (positions != null)
                                        positions.Close ();

                                if (enumerator != null)
                                        enumerator.Close ();

                                if (reader != null)
                                        reader.Close ();
                        }

                        return true;
                }

                private Lucene.Net.Store.Directory CreateIndex (string path)
                {
                        // Create a directory to put the index in.
                        Directory.CreateDirectory (path);

                        // Create a new store.
                        Lucene.Net.Store.Directory store;
                        store = Lucene.Net.Store.FSDirectory.GetDirectory (path, new Lucene.Net.Store.SimpleFSLockFactory (LockDirectory));

                        // Create an empty index in that store.
                        IndexWriter writer;
                        writer = new IndexWriter (store, null, true);
                        writer.Close ();

                        return store;
                }

                // Create will kill your index dead.  Use it with care.
                // You don't need to call Open after calling Create.
                protected void Create ()
                {
                        if (minor_version < 0)
                                minor_version = 0;

                        // Purge any existing directories.
                        if (Directory.Exists (top_dir)) {
                                Logger.Log.Debug ("Purging {0}", top_dir);
                                Directory.Delete (top_dir, true);
                        }

                        // Create any necessary directories.
                        Directory.CreateDirectory (top_dir);
                        Directory.CreateDirectory (LockDirectory);

                        // Create the indexes.
                        primary_store = CreateIndex (PrimaryIndexDirectory);
                        secondary_store = CreateIndex (SecondaryIndexDirectory);

                        // Generate and store the index fingerprint.
                        fingerprint = GuidFu.ToShortString (Guid.NewGuid ());
                        TextWriter writer;
                        writer = new StreamWriter (FingerprintFile, false);
                        writer.WriteLine (fingerprint);
                        writer.Close ();

                        // Store our index version information.
                        writer = new StreamWriter (VersionFile, false);
                        writer.WriteLine ("{0}.{1}", MAJOR_VERSION, minor_version);
                        writer.Close ();
                }

                protected void Open ()
                {
                        Open (false);
                }

                protected void Open (bool read_only_mode)
                {
                        // Read our index fingerprint.
                        TextReader reader;
                        reader = new StreamReader (FingerprintFile);
                        fingerprint = reader.ReadLine ();
                        reader.Close ();

                        // Create stores for our indexes.
                        // Use separate lock factories since each lock factory is tied to the index directory
                        if (read_only_mode) {
                                primary_store = Lucene.Net.Store.FSDirectory.GetDirectory (PrimaryIndexDirectory, Lucene.Net.Store.NoLockFactory.GetNoLockFactory ());
                                secondary_store = Lucene.Net.Store.FSDirectory.GetDirectory (SecondaryIndexDirectory, Lucene.Net.Store.NoLockFactory.GetNoLockFactory ());
                        } else {
                                primary_store = Lucene.Net.Store.FSDirectory.GetDirectory (PrimaryIndexDirectory, new Lucene.Net.Store.SimpleFSLockFactory (LockDirectory));
                                secondary_store = Lucene.Net.Store.FSDirectory.GetDirectory (SecondaryIndexDirectory, new Lucene.Net.Store.SimpleFSLockFactory (LockDirectory));
                        }
                }

                ////////////////////////////////////////////////////////////////

                //
                // Custom Analyzers
                //

                private class SingletonTokenStream : TokenStream {

                        private string singleton_str;

                        public SingletonTokenStream (string singleton_str)
                        {
                                this.singleton_str = singleton_str;
                        }

                        override public Lucene.Net.Analysis.Token Next ()
                        {
                                if (singleton_str == null)
                                        return null;

                                Lucene.Net.Analysis.Token token;
                                token = new Lucene.Net.Analysis.Token (singleton_str, 0, singleton_str.Length);

                                singleton_str = null;

                                return token;
                        }
                }

                // FIXME: This assumes everything being indexed is in English!
                public class BeagrepAnalyzer : StandardAnalyzer {

                        private char [] buffer = new char [2];
                        private bool strip_extra_property_info = false;
                        private bool tokenize_email_hostname = false;

                        public BeagrepAnalyzer (bool is_indexing_analyzer)
                        {
                                if (is_indexing_analyzer) {
                                        this.strip_extra_property_info = true;
                                        this.tokenize_email_hostname = true;
                                } else {
                                        this.strip_extra_property_info = false;
                                        this.tokenize_email_hostname = false;
                                }
                        }

                        public override TokenStream TokenStream (string fieldName, TextReader reader)
                        {
                                bool is_text_prop = false;

                                // Strip off the first two characters in a property.
                                // We store type information in those two characters, so we don't
                                // want to index them.
                                if (fieldName.StartsWith ("prop:")) {

                                        if (strip_extra_property_info) {
                                                // Skip everything up to and including the first :
                                                int c;
                                                do {
                                                        c = reader.Read ();
                                                } while (c != -1 && c != ':');
                                        }

                                        is_text_prop = fieldName.StartsWith ("prop:t");

                                        // If this is non-text property, just return one token
                                        // containing the entire string.  We do this to avoid
                                        // tokenizing keywords.
                                        if (! is_text_prop) {
                                                // We don't want to lower case the token if it's
                                                // not in the private namespace.

                                                TokenStream singleton_stream = new SingletonTokenStream (reader.ReadToEnd ());

                                                if (fieldName.StartsWith ("prop:k:" + Property.PrivateNamespace))
                                                        return singleton_stream;
                                                else
                                                        return new LowerCaseFilter (singleton_stream);
                                        }
                                } else if (fieldName == "PropertyKeyword")
                                        return new LowerCaseFilter (new SingletonTokenStream (reader.ReadToEnd ()));
                                else if (fieldName == "Properties")
                                        return new WhitespaceTokenizer (new StringReader (reader.ReadToEnd ()));
                                else if (fieldName == "TextLinks")
                                        return new WhitespaceTokenizer (new StringReader (reader.ReadToEnd ()));

                                TokenStream outstream;
                                outstream = base.TokenStream (fieldName, reader);

                                return outstream;
                        }
                }

                static private BeagrepAnalyzer indexing_analyzer = new BeagrepAnalyzer (true);
                static private BeagrepAnalyzer query_analyzer = new BeagrepAnalyzer (false);

                static protected BeagrepAnalyzer IndexingAnalyzer { get { return indexing_analyzer; } }
                static protected BeagrepAnalyzer QueryAnalyzer { get { return query_analyzer; } }

                ////////////////////////////////////////////////////////////////

                //
                // Dealing with properties
                //

                static private char TypeToCode (PropertyType type)
                {
                        switch (type) {
                        case PropertyType.Text:    return 't';
                        case PropertyType.Keyword: return 'k';
                        case PropertyType.Date:    return 'd';
                        }
                        throw new Exception ("Bad property type: " + type);
                }

                static private PropertyType CodeToType (char c)
                {
                        switch (c) {
                        case 't': return PropertyType.Text;
                        case 'k': return PropertyType.Keyword;
                        case 'd': return PropertyType.Date;
                        }

                        throw new Exception ("Bad property code: " + c);
                }

                static private string TypeToWildcardField (PropertyType type)
                {
                        switch (type) {
                        case PropertyType.Text:    return "PropertyText";
                        case PropertyType.Keyword: return "PropertyKeyword";
                        case PropertyType.Date:    return "PropertyDate";
                        }

                        throw new Exception ("Bad property type: " + type);
                }

                static private Field.Index TypeToIndexInstruction (PropertyType type)
                {
                        switch (type) {
                        case PropertyType.Text:    return Field.Index.TOKENIZED; // Full analysis
                        case PropertyType.Keyword: return Field.Index.TOKENIZED; // Lowercases keywords
                        case PropertyType.Date:    return Field.Index.NO_NORMS;  // Do nothing
                        }

                        throw new Exception ("Bad property type: " + type);
                }

                // Exposing this is a little bit suspicious.
                static protected string PropertyToFieldName (PropertyType type, string key)
                {
                        if (type == PropertyType.Internal)
                                return key;
                        return String.Format ("prop:{0}:{1}", TypeToCode (type), key);

                }

                static private void AddDateFields (string field_name, Property prop, Document doc)
                {
                        DateTime dt = StringFu.StringToDateTime (prop.Value);

                        Field f;
                        f = new Field (field_name + "(YM)",
                                       StringFu.DateTimeToYearMonthString (dt),
                                       Field.Store.NO,
                                       Field.Index.NO_NORMS);
                        doc.Add (f);

                        f = new Field (field_name + "(D)",
                                       StringFu.DateTimeToDayString (dt),
                                       Field.Store.NO,
                                       Field.Index.NO_NORMS);
                        doc.Add (f);
                }

                static protected void AddPropertyToDocument (Property prop, Document doc)
                {
                        if (prop == null || String.IsNullOrEmpty (prop.Value))
                                return;

                        // Don't actually put properties in the UnindexedNamespace
                        // in the document.  A horrible (and yet lovely!) hack.
                        if (prop.Key.StartsWith (StringFu.UnindexedNamespace))
                                return;

                        Field f;

                        if (prop.IsSearched) {
                                string wildcard_field = TypeToWildcardField (prop.Type);

                                f = new Field (wildcard_field,
                                               prop.Value,
                                               Field.Store.NO,
                                               TypeToIndexInstruction (prop.Type));

                                // We don't want to include norms for non-text
                                // fields, even if we do tokenize them.
                                if (prop.Type == PropertyType.Keyword || prop.Type == PropertyType.Date)
                                        f.SetOmitNorms (true);

                                doc.Add (f);

                                if (prop.Type == PropertyType.Date)
                                        AddDateFields (wildcard_field, prop, doc);
                        }

                        string coded_value;
                        coded_value = String.Format ("{0}{1}:{2}",
                                                     prop.IsSearched ? 's' : '_',
                                                     prop.IsPersistent ? 'p' : '_',
                                                     prop.Value);

                        string field_name = PropertyToFieldName (prop.Type, prop.Key);

                        f = new Field (field_name,
                                       coded_value,
                                       prop.IsStored ? Field.Store.YES : Field.Store.NO,
                                       Field.Index.TOKENIZED);
                        doc.Add (f);

                        if (prop.Type == PropertyType.Date)
                                AddDateFields (field_name, prop, doc);
                }

                public static bool DumpIndexMode = false;

                static protected Property GetPropertyFromDocument (Field f, Document doc, bool from_primary_index)
                {
                        // Note: we don't use the document that we pass in,
                        // but in theory we could.  At some later point we
                        // might need to split a property's data across two or
                        // more fields in the document.

                        if (f == null)
                                return null;

                        bool internal_prop = false;

                        string field_name;
                        field_name = f.Name ();
                        if (field_name.Length < 7
                            || ! field_name.StartsWith ("prop:")) {
                                if (DumpIndexMode)
                                        internal_prop = true;
                                else
                                        return null;
                        }

                        string field_value;
                        field_value = f.StringValue ();

                        Property prop;
                        prop = new Property ();

                        if (DumpIndexMode) {
                                prop.Type = CodeToType ( internal_prop ? 'k' : field_name [5]);
                                prop.Key = (internal_prop ? field_name : field_name.Substring (7));
                                prop.Value = (internal_prop ? field_value : field_value.Substring (3));
                        } else {
                                prop.Type = CodeToType (field_name [5]);
                                prop.Key = field_name.Substring (7);
                                prop.Value = field_value.Substring (3);
                        }

                        prop.IsSearched = (field_value [0] == 's');
                        prop.IsPersistent = (field_value [1] == 'p');
                        prop.IsMutable = ! from_primary_index;
                        prop.IsStored = true; // Unstored fields cannot be retrieved

                        return prop;
                }

                //////////////////////////////////////////////////////////////////////////////

                //
                // Dealing with documents
                //

                static protected void BuildDocuments (Indexable indexable,
                                                      out Document primary_doc,
                                                      out Document secondary_doc)
                {
                        primary_doc = new Document ();
                        secondary_doc = null;

                        Field f;

                        // During querying, we retrieve a lucene document with only certain fields
                        // like Uri and Timestamp and quickly check if the document is a good one
                        // The field specified document constructor runs faster if the fields that
                        // are asked for are located at the beginning of the document.
                        // Hence it is better to keep "Uri" and "Timestamp" at the beginning.
                        f = new Field ("Uri", UriFu.UriToEscapedString (indexable.Uri),
                                       Field.Store.YES, Field.Index.NO_NORMS);
                        primary_doc.Add (f);

                        if (indexable.ParentUri != null) {
                                f = new Field ("ParentUri", UriFu.UriToEscapedString (indexable.ParentUri),
                                               Field.Store.YES, Field.Index.NO_NORMS);
                                primary_doc.Add (f);
                        }

                        if (indexable.ValidTimestamp) {
                                // Note that we also want to search in the
                                // Timestamp field when we do a wildcard date
                                // query, so that's why we also add a wildcard
                                // field for each item here.

                                string wildcard_field = TypeToWildcardField (PropertyType.Date);

                                string str = StringFu.DateTimeToString (indexable.Timestamp);
                                f = new Field ("Timestamp", str, Field.Store.YES, Field.Index.NO_NORMS);
                                primary_doc.Add (f);
                                f = new Field (wildcard_field, str, Field.Store.NO, Field.Index.NO_NORMS);
                                primary_doc.Add (f);

                                // Create an inverted timestamp so that we can
                                // sort by timestamp at search-time.
                                long timeval = Convert.ToInt64 (str);
                                // Pad the inverted timestamp with zeroes for proper string comparison during termenum enumeration
                                f = new Field ("InvertedTimestamp", (Int64.MaxValue - timeval).ToString ("d19"),
                                               Field.Store.NO, Field.Index.NO_NORMS);
                                primary_doc.Add (f);

                                str = StringFu.DateTimeToYearMonthString (indexable.Timestamp);
                                f = new Field ("Timestamp(YM)", str, Field.Store.YES, Field.Index.NO_NORMS);
                                primary_doc.Add (f);
                                f = new Field (wildcard_field + "(YM)", str,
                                               Field.Store.NO, Field.Index.NO_NORMS);
                                primary_doc.Add (f);

                                str = StringFu.DateTimeToDayString (indexable.Timestamp);
                                f = new Field ("Timestamp(D)", str, Field.Store.YES, Field.Index.NO_NORMS);
                                primary_doc.Add (f);
                                f = new Field (wildcard_field + "(D)", str,
                                               Field.Store.NO, Field.Index.NO_NORMS);
                                primary_doc.Add (f);
                        }

                        if (indexable.NoContent) {
                                // If there is no content, make a note of that
                                // in a special property.
                                Property prop;
                                prop = Property.NewBool ("beagrep:NoContent", true);
                                AddPropertyToDocument (prop, primary_doc);

                        } else {

                                // Since we might have content, add our text
                                // readers.

                                TextReader reader;

                                // Add the field "Text" first
                                // It is important that the order is preserved
                                reader = indexable.GetTextReader ();
                                if (reader != null) {
                                        f = new Field ("Text", reader);
                                        primary_doc.Add (f);
                                }

                                // FIXME: HotText is ignored for now!
                                // Then add "HotText"
                                //reader = indexable.GetHotTextReader ();
                                //if (reader != null) {
                                //      f = new Field ("HotText", reader);
                                //      primary_doc.Add (f);
                                //}
                        }

                        // Store the Type and MimeType in special properties

                        if (indexable.HitType != null) {
                                Property prop;
                                prop = Property.NewUnsearched ("beagrep:HitType", indexable.HitType);
                                AddPropertyToDocument (prop, primary_doc);
                        }

                        if (indexable.MimeType != null) {
                                Property prop;
                                prop = Property.NewUnsearched ("beagrep:MimeType", indexable.MimeType);
                                AddPropertyToDocument (prop, primary_doc);
                        }

                        if (indexable.Source != null) {
                                Property prop;
                                prop = Property.NewUnsearched ("beagrep:Source", indexable.Source);
                                AddPropertyToDocument (prop, primary_doc);
                        }

                        {
                                Property prop;
                                prop = Property.NewBool (Property.IsChildPropKey, indexable.IsChild);
                                AddPropertyToDocument (prop, primary_doc);
                        }

                        // Store the other properties

                        foreach (Property prop in indexable.Properties) {
                                Document target_doc = primary_doc;
                                if (prop.IsMutable) {
                                        if (secondary_doc == null)
                                                secondary_doc = CreateSecondaryDocument (indexable.Uri, indexable.ParentUri);

                                        target_doc = secondary_doc;
                                }

                                AddPropertyToDocument (prop, target_doc);
                        }
                }


                static private Document CreateSecondaryDocument (Uri uri, Uri parent_uri)
                {
                        Document secondary_doc = new Document ();

                        Field f = new Field ("Uri", UriFu.UriToEscapedString (uri), Field.Store.YES, Field.Index.NO_NORMS);
                        secondary_doc.Add (f);

                        if (parent_uri != null) {
                                // Store both Uri and ParentUri in secondary index for easy removal
                                f = new Field ("ParentUri", UriFu.UriToEscapedString (parent_uri), Field.Store.YES, Field.Index.NO_NORMS);
                                secondary_doc.Add (f);
                        }

                        return secondary_doc;
                }

                static protected Document RewriteDocument (Document old_secondary_doc,
                                                           Indexable prop_only_indexable)
                {
                        Hashtable seen_props;
                        seen_props = new Hashtable ();

                        Document new_doc;
                        new_doc = new Document ();

                        Field uri_f;
                        uri_f = new Field ("Uri", UriFu.UriToEscapedString (prop_only_indexable.Uri), Field.Store.YES, Field.Index.NO_NORMS);
                        new_doc.Add (uri_f);

                        Logger.Log.Debug ("Rewriting {0}", prop_only_indexable.DisplayUri);

                        if (prop_only_indexable.ParentUri != null) {
                                uri_f = new Field ("ParentUri", UriFu.UriToEscapedString (prop_only_indexable.ParentUri), Field.Store.YES, Field.Index.NO_NORMS);
                                new_doc.Add (uri_f);
                                Logger.Log.Debug ("Parent Uri {0}", prop_only_indexable.ParentUri);
                        }

                        // Add the new properties to the new document.  To
                        // delete a property, set the Value to null... then it
                        // will be added to seen_props (so the old value will
                        // be ignored below), but AddPropertyToDocument will
                        // return w/o doing anything.
                        foreach (Property prop in prop_only_indexable.Properties) {
                                seen_props [prop.Key] = prop;

                                // Don't add properties that are empty; they
                                // essentially mean "reset this property"
                                if (prop.Value == String.Empty) {
                                        Log.Debug ("Resetting prop '{0}'", prop.Key);
                                        continue;
                                }

                                AddPropertyToDocument (prop, new_doc);
                                Logger.Log.Debug ("New prop '{0}' = '{1}'", prop.Key, prop.Value);
                        }

                        // Copy the other properties from the old document to the
                        // new one, skipping any properties that we got new values
                        // for out of the Indexable.
                        if (old_secondary_doc != null) {
                                foreach (Field f in old_secondary_doc.Fields ()) {
                                        Property prop;
                                        prop = GetPropertyFromDocument (f, old_secondary_doc, false);
                                        if (prop != null && ! seen_props.Contains (prop.Key)) {
                                                Logger.Log.Debug ("Old prop '{0}' = '{1}'", prop.Key, prop.Value);
                                                AddPropertyToDocument (prop, new_doc);
                                        }
                                }
                        }

                        return new_doc;
                }

                static protected Document MergeDocuments (Document doc,
                                                          Document prop_change_doc)
                {
                        if (doc == null)
                                return prop_change_doc;

                        if (prop_change_doc == null)
                                return doc;

                        foreach (Field f in prop_change_doc.Fields ()) {
                                Property prop = GetPropertyFromDocument (f, prop_change_doc, false);

                                if (prop != null && prop.IsPersistent) {
                                        Log.Debug ("Moving old persistent prop '{0}' = '{1}' forward", prop.Key, prop.Value);
                                        AddPropertyToDocument (prop, doc);
                                }
                        }

                        return doc;
                }

                // Add a new field with whitespace separated names of the existing fields
                static protected void AddFieldProperies (Document doc)
                {
                        const string Separator = " ";

                        StringBuilder sb = new StringBuilder ();
                        bool seen_properties = false;

                        foreach (Fieldable f in doc.Fields ()) {
                                if (f.Name () == "Properties") {
                                        seen_properties = true;
                                        continue;
                                }

                                sb.Append (f.Name ());
                                sb.Append (Separator);
                        }

                        if (sb.Length > 0)
                                sb.Length -= Separator.Length;

                        if (seen_properties)
                                doc.RemoveFields ("Properties");

                        Field field = new Field ("Properties", sb.ToString (), Field.Store.NO, Field.Index.TOKENIZED);
                        doc.Add (field);
                }

                static protected Uri GetUriFromDocument (Document doc)
                {
                        string uri;
                        uri = doc.Get ("Uri");
                        if (uri == null)
                                throw new Exception ("Got document from Lucene w/o a URI!");
                        return UriFu.EscapedStringToUri (uri);
                }

                static protected Hit DocumentToHit (Document doc)
                {
                        Hit hit;
                        hit = new Hit ();

                        hit.Uri = GetUriFromDocument (doc);

                        string str;
                        str = doc.Get ("ParentUri");
                        if (str != null)
                                hit.ParentUri = UriFu.EscapedStringToUri (str);

                        hit.Timestamp = StringFu.StringToDateTime (doc.Get ("Timestamp"));

                        AddPropertiesToHit (hit, doc, true);

                        return hit;
                }

                static protected void AddPropertiesToHit (Hit hit, Document doc, bool from_primary_index)
                {
                        Property prop;
                        foreach (Field f in doc.Fields ()) {
                                prop = GetPropertyFromDocument (f, doc, from_primary_index);
                                if (prop != null)
                                        hit.AddProperty (prop);
                        }
                }


                //////////////////////////////////////////////////////////////////////////////

                //
                // Handle the index's item count
                //

                public int GetItemCount ()
                {
                        if (last_item_count < 0) {
                                IndexReader reader;
                                reader = GetReader (PrimaryStore);
                                last_item_count = reader.NumDocs ();
                                ReleaseReader (reader);
                        }
                        return last_item_count;
                }

                // We should set the cached count of index items when IndexReaders
                // are open and available, so calls to GetItemCount will return immediately.

                protected bool HaveItemCount { get { return last_item_count >= 0; } }

                protected void SetItemCount (IndexReader reader)
                {
                        last_item_count = reader.NumDocs ();
                }

                public void SetItemCount (int count)
                {
                        last_item_count = count;
                }

                protected void AdjustItemCount (int delta)
                {
                        if (last_item_count >= 0)
                                last_item_count += delta;
                }

                //////////////////////////////////////////////////////////////////////////////

                //
                // Access to the stemmer and list of stop words
                //

                static public string Stem (string str)
                {
                        return str;
                }

                public static bool IsStopWord (string stemmed_word)
                {
                        return Array.IndexOf<string> (StopAnalyzer.ENGLISH_STOP_WORDS, stemmed_word) != -1;
                }

                //////////////////////////////////////////////////////////////////////////////

                //
                // Special Hit Filtering classes
                //

                static private bool TrueHitFilter (Hit hit)
                {
                        return true;
                }

                static private HitFilter true_hit_filter = new HitFilter (TrueHitFilter);

                public class OrHitFilter {

                        private ArrayList all = new ArrayList ();
                        private bool contains_known_true = false;

                        public void Add (HitFilter hit_filter)
                        {
                                if (hit_filter == true_hit_filter)
                                        contains_known_true = true;
                                all.Add (hit_filter);
                        }

                        public bool HitFilter (Hit hit)
                        {
                                if (contains_known_true)
                                        return true;
                                foreach (HitFilter hit_filter in all)
                                        if (hit_filter (hit))
                                                return true;
                                return false;
                        }
                }

                public class AndHitFilter {

                        private ArrayList all = new ArrayList ();

                        public void Add (HitFilter hit_filter)
                        {
                                all.Add (hit_filter);
                        }

                        public bool HitFilter (Hit hit)
                        {
                                foreach (HitFilter hit_filter in all)
                                        if (! hit_filter (hit))
                                                return false;
                                return true;
                        }
                }

                public class NotHitFilter {
                        HitFilter original;

                        public NotHitFilter (HitFilter original)
                        {
                                this.original = original;
                        }

                        public bool HitFilter (Hit hit)
                        {
                                return ! original (hit);
                        }
                }

                //////////////////////////////////////////////////////////////////////////////

                //
                // Queries
                //

                static internal LNS.Query StringToQuery (string field_name,
                                                        string text,
                                                        ArrayList term_list)
                {
                        ArrayList tokens = new ArrayList ();

                        // Use the analyzer to extract the query's tokens.
                        // This code is taken from Lucene's query parser.
                        TokenStream source = QueryAnalyzer.TokenStream (field_name, new StringReader (text));
                        while (true) {
                                Lucene.Net.Analysis.Token token;
                                try {
                                        token = source.Next ();
                                        if (token == null)
                                                break;
                                } catch (IOException) {
                                        break;
                                }
                                if (token != null)
                                        tokens.Add (token.TermText ());
                        }
                        try {
                                source.Close ();
                        } catch (IOException) {
                                // ignore
                        }

                        if (tokens.Count == 0)
                                return null;

                        LNS.PhraseQuery query = new LNS.PhraseQuery ();

                        foreach (string token in tokens) {
                                Term term;
                                term = new Term (field_name, token);
                                query.Add (term);
                                if (term_list != null)
                                        term_list.Add (term);
                        }

                        return query;
                }

                //
                // Date Range Handling
                //

                // This function will break down dates to discrete chunks of
                // time to avoid expanding RangeQuerys as much as possible.
                // For example, searching for
                //
                // YMD(5 May 2005, 16 Oct 2006)
                //
                // would break down into three queries:
                //
                // (YM(May 2005) AND D(5,31)) OR
                // YM(Jun 2005, Sep 2006) OR
                // (YM(Oct 2006) AND D(1,16))

                static private DateTime lower_bound = DateTimeUtil.UnixToDateTimeUtc (0);

                // FIXME: we should probably boost this sometime around 2030.
                // Mark your calendar.
                static private DateTime upper_bound = new DateTime (2038, 12, 31);

                static private Term NewYearMonthTerm (string field_name, int y, int m)
                {
                        return new Term (field_name + "(YM)", String.Format ("{0}{1:00}", y, m));
                }

                static private LNS.Query NewYearMonthQuery (string field_name, int y, int m)
                {
                        return new LNS.TermQuery (NewYearMonthTerm (field_name, y, m));
                }

                static private LNS.Query NewYearMonthQuery (string field_name, int y1, int m1, int y2, int m2)
                {
                        return new LNS.RangeQuery (NewYearMonthTerm (field_name, y1, m1),
                                                   NewYearMonthTerm (field_name, y2, m2),
                                                   true); // query is inclusive
                }

                static private Term NewDayTerm (string field_name, int d)
                {
                        return new Term (field_name + "(D)", String.Format ("{0:00}", d));
                }

                static private LNS.Query NewDayQuery (string field_name, int d1, int d2)
                {
                        if (d1 == d2)
                                return new LNS.TermQuery (NewDayTerm (field_name, d1));

                        return new LNS.RangeQuery (NewDayTerm (field_name, d1),
                                                   NewDayTerm (field_name, d2),
                                                   true); // query is inclusive
                }

                private class DateRangeHitFilter {
                        public string Key;
                        public DateTime StartDate;
                        public DateTime EndDate;

                        public bool HitFilter (Hit hit)
                        {
                                // First, check the Timestamp
                                if (Key == QueryPart_DateRange.AllPropertiesKey
                                    || Key == QueryPart_DateRange.TimestampKey) {
                                        DateTime dt;
                                        dt = hit.Timestamp;
                                        if (StartDate <= dt && dt <= EndDate)
                                                return true;
                                        if (Key == QueryPart_DateRange.TimestampKey)
                                                return false;
                                }

                                if (Key == QueryPart_DateRange.AllPropertiesKey) {
                                        // Walk through all of the properties, and see if any
                                        // date properties fall inside the range.
                                        foreach (Property prop in hit.Properties) {
                                                if (prop.Type == PropertyType.Date) {
                                                        DateTime dt;
                                                        dt = StringFu.StringToDateTime (prop.Value);
                                                        if (StartDate <= dt && dt <= EndDate)
                                                                return true;
                                                }
                                        }
                                        return false;
                                } else {
                                        // Walk through all of the properties with the given key,
                                        // and see if any of them fall inside of the range.
                                        string[] values;
                                        values = hit.GetProperties (Key);
                                        foreach (string v in values) {
                                                DateTime dt;
                                                dt = StringFu.StringToDateTime (v);
                                                if (StartDate <= dt && dt <= EndDate)
                                                        return true;
                                        }
                                        return false;
                                }
                        }
                }

                static private LNS.Query GetDateRangeQuery (QueryPart_DateRange part, out HitFilter hit_filter)
                {
                        string field_name;
                        if (part.Key == QueryPart_DateRange.AllPropertiesKey)
                                field_name = TypeToWildcardField (PropertyType.Date);
                        else if (part.Key == QueryPart_DateRange.TimestampKey)
                                field_name = "Timestamp";
                        else
                                field_name = PropertyToFieldName (PropertyType.Date, part.Key);

                        // FIXME: We could optimize this and reduce the size of our range
                        // queries if we actually new the min and max date that appear in
                        // any properties in the index.  We would need to inspect the index to
                        // determine that at start-up, and then track it as new documents
                        // get added to the index.
                        if (part.StartDate < lower_bound)
                                part.StartDate = lower_bound;
                        if (part.EndDate > upper_bound || part.EndDate == DateTime.MinValue)
                                part.EndDate = upper_bound;

                        // Swap the start and end dates if they come in reversed.
                        if (part.StartDate > part.EndDate) {
                                DateTime swap;
                                swap = part.StartDate;
                                part.StartDate = part.EndDate;
                                part.EndDate = swap;
                        }

                        // Set up our hit filter to cull out the bad dates.
                        DateRangeHitFilter drhf;
                        drhf = new DateRangeHitFilter ();
                        drhf.Key = part.Key;
                        drhf.StartDate = part.StartDate;
                        drhf.EndDate = part.EndDate;
                        hit_filter = new HitFilter (drhf.HitFilter);

                        Logger.Log.Debug ("Building new date range query");
                        Logger.Log.Debug ("Start: {0}", part.StartDate);
                        Logger.Log.Debug ("End: {0}", part.EndDate);

                        int y1, m1, d1, y2, m2, d2;
                        y1 = part.StartDate.Year;
                        m1 = part.StartDate.Month;
                        d1 = part.StartDate.Day;
                        y2 = part.EndDate.Year;
                        m2 = part.EndDate.Month;
                        d2 = part.EndDate.Day;

                        LNS.BooleanQuery top_level_query;
                        top_level_query = new LNS.BooleanQuery ();

                        // A special case: both the start and the end of our range fall
                        // in the same month.
                        if (y1 == y2 && m1 == m2) {
                                LNS.Query ym_query;
                                ym_query = NewYearMonthQuery (field_name, y1, m1);

                                // If our range only covers a part of the month, do a range query on the days.
                                if (d1 != 1 || d2 != DateTime.DaysInMonth (y2, m2)) {
                                        LNS.BooleanQuery sub_query;
                                        sub_query = new LNS.BooleanQuery ();
                                        sub_query.Add (ym_query, LNS.BooleanClause.Occur.MUST);
                                        sub_query.Add (NewDayQuery (field_name, d1, d2), LNS.BooleanClause.Occur.MUST);
                                        top_level_query.Add (sub_query, LNS.BooleanClause.Occur.SHOULD);
                                } else {
                                        top_level_query.Add (ym_query, LNS.BooleanClause.Occur.SHOULD);
                                }

                        } else {

                                // Handle a partial month at the beginning of our range.
                                if (d1 > 1) {
                                        LNS.BooleanQuery sub_query;
                                        sub_query = new LNS.BooleanQuery ();
                                        sub_query.Add (NewYearMonthQuery (field_name, y1, m1), LNS.BooleanClause.Occur.MUST);
                                        sub_query.Add (NewDayQuery (field_name, d1, DateTime.DaysInMonth (y1, m1)), LNS.BooleanClause.Occur.MUST);
                                        top_level_query.Add (sub_query, LNS.BooleanClause.Occur.SHOULD);

                                        ++m1;
                                        if (m1 == 13) {
                                                m1 = 1;
                                                ++y1;
                                        }
                                }

                                // And likewise, handle a partial month at the end of our range.
                                if (d2 < DateTime.DaysInMonth (y2, m2)) {
                                        LNS.BooleanQuery sub_query;
                                        sub_query = new LNS.BooleanQuery ();
                                        sub_query.Add (NewYearMonthQuery (field_name, y2, m2), LNS.BooleanClause.Occur.MUST);
                                        sub_query.Add (NewDayQuery (field_name, 1, d2), LNS.BooleanClause.Occur.MUST);
                                        top_level_query.Add (sub_query, LNS.BooleanClause.Occur.SHOULD);

                                        --m2;
                                        if (m2 == 0) {
                                                m2 = 12;
                                                --y2;
                                        }
                                }

                                // Generate the query for the "middle" of our period, if it is non-empty
                                if (y1 < y2 || ((y1 == y2) && m1 <= m2))
                                        top_level_query.Add (NewYearMonthQuery (field_name, y1, m1, y2, m2),
                                                             LNS.BooleanClause.Occur.SHOULD);
                        }

                        return top_level_query;
                }

                // search_subset_uris is a list of Uris that this search should be
                // limited to.
                static protected void QueryPartToQuery (QueryPart     abstract_part,
                                                        bool          only_build_primary_query,
                                                        ArrayList     term_list,
                                                        QueryPartHook query_part_hook,
                                                        out LNS.Query primary_query,
                                                        out LNS.Query secondary_query,
                                                        out HitFilter hit_filter)
                {
                        primary_query = null;
                        secondary_query = null;

                        // By default, we assume that our lucene queries will return exactly the
                        // matching set of objects.  We need to set the hit filter if further
                        // refinement of the search results is required.  (As in the case of
                        // date range queries, for example.)  We essentially have to do this
                        // to make OR queries work correctly.
                        hit_filter = true_hit_filter;

                        // The exception is when dealing with a prohibited part.  Just return
                        // null for the hit filter in that case.  This works since
                        // prohibited parts are not allowed inside of OR queries.
                        if (abstract_part.Logic == QueryPartLogic.Prohibited)
                                hit_filter = null;

                        if (abstract_part == null)
                                return;

                        // Run the backend hook first.
                        // This gives a chance to modify create new queries based on
                        // backend specific properties

                        if (query_part_hook != null)
                                abstract_part = query_part_hook (abstract_part);

                        if (abstract_part == null)
                                return;

                        if (abstract_part is QueryPart_Text) {
                                QueryPart_Text part = (QueryPart_Text) abstract_part;

                                if (! (part.SearchFullText || part.SearchTextProperties))
                                        return;

                                LNS.BooleanQuery p_query = new LNS.BooleanQuery ();
                                LNS.BooleanQuery s_query = new LNS.BooleanQuery ();

                                bool added_subquery = false;

                                if (part.SearchFullText) {
                                        LNS.Query subquery;
                                        subquery = StringToQuery ("Text", part.Text, term_list);
                                        if (subquery != null) {
                                                p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                                added_subquery = true;
                                        }

                                        // FIXME: HotText is ignored for now!
                                        // subquery = StringToQuery ("HotText", part.Text);
                                        // if (subquery != null) {
                                        //    p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                        //    added_subquery = true;
                                        // }
                                }

                                if (part.SearchTextProperties) {
                                        LNS.Query subquery;
                                        subquery = StringToQuery ("PropertyText", part.Text, term_list);
                                        if (subquery != null) {
                                                p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                                // Properties can live in either index
                                                if (! only_build_primary_query)
                                                        s_query.Add (subquery.Clone () as LNS.Query, LNS.BooleanClause.Occur.SHOULD);
                                                added_subquery = true;
                                        }

                                        // The "added_subquery" check is to handle the situation where
                                        // a part of the text is a stop word.  Normally, a search for
                                        // "hello world" would break down into this query:
                                        //
                                        // (Text:hello OR PropertyText:hello OR PropertyKeyword:hello)
                                        // AND (Text:world OR PropertText:world OR PropertyKeyword:world)
                                        //
                                        // This fails with stop words, though.  Let's assume that "world"
                                        // is a stop word.  You would end up with:
                                        //
                                        // (Text:hello OR PropertyText:hello OR PropertyKeyword:hello)
                                        // AND (PropertyKeyword:world)
                                        //
                                        // Which is not what we want.  We'd want to match documents that
                                        // had only "hello" without also having a keyword "world".  In
                                        // this case, don't create the PropertyKeyword part of the query,
                                        // since it would be included in the larger set if it weren't
                                        // required anyway.
                                        if (added_subquery) {
                                                Term term;
                                                term = new Term ("PropertyKeyword", part.Text.ToLower ()); // make sure text is lowercased
                                                // FIXME: terms are already added in term_list. But they may have been tokenized
                                                // The term here is non-tokenized version. Should this be added to term_list ?
                                                // term_list is used to calculate scores
                                                if (term_list != null)
                                                        term_list.Add (term);
                                                subquery = new LNS.TermQuery (term);
                                                p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                                // Properties can live in either index
                                                if (! only_build_primary_query)
                                                        s_query.Add (subquery.Clone () as LNS.Query, LNS.BooleanClause.Occur.SHOULD);
                                        } else {
                                                // Reset these so we return a null query
                                                p_query = null;
                                                s_query = null;
                                        }
                                }

                                primary_query = p_query;
                                if (! only_build_primary_query)
                                        secondary_query = s_query;

                                return;
                        }

                        if (abstract_part is QueryPart_Wildcard) {
                                QueryPart_Wildcard part = (QueryPart_Wildcard) abstract_part;

                                LNS.BooleanQuery p_query = new LNS.BooleanQuery ();
                                LNS.BooleanQuery s_query = new LNS.BooleanQuery ();

                                Term term;
                                LNS.Query subquery;

                                // Lower case the terms for searching
                                string query_string_lower = part.QueryString.ToLower ();

                                // Search text content
                                if (! part.PropertyOnly) {
                                    term = new Term ("Text", query_string_lower);
                                    subquery = new LNS.WildcardQuery (term);
                                    p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                    term_list.Add (term);
                                }

                                // Search text properties
                                term = new Term ("PropertyText", query_string_lower);
                                subquery = new LNS.WildcardQuery (term);
                                p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                // Properties can live in either index
                                if (! only_build_primary_query)
                                        s_query.Add (subquery.Clone () as LNS.Query, LNS.BooleanClause.Occur.SHOULD);
                                term_list.Add (term);

                                if (! part.PropertyOnly) {
                                    // Search property keywords
                                    term = new Term ("PropertyKeyword", query_string_lower);
                                    term_list.Add (term);
                                    subquery = new LNS.WildcardQuery (term);
                                    p_query.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                                    // Properties can live in either index
                                    if (! only_build_primary_query)
                                        s_query.Add (subquery.Clone () as LNS.Query, LNS.BooleanClause.Occur.SHOULD);
                                }

                                primary_query = p_query;
                                if (! only_build_primary_query)
                                        secondary_query = s_query;

                                return;
                        }

                        if (abstract_part is QueryPart_DateRange) {

                                QueryPart_DateRange part = (QueryPart_DateRange) abstract_part;

                                // FIXME: We don't handle prohibited queries with sub-date
                                // accuracy.  For example, if we say we prohibit matches
                                // between 5 May 2007 at 2 PM and 8 May at 5 AM, we'll
                                // miss any matches that happen between midnight and 2 PM
                                // on 5 May 2007 and between midnight and 5 AM on 8 May.

                                primary_query = GetDateRangeQuery (part, out hit_filter);
                                // Date properties can live in either index
                                if (! only_build_primary_query && primary_query != null)
                                        secondary_query = primary_query.Clone () as LNS.Query;

                                return;
                        }

                        if (abstract_part is QueryPart_Or) {
                                QueryPart_Or part = (QueryPart_Or) abstract_part;

                                // Assemble a new BooleanQuery combining all of the sub-parts.
                                LNS.BooleanQuery p_query;
                                p_query = new LNS.BooleanQuery ();

                                LNS.BooleanQuery s_query = null;
                                if (! only_build_primary_query)
                                        s_query = new LNS.BooleanQuery ();

                                primary_query = p_query;
                                secondary_query = s_query;

                                OrHitFilter or_hit_filter = null;

                                foreach (QueryPart  sub_part in part.SubParts) {
                                        LNS.Query p_subq, s_subq;
                                        HitFilter sub_hit_filter; // FIXME: This is (and must be) ignored
                                        // FIXME: Any subpart in an OR which has a hit filter won't work
                                        // correctly, because we can't tell which part of an OR we matched
                                        // against to filter correctly.  This affects date range queries.
                                        QueryPartToQuery (sub_part, only_build_primary_query,
                                                          term_list, query_part_hook,
                                                          out p_subq, out s_subq, out sub_hit_filter);
                                        if (p_subq != null)
                                                p_query.Add (p_subq, LNS.BooleanClause.Occur.SHOULD);
                                        if (s_subq != null)
                                                s_query.Add (s_subq, LNS.BooleanClause.Occur.SHOULD);
                                        if (sub_hit_filter != null) {
                                                if (or_hit_filter == null)
                                                        or_hit_filter = new OrHitFilter ();
                                                or_hit_filter.Add (sub_hit_filter);
                                        }
                                }

                                if (or_hit_filter != null)
                                        hit_filter = new HitFilter (or_hit_filter.HitFilter);

                                return;
                        }

                        if (abstract_part is QueryPart_Uri) {
                                QueryPart_Uri part = (QueryPart_Uri) abstract_part;

                                // Do a term query on the Uri field.
                                // This is probably less efficient that using a TermEnum;
                                // but this is required for the query API where the uri query
                                // can be part of a prohibited query or a boolean or query.
                                Term term;
                                term = new Term ("Uri", UriFu.UriToEscapedString (part.Uri));
                                if (term_list != null)
                                        term_list.Add (term);
                                primary_query = new LNS.TermQuery (term);

                                // Query only the primary index
                                return;
                        }

                        if (abstract_part is QueryPart_Property) {
                                QueryPart_Property part = (QueryPart_Property) abstract_part;

                                string field_name;
                                if (part.Key == QueryPart_Property.AllProperties)
                                        field_name = TypeToWildcardField (part.Type);
                                else
                                        field_name = PropertyToFieldName (part.Type, part.Key);

                                // Details of the conversion here depends on BeagrepAnalyzer::TokenStream
                                if (part.Type == PropertyType.Text)
                                        primary_query = StringToQuery (field_name, part.Value, term_list);
                                else {
                                        Term term;
                                        // FIXME: Handle date queries for other date fields
                                        if (part.Type == PropertyType.Internal || field_name.StartsWith ("prop:k:" + Property.PrivateNamespace))
                                                term = new Term (field_name, part.Value);
                                        else
                                                term = new Term (field_name, part.Value.ToLower ());
                                        if (term_list != null)
                                                term_list.Add (term);
                                        primary_query = new LNS.TermQuery (term);
                                }

                                // Properties can live in either index
                                if (! only_build_primary_query && primary_query != null)
                                        secondary_query = primary_query.Clone () as LNS.Query;

                                return;
                        }

                        throw new Exception ("Unhandled QueryPart type! " + abstract_part.ToString ());
                }

                static protected LNS.Query UriQuery (string field_name, Uri uri)
                {
                        return new LNS.TermQuery (new Term (field_name, UriFu.UriToEscapedString (uri)));
                }

                static protected LNS.Query UriQuery (string field_name, ICollection uri_list)
                {
                        return UriQuery (field_name, uri_list, null);
                }

                static protected LNS.Query UriQuery (string field_name, ICollection uri_list, LNS.Query extra_requirement)
                {
                        if (uri_list.Count == 0)
                                return null;

                        int max_clauses;
                        max_clauses = LNS.BooleanQuery.GetMaxClauseCount ();

                        int N;
                        N = 1 + (uri_list.Count - 1) / max_clauses;

                        LNS.BooleanQuery top_query;
                        top_query = new LNS.BooleanQuery ();

                        int cursor = 0;
                        if (extra_requirement != null) {
                                top_query.Add (extra_requirement, LNS.BooleanClause.Occur.MUST);
                                ++cursor;
                        }

                        ArrayList bottom_queries = null;

                        if (N > 1) {
                                bottom_queries = new ArrayList ();
                                for (int i = 0; i < N; ++i) {
                                        LNS.BooleanQuery bq;
                                        bq = new LNS.BooleanQuery ();
                                        bottom_queries.Add (bq);
                                        top_query.Add (bq, LNS.BooleanClause.Occur.SHOULD);
                                }
                        }

                        foreach (Uri uri in uri_list) {
                                LNS.Query subquery;
                                subquery = UriQuery (field_name, uri);

                                LNS.BooleanQuery target;
                                if (N == 1)
                                        target = top_query;
                                else {
                                        target = (LNS.BooleanQuery) bottom_queries [cursor];
                                        ++cursor;
                                        if (cursor >= N)
                                                cursor = 0;
                                }

                                target.Add (subquery, LNS.BooleanClause.Occur.SHOULD);
                        }

                        return top_query;
                }

                ///////////////////////////////////////////////////////////////////////////////////

                public int SegmentCount {
                        get {
                                DirectoryInfo dir_info;
                                int p_count = 0, s_count = 0;

                                dir_info = new DirectoryInfo (PrimaryIndexDirectory);
                                foreach (FileInfo file_info in dir_info.GetFiles ())
                                        if (file_info.Extension == ".cfs")
                                                ++p_count;

                                dir_info = new DirectoryInfo (SecondaryIndexDirectory);
                                foreach (FileInfo file_info in dir_info.GetFiles ())
                                        if (file_info.Extension == ".cfs")
                                                ++s_count;

                                return p_count > s_count ? p_count : s_count;
                        }
                }

                ///////////////////////////////////////////////////////////////////////////////////

                // Cache IndexReaders on a per-Lucene index basis, since they
                // are extremely expensive to create.  Note that using this
                // only makes sense in situations where the index only
                // possibly might change from underneath us, but most of the
                // time probably won't.  This means it makes sense to do
                // this in LuceneQueryingDriver.cs, but it doesn't in
                // LuceneIndexingDriver.cs.

                private class ReaderAndVersion {

                        public IndexReader Reader;
                        public long Version;
                        public int Refcount;

                        public ReaderAndVersion (IndexReader reader, long version)
                        {
                                this.Reader = reader;
                                this.Version = version;
                                this.Refcount = 1;
                        }
                }

                static private Hashtable directory_rav_map = new Hashtable ();
                static private Hashtable reader_rav_map = new Hashtable ();

                static public LNS.IndexSearcher GetSearcher (Lucene.Net.Store.Directory directory)
                {
                        IndexReader reader = GetReader (directory);

                        return new LNS.IndexSearcher (reader);
                }

                static public IndexReader GetReader (Lucene.Net.Store.Directory directory)
                {
                        IndexReader reader;
                        long version;

                        lock (reader_rav_map) {
                                ReaderAndVersion rav = (ReaderAndVersion) directory_rav_map [directory];

                                if (rav == null) {
                                        version = IndexReader.GetCurrentVersion (directory);
                                        reader = IndexReader.Open (directory);

                                        rav = new ReaderAndVersion (reader, version);
                                        rav.Refcount ++; // add one refcount to keep this reader open until...

                                        directory_rav_map [directory] = rav;
                                        reader_rav_map [reader] = rav;

                                        return reader;
                                }

                                version = IndexReader.GetCurrentVersion (directory);

                                if (version != rav.Version) {
                                        // unref once and then close reader or call UnrefReaderAndVersion_Unlocked as a single step
                                        UnrefReaderAndVersion_Unlocked (rav);

                                        reader = IndexReader.Open (directory);

                                        rav = new ReaderAndVersion (reader, version);

                                        directory_rav_map [directory] = rav;
                                        reader_rav_map [reader] = rav;
                                } else {
                                        rav.Refcount++;
                                }

                                return rav.Reader;
                        }
                }

                static private void UnrefReaderAndVersion_Unlocked (ReaderAndVersion rav)
                {
                        rav.Refcount--;

                        if (rav.Refcount == 0) {
                                reader_rav_map.Remove (rav.Reader);
                                directory_rav_map.Remove (rav.Reader.Directory ());
                                rav.Reader.Close ();
                        }
                }

                static public void ReleaseReader (IndexReader reader)
                {
                        lock (reader_rav_map) {
                                ReaderAndVersion rav = (ReaderAndVersion) reader_rav_map [reader];

                                if (rav != null)
                                        UnrefReaderAndVersion_Unlocked (rav);
                                else
                                        reader.Close ();
                        }
                }

                // Special function to permanently remove this reader
                // Called when StaticQueryables need to unload
                static internal void CloseReader (IndexReader reader)
                {
                        lock (reader_rav_map) {
                                ReaderAndVersion rav = (ReaderAndVersion) reader_rav_map [reader];

                                if (rav != null) {
                                        rav.Refcount --; // Remove one extra refcount that was added to keep the reader opened
                                        UnrefReaderAndVersion_Unlocked (rav);
                                } else
                                        reader.Close ();
                        }
                }

                static public void ReleaseSearcher (LNS.IndexSearcher searcher)
                {
                        IndexReader reader = searcher.GetIndexReader ();

                        searcher.Close ();
                        ReleaseReader (reader);
                }


                // For a large index, this will be very slow and will consume
                // a lot of memory.  Don't call it without a good reason!
                // We return a hashtable indexed by Uri.
                public Hashtable GetAllHitsByUri ()
                {
                        Hashtable all_hits;
                        all_hits = UriFu.NewHashtable ();

                        IndexReader primary_reader;
                        IndexReader secondary_reader;
                        primary_reader = GetReader (PrimaryStore);
                        secondary_reader = GetReader (SecondaryStore);

                        // Load everything from the primary index
                        int max_doc;
                        max_doc = primary_reader.MaxDoc ();
                        for (int i = 0; i < max_doc; ++i) {

                                if (primary_reader.IsDeleted (i))
                                        continue;

                                Document doc;
                                doc = primary_reader.Document (i);

                                Hit hit;
                                hit = DocumentToHit (doc);
                                all_hits [hit.Uri] = hit;
                        }

                        // Now add in everything from the secondary index, if it exists
                        if (secondary_reader != null) {
                                max_doc = secondary_reader.MaxDoc ();
                                for (int i = 0; i < max_doc; ++i) {

                                        if (secondary_reader.IsDeleted (i))
                                                continue;

                                        Document doc;
                                        doc = secondary_reader.Document (i);

                                        Uri uri;
                                        uri = GetUriFromDocument (doc);

                                        Hit hit;
                                        hit = (Hit) all_hits [uri];
                                        if (hit != null)
                                                AddPropertiesToHit (hit, doc, false);
                                }
                        }

                        ReleaseReader (primary_reader);
                        ReleaseReader (secondary_reader);

                        return all_hits;
                }

                // For a large index, this will be very slow and will consume
                // a lot of memory.  Don't call it without a good reason!
                public ICollection GetHitsForUris (ICollection uris)
                {
                        return GetHitsForUris (uris, null);
                }

                public ICollection GetHitsForUris (ICollection uris, FieldSelector fields)
                {
                        Hashtable hits_by_uri = UriFu.NewHashtable ();

                        LNS.IndexSearcher primary_searcher = GetSearcher (PrimaryStore);
                        LNS.IndexSearcher secondary_searcher = GetSearcher (SecondaryStore);

                        LNS.Query uri_query = UriQuery ("Uri", uris);

                        LNS.Hits primary_hits = primary_searcher.Search (uri_query);

                        for (int i = 0; i < primary_hits.Length (); i++) {
                                Document doc = ((fields == null) ?
                                        primary_hits.Doc (i) :
                                        primary_hits.Doc (i, fields));

                                Uri u = GetUriFromDocument (doc);

                                Hit hit = DocumentToHit (doc);
                                hits_by_uri [hit.Uri] = hit;
                        }

                        if (secondary_searcher != null) {
                                LNS.Hits secondary_hits = secondary_searcher.Search (uri_query);

                                for (int i = 0; i < secondary_hits.Length (); i++) {
                                        Document doc = ((fields == null) ?
                                                secondary_hits.Doc (i) :
                                                secondary_hits.Doc (i, fields));

                                        Uri uri = GetUriFromDocument (doc);
                                        Hit hit = (Hit) hits_by_uri [uri];
                                        if (hit != null)
                                                AddPropertiesToHit (hit, doc, false);
                                }
                        }

                        ReleaseSearcher (primary_searcher);
                        ReleaseSearcher (secondary_searcher);

                        return hits_by_uri.Values;
                }

                //////////////////////////////////////////////////

                public static void DebugHook ()
                {
                        lock (reader_rav_map) {
                                Lucene.Net.Store.Directory dir;
                                ReaderAndVersion rav;
                                IndexReader reader;

                                Log.Debug ("Cached readers per directory:");
                                foreach (DictionaryEntry entry in directory_rav_map) {
                                        dir = entry.Key as Lucene.Net.Store.Directory;
                                        rav = entry.Value as ReaderAndVersion;
                                        Log.Debug ("\tDirectory {0} has {1} readers open", dir.GetLockID (), rav.Refcount);
                                }

                                Log.Debug ("Opened readers:");
                                foreach (DictionaryEntry entry in reader_rav_map) {
                                        reader = entry.Key as IndexReader;
                                        rav = entry.Value as ReaderAndVersion;
                                        dir = reader.Directory ();
                                        Log.Debug ("\t{2} readers v{0} opened for directory {1}", rav.Version, dir.GetLockID (), rav.Refcount);
                                }
                        }
                }
        }
}
