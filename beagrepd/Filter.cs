//
// Filter.cs
//
// Copyright (C) 2004 Novell, Inc.
// Copyright (C) 2006-2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Text;
using System.Reflection;

using Beagrep.Util;

namespace Beagrep.Daemon {

	public class Filter {

		static private bool Debug = Beagrep.Util.Debug.Enabled ("Filter");

		// Number of characters Filters should return in each DoPull()
		// Optimization hint. -1 means unlimited.
		const int DEFAULT_CHARS_TO_PULL = 2048;
		int chars_to_pull = -1;
		int chars_added = 0;

		// Derived classes always must have a constructor that
		// takes no arguments.
		public Filter () { }

		//////////////////////////

		private ArrayList supported_flavors = null;
		
		protected void AddSupportedFlavor (FilterFlavor flavor) 
		{
			// Add flavor only when called from RegisterSupportedTypes
			if (supported_flavors == null)
				throw new Exception ("AddSupportedFlavor() should be only called from RegisterSupportedTypes()");

			supported_flavors.Add (flavor);
		}

		public ICollection SupportedFlavors {
			get {
				if (supported_flavors == null) {
					supported_flavors = new ArrayList ();
					RegisterSupportedTypes ();
				}
				return supported_flavors;
			}
		}

		protected virtual void RegisterSupportedTypes ()
		{
		}
		
		//////////////////////////

		// Filters are versioned.  This allows us to automatically re-index
		// files when a newer filter is available.

		public string Name {
			get { return this.GetType ().Name; }
		}

		private int version = -1;

		public int Version {
			get { return version < 0 ? 0 : version; }
		}

		protected void SetVersion (int v)
		{
			if (v < 0) {
				string msg;
				msg = String.Format ("Attempt to set invalid version {0} on Filter {1}", v, Name);
				throw new Exception (msg);
			}

			if (v <= version) {
				string msg;
				msg = String.Format ("Attempt to re-set version from {0} to {1} on Filter {2}", version, v, Name);
				throw new Exception (msg);
			}

			version = v;
		}

		

		//////////////////////////

		private string this_mime_type = null;
		private string this_extension = null;
		private string this_file_type = null;
		private Indexable indexable = null;

		public string MimeType {
			get { return this_mime_type; }
			set { this_mime_type = value; }
		}

		public string Extension {
			get { return this_extension; }
			set { this_extension = value; }
		}

		/// <summary>
		///  Filter may set the filetype to document, source, music etc.
		/// Use lower case for file_type
		/// </summary>
		/// <param name="file_type">
		/// A <see cref="System.String"/>
		/// </param>
		protected void SetFileType (string file_type)
		{
			this_file_type = file_type;
		}

		public string FileType {
			get { return this_file_type; }
		}

		public Indexable Indexable {
			get { return indexable; }
			set { indexable = value; }
		}
		
		

		private bool preload = true;
		/// <value>
		///  Filters which deal with big files, and that don't need
		/// to read in whole files may want to set this to false
		/// to avoid wasting cycles in disk wait.
		/// </value>
		protected bool PreLoad {
			get { return preload; }
			set { preload = value; }
		}

		//////////////////////////

		int hotCount = 0;
		int freezeCount = 0;

		public void HotUp ()
		{
			++hotCount;
		}
		
		public void HotDown ()
		{
			if (hotCount > 0)
				--hotCount;
		}

		public bool IsHot {
			get { return hotCount > 0; }
		}

		public void FreezeUp ()
		{
			++freezeCount;
		}

		public void FreezeDown ()
		{
			if (freezeCount > 0)
				--freezeCount;
		}

		public bool IsFrozen {
			get { return freezeCount > 0; }
		}

		//////////////////////////
		
		private bool snippetMode = false;
		private bool originalIsText = false;
		private TextWriter snippetWriter = null;

		public bool SnippetMode {
			get { return snippetMode; }
			set { snippetMode = value; }
		}

		public bool OriginalIsText {
			get { return originalIsText; }
			set { originalIsText = value; }
		}
		
		public void AttachSnippetWriter (TextWriter writer)
		{
			if (snippetMode)
				snippetWriter = writer;
		}

		//////////////////////////

		// Various methods for filters to report extracted text
		// All the methods return true/false saying whether more text is requested
		// Filters can choose to ignore the return value

		// Use a shared text builder
		private StringBuilder text_builder = new StringBuilder (DEFAULT_CHARS_TO_PULL);
		private ArrayList textPool;
		private ArrayList hotPool;

		private bool last_was_structural_break = false;
		const string WHITESPACE = " ";
		const string NEWLINE = "\n";

		
		/// <summary>
		///  Append text to the textpool. If IsHot is true, then also add to the hottext pool.
		///  Handles null str.
		/// </summary>
		public bool AppendText (string str)
		{
			if (Debug)
				Logger.Log.Debug ("AppendText (\"{0}\")", str);

			return AppendText (str, IsHot ? str : null);
		}

		/*
		 * This two-arg AppendText() will give flexibility to
		 * filters to segregate hot-contents and
		 * normal-contents of a para and call this method with
		 * respective contents.  
		 * 
		 * str : Holds both the normal-contents and hot contents.
		 * strHot: Holds only hot-contents.
		 * Both arguments can be null.
		 * 
		 * Ex:- suppose the actual-content is "one <b>two</b> three"
		 * str = "one two three"
		 * strHot = "two"
		 * 
		 * NOTE: HotUp() or HotDown() has NO-EFFECT on this variant 
		 * of AppendText ()
		 */

		public bool AppendText (string str, string strHot)
		{
			if (Debug)
				Logger.Log.Debug ("AppendText (\"{0}, {1}\")", str, strHot);

			if (IsFrozen)
				return true;

			if (! string.IsNullOrEmpty (str)) {
				last_was_structural_break = false;

				// If the filter is not storing snippets, there
				// is no need to break strings into different lines.
				ReallyAddText (str);
				if (snippetWriter != null)
					// Previously, str was stripped into newlines and
					// each line was added individually. Effectively
					// that was removing blank lines from snippets.
					// Now blank lines are checked while reading from
					// the cache. No processing done here. TextCache files
					// are stored compressed so there would not be significant
					// storage overhead in doing this.
					snippetWriter.Write (str);

				return UpdateCharsAdded (str.Length);
			} else {
				return true;
			}

			/* FIXME: Disable HotText for now. Enable it when we start using it for query.
			if (! string.IsNullOrEmpty (strHot)) {
				hotPool.Add (strHot);
				hotPool.Add (WHITESPACE);
			}
			*/
		}

		/// <summary>
		/// Add a word followed by a whitespace. word may not be whitespace or newline.
		/// </summary>
		/// <param name="word">
		/// A <see cref="System.String"/>
		/// </param>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool AppendWord (string word)
		{
			if (Debug)
				Logger.Log.Debug ("AppendWord (\"{0}\")", word);

			return AppendWords (word, false);
		}

		/// <summary>
		/// Add a line followed by a newline.
		/// </summary>
		/// <param name="line">
		/// A <see cref="System.String"/>
		/// </param>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool AppendLine (string line)
		{
			if (Debug)
				Logger.Log.Debug ("AppendLine (\"{0}\")", line);

			return AppendWords (line, true);
		}

		private bool AppendWords (string words, bool is_line)
		{
			if (IsFrozen || string.IsNullOrEmpty (words))
				return true;

			/* FIXME: Disable hottext for now. Enable it when LuceneCommon starts using it for query.
			if (IsHot) {
				hotPool.Add (words);
				hotPool.Add (WHITESPACE);
			}
			*/

			ReallyAddText (words);
			if (is_line) {
				text_builder.Append (NEWLINE);
				last_was_structural_break = true;
			} else
				text_builder.Append (WHITESPACE);

			if (snippetWriter != null) {
				snippetWriter.Write (words);

				if (is_line) {
					snippetWriter.WriteLine ();
				} else {
					snippetWriter.Write (WHITESPACE);
				}
			}

			return UpdateCharsAdded (words.Length + 1);
		}

		/// <summary>
		/// Does not check for structural breaks
		/// </summary>
		/// <param name="buffer">
		/// A <see cref="System.Char"/>
		/// </param>
		/// <param name="index">
		/// A <see cref="System.Int32"/>
		/// </param>
		/// <param name="count">
		/// A <see cref="System.Int32"/>
		/// </param>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool AppendChars (char[] buffer, int index, int count)
		{
			if (Debug)
				Logger.Log.Debug ("AppendChars ()");

			ReallyAddText (buffer, index, count);

			if (snippetWriter != null)
				snippetWriter.Write (buffer, index, count);

			return UpdateCharsAdded (count);
		}

		
		 
		/// <summary>
		/// Adds whitespace to the textpool.
		/// </summary>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool AppendWhiteSpace ()
		{
			if (Debug)
				Logger.Log.Debug ("AppendWhiteSpace ()");

			if (last_was_structural_break)
				return true;

			last_was_structural_break = false;

			text_builder.Append (WHITESPACE);

			if (snippetWriter != null)
				snippetWriter.Write (WHITESPACE);

			return UpdateCharsAdded (1);
		}

		
		
		/// <summary>
		/// Creates a new paragraph. Mainly useful for storing cached contents.
		/// </summary>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool AppendStructuralBreak ()
		{
			if (Debug)
				Logger.Log.Debug ("AppendStructuralBreak ()");

			if (last_was_structural_break)
				return true;

			last_was_structural_break = true;

			text_builder.Append (NEWLINE);

			if (snippetWriter != null)
				snippetWriter.WriteLine ();

			return UpdateCharsAdded (1);
		}


		public void AddLink (string link)
		{
		}

		//private bool NeedsWhiteSpace (ArrayList array)
		//{
		//	if (array.Count == 0)
		//		return true;
		//	
		//	string last = (string) array [array.Count-1];
		//	if (last.Length > 0
		//	    && char.IsWhiteSpace (last [last.Length-1]))
		//		return false;

		//	return true;
		//}

		private void ReallyAddText (string str)
		{
			text_builder.Append (str);

			// Store text in blocks of whatever Pull() specified or DEFAULT_CHARS_TO_PULL o/w
			if (text_builder.Length >= (chars_to_pull > 0 ? chars_to_pull : DEFAULT_CHARS_TO_PULL)) {
				// FIXME: If text-cache needs to be cleaned up (i.e. whitespaces merged and blank
				// lines removed, then process text_builder here before adding it to textPool
				// Also, add to snippet-writer from here
				textPool.Add (text_builder.ToString ());
				text_builder.Length = 0;
			}
		}	

		private void ReallyAddText (char[] buffer, int index, int count)
		{
			text_builder.Append (buffer, index, count);

			// Store text in blocks of whatever Pull() specified or DEFAULT_CHARS_TO_PULL o/w
			if (text_builder.Length >= (chars_to_pull > 0 ? chars_to_pull : DEFAULT_CHARS_TO_PULL)) {
				// FIXME: If text-cache needs to be cleaned up (i.e. whitespaces merged and blank
				// lines removed, then process text_builder here before adding it to textPool
				// Also, add to snippet-writer from here
				textPool.Add (text_builder.ToString ());
				text_builder.Length = 0;
			}
		}	

		private bool UpdateCharsAdded (int append_chars_added)
		{
			chars_added += append_chars_added;

			if (chars_to_pull >= 0 && chars_added >= chars_to_pull) {
				return false;
			}

			return true;
		}
		
		//////////////////////////

		public void AddProperty (Property prop)
		{
			this.Indexable.AddProperty (prop);
		}

		//////////////////////////

		private bool isFinished = false;

		public bool IsFinished {
			get { return isFinished; }
		}
		
		protected void Finished ()
		{
			textPool.Add (text_builder.ToString ());
			text_builder.Length = 0;

			Close (); // Close streams and clear tmpfiles when done
			isFinished = true;
		}

		private bool has_error = false;

		public bool HasError {
			get { return has_error; }
		}

		protected void Error ()
		{
			// Add whatever was reported till error
			textPool.Add (text_builder.ToString ());
			text_builder.Length = 0;

			Close (); // Close streams and clear tmpfiles on error
			has_error = true;
		}

		//////////////////////////

		protected virtual void DoOpen (FileSystemInfo info) {
			if (info is FileInfo)
				DoOpen (info as FileInfo);
			else if (info is DirectoryInfo)
				DoOpen (info as DirectoryInfo);
		}

		protected virtual void DoOpen (FileInfo info) { }

		protected virtual void DoOpen (DirectoryInfo info) { }

		protected virtual void DoPullProperties () { }

		protected virtual void DoPullSetup () { }

		protected virtual void DoPull () { Finished (); }

		protected virtual void DoClose () { }

		//////////////////////////

		/*
		  Open () calls:
		  (1) DoOpen (FileInfo info) or DoOpen (Stream)
		  (2) DoPullProperties ()
		  (3) DoPullSetup ()
		  At this point all properties must be in place

		  Once someone starts reading from the TextReader,
		  the following are called:
		  DoPull () [until Finished() is called]
		  DoClose () [when finished]
		  
		*/

		private string tempFile = null;
		private FileSystemInfo currentInfo = null;
		private Stream currentStream = null;
		private StreamReader currentReader = null;

		public bool Open (TextReader reader)
		{
			tempFile = Path.GetTempFileName ();
                        FileStream file_stream = File.OpenWrite (tempFile);

			if (Debug)
				Logger.Log.Debug ("Storing text in tempFile {0}", tempFile);

                        // When we dump the contents of a reader into a file, we
                        // expect to use it again soon.
			FileAdvise.PreLoad (file_stream);

                        // Make sure the temporary file is only readable by the owner.
                        // FIXME: There is probably a race here.  Could some malicious program
                        // do something to the file between creation and the chmod?
                        Mono.Unix.Native.Syscall.chmod (tempFile, (Mono.Unix.Native.FilePermissions) 256);

                        BufferedStream buffered_stream = new BufferedStream (file_stream);
                        StreamWriter writer = new StreamWriter (buffered_stream);

                        const int BUFFER_SIZE = 8192;
                        char [] buffer = new char [BUFFER_SIZE];

                        int read;
                        do {
                                read = reader.Read (buffer, 0, BUFFER_SIZE);
                                if (read > 0)
                                        writer.Write (buffer, 0, read);
                        } while (read > 0);

                        writer.Close ();

			return Open (new FileInfo (tempFile));
		}
		
		public bool Open (Stream stream)
		{
			tempFile = Path.GetTempFileName ();
                        FileStream file_stream = File.OpenWrite (tempFile);
			
			if (Debug)
				Logger.Log.Debug ("Storing stream in tempFile {0}", tempFile);

                        // When we dump the contents of a reader into a file, we
                        // expect to use it again soon.
                        FileAdvise.PreLoad (file_stream);

                        // Make sure the temporary file is only readable by the owner.
                        // FIXME: There is probably a race here.  Could some malicious program
                        // do something to the file between creation and the chmod?
                        Mono.Unix.Native.Syscall.chmod (tempFile, (Mono.Unix.Native.FilePermissions) 256);

                        BufferedStream buffered_stream = new BufferedStream (file_stream);

                        const int BUFFER_SIZE = 8192;
                        byte [] buffer = new byte [BUFFER_SIZE];

                        int read;
                        do {
                                read = stream.Read (buffer, 0, BUFFER_SIZE);
                                if (read > 0)
                                        buffered_stream.Write (buffer, 0, read);
                        } while (read > 0);

                        buffered_stream.Close ();

			return Open (new FileInfo (tempFile));
		}

		/// <summary>
		/// This will throw an exception; callers should catch it and appropriately
		/// display the error message showing the filename etc.
		/// </summary>
		/// <param name="stream">
		/// A <see cref="Stream"/>
		/// </param>
		/// <param name="store_tempfile">
		/// A <see cref="System.Boolean"/>
		/// </param>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool Open (Stream stream, bool store_tempfile)
		{
			if (store_tempfile)
				return Open (stream);

			currentStream = stream;

			isFinished = false;
			if (textPool == null)
				textPool = new ArrayList ();
			if (hotPool == null)
				hotPool = new ArrayList ();
			text_builder.Length = 0;

			// Note: No call to DoOpen ()
			// Some filter might need a DoOpen(),
			// but then they should be stored in a tempfile.

			DoPullProperties ();
			
			if (IsFinished) 
				return true;
			else if (HasError)
				return false;
			
			DoPullSetup ();
			
			if (HasError)
				return false;				

			return true;
		}

		public bool Open (FileSystemInfo info)
		{
			isFinished = false;
			textPool = new ArrayList ();
			hotPool = new ArrayList ();
			text_builder.Length = 0;

			currentInfo = info;

			if (info is FileInfo) {
				// Open a stream for this file.
				currentStream = new FileStream (info.FullName,
								FileMode.Open,
								FileAccess.Read,
								FileShare.Read);
				
				if (preload) {
					// Our default assumption is sequential reads.
					// FIXME: Is this the right thing to do here?
					FileAdvise.IncreaseReadAhead ((FileStream) currentStream);
				
					// Give the OS a hint that we will be reading this
					// file soon.
					FileAdvise.PreLoad ((FileStream) currentStream);				
				}
			}

			try {
				DoOpen (info);

				if (IsFinished)
					return true;
				else if (HasError)
					return false;
				    
				return Open (currentStream, false);
			} catch (Exception e) {
				Log.Warn (e, "Unable to filter {0}:", info.FullName);
				Cleanup (); // clean up temporary files on an exception
				return false;
			}
		}

		public bool Open (string path)
		{
			if (FileSystem.IsSpecialFile(path)) {
				return false;
			} else if (File.Exists (path)) {
				return Open (new FileInfo (path));
			} else if (Directory.Exists (path)) {
				return Open (new DirectoryInfo (path));
			} else {
				return false;
			}
		}

		public FileInfo FileInfo {
			get { return currentInfo as FileInfo; }
		}

		public DirectoryInfo DirectoryInfo {
			get { return currentInfo as DirectoryInfo; }
		}

		public Stream Stream {
			get { return currentStream; }
		}

		public TextReader TextReader {
			get {
				if (currentReader == null
				    && currentStream != null) {
					currentReader = new StreamReader (currentStream);
				}

				return currentReader;
			}
		}

		// Should only be called for Text and NOT for hottext
		// hottext should already be in place by the time Text pull is over
		private bool Pull ()
		{
			while (! IsFinished && ! HasError) {
				DoPull ();
				if (chars_to_pull >= 0 && chars_added >= chars_to_pull) {
					chars_added = 0;
					break;
				}
			}

			if (IsFinished || HasError)
				return false;

			return true;
		}

		private bool closed = false;

		private void Close ()
		{
			if (closed)
				return;

			Cleanup ();

			DoClose ();

			if (currentReader != null)
				currentReader.Close ();

			if (currentStream != null) {
				// When crawling, give the OS a hint that we don't
				// need to keep this file around in the page cache.
				if (indexable.FlushBufferCache && currentStream is FileStream)
					FileAdvise.FlushCache ((FileStream) currentStream);

				currentStream.Close ();
				currentStream = null;
			}

			if (snippetWriter != null)
				snippetWriter.Close ();

			closed = true;
		}

		public void Cleanup ()
		{
			if (tempFile != null) {
				try {
					File.Delete (tempFile);
				} catch (Exception ex) {
					// Just in case it is gone already
				}
				tempFile = null;
			}
		}

		private bool PullFromArray (ArrayList array, StringBuilder sb, bool is_hot)
		{
			if (! is_hot) {
				// HotText is read after Text by DoPull()*.DoClose()
				// So, do not Pull() again for HotText - there ain't anything to pull
				while (array.Count == 0 && Pull ()) { }
			}

			// FIXME: Do we want to try to extract as much data as
			// possible from the filter if we get an error, or
			// should we just give up afterward entirely?

			if (array.Count > 0) {
				foreach (string str in array)
					sb.Append (str);

				array.Clear ();
				return true;
			}
			return false;
		}

		private bool PullTextCarefully (ArrayList array, StringBuilder sb, int chars_to_pull, bool is_hot)
		{
			bool pulled = false;
			this.chars_to_pull = chars_to_pull;

			try {
				pulled = PullFromArray (array, sb, is_hot);
			} catch (Exception ex) {
				Logger.Log.Debug (ex, "Caught exception while pulling text in filter '{0}'", Name);
			}

			return pulled;
		}

		// chars_to_pull is a hint about the number of chars to pull before PullText is called again
		private bool PullText (StringBuilder sb, int chars_to_pull)
		{
			return PullTextCarefully (textPool, sb, chars_to_pull, false);
		}

		private bool PullHotText (StringBuilder sb, int chars_to_pull)
		{
			return PullTextCarefully (hotPool, sb, -1, true);
		}

		public TextReader GetTextReader ()
		{
			PullingReader pr = new PullingReader (
				new PullingReader.Pull (PullText),
				new PullingReader.DoClose (Close));
			return pr;
		}

		public TextReader GetHotTextReader ()
		{
			return new PullingReader (new PullingReader.Pull (PullHotText));
		}

		//////////////////////////////
		//////////// Default implementation of generated indexables

		private ArrayList generated_indexables = new ArrayList ();

		public virtual bool HasGeneratedIndexable {
			get { return generated_indexables.Count > 0; }
		}

		/// <summary>
		/// Good filters should replace this by an IEnumerable that does not require generating
		/// all the indexables beforehand
		/// </summary>
		/// <param name="indexable">
		/// A <see cref="Indexable"/>
		/// </param>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public virtual bool GenerateNextIndexable (out Indexable indexable)
		{
			indexable = null;
			if (generated_indexables.Count == 0)
				return false;

			indexable = (Indexable) generated_indexables [0];
			generated_indexables.RemoveAt (0);
			return true;
		}

		protected void AddIndexable (Indexable indexable)
		{
			this.generated_indexables.Add (indexable);
		}

		protected void AddIndexables (ICollection indexables)
		{
			this.generated_indexables.AddRange (indexables);
		}

		public void CleanGeneratedIndexables ()
		{
			foreach (Indexable indexable in generated_indexables)
				indexable.Cleanup ();
		}
	}

	[AttributeUsage (AttributeTargets.Assembly)]
	public class FilterTypesAttribute : TypeCacheAttribute {
		public FilterTypesAttribute (params Type[] filter_types) : base (filter_types) { }
	}
}
