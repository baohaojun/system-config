//#define HTML_DEBUG
// HtmlAgilityPack V1.0 - Simon Mourier <simonm@microsoft.com>

/*
 * Copyright (C) 2005-2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.IO;
using System.Text;
using System.Diagnostics;
using System.Collections;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.XPath;

// Legend: SLIM=Comment added describing changes to original HtmlAgilityPack
//		to reduce memory consumption
// Once the parser is free of bugs, the comments will be taken out
namespace HtmlAgilityPack
{
	/// <summary>
	/// Represents the type of parsing error.
	/// </summary>
	public enum HtmlParseErrorCode
	{
		/// <summary>
		/// A tag was not closed.
		/// </summary>
		TagNotClosed,

		/// <summary>
		/// A tag was not opened.
		/// </summary>
		TagNotOpened,

		/// <summary>
		/// There is a charset mismatch between stream and declared (META) encoding.
		/// </summary>
		CharsetMismatch,

		/// <summary>
		/// An end tag was not required.
		/// </summary>
		EndTagNotRequired,

		/// <summary>
		/// An end tag is invalid at this position.
		/// </summary>
		EndTagInvalidHere
	}

	/// <summary>
	/// Represents a parsing error found during document parsing.
	/// </summary>
	public class HtmlParseError
	{
		private HtmlParseErrorCode _code;
		private int _line;
		private int _linePosition;
		private int _streamPosition;
		private string _sourceText;
		private string _reason;

		internal HtmlParseError(
			HtmlParseErrorCode code,
			int line,
			int linePosition,
			int streamPosition,
			string sourceText,
			string reason)
		{
			_code = code;
			_line = line;
			_linePosition = linePosition;
			_streamPosition = streamPosition;
			_sourceText = sourceText;
			_reason = reason;
		}

		/// <summary>
		/// Gets the type of error.
		/// </summary>
		public HtmlParseErrorCode Code
		{
			get
			{
				return _code;
			}
		}

		/// <summary>
		/// Gets the line number of this error in the document.
		/// </summary>
		public int Line
		{
			get
			{
				return _line;
			}
		}

		/// <summary>
		/// Gets the column number of this error in the document.
		/// </summary>
		public int LinePosition
		{
			get
			{
				return _linePosition;
			}
		}

		/// <summary>
		/// Gets the absolstream position of this error in the document, relative to the start of the document.
		/// </summary>
		public int StreamPosition
		{
			get
			{
				return _streamPosition;
			}
		}

		/// <summary>
		/// Gets the the full text of the line containing the error.
		/// </summary>
		public string SourceText
		{
			get
			{
				return _sourceText;
				}
		}

		/// <summary>
		/// Gets a description for the error.
		/// </summary>
		public string Reason
		{
			get
			{
				return _reason;
			}
		}
	}
	

	abstract class StreamAsArray {
		public abstract bool Eof (int index);
		public abstract char this [int index] { get;}
		public abstract string Substring (int startindex, int length);
		public abstract int FullLength { get;}
	}
		
	// SLIM: creating this class to wrap around a textreader
	//	 to emulate ReadToEnd () behaviour
	class ImplStreamAsArray : StreamAsArray {
		private StreamReader _reader;
		private int _length;
		private int _position;
		private bool _eof;
		private char[] _buf_previous; // could have used only one array
		private char[] _buf_current; // but, this is cleaner
		private int _block_size;
		
		public ImplStreamAsArray (StreamReader r)
		{
			_reader = r;
			_length = 0;
			_position = 0;
			_eof = false;

			_block_size = 1024;
			_buf_previous = new char [_block_size];
			_buf_current = new char [_block_size];

			Read (true);
		}
		
		private void Read (bool initial)
		{
			if ( !initial) {
				Array.Copy (_buf_current, _buf_previous, _block_size);
				_position += _block_size;
			}
#if HTML_DEBUG
			HtmlDocument.Debug ("Debug: Read in buffer at:" + _position);
#endif

			int num_read = _reader.Read (_buf_current, 0, _block_size);
			if (num_read < _block_size) {
				_eof = true;
				_length = _position + num_read;
			}
#if HTML_DEBUG
			HtmlDocument.Debug ("[" + new string (_buf_current, 0, num_read) + "]");
#endif
		}
		
		public override bool Eof (int index) {
			if (_eof)
				return (index == _length);
			else {
				if (index >= _position + _block_size &&
				    index < _position + _block_size + _block_size)
					Read (false);
				if (_eof)
					return (index == _length);
				else
					return false;
			}
		}
		
		public override char this[int index] {
			get {
				if (index >= _position && 
				    index < _position + _block_size)
					return _buf_current [index % _block_size];
				if (index >= _position - _block_size && 
				    index < _position)
					return _buf_previous [ index % _block_size];
				if (index >= _position + _block_size &&
				    index < _position + _block_size + _block_size) {
					Read (false);
					return _buf_current [index % _block_size];
				}
				return OutOfBandRead (index, 1) [0];
			}
		}

		// evil function ... you get what you pay for!
		private string OutOfBandRead (int startindex, int length)
		{
#if HTML_DEBUG
			HtmlDocument.Debug ("Out of band read! From " + startindex + " to " + (startindex + length - 1));
#endif
			ResetPosition (startindex);
			// ahh.. now we are at the correct place
			// create a buffer of required length
			// who cares if the buffer size does not align well
			// with page boundary
			char[] temp_buf = new char [length];
			int num_read = _reader.Read (temp_buf, 0, length);
			if (num_read < length) {
				// Shouldnt occur!!!
				_eof = true;
				_length = startindex + num_read;
			}
			// discard data and reset stream position
			int t = (_eof ? _length :_position + _block_size);
			ResetPosition (t);
			return new String (temp_buf);
		}

		// streamreader does not allow seeking
		// seek on its basestream does not reflect the position
		// of the reader - it is governed by the buffer size
		// of the underlying stream
		// :( so, read character by character from beginning ...
		private void ResetPosition (int pos)
		{
			_reader.DiscardBufferedData ();
			_reader.BaseStream.Position = 0;
			// read in chunks of block_size
			int n1 = pos / _block_size;
			int n2 = pos % _block_size;
			char[] tmp = new char [_block_size];
			// yo ho... start reading till we have reach pos
			// hopefully, reader will buffer itself, so we can be mean and get one char at a time
			for (int i = 0; i < n1; ++i)
				_reader.Read (tmp, 0, _block_size);
			for (int i = 0; i < n2; ++i)
				_reader.Read ();
			tmp = null;
		}

		public override string Substring (int startindex, int length)
		{
			if (length == 0) {
#if HTML_DEBUG
				HtmlDocument.Debug ("substring:" + startindex + " " + length + " " + _position + ":");
#endif
				return String.Empty;
			}
			if (length > _block_size || startindex < _position - _block_size) {
				return OutOfBandRead (startindex, length);
			}
			while (startindex + length - 1 >= _position + _block_size) {
				Read (false);
			}
			string substr;
			if (startindex < _position) {
				int len_1 = _position - startindex;
				if (length < len_1)
					substr = new String (_buf_previous, _block_size - len_1, length);
				else {
					substr = new String (_buf_previous, _block_size - len_1, len_1);
					substr += new String (_buf_current, 0, length - len_1);
				}
			} else {
				substr = new String (_buf_current, startindex - _position, length);
			}
			return substr;
		}

		// FIXME: Is this costly ?
		public override int FullLength {
			get {
				return (int)_reader.BaseStream.Length;
			}
		}
	}

	// A dummy StreamAsArray wrapper around a string
	class DummyStreamAsArray : StreamAsArray {
		private string _base_string;
		private int _length;

		public DummyStreamAsArray(string str)
		{
			_base_string = str;
			_length = str.Length;
		}

		public override bool Eof(int index)
		{
			return (index >= _length);
		}

		public override char this[int index] {
			get { return _base_string [index]; }
		}

		public override string Substring (int startindex, int length)
		{
			return _base_string.Substring (startindex, length);
		}

		public override int FullLength {
			get { return _length; }
		}
	}

	/// <summary>
	/// Represents a complete HTML document.
	/// </summary>
	public class HtmlDocument: IXPathNavigable
	{
		// SLIM: Make the parser event driven
		// callback for FilterHtml
		// return value is a way for the callback to signal to continue or stop parsing
		public delegate bool NodeHandler (HtmlNode node);
		public NodeHandler ReportNode;
		// misnomer ... should be called event_driven_mode
		private bool _streammode = false;
		private bool _stop_parsing = false;
		private bool _pause_parsing = false;
		private bool _done_parsing = false;

		internal static readonly string HtmlExceptionRefNotChild = "Reference node must be a child of this node";
		internal static readonly string HtmlExceptionUseIdAttributeFalse = "You need to set UseIdAttribute property to true to enable this feature";

		internal Hashtable _openednodes;
		internal Hashtable _lastnodes = new Hashtable();
		internal Hashtable _nodesid;
		private HtmlNode _documentnode;
		//SLIM: internal string _text;
		internal StreamAsArray _text;
		private HtmlNode _currentnode;
		private HtmlNode _lastparentnode;
		private HtmlAttribute _currentattribute;
		private int _index;
		private int _line;
		private int _lineposition, _maxlineposition;
		private int _c;
		private bool _fullcomment;
		private System.Text.Encoding _streamencoding;
		private System.Text.Encoding _declaredencoding;
		private ArrayList _parseerrors = new ArrayList();
		private ParseState _state, _oldstate;
		private Crc32 _crc32 = null;
		private bool _onlyDetectEncoding = false;
		private int _pcdata_quote_char = '\0';

		struct HtmlParserState {
			internal int _lastquote;
		}
		private HtmlParserState _parserState;

		internal static void Debug (string s)
		{
			Console.WriteLine (s);
		}

		// public props

		/// <summary>
		/// Defines if a checksum must be computed for the document while parsing. Default is false.
		/// </summary>
		public bool OptionComputeChecksum = false;

		/// <summary>
		/// Defines if declared encoding must be read from the document.
		/// Declared encoding is determined using the meta http-equiv="content-type" content="text/html;charset=XXXXX" html node.
		/// Default is true.
		/// </summary>
		public bool OptionReadEncoding = true;


		/// <summary>
		/// Defines if non closed nodes will be checked at the end of parsing. Default is true.
		/// </summary>
		public bool OptionCheckSyntax = true;

		/// <summary>
		/// Defines if the 'id' attribute must be specifically used. Default is true.
		/// </summary>
		public bool OptionUseIdAttribute = true;

		/// <summary>
		/// Defines if empty nodes must be written as closed during output. Default is false.
		/// </summary>
		public bool OptionWriteEmptyNodes = false;

		/// <summary>
		/// Defines if output must conform to XML, instead of HTML.
		/// </summary>
		public bool OptionOutputAsXml = false;

		/// <summary>
		/// Defines if name must be output in uppercase. Default is false.
		/// </summary>
		public bool OptionOutputUpperCase = false;

		/// <summary>
		/// Defines if attribute value output must be optimized (not bound with double quotes if it is possible). Default is false.
		/// </summary>
		public bool OptionOutputOptimizeAttributeValues = false;

		/// <summary>
		/// Adds Debugging attributes to node. Default is false.
		/// </summary>
		public bool OptionAddDebuggingAttributes = false;

		/// <summary>
		/// Defines if source text must be extracted while parsing errors.
		/// If the document has a lot of errors, or cascading errors, parsing performance can be dramatically affected if set to true.
		/// Default is false.
		/// </summary>
		public bool OptionExtractErrorSourceText = false; // turning this on can dramatically slow performance if a lot of errors are detected

		/// <summary>
		/// Defines if closing for non closed nodes must be done at the end or directly in the document.
		/// Setting this to true can actually change how browsers render the page. Default is false.
		/// </summary>
		public bool OptionAutoCloseOnEnd = false; // close errors at the end

		/// <summary>
		/// Defines if LI, TR, TH, TD tags must be partially fixed when nesting errors are detected. Default is false.
		/// </summary>
		public bool OptionFixNestedTags = false; // fix li, tr, th, td tags

		/// <summary>
		/// Defines the maximum length of source text or parse errors. Default is 100.
		/// </summary>
		public int OptionExtractErrorSourceTextMaxLength = 100;

		/// <summary>
		/// Defines the default stream encoding to use. Default is System.Text.Encoding.Default.
		/// </summary>
		// From http://www.w3.org/TR/REC-html40/charset.html
		// The HTTP protocol ([RFC2616], section 3.7.1) mentions ISO-8859-1 as a default character encoding when the "charset" parameter is absent from the "Content-Type" header field.
		// So, however we are still using UTF-8 for some unknown reason
		//FIXME: Fix the default encoding!
		public System.Text.Encoding OptionDefaultStreamEncoding = Encoding.UTF8;

		/// <summary>
		/// Gets a list of parse errors found in the document.
		/// </summary>
		public ArrayList ParseErrors
		{
			get
			{
				return _parseerrors;
			}
		}

		/// <summary>
		/// Gets the document's stream encoding.
		/// </summary>
		public System.Text.Encoding StreamEncoding
		{
			get
			{
				return _streamencoding;
			}
		}

		/// <summary>
		/// Gets the document's declared encoding.
		/// Declared encoding is determined using the meta http-equiv="content-type" content="text/html;charset=XXXXX" html node.
		/// </summary>
		public System.Text.Encoding DeclaredEncoding
		{
			get
			{
				return _declaredencoding;
			}
		}

		public bool DoneParsing {
			get
			{
				return _done_parsing;
			}
		}

		/// <summary>
		/// Creates an instance of an HTML document.
		/// </summary>
		public HtmlDocument()
		{
			_documentnode = CreateNode(HtmlNodeType.Document, 0);
			_parserState = new HtmlParserState ();
			_parserState._lastquote = -1;
		}

		internal HtmlNode GetXmlDeclaration()
		{
			if (!_documentnode.HasChildNodes)
			{
				return null;
			}

			foreach(HtmlNode node in _documentnode._childnodes)
			{
				if (node.Name == "?xml") // it's ok, names are case sensitive
				{
					return node;
				}
			}
			return null;
		}

		/// <summary>
		/// Applies HTML encoding to a specified string.
		/// </summary>
		/// <param name="html">The input string to encode. May not be null.</param>
		/// <returns>The encoded string.</returns>
		public static string HtmlEncode(string html)
		{
			if (html == null)
			{
				throw new ArgumentNullException("html");
			}
			// replace & by &amp; but only once!
			Regex rx = new Regex("&(?!(amp;)|(lt;)|(gt;)|(quot;))", RegexOptions.IgnoreCase);
			return rx.Replace(html, "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\"", "&quot;");
		}

		/// <summary>
		/// Detects the encoding of an HTML stream.
		/// </summary>
		/// <param name="stream">The input stream. May not be null.</param>
		/// <returns>The detected encoding.</returns>
		public Encoding DetectEncoding(Stream stream)
		{
			if (stream == null)
			{
				throw new ArgumentNullException("stream");
			}
			return DetectEncoding(new StreamReader(stream));
		}

		/// <summary>
		/// Detects the encoding of an HTML file.
		/// </summary>
		/// <param name="path">Path for the file containing the HTML document to detect. May not be null.</param>
		/// <returns>The detected encoding.</returns>
		public Encoding DetectEncoding(string path)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			StreamReader sr = new StreamReader(path, OptionDefaultStreamEncoding);
			Encoding encoding = DetectEncoding(sr);
			sr.Close();
			return encoding;
		}

		/// <summary>
		/// Detects the encoding of an HTML text.
		/// </summary>
		/// <param name="html">The input html text. May not be null.</param>
		/// <returns>The detected encoding.</returns>
		public Encoding DetectEncodingHtml(string html)
		{
			if (html == null)
			{
				throw new ArgumentNullException("html");
			}
			StringReader sr = new StringReader(html);
			Encoding encoding = DetectEncoding(sr);
			sr.Close();
			return encoding;
		}

		/// <summary>
		/// Detects the encoding of an HTML text provided on a TextReader.
		/// </summary>
		/// <param name="reader">The TextReader used to feed the HTML. May not be null.</param>
		/// <returns>The detected encoding.</returns>
		public Encoding DetectEncoding(TextReader reader)
		{
			if (reader == null)
			{
				throw new ArgumentNullException("reader");
			}
			_onlyDetectEncoding = true;
			if (OptionCheckSyntax)
			{
				_openednodes = new Hashtable();
			}
			else
			{
				_openednodes = null;
			}

			if (OptionUseIdAttribute)
			{
				_nodesid = new Hashtable();
			}
			else
			{
				_nodesid = null;
			}

			StreamReader sr = reader as StreamReader;
			if (sr != null)
			{
				_streamencoding = sr.CurrentEncoding;
				_text = new ImplStreamAsArray (sr);
			}
			else
			{
				_streamencoding = null;
				// Expensive, but cannot avoid since TextReader doesnt have any length of the underlying data
				_text = new DummyStreamAsArray (reader.ReadToEnd());
			}
			_declaredencoding = null;

			// SLIM: _text = reader.ReadToEnd();
			_documentnode = CreateNode(HtmlNodeType.Document, 0);

			// this is a hack, but it allows us not to muck with the original parsing code
			try
			{
				Parse();
			}
			catch(EncodingFoundException ex)
			{
				_lastnodes.Clear();
				return ex.Encoding;
			}
			return null;
		}

		/// <summary>
		/// Loads an HTML document from a stream.
		/// </summary>
		/// <param name="stream">The input stream.</param>
		public void Load(Stream stream)
		{
			Load(new StreamReader(stream, OptionDefaultStreamEncoding));
		}

		/// <summary>
		/// Loads an HTML document from a stream.
		/// </summary>
		/// <param name="stream">The input stream.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the stream.</param>
		public void Load(Stream stream, bool detectEncodingFromByteOrderMarks)
		{
			Load(new StreamReader(stream, detectEncodingFromByteOrderMarks));
		}

		/// <summary>
		/// Loads an HTML document from a stream.
		/// </summary>
		/// <param name="stream">The input stream.</param>
		/// <param name="encoding">The character encoding to use.</param>
		public void Load(Stream stream, Encoding encoding)
		{
			Load(new StreamReader(stream, encoding));
		}

		/// <summary>
		/// Loads an HTML document from a stream.
		/// </summary>
		/// <param name="stream">The input stream.</param>
		/// <param name="encoding">The character encoding to use.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the stream.</param>
		public void Load(Stream stream, Encoding encoding, bool detectEncodingFromByteOrderMarks)
		{
			Load(new StreamReader(stream, encoding, detectEncodingFromByteOrderMarks));
		}

		/// <summary>
		/// Loads an HTML document from a stream.
		/// </summary>
		/// <param name="stream">The input stream.</param>
		/// <param name="encoding">The character encoding to use.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the stream.</param>
		/// <param name="buffersize">The minimum buffer size.</param>
		public void Load(Stream stream, Encoding encoding, bool detectEncodingFromByteOrderMarks, int buffersize)
		{
			Load(new StreamReader(stream, encoding, detectEncodingFromByteOrderMarks, buffersize));
		}

		/// <summary>
		/// Loads an HTML document from a file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		public void Load(string path)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			StreamReader sr = new StreamReader(path, OptionDefaultStreamEncoding);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Loads an HTML document from a file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the file.</param>
		public void Load(string path, bool detectEncodingFromByteOrderMarks)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			StreamReader sr = new StreamReader(path, detectEncodingFromByteOrderMarks);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Loads an HTML document from a file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		/// <param name="encoding">The character encoding to use. May not be null.</param>
		public void Load(string path, Encoding encoding)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			if (encoding == null)
			{
				throw new ArgumentNullException("encoding");
			}
			StreamReader sr = new StreamReader(path, encoding);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Loads an HTML document from a file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		/// <param name="encoding">The character encoding to use. May not be null.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the file.</param>
		public void Load(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			if (encoding == null)
			{
				throw new ArgumentNullException("encoding");
			}
			StreamReader sr = new StreamReader(path, encoding, detectEncodingFromByteOrderMarks);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Loads an HTML document from a file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		/// <param name="encoding">The character encoding to use. May not be null.</param>
		/// <param name="detectEncodingFromByteOrderMarks">Indicates whether to look for byte order marks at the beginning of the file.</param>
		/// <param name="buffersize">The minimum buffer size.</param>
		public void Load(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, int buffersize)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			if (encoding == null)
			{
				throw new ArgumentNullException("encoding");
			}
			StreamReader sr = new StreamReader(path, encoding, detectEncodingFromByteOrderMarks, buffersize);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Loads the HTML document from the specified string.
		/// </summary>
		/// <param name="html">String containing the HTML document to load. May not be null.</param>
		public void LoadHtml(string html)
		{
			if (html == null)
			{
				throw new ArgumentNullException("html");
			}
			StringReader sr = new StringReader(html);
			Load(sr);
			sr.Close();
		}

		/// <summary>
		/// Detects the encoding of an HTML document from a file first, and then loads the file.
		/// </summary>
		/// <param name="path">The complete file path to be read.</param>
		public void DetectEncodingAndLoad(string path)
		{
			DetectEncodingAndLoad(path, true);
		}

		/// <summary>
		/// Detects the encoding of an HTML document from a file first, and then loads the file.
		/// </summary>
		/// <param name="path">The complete file path to be read. May not be null.</param>
		/// <param name="detectEncoding">true to detect encoding, false otherwise.</param>
		public void DetectEncodingAndLoad(string path, bool detectEncoding)
		{
			if (path == null)
			{
				throw new ArgumentNullException("path");
			}
			System.Text.Encoding enc;
			if (detectEncoding)
			{
				enc = DetectEncoding(path);
			}
			else
			{
				enc = null;
			}

			if (enc == null)
			{
				Load(path);
			}
			else
			{
				Load(path, enc);
			}
		}

		/// <summary>
		/// Loads the HTML document from the specified TextReader.
		/// </summary>
		/// <param name="reader">The TextReader used to feed the HTML data into the document. May not be null.</param>
		public void Load(TextReader reader)
		{
			// all Load methods pass down to this one
			if (reader == null)
			{
				throw new ArgumentNullException("reader");
			}

			_onlyDetectEncoding = false;

			if (OptionCheckSyntax)
			{
				_openednodes = new Hashtable();
			}
			else
			{
				_openednodes = null;
			}

			if (OptionUseIdAttribute)
			{
				_nodesid = new Hashtable();
			}
			else
			{
				_nodesid = null;
			}

			StreamReader sr = reader as StreamReader;
			if (sr != null)
			{
                                try
				{
				    // trigger bom read if needed
				    sr.Peek();
				}
				catch
				{
				    // void on purpose
				}
				_streamencoding = sr.CurrentEncoding;
				_text = new ImplStreamAsArray (sr);
			}
			else
			{
				_streamencoding = null;
				// Expensive, but cannot avoid since TextReader doesnt have any length of the underlying data
				_text = new DummyStreamAsArray (reader.ReadToEnd());
			}
			_declaredencoding = null;

			// SLIM: _text = reader.ReadToEnd();
			_documentnode = CreateNode(HtmlNodeType.Document, 0);
			Parse();

			if (OptionCheckSyntax)
			{
				foreach(HtmlNode node in _openednodes.Values)
				{
					if (!node._starttag)	// already reported
					{
						continue;
					}

					string html;
					if (OptionExtractErrorSourceText)
					{
						html = node.OuterHtml;
						if (html.Length > OptionExtractErrorSourceTextMaxLength)
						{
							html = html.Substring(0, OptionExtractErrorSourceTextMaxLength);
						}
					}
					else
					{
						html = string.Empty;
					}
					AddError(
						HtmlParseErrorCode.TagNotClosed,
						node._line, node._lineposition,
						node._streamposition, html,
						"End tag </" + node.Name + "> was not found");
				}

				// we don't need this anymore
				_openednodes.Clear();
			}
		}

		public void PauseLoad()
		{
#if HTML_DEBUG
			Debug ("Pausing load");
#endif
			_pause_parsing = true;
		}

		public void ResumeLoad()
		{
			if (_done_parsing)
				return;

			if (! _pause_parsing || _parserState._lastquote == -1)
				throw new Exception ("Load() was not paused previously");

			// Reset the old states and values
			int _lastquote = _parserState._lastquote;
			_parserState._lastquote = -1;
			_pause_parsing = false;

#if HTML_DEBUG
			Debug ("Resuming parsing");
#endif
			DoParse (_lastquote);
		}

		internal System.Text.Encoding GetOutEncoding()
		{
			// when unspecified, use the stream encoding first
			if (_declaredencoding != null)
			{
				return _declaredencoding;
			}
			else
			{
				if (_streamencoding != null)
				{
					return _streamencoding;
				}
			}
			return OptionDefaultStreamEncoding;
		}


		/// <summary>
		/// Gets the document's output encoding.
		/// </summary>
		public System.Text.Encoding Encoding
		{
			get
			{
				return GetOutEncoding();
			}
		}

		/// <summary>
		/// Saves the HTML document to the specified stream.
		/// </summary>
		/// <param name="outStream">The stream to which you want to save.</param>
		public void Save(Stream outStream)
		{
			StreamWriter sw = new StreamWriter(outStream, GetOutEncoding());
			Save(sw);
		}

		/// <summary>
		/// Saves the HTML document to the specified stream.
		/// </summary>
		/// <param name="outStream">The stream to which you want to save. May not be null.</param>
		/// <param name="encoding">The character encoding to use. May not be null.</param>
		public void Save(Stream outStream, System.Text.Encoding encoding)
		{
			if (outStream == null)
			{
				throw new ArgumentNullException("outStream");
			}
			if (encoding == null)
			{
				throw new ArgumentNullException("encoding");
			}
			StreamWriter sw = new StreamWriter(outStream, encoding);
			Save(sw);
		}

		/// <summary>
		/// Saves the mixed document to the specified file.
		/// </summary>
		/// <param name="filename">The location of the file where you want to save the document.</param>
		public void Save(string filename)
		{
			StreamWriter sw = new StreamWriter(filename, false, GetOutEncoding());
			Save(sw);
			sw.Close();
		}

		/// <summary>
		/// Saves the mixed document to the specified file.
		/// </summary>
		/// <param name="filename">The location of the file where you want to save the document. May not be null.</param>
		/// <param name="encoding">The character encoding to use. May not be null.</param>
		public void Save(string filename, System.Text.Encoding encoding)
		{
			if (filename == null)
			{
				throw new ArgumentNullException("filename");
			}
			if (encoding == null)
			{
				throw new ArgumentNullException("encoding");
			}
			StreamWriter sw = new StreamWriter(filename, false, encoding);
			Save(sw);
			sw.Close();
		}

		/// <summary>
		/// Saves the HTML document to the specified StreamWriter.
		/// </summary>
		/// <param name="writer">The StreamWriter to which you want to save.</param>
		public void Save(StreamWriter writer)
		{
			Save((TextWriter)writer);
		}

		/// <summary>
		/// Saves the HTML document to the specified TextWriter.
		/// </summary>
		/// <param name="writer">The TextWriter to which you want to save. May not be null.</param>
		public void Save(TextWriter writer)
		{
			if (writer == null)
			{
				throw new ArgumentNullException("writer");
			}
			DocumentNode.WriteTo(writer);
		}

		/// <summary>
		/// Saves the HTML document to the specified XmlWriter.
		/// </summary>
		/// <param name="writer">The XmlWriter to which you want to save.</param>
		public void Save(XmlWriter writer)
		{
			DocumentNode.WriteTo(writer);
			writer.Flush();
		}

		/// <summary>
		/// Creates a new XPathNavigator object for navigating this HTML document.
		/// </summary>
		/// <returns>An XPathNavigator object. The XPathNavigator is positioned on the root of the document.</returns>
		public XPathNavigator CreateNavigator()
		{
			return new HtmlNodeNavigator(this, _documentnode);
		}

		internal void SetIdForNode(HtmlNode node, string id)
		{
			if (!OptionUseIdAttribute)
			{
				return;
			}

			if ((_nodesid == null) || (id == null))
			{
				return;
			}

			if (node == null)
			{
				_nodesid.Remove(id.ToLower());
			}
			else
			{
				_nodesid[id.ToLower()] = node;
			}
		}

		/// <summary>
		/// Gets the HTML node with the specified 'id' attribute value.
		/// </summary>
		/// <param name="id">The attribute id to match. May not be null.</param>
		/// <returns>The HTML node with the matching id or null if not found.</returns>
		public HtmlNode GetElementbyId(string id)
		{
			if (id == null)
			{
				throw new ArgumentNullException("id");
			}
			if (_nodesid == null)
			{
				throw new Exception(HtmlExceptionUseIdAttributeFalse);
			}

			return _nodesid[id.ToLower()] as HtmlNode;
		}

		/// <summary>
		/// Creates an HTML element node with the specified name.
		/// </summary>
		/// <param name="name">The qualified name of the element. May not be null.</param>
		/// <returns>The new HTML node.</returns>
		public HtmlNode CreateElement(string name)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name");
			}
			HtmlNode node = CreateNode(HtmlNodeType.Element);
			node._name = name;
			return node;
		}

		/// <summary>
		/// Creates an HTML comment node.
		/// </summary>
		/// <returns>The new HTML comment node.</returns>
		public HtmlCommentNode CreateComment()
		{
			return (HtmlCommentNode)CreateNode(HtmlNodeType.Comment);
		}

		/// <summary>
		/// Creates an HTML comment node with the specified comment text.
		/// </summary>
		/// <param name="comment">The comment text. May not be null.</param>
		/// <returns>The new HTML comment node.</returns>
		public HtmlCommentNode CreateComment(string comment)
		{
			if (comment == null)
			{
				throw new ArgumentNullException("comment");
			}
			HtmlCommentNode c = CreateComment();
			c.Comment = comment;
			return c;
		}

		/// <summary>
		/// Creates an HTML text node.
		/// </summary>
		/// <returns>The new HTML text node.</returns>
		public HtmlTextNode CreateTextNode()
		{
			return (HtmlTextNode)CreateNode(HtmlNodeType.Text);
		}

		/// <summary>
		/// Creates an HTML text node with the specified text.
		/// </summary>
		/// <param name="text">The text of the node. May not be null.</param>
		/// <returns>The new HTML text node.</returns>
		public HtmlTextNode CreateTextNode(string text)
		{
			if (text == null)
			{
				throw new ArgumentNullException("text");
			}
			HtmlTextNode t = CreateTextNode();
			t.Text = text;
			return t;
		}

		internal HtmlNode CreateNode(HtmlNodeType type)
		{
			return CreateNode(type, -1);
		}

		internal HtmlNode CreateNode(HtmlNodeType type, int index)
		{
			switch (type)
			{
				case HtmlNodeType.Comment:
					return new HtmlCommentNode(this, index);

				case HtmlNodeType.Text:
					return new HtmlTextNode(this, index);

				default:
					return new HtmlNode(type, this, index);
			}
		}

		internal HtmlAttribute CreateAttribute()
		{
			return new HtmlAttribute(this);
		}

		/// <summary>
		/// Creates an HTML attribute with the specified name.
		/// </summary>
		/// <param name="name">The name of the attribute. May not be null.</param>
		/// <returns>The new HTML attribute.</returns>
		public HtmlAttribute CreateAttribute(string name)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name");
			}
			HtmlAttribute att = CreateAttribute();
			att.Name = name;
			return att;
		}

		/// <summary>
		/// Creates an HTML attribute with the specified name.
		/// </summary>
		/// <param name="name">The name of the attribute. May not be null.</param>
		/// <param name="value">The value of the attribute.</param>
		/// <returns>The new HTML attribute.</returns>
		public HtmlAttribute CreateAttribute(string name, string value)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name");
			}
			HtmlAttribute att = CreateAttribute(name);
			att.Value = value;
			return att;
		}

		/// <summary>
		/// Gets the root node of the document.
		/// </summary>
		public HtmlNode DocumentNode
		{
			get
			{
				return _documentnode;
			}
		}

		/// <summary>
		/// Gets the document CRC32 checksum if OptionComputeChecksum was set to true before parsing, 0 otherwise.
		/// </summary>
		public int CheckSum
		{
			get
			{
				if (_crc32 == null)
				{
					return 0;
				}
				else
				{
					return (int)_crc32.CheckSum;
				}
			}
		}

		public bool StreamMode
		{
			get
			{
				return _streammode;
			}
			set
			{
				_streammode = value;
			}
		}

		private HtmlParseError AddError(
				HtmlParseErrorCode code,
				int line,
				int linePosition,
				int streamPosition,
				string sourceText,
				string reason)
			{
			HtmlParseError err = new HtmlParseError(code, line, linePosition, streamPosition, sourceText, reason);
			_parseerrors.Add(err);
			return err;
		}

		private enum ParseState
		{
			Text,
			WhichTag,
			Tag,
			BetweenAttributes,
			EmptyTag,
			AttributeName,
			AttributeBeforeEquals,
			AttributeAfterEquals,
			AttributeValue,
			Comment,
			QuotedAttributeValue,
			ServerSideCode,
			PcDataQuote,
			PcDataCommentSingleLine,
			PcDataCommentMultiLine,
			PcData
		}

		private void IncrementPosition()
		{
			if (_crc32 != null)
			{
				// REVIEW: should we add some checksum code in DecrementPosition too?
				_crc32.AddToCRC32(_c);
			}

			_index++;
			_maxlineposition = _lineposition;
			if (_c == 10)
			{
				_lineposition = 1;
				_line++;
			}
			else
			{
				_lineposition++;
			}
		}

		private void DecrementPosition()
		{
			_index--;
			if (_lineposition == 1)
			{
				_lineposition = _maxlineposition;
				_line--;
			}
			else
			{
				_lineposition--;
			}
		}

		private void Parse()
		{
			int lastquote = 0;
			if (OptionComputeChecksum)
			{
				_crc32 = new Crc32();
			}

			_lastnodes = new Hashtable();
			_c = 0;
			_fullcomment = false;
			_parseerrors = new ArrayList();
			_line = 1;
			_lineposition = 1;
			_maxlineposition = 1;

			_state = ParseState.Text;
			_oldstate = _state;
			_documentnode._innerlength = _text.FullLength;
			_documentnode._outerlength = _text.FullLength;

			_lastparentnode = _documentnode;
			_currentnode = CreateNode(HtmlNodeType.Text, 0);
			_currentattribute = null;

			_index = 0;
			PushNodeStart(HtmlNodeType.Text, 0);

			DoParse (lastquote);
		}

		private void DoParse (int lastquote)
		{
			// SLIM: while (_index<_text.Length)
			while (! _pause_parsing && ! _stop_parsing && ! _text.Eof (_index))
			{
				_c = _text[_index];
#if HTML_DEBUG
				Debug (String.Format ("_index : {0}({2})({1}) ", _index, (char)_c, _state));
#endif
				IncrementPosition();

				switch(_state)
				{
					case ParseState.Text:
						if (NewCheck())
							continue;
						break;

					case ParseState.WhichTag:
						if (NewCheck())
							continue;
						if (_c == '/')
						{
							PushNodeNameStart(false, _index);
						}
						else
						{
							PushNodeNameStart(true, _index-1);
							DecrementPosition();
						}
						_state = ParseState.Tag;
						break;

					case ParseState.Tag:
						if (NewCheck())
							continue;
						if (IsWhiteSpace(_c))
						{
							PushNodeNameEnd(_index-1);
							if (_state != ParseState.Tag)
								continue;
							_state = ParseState.BetweenAttributes;
							continue;
						}
						if (_c == '/')
						{
							PushNodeNameEnd(_index-1);
							if (_state != ParseState.Tag)
								continue;
							_state = ParseState.EmptyTag;
							continue;
						}
						if (_c == '>')
						{
							PushNodeNameEnd(_index-1);
							if (_state != ParseState.Tag)
								continue;
							PushNodeEnd(_index, false);
							if (_state != ParseState.Tag)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
						}
						break;

					case ParseState.BetweenAttributes:
						if (NewCheck())
							continue;

						if (IsWhiteSpace(_c))
							continue;

						if ((_c == '/') || (_c == '?'))
						{
							_state = ParseState.EmptyTag;
							continue;
						}

						if (_c == '>')
						{
							PushNodeEnd(_index, false);
							if (_state != ParseState.BetweenAttributes)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}

						PushAttributeNameStart(_index-1);
						_state = ParseState.AttributeName;
						break;

					case ParseState.EmptyTag:
						if (NewCheck())
							continue;

						if (_c == '>')
						{
							PushNodeEnd(_index, true);
							if (_state != ParseState.EmptyTag)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						_state = ParseState.BetweenAttributes;
						break;

					case ParseState.AttributeName:
						if (NewCheck())
							continue;

						if (IsWhiteSpace(_c))
						{
							PushAttributeNameEnd(_index-1);
							_state = ParseState.AttributeBeforeEquals;
							continue;
						}
						if (_c == '=')
						{
							PushAttributeNameEnd(_index-1);
							_state = ParseState.AttributeAfterEquals;
							continue;
						}
						if (_c == '>')
						{
							PushAttributeNameEnd(_index-1);
							PushNodeEnd(_index, false);
							if (_state != ParseState.AttributeName)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						break;

					case ParseState.AttributeBeforeEquals:
						if (NewCheck())
							continue;

						if (IsWhiteSpace(_c))
							continue;
						if (_c == '>')
						{
							PushNodeEnd(_index, false);
							if (_state != ParseState.AttributeBeforeEquals)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						if (_c == '=')
						{
							_state = ParseState.AttributeAfterEquals;
							continue;
						}
						// no equals, no whitespace, it's a new attrribute starting
						_state = ParseState.BetweenAttributes;
						DecrementPosition();
						break;

					case ParseState.AttributeAfterEquals:
						if (NewCheck())
							continue;

						if (IsWhiteSpace(_c))
							continue;

						if ((_c == '\'') || (_c == '"'))
						{
							_state = ParseState.QuotedAttributeValue;
							PushAttributeValueStart(_index);
							lastquote = _c;
							continue;
						}
						if (_c == '>')
						{
							PushNodeEnd(_index, false);
							if (_state != ParseState.AttributeAfterEquals)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						PushAttributeValueStart(_index-1);
						_state = ParseState.AttributeValue;
						break;

					case ParseState.AttributeValue:
						if (NewCheck())
							continue;

						if (IsWhiteSpace(_c))
						{
							PushAttributeValueEnd(_index-1);
							_state = ParseState.BetweenAttributes;
							continue;
						}

						if (_c == '>')
						{
							PushAttributeValueEnd(_index-1);
							PushNodeEnd(_index, false);
							if (_state != ParseState.AttributeValue)
								continue;
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						break;

					case ParseState.QuotedAttributeValue:
						if (_c == lastquote)
						{
							PushAttributeValueEnd(_index-1);
							_state = ParseState.BetweenAttributes;
							continue;
						}
						if (_c == '<')
						{
							//SLIM: if (_index<_text.Length)
							if (!_text.Eof (_index))
							{
								if (_text[_index] == '%')
								{
									_oldstate = _state;
									_state = ParseState.ServerSideCode;
									continue;
								}
							}
						}
						break;

					case ParseState.Comment:
						if (_c == '>')
						{
							if (_fullcomment)
							{
								if ((_text[_index-2] != '-') ||
									(_text[_index-3] != '-'))
								{
									continue;
								}
							}
							PushNodeEnd(_index, false);
							_state = ParseState.Text;
							PushNodeStart(HtmlNodeType.Text, _index);
							continue;
						}
						break;

					case ParseState.ServerSideCode:
						if (_c == '%')
						{
							//SLIM: if (_index<_text.Length)
							if (! _text.Eof (_index))
							{
								if (_text[_index] == '>')
								{
									switch(_oldstate)
									{
										case ParseState.AttributeAfterEquals:
											_state = ParseState.AttributeValue;
											break;

										case ParseState.BetweenAttributes:
											PushAttributeNameEnd(_index+1);
											_state = ParseState.BetweenAttributes;
											break;

										default:
											_state = _oldstate;
											break;
									}
									IncrementPosition();
								}
							}
						}
						break;

					// handle <script>a="</script>"</script>
					case ParseState.PcDataQuote:
						if ((_c == _pcdata_quote_char) && (_text [_index - 2] != '\\')) {
							_pcdata_quote_char = '\0';
							_state = ParseState.PcData;
						}
						break;

					// handle // abcd A's webpage
					case ParseState.PcDataCommentSingleLine:
						if (_c == '\n')
							_state = ParseState.PcData;
						break;

					// handle /* multiple lines */
					case ParseState.PcDataCommentMultiLine:
						if (_c == '/' && _text [_index - 2] == '*')
							_state = ParseState.PcData;
						break;
					case ParseState.PcData:
#if HTML_DEBUG
						Debug (String.Format ("PCDATA ({0}) {1} {2}", _index, _currentnode.Name, _text.Substring(_index-1,  _currentnode._namelength+2)));
#endif
						if (_c == '\"' || _c == '\''){
							_pcdata_quote_char = _c;
							_state = ParseState.PcDataQuote;
							break;
						} else if (_c == '/' &&
							   _text [_index - 2] == '/' && // A nasty hack to prevent the 'foobar' in url(http://foobar) to be parsed as a comment
							   _text [_index - 3] != ':') {
							_state = ParseState.PcDataCommentSingleLine;
							break;
						} else if (_c == '*' && _text [_index - 2] == '/') {
							_state = ParseState.PcDataCommentMultiLine;
							break;
						}
						// look for </tag + 1 char

						// check buffer end
						//SLIM: if ((_currentnode._namelength+3)<=(_text.Length-(_index-1)))
						if (! _text.Eof (_currentnode._namelength + _index + 1))  
						{
							if (string.Compare(_text.Substring(_index-1, _currentnode._namelength+2),
								"</" + _currentnode.Name, true) == 0)
							{
								int c = _text[_index-1 + 2 + _currentnode.Name.Length];
								if ((c == '>') || (IsWhiteSpace(c)))
								{
									// add the script as a text node
									HtmlNode script = CreateNode(HtmlNodeType.Text,
										_currentnode._outerstartindex + _currentnode._outerlength);
									script._outerlength = _index-1 - script._outerstartindex;
									if (_streammode && ReportNode != null)
										_stop_parsing = ! ReportNode (script);
									else
										_currentnode.AppendChild(script);
#if HTML_DEBUG
									Debug ("Found script: [" + script.InnerText + "]");
#endif

									PushNodeStart(HtmlNodeType.Element, _index-1);
									PushNodeNameStart(false, _index-1 +2);
									_state = ParseState.Tag;
									IncrementPosition();
								}
							}
						}
						break;
				}
			}

			if (_pause_parsing)
			{
				_parserState._lastquote = lastquote;
#if HTML_DEBUG
				Debug ("Pausing parsing");
#endif
			}

			if (_stop_parsing || _text.Eof (_index))
			{
				// Mark that parsing is over
				_done_parsing = true;
#if HTML_DEBUG
				Debug ("Done parsing");
#endif

				// finish the current work
				if (_currentnode._namestartindex > 0)
				{
					PushNodeNameEnd(_index);
				}
				PushNodeEnd(_index, false);

				// we don't need this anymore
				_lastnodes.Clear();
			}
		}

		private bool NewCheck()
		{
			if (_c != '<')
			{
				return false;
			}
			//SLIM: if (_index<_text.Length)
			if (! _text.Eof (_index))
			{
				if (_text[_index] == '%')
				{
					switch(_state)
					{
						case ParseState.AttributeAfterEquals:
                            PushAttributeValueStart(_index-1);
							break;

						case ParseState.BetweenAttributes:
							PushAttributeNameStart(_index-1);
							break;

						case ParseState.WhichTag:
							PushNodeNameStart(true, _index-1);
							_state = ParseState.Tag;
							break;
					}
					_oldstate = _state;
					_state = ParseState.ServerSideCode;
					return true;
				}
			}

			PushNodeEnd(_index-1, true);
			_state = ParseState.WhichTag;
			//SLIM: if ((_index-1) <= (_text.Length-2))
			if (!_text.Eof (_index))
			{
				if (_text[_index] == '!')
				{
					PushNodeStart(HtmlNodeType.Comment, _index-1);
					PushNodeNameStart(true, _index);
					PushNodeNameEnd(_index+1);
					_state = ParseState.Comment;
					//SLIM: if (_index<(_text.Length-2))
					if (! _text.Eof (_index + 2))
					{
						if ((_text[_index+1] == '-') &&
							(_text[_index+2] == '-'))
						{
							_fullcomment = true;
						}
						else
						{
							_fullcomment = false;
						}
					}
					return true;
				}
			}
			PushNodeStart(HtmlNodeType.Element, _index-1);
			return true;
		}

		private void ReadDocumentEncoding(HtmlNode node)
		{
			if (!OptionReadEncoding)
				return;
			// format is 
			// <meta http-equiv="content-type" content="text/html;charset=iso-8859-1" />
			
			// when we append a child, we are in node end, so attributes are already populated
			if (node._namelength == 4)	// quick check, avoids string alloc
			{
				// only these nodes can occur before meta
				// if we started seeing any other node, we will never see a meta node
				if (node.NodeType == HtmlNodeType.Element &&
					     (node.Name != "head" && node.Name != "script" &&
					      node.Name != "style" && node.Name != "title" &&
					      node.Name != "head" && node.Name != "link" &&
					      node.Name != "html" && node.Name != "meta")) {
					    _declaredencoding = null;
					    if (_onlyDetectEncoding)
						    throw new EncodingFoundException (null);
					    else
						    return;
					    // FIXME: Should also handle declaredencoding mismatch with detected
					    // encoding, as done below. None of the current filters run in error
					    // detection mode currently, so its not needed now.
				}
				else if (node.Name == "meta") // all nodes names are lowercase
				{
					HtmlAttribute att = node.Attributes["http-equiv"];
					if (att != null)
					{
						if (string.Compare(att.Value, "content-type", true) == 0)
						{
							HtmlAttribute content = node.Attributes["content"];
							if (content != null)
							{
								string charset = NameValuePairList.GetNameValuePairsValue(content.Value, "charset");
								if (charset != null)
								{
									_declaredencoding = Encoding.GetEncoding(charset);
									if (_onlyDetectEncoding)
									{
										throw new EncodingFoundException(_declaredencoding);
									}

									if (_streamencoding != null)
									{
										if (_declaredencoding.WindowsCodePage != _streamencoding.WindowsCodePage)
										{
											AddError(
												HtmlParseErrorCode.CharsetMismatch,
												_line, _lineposition,
												_index, node.OuterHtml,
												"Encoding mismatch between StreamEncoding: " +
												_streamencoding.WebName + " and DeclaredEncoding: " + _declaredencoding.WebName);
										}
									}
								}
							}
						}
					}
				}
			}
		}

		private void PushAttributeNameStart(int index)
		{
			_currentattribute = CreateAttribute();
			_currentattribute._namestartindex = index;
			_currentattribute._line = _line;
			_currentattribute._lineposition = _lineposition;
			_currentattribute._streamposition = index;
		}

		private void PushAttributeNameEnd(int index)
		{
			_currentattribute._namelength = index - _currentattribute._namestartindex;
			_currentnode.Attributes.Append(_currentattribute);
		}

		private void PushAttributeValueStart(int index)
		{
			_currentattribute._valuestartindex = index;
		}

		private void PushAttributeValueEnd(int index)
		{
			_currentattribute._valuelength = index - _currentattribute._valuestartindex;
		}

		private void PushNodeStart(HtmlNodeType type, int index)
		{
			_currentnode = CreateNode(type, index);
			_currentnode._line = _line;
			_currentnode._lineposition = _lineposition;
			if (type == HtmlNodeType.Element)
			{
				_currentnode._lineposition--;
			}
			_currentnode._streamposition = index;
		}

		private void PushNodeEnd(int index, bool close)
		{
			_currentnode._outerlength = index - _currentnode._outerstartindex;

			//SLIM: inform caller
			if (_streammode && ReportNode != null)
				_stop_parsing = ! ReportNode (_currentnode);

#if HTML_DEBUG
			{
				if (_currentnode._nodetype == HtmlNodeType.Text)
					Debug ("Text:" + _currentnode.InnerText);
				else
					Debug ((_currentnode.StartTag ? "Start-" : "End-") + _currentnode.Name);
			}
#endif
			if ((_currentnode._nodetype == HtmlNodeType.Text) ||
				(_currentnode._nodetype == HtmlNodeType.Comment))
			{
				// forget about void nodes
				if (_currentnode._outerlength>0)
				{
					_currentnode._innerlength = _currentnode._outerlength;
					_currentnode._innerstartindex = _currentnode._outerstartindex;
					// SLIM: no need to append child in stream mode
					// SLIM: whatever the caller needs to do, tell it to do now
					if (!_streammode && _lastparentnode != null)
					{
					   _lastparentnode.AppendChild(_currentnode);
					}
				}
			}
			else
			{
				if ((_currentnode._starttag) && (_lastparentnode != _currentnode))
				{
					// add to parent node
					// SLIM: no need to append child in stream mode
					// SLIM: whatever the caller needs to do, tell it to do now
					if (!_streammode && _lastparentnode != null)
					{
					   _lastparentnode.AppendChild(_currentnode);
					}

					ReadDocumentEncoding(_currentnode);

					// remember last node of this kind
					// SLIM: we still to store _currentnode to help other tags in the same level
					HtmlNode prev = (HtmlNode)_lastnodes[_currentnode.Name];
					_currentnode._prevwithsamename = prev;
					_lastnodes[_currentnode.Name] = _currentnode;

					// change parent?
					if ((_currentnode.NodeType == HtmlNodeType.Document) ||
						(_currentnode.NodeType == HtmlNodeType.Element))
					{
						_lastparentnode = _currentnode;
					}

					if (HtmlNode.IsCDataElement(CurrentNodeName()))
					{
						_state = ParseState.PcData;
						return;
					}

					if ((HtmlNode.IsClosedElement(_currentnode.Name)) ||
						(HtmlNode.IsEmptyElement(_currentnode.Name)))
					{
						close = true;
					}
				}
			}

			if ((close) || (!_currentnode._starttag))
			{
				CloseCurrentNode();
				if ((_currentnode._nodetype == HtmlNodeType.Text) || 
				    (_currentnode._nodetype == HtmlNodeType.Comment))
					_currentnode = null;
			}
		}

		private void PushNodeNameStart(bool starttag, int index)
		{
			_currentnode._starttag = starttag;
			_currentnode._namestartindex = index;
		}

		private string[] GetResetters(string name)
		{
			switch (name)
			{
				case "li":
					return new string[]{"ul"};

				case "tr":
					return new string[]{"table"};

				case "th":
				case "td":
					return new string[]{"tr", "table"};

				default:
					return null;
			}
		}

		private void FixNestedTags()
		{
			// we are only interested by start tags, not closing tags
			if (!_currentnode._starttag)
				return;

			string name = CurrentNodeName().ToLower();
			FixNestedTag(name, GetResetters(name));
		}
		
		private void FixNestedTag(string name, string[] resetters)
		{
			if (resetters == null)
				return;

			HtmlNode prev;
					
			// if we find a previous unclosed same name node, without a resetter node between, we must close it
			prev = (HtmlNode)_lastnodes[name];
			if ((prev != null) && (!prev.Closed))
			{

				// try to find a resetter node, if found, we do nothing
				if (FindResetterNodes(prev, resetters))
				{
					return;
				}

				// ok we need to close the prev now
				// create a fake closer node
				HtmlNode close = new HtmlNode(prev.NodeType, this, -1);
				close._endnode = close;
				prev.CloseNode(close);

			}
		}

		private bool FindResetterNodes(HtmlNode node, string[] names)
		{
			if (names == null)
			{
				return false;
			}
			for(int i=0;i<names.Length;i++)
			{
				if (FindResetterNode(node, names[i]) != null)
				{
					return true;
				}
			}
			return false;
		}

		private HtmlNode FindResetterNode(HtmlNode node, string name)
		{
			HtmlNode resetter = (HtmlNode)_lastnodes[name];
			if (resetter == null)
				return null;
			if (resetter.Closed)
			{
				return null;
			}
			if (resetter._streamposition<node._streamposition)
			{
				return null;
			}
			return resetter;
		}

		private void PushNodeNameEnd(int index)
		{
			_currentnode._namelength = index - _currentnode._namestartindex;
			if (OptionFixNestedTags)
			{
				FixNestedTags();
			}
		}

		private void CloseCurrentNode()
		{
			if (_currentnode.Closed) // text or document are by def closed
				return;

			bool error = false;

			// find last node of this kind
			HtmlNode prev = (HtmlNode)_lastnodes[_currentnode.Name];
			if (prev == null)
			{
				if (HtmlNode.IsClosedElement(_currentnode.Name))
				{
					// </br> will be seen as <br>
					_currentnode.CloseNode(_currentnode);

					// add to parent node
					if (_lastparentnode != null)
					{
						HtmlNode foundNode = null;
						Stack futureChild = new Stack();
						for (HtmlNode node = _lastparentnode.LastChild; node != null; node = node.PreviousSibling)
						{
							if ((node.Name == _currentnode.Name) && (! node.HasChildNodes))
							{
								foundNode = node;
								break;
							}
							futureChild.Push(node);
						}
						if (foundNode != null)
						{
							HtmlNode node = null;
							while(futureChild.Count != 0) 
							{
								node = (HtmlNode)futureChild.Pop();
								_lastparentnode.RemoveChild(node);
								foundNode.AppendChild(node);
							}
						}
						else
						{
							_lastparentnode.AppendChild(_currentnode);
						}

					}
				}
				else
				{
					// node has no parent
					// node is not a closed node

					if (HtmlNode.CanOverlapElement(_currentnode.Name))
					{
						// this is a hack: add it as a text node
						HtmlNode closenode = CreateNode(HtmlNodeType.Text, _currentnode._outerstartindex);
						closenode._outerlength = _currentnode._outerlength;
						((HtmlTextNode)closenode).Text = ((HtmlTextNode)closenode).Text.ToLower();
						if (_lastparentnode != null)
						{
							_lastparentnode.AppendChild(closenode);
						}

					}
					else
					{
						if (HtmlNode.IsEmptyElement(_currentnode.Name))
						{
							AddError(
								HtmlParseErrorCode.EndTagNotRequired,
								_currentnode._line, _currentnode._lineposition,
								_currentnode._streamposition, _currentnode.OuterHtml,
								"End tag </" + _currentnode.Name + "> is not required");
						}
						else
						{
							// node cannot overlap, node is not empty
							AddError(
								HtmlParseErrorCode.TagNotOpened,
								_currentnode._line, _currentnode._lineposition,
								_currentnode._streamposition, _currentnode.OuterHtml,
								"Start tag <" + _currentnode.Name + "> was not found");
							error = true;
						}
					}
				}
			}
			else
			{
				if (OptionFixNestedTags)
				{
					if (FindResetterNodes(prev, GetResetters(_currentnode.Name)))
					{
						AddError(
							HtmlParseErrorCode.EndTagInvalidHere,
							_currentnode._line, _currentnode._lineposition,
							_currentnode._streamposition, _currentnode.OuterHtml,
							"End tag </" + _currentnode.Name + "> invalid here");
						error = true;					
					}
				}

				if (!error)
				{
					_lastnodes[_currentnode.Name] = prev._prevwithsamename;
					prev.CloseNode(_currentnode);
				}
			}


			// we close this node, get grandparent
			if (!error)
			{
				if ((_lastparentnode != null) &&
					((!HtmlNode.IsClosedElement(_currentnode.Name)) || 
					(_currentnode._starttag)))
				{
					UpdateLastParentNode();
				}
			}
		}

		internal void UpdateLastParentNode()
		{
			do
			{
				if (_lastparentnode.Closed)
				{
					_lastparentnode = _lastparentnode.ParentNode;
				}
			}
			while ((_lastparentnode != null) && (_lastparentnode.Closed));
			if (_lastparentnode == null)
			{
				_lastparentnode = _documentnode;
			}
		}
		
		private string CurrentAttributeName()
		{
			return _text.Substring(_currentattribute._namestartindex, _currentattribute._namelength);
		}

		private string CurrentAttributeValue()
		{
			return _text.Substring(_currentattribute._valuestartindex, _currentattribute._valuelength);
		}

		private string CurrentNodeName()
		{
			return _text.Substring(_currentnode._namestartindex, _currentnode._namelength);
		}

		private string CurrentNodeOuter()
		{
			return _text.Substring(_currentnode._outerstartindex, _currentnode._outerlength);
		}

		private string CurrentNodeInner()
		{
			return _text.Substring(_currentnode._innerstartindex, _currentnode._innerlength);
		}

		/// <summary>
		/// Determines if the specified character is considered as a whitespace character.
		/// </summary>
		/// <param name="c">The character to check.</param>
		/// <returns>true if if the specified character is considered as a whitespace character.</returns>
		public static bool IsWhiteSpace(int c)
		{
			if ((c == 10) || (c == 13) || (c == 32) || (c == 9))
			{
				return true;
			}
			return false;
		}

	}

	internal class EncodingFoundException: Exception
	{
		private Encoding _encoding;

		internal EncodingFoundException(Encoding encoding)
		{
			_encoding = encoding;
		}

		internal Encoding Encoding
		{
			get
			{
				return _encoding;
			}
		}
	}
}
