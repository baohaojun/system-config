//
// FilterSource.cs
//
// Copyright (C) 2004, 2005 Novell, Inc.
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
using System.Collections.Generic;
using System.IO;
using System.Text;
namespace Beagle.Filters {

	public abstract class FilterSource : Beagle.Daemon.Filter {

		protected enum LangType {
			None,
			C_Style,
			C_Sharp_Style,
			Python_Style,
			Fortran_Style,
			Pascal_Style,
			Lisp_Style,
			Matlab_Style,
			Shell_Style
		};
		
		protected LangType SrcLangType;

		private enum LineType {
			None,
			SingleLineComment,
			BlockComment,
			StringConstant
		};
		
		LineType SrcLineType;
		string StrConstIdentifier;
		StringBuilder token;

		private int version = 0;

		public FilterSource ()
		{
			// Initialize the linetype member.
			SrcLineType = LineType.None;
			SrcLangType = LangType.None;
			StrConstIdentifier = " ";

			SnippetMode = true;
			OriginalIsText = true;
			token = new StringBuilder ();
			SetFileType ("source");
		}

		protected new void SetVersion (int version)
		{
			this.version += version;
			base.SetVersion (version);
		}

		protected abstract Dictionary<string, bool> KeyWordsHash {
			get;
		}

		override protected void DoPull ()
		{
			string str = TextReader.ReadLine ();
			if (str == null)
				Finished ();
			else
				ExtractTokens (str);
		}

		protected override void DoClose ()
		{
			token.Length = 0;
			token = null;
		}

		// Validate the character and append it to the token,
		// that will be added to the text-pool of the filter.
		// Returns: False, if token is not complete enough to be added to the pool.
		//          True, if it is a valid word that can be added to the pool.
		private bool AppendToToken (char ch, int index, int length)
		{
			if (ch == ' ')
				return true;

			if (Char.IsLetter (ch) || Char.IsDigit (ch) || ch == '_') {
				token.Append (ch);
				if ((index + 1) < length) 
					return false;
				else
					return true;
			}

			return true;
		}

		// Tokenize the passed string and add the relevant 
		// tokens for indexing.
		//
		// FIXME: Perl has embedded "POD" (documentation) style, which needs a little processing.
		//
 		protected void ExtractTokens (string str)
		{
			int index;
			token.Length = 0;
			string splCharSeq = String.Empty;

			for (index = 0; index < str.Length; index++) {
		         	if (((str[index] == '{' 
				      || str[index] == '}' 
				      || str[index] == '(' 
				      || str[index] == ')'
				      || str[index] == '*'
				      || str[index] == '/')
				     && SrcLangType == LangType.Pascal_Style)
				    || ((str[index] == '/' 
					 || str[index] == '*') 
					&& (SrcLangType == LangType.C_Style 
					    || SrcLangType == LangType.C_Sharp_Style))) {		
					
					splCharSeq += str[index];
					
					switch (splCharSeq) {

                                        case "(*":
                                                if (SrcLineType == LineType.None) {
							SrcLineType = LineType.BlockComment;
							token.Length = 0;
						    } else 
							token.Append (splCharSeq);
						splCharSeq = String.Empty; 
                                                break;

					case "*)":
						if (SrcLineType == LineType.BlockComment) {
							SrcLineType = LineType.None;
							token.Append (" ");
							AppendText (token.ToString());
							token.Length = 0;
						} else if (SrcLineType != LineType.None)
							token.Append (splCharSeq);
						splCharSeq = String.Empty;
						break;          
                           
                                        case "{":
                                                if (SrcLineType == LineType.None) {
							SrcLineType = LineType.BlockComment;
							token.Length = 0;
						    } else 
							token.Append (splCharSeq);
						splCharSeq = String.Empty; 
                                                break;

					case "}":
						if (SrcLineType == LineType.BlockComment) {
							SrcLineType = LineType.None;
							token.Append (" ");
							AppendText (token.ToString());
							token.Length = 0;
						} else if (SrcLineType != LineType.None)
							token.Append (splCharSeq);
						splCharSeq = String.Empty;
						break;
					case "//":
						if (SrcLineType == LineType.None) {
							SrcLineType = LineType.SingleLineComment;
							token.Length = 0;
						} else
							token.Append (splCharSeq);
						splCharSeq = String.Empty;
						break;
						
					case "/*":
						if (SrcLineType == LineType.None) {
							SrcLineType = LineType.BlockComment;
							token.Length = 0;
						} else 
							token.Append (splCharSeq);
						splCharSeq = String.Empty;
						break;
							
					case "*/":
						if (SrcLineType == LineType.BlockComment) {
							SrcLineType = LineType.None;
							token.Append (" ");
							AppendText (token.ToString());
							token.Length = 0;
						} else if (SrcLineType != LineType.None)
							token.Append (splCharSeq);
						splCharSeq = String.Empty;
						break;
					}
				} else if ((str[index] == '#' && (SrcLangType == LangType.Python_Style ||
								  SrcLangType == LangType.Shell_Style)) ||
					   (str[index] == '!' && SrcLangType == LangType.Fortran_Style) ||
					   (str[index] == ';' && SrcLangType == LangType.Lisp_Style) ||
					   (str[index] == '%' && SrcLangType == LangType.Matlab_Style)) {
					if (SrcLineType == LineType.None) {
						SrcLineType = LineType.SingleLineComment;
						token.Length = 0;
					} else
						token.Append (str[index]);
				}
				// FIXME: we evaluate *ALL* escape 
				// sequences on strings.  Do we really need to 
				// do this for comments??? And also "\n", "\t" etc????
				else if (SrcLineType == LineType.StringConstant && 
					 str[index] == '\\') {
					if ((index + 1) <= (str.Length-1))
						token.Append (str[index + 1]);
					index ++; 
				}
				// Well the typical python ''' or """ stuff 
				else if ((SrcLangType == LangType.Python_Style) &&
					 ((index + 2) <= (str.Length-1)) && 
					 (str[index] == '\"' || str[index] == '\'') &&
					 (str[index] == str[index + 1] && str[index] == str[index + 2]) &&
					 StrConstIdentifier[0] == str[index]) {

					if (SrcLineType == LineType.StringConstant) {
						SrcLineType = LineType.None;
						token.Append (" ");
						AppendText (token.ToString());
						token.Length = 0;
					} else {
						StrConstIdentifier = str.Substring (index, 3);
						SrcLineType = LineType.StringConstant;
						token.Length = 0;
						index += 2;
					}
					       
					splCharSeq = String.Empty;
				}
				// Lisp: ignore the single quote character; do another iteration
				else if (SrcLangType == LangType.Lisp_Style && str[index] == '\'') {
					continue;
				}
				else if (str[index] == '\"' || str[index] == '\'' ||
					 (str[index] == '`' && SrcLangType == LangType.Shell_Style)) {

					if (SrcLineType == LineType.StringConstant &&
					    StrConstIdentifier.Length == 1 &&
					    StrConstIdentifier[0] == str[index]) {
						SrcLineType = LineType.None;
						token.Append (" ");
						AppendText (token.ToString());
						token.Length = 0;

					} else if (SrcLineType == LineType.None) {
						StrConstIdentifier = str.Substring (index, 1);
						SrcLineType = LineType.StringConstant;
						token.Length = 0;
					} else
						token.Append (str[index]);
					splCharSeq = String.Empty;

				} else if (SrcLineType != LineType.None) {
					token.Append (splCharSeq);
					token.Append (str[index]);
					splCharSeq = String.Empty;

				} else if (SrcLineType == LineType.None) { 
					if (AppendToToken (str[index], index, str.Length)) {			       
						if (SrcLangType == LangType.Lisp_Style) {

							// Lisp identifiers: letters, digits, and:
							// ! $ % & * + - . / : < = > ? @ ^ _ ~
							switch (str[index]) {
							case '!': case '$': case '%': case '&':
							case '*': case '+': case '-': case '.':
							case '/': case ':': case '<': case '=':
							case '>': case '?': case '@': case '^':
							case '_': case '~':
								token.Append (str[index]);
								continue;
							}
						}
						//token = token.Replace(" ", String.Empty);
						if (token.Length > 0) {
							string tok;
							if (SrcLangType == LangType.Fortran_Style)
								tok = token.ToString().ToLower();
							else
								tok = token.ToString ();
							if (! KeyWordsHash.ContainsKey (tok)) {
								if (!Char.IsDigit (token[0])) {
									AppendText (tok);
									AppendWhiteSpace ();
								}
							}
						}
						// reset the token
						token.Length = 0;
					}
					splCharSeq = String.Empty;
				}
		        }
			if (SrcLineType != LineType.None) {
				bool trailing_backslash = false;

				token.Append (splCharSeq);

				if (token.Length > 0 && token [token.Length - 1] == '\\') {
					token = token.Remove (token.Length-1, 1);
					trailing_backslash = true;
				}
			       
				token.Append (" ");
				AppendText (token.ToString());
				
				// if a single-line-comment ends with a "\", 
				// the lines that follows it are also considered as a comment,
				// till a line with out a "\" is found
				// C# and Lisp don't follow this syntax.
				if (SrcLineType == LineType.SingleLineComment) 
					if (!trailing_backslash 
					    || SrcLangType == LangType.C_Sharp_Style
					    || SrcLangType == LangType.Lisp_Style)
						SrcLineType = LineType.None;
			} else if (token.Length > 0 
				   && !Char.IsDigit (token[0])) { 
				/* we don't want any numeric const */
				token.Append (" ");
				AppendText (token.ToString());
			}
		}
	}
}
