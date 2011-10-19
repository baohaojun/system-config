//
// FilterTeX.cs : Basic LaTeX/Lambda filter, adapted from an old version of FilterRTF.cs.
//
// Copyright (C) 2004 Novell, Inc.
// COpyright (C) 2007 auxsvr@gmail.com
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

/*
   Input files are in general supposed to be syntactically perfect (i.e. LaTeX does parse them succesfully).
   In certain cases syntax checks are performed, though they aren't thorough. In particular, begin/end block mismatch can
   cause the filter to crash (stack exception). At the moment it doesn't have any special support for plain TeX files (it
   will index them as text).
*/

using System;
using System.Collections;
using System.IO;
using System.Text;

using Beagle.Util;
using Beagle.Daemon;
using Beagle.Filters;

namespace Beagle.Filters {

	public class FilterTeX : Beagle.Daemon.Filter {

		private class TeXControlWordType {

			public enum Type {
				None,
				Comment,
				Skip, // Uninmportant symbols for the parser
				MetaDataTag,
				Document,
				SplSection,
				EscSeq,
				BraceStart,
				BraceEnd,
				Hot, //For hot text
				MathMode,
				BlockBegin,
				BlockEnd,
				Bibitem
			};

			public Type Types;
			public string ctrlWord;

			TeXControlWordType (Type types, string ctrlword)
			{
				this.Types = types;
				this.ctrlWord = ctrlword;
			}

			static TeXControlWordType[] types =
			{
				new TeXControlWordType (Type.None, ""),
				new TeXControlWordType (Type.MetaDataTag, "abstract"),
				new TeXControlWordType (Type.MetaDataTag, "author"),
				new TeXControlWordType (Type.MetaDataTag, "title"),
				new TeXControlWordType (Type.MetaDataTag, "email"),
				new TeXControlWordType (Type.Document, "document"),
				new TeXControlWordType (Type.MathMode, "displaymath"),
				new TeXControlWordType (Type.MathMode, "equation"),
				new TeXControlWordType (Type.MathMode, "gather"),
				new TeXControlWordType (Type.Hot, "bf"),
				new TeXControlWordType (Type.Hot, "emph"),
				new TeXControlWordType (Type.Hot, "section"),
				new TeXControlWordType (Type.Hot, "subsection"),
				new TeXControlWordType (Type.Hot, "subsubsection"),
				new TeXControlWordType (Type.Hot, "chapter"),
				new TeXControlWordType (Type.Hot, "caption"),
				new TeXControlWordType (Type.BlockBegin, "begin"),
				new TeXControlWordType (Type.BlockEnd, "end"),
				new TeXControlWordType (Type.Comment,"%"),
				new TeXControlWordType (Type.Skip, "\\"),
				new TeXControlWordType (Type.BraceStart,"{"),
				new TeXControlWordType (Type.BraceEnd, "}"),
				new TeXControlWordType (Type.EscSeq, "&"),
				new TeXControlWordType (Type.EscSeq, "|"),
				new TeXControlWordType (Type.Hot, "thebibliography"),
				new TeXControlWordType (Type.Bibitem, "bibitem")
			};

			public static TeXControlWordType Find (string str_ctrl_word)
			{
				for (int i = 0; i < types.Length; i++) {
					if (String.Compare (types [i].ctrlWord, str_ctrl_word) == 0)
						return types [i];
				}
				return types [0];
			}

			public static TeXControlWordType Find (char c)
			{
				for (int i = 0; i < types.Length; i++) {
					if (types [i].ctrlWord.Length == 1 && types [i].ctrlWord [0] == c)
						return types [i];
				}
				return types [0];
			}
		}

		public enum Position {
			None,
			InMetaData,
			InBody,
			InPara,
			InMathMode,
			InHot
		};

		public enum ErrorCodes {
			ERROR_TEX_OK,
			ERROR_TEX_EOF,
			ERROR_TEX_UNHANDLED_SYMBOL,
			ERROR_TEX_IMPOSSIBLE
		};

		private Stack env_data_stack;
		StringBuilder str_block_sb = new StringBuilder ();

		public FilterTeX ()
		{
			SnippetMode = true;
			// No need to store snippets since the raw data (with all its markup)
			// is better than the extracted data for showing context.
			// This also means, there is no need to AppendStructuralBreak! Yay.
			OriginalIsText = true;
			SetFileType ("document");

			env_data_stack = new Stack ();

		}

		override protected void RegisterSupportedTypes()
		{
//			AddSupportedFlavor(FilterFlavor.NewFromMimeType ("text/x-tex"));
//			AddSupportedFlavor(FilterFlavor.NewFromMimeType ("text/x-latex"));

			// These two are in my system mimetypes; add them just in case.
			AddSupportedFlavor(FilterFlavor.NewFromMimeType ("application/x-tex"));
			AddSupportedFlavor(FilterFlavor.NewFromMimeType ("application/x-latex"));	
		}

		private string ExtractArgument ()
		{
			//Extracts text delimited by BraceStart and BraceEnd
			//First char must be a BraceStart
			int aByte = -1;
			char ch;
			StringBuilder str_ctrl_word = new StringBuilder ();
			int counter = 1;
			
			aByte = TextReader.Read();
			if (aByte == -1)
				return String.Empty;

			ch = (char) aByte;
			TeXControlWordType ctrl_word_type = TeXControlWordType.Find (ch);
			if (ctrl_word_type.Types != TeXControlWordType.Type.BraceStart)
				return String.Empty;

			while (aByte != -1 && counter != 0) {
				aByte = TextReader.Read ();
				ch = (char) aByte;
				ctrl_word_type = TeXControlWordType.Find (ch);

				if (ctrl_word_type.Types == TeXControlWordType.Type.BraceStart)
					counter++;
				else if (ctrl_word_type.Types == TeXControlWordType.Type.BraceEnd) 
					counter--;
				// Comment character -> skip to the end of line
				else if (ctrl_word_type.Types == TeXControlWordType.Type.Comment) 
					TextReader.ReadLine ();
				else if (counter > 0)
					str_ctrl_word.Append (ch);
			}

			return str_ctrl_word.ToString();
		}

		private ErrorCodes HandleControlWord (string str_ctrl_word)
		{
			string str_temp;
			TeXControlWordType new_ctrl_word_type;
			TeXControlWordType ctrl_word_type = TeXControlWordType.Find (str_ctrl_word.ToString());

			switch (ctrl_word_type.Types) {
			case TeXControlWordType.Type.MetaDataTag:
				if (env_data_stack.Count > 0) {
					str_temp = (string) env_data_stack.Peek();
					new_ctrl_word_type = TeXControlWordType.Find (str_temp);

					switch (new_ctrl_word_type.Types) {

					case TeXControlWordType.Type.BlockBegin:
						AppendWord (str_block_sb.ToString());
						str_block_sb.Length = 0;
						break;

					case TeXControlWordType.Type.BlockEnd:
						str_temp = str_block_sb.ToString().Trim ();
						AddProperty (Beagle.Property.New ("dc:" + str_ctrl_word, str_temp));
						str_block_sb.Length = 0;

						/*Assuming everything is fine (closed begin-end pairs), discard 2
						objects (current environment, begin) */
						for (int i = 0; i < 2; i++) {
							env_data_stack.Pop();
						}
						break;

					default:
						str_temp = ExtractArgument().Trim();
						AddProperty (Beagle.Property.New ( "dc:" + str_ctrl_word, str_temp));

						break;
					}
				} else {
					str_temp = ExtractArgument().Trim();
					AddProperty (Beagle.Property.New ("dc:" + str_ctrl_word, str_temp));
				}
				break;

			case TeXControlWordType.Type.EscSeq:
				break;

			case TeXControlWordType.Type.BlockBegin:
				env_data_stack.Push (str_ctrl_word);
				HandleControlWord (ExtractArgument());
				AppendWord (str_block_sb.ToString());
				str_block_sb.Length = 0;
				break;

			case TeXControlWordType.Type.BlockEnd:
				env_data_stack.Push (str_ctrl_word);
				HandleControlWord (ExtractArgument());
				break;

			case TeXControlWordType.Type.Document:
				if (env_data_stack.Count > 0) {
					str_temp = (string) env_data_stack.Peek();
					new_ctrl_word_type = TeXControlWordType.Find (str_temp);

					switch (new_ctrl_word_type.Types) {
					case TeXControlWordType.Type.BlockBegin:
						//TeX files have no document environment, so the following
						//doesn't index them at all.
						str_block_sb.Length = 0;
						break;
					case TeXControlWordType.Type.BlockEnd:
						AppendWord (str_block_sb.ToString());
						str_block_sb.Length = 0;
						break;
					default:
						Log.Error ("ERROR: document is a block environment");
						break;
					}
				} else {
					Log.Error ("ERROR: document is a block environment");
				}
				env_data_stack.Push (str_ctrl_word);
				break;

			case TeXControlWordType.Type.Hot:
				//There needn't exist an enclosing environment (TeX)
				if (env_data_stack.Count > 0) {
					str_temp = (string) env_data_stack.Peek();
					new_ctrl_word_type = TeXControlWordType.Find (str_temp);
					switch(new_ctrl_word_type.Types) {
					case TeXControlWordType.Type.BlockBegin:
						AppendWord (str_block_sb.ToString());
						str_block_sb.Length = 0;

						break;
					case TeXControlWordType.Type.BlockEnd:
						HotUp ();
						AppendWord (str_block_sb.ToString());
						str_block_sb.Length = 0;
						HotDown ();
						
						break;
					default:
						HotUp ();
						//Console.WriteLine("content: {0}, {1}", ExtractArgument(), str_temp);
						AppendWord (ExtractArgument());
						HotDown ();
						
						break;
					}
				} else {
					HotUp ();
					//Console.WriteLine("content: {0}, {1}", ExtractArgument(), str_temp);
					AppendWord (ExtractArgument());
					HotDown ();
				}
				break;

			case TeXControlWordType.Type.Bibitem:
				ExtractArgument();
				break;

			case TeXControlWordType.Type.None:
				if (env_data_stack.Count > 0) {
					new_ctrl_word_type = TeXControlWordType.Find ((string) env_data_stack.Peek());
					if (new_ctrl_word_type.Types == TeXControlWordType.Type.BlockBegin ||
				  	new_ctrl_word_type.Types == TeXControlWordType.Type.BlockEnd) {
						env_data_stack.Pop();
					}
					if (new_ctrl_word_type.Types == TeXControlWordType.Type.MathMode) {
						str_block_sb.Append ("\\" + str_ctrl_word + ' ');
					}
				}
				//Check unknown macros
				//Console.WriteLine ("Unimplemented macro: {0}", str_ctrl_word.ToString());
				break;
			}
			return ErrorCodes.ERROR_TEX_OK;
		}
		
		private ErrorCodes ProcessControlWords ()
		{
			int aByte = -1;
			char ch;
			StringBuilder str_ctrl_word = new StringBuilder ();
			
			aByte = TextReader.Read();
			if (aByte == -1)
				return ErrorCodes.ERROR_TEX_EOF;
			ch = (char) aByte;
			//Just in order to identify symbols I may have forgottten
			/*TeXControlWordType ctrl_word_type = TeXControlWordType.Find (new String (ch, 1));
			
			if (!Char.IsLetter (ch) && ctrl_word_type.Types != TeXControlWordType.Type.EscSeq &&
			  ctrl_word_type.Types != TeXControlWordType.Type.Skip) {
				Logger.Log.Error ("Unhandled symbol: {0}, {1}", ch, ctrl_word_type.Types);
				return ErrorCodes.ERROR_TEX_UNHANDLED_SYMBOL;
			}
			*/
			while (aByte != -1) {
				str_ctrl_word.Append (ch);
				aByte = TextReader.Peek ();
				ch = (char) aByte;
				//Macro name is separated with {, [, \ and spaces.
				if (!Char.IsWhiteSpace(ch) && ch != '{' && ch != '[' && ch != '\\') {
					aByte = TextReader.Read ();
					ch = (char) aByte;
				}
				else
					break;
			}
			
			return HandleControlWord (str_ctrl_word.ToString());
		}

		private ErrorCodes TeXParse ()
		{
			int aByte = -1;
			char ch;
			ErrorCodes ec;

			while ((aByte = TextReader.Read ()) != -1) {
				ch = (char) aByte;
				switch (ch) {
				case '\\': //Process keywords
					ec = ProcessControlWords ();
					if (ec != ErrorCodes.ERROR_TEX_OK)
						return ec;
					break;
				case '\r': //ignore
				case '\n':
					str_block_sb.Append (' ');
					break;
				case '$':
					if (env_data_stack.Count > 0) {
						string str_temp = (string) env_data_stack.Peek();
						if (String.Compare(str_temp, "displaymath") == 0) {	
							env_data_stack.Push ("end");
							env_data_stack.Push ("displaymath");
						} else {
							env_data_stack.Push ("begin");
							env_data_stack.Push ("displaymath");
						}
					} else {
						env_data_stack.Push ("begin");
						env_data_stack.Push ("displaymath");
					}
					//str_block_sb.Append ("$");
					break;
				case '%':
					//Skip to the end of the line
					TextReader.ReadLine ();
					break;
				default:
					str_block_sb.Append (ch);
					break;
				}
			}
			return ErrorCodes.ERROR_TEX_OK;
		}

		override protected void DoPullProperties ()
		{
			ErrorCodes ec;
			ec = TeXParse ();

			if (ec != ErrorCodes.ERROR_TEX_OK)
				Logger.Log.Error ("{0}", ec);
			Finished ();
		}

	}
}	
