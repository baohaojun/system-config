//
// FilterRTF.cs
//
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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

using Beagle.Util;
using Beagle.Daemon;
using System.Windows.Forms.RTF;

namespace Beagle.Filters {

	public class FilterRTF : Beagle.Daemon.Filter {
		
		int		skip_width;
		int		skip_count;

		static TextMap text_map = null;

		StringBuilder sb;

		Stack<bool> group_stack = null;
		bool current_is_hot; // Whether current group is hot
		bool reading_properties; // Set to true in DoPullProperties
		RTF rtf;
		int info_group = -1; // Stores the depth of groups in {\info ...}
		bool debug = false;

		/////////////////////////////////////////////////////////////////////

		public FilterRTF ()
		{
			SnippetMode = true;
			// 1: Modified filter based on System.Windows.Forms.RTF
			SetVersion (1);
			SetFileType ("document");
		}

		protected override void RegisterSupportedTypes ()
		{
			AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/rtf"));
		}

		void Init ()
		{
			rtf = new RTF (Stream);

			if (text_map == null) {
				text_map = new TextMap ();
				TextMap.SetupStandardTable(text_map.Table);
			}

			sb = new StringBuilder ();
			sb.Length = 0;
			group_stack = new Stack<bool> ();
			current_is_hot = false;

			skip_width = 0;
			skip_count = 0;
		}

		override protected void DoPullProperties ()
		{
			Init ();

			reading_properties = true;

			rtf.ClassCallback [TokenClass.Control] = new ClassDelegate (HandleControl);
			rtf.ClassCallback [TokenClass.Group] = new ClassDelegate (HandleGroup);

			rtf.DestinationCallback [Minor.FontTbl] = new DestinationDelegate (SkipGroup);
			rtf.DestinationCallback [Minor.ColorTbl] = new DestinationDelegate (SkipGroup);
			rtf.DestinationCallback [Minor.StyleSheet] = new DestinationDelegate (SkipGroup);
			rtf.DestinationCallback [Minor.Info] = null;
			rtf.DestinationCallback [Minor.Pict] = new DestinationDelegate (SkipGroup);
			rtf.DestinationCallback [Minor.Object] = new DestinationDelegate (SkipGroup);

			while (rtf.GetToken() != TokenClass.EOF) {
				rtf.RouteToken();
				if (info_group == 0)
					break;
			}
		}
		
		override protected void DoPull ()
		{
			rtf.ClassCallback [TokenClass.Text] = new ClassDelegate (HandleText);
			rtf.DestinationCallback [Minor.Info] = new DestinationDelegate (SkipGroup);

			reading_properties = false;
			sb.Length = 0;
			current_is_hot = false;

			rtf.Read();
			FlushText ();

			Finished ();
		}
		
		override protected void DoClose ()
		{
			sb.Length = 0;
			sb = null;

			group_stack.Clear ();
			group_stack = null;

			rtf = null;
		}

		/////////////////////////////////////////////////////////////////////

		void HandleText(RTF rtf) {
			if (skip_count > 0) {
				skip_count--;
				return;
			}
			if ((StandardCharCode)rtf.Minor != StandardCharCode.nothing) {
				if (debug)
					Console.Write("{0}", text_map [(StandardCharCode) rtf.Minor]);
				AppendTextSmartly (text_map [(StandardCharCode) rtf.Minor]);
			} else {
				if ((int)rtf.Major > 31 && (int)rtf.Major < 128) {
					if (debug)
						Console.Write("[{0}]", (char)rtf.Major);
					AppendTextSmartly ((char)rtf.Major);
				}
			}
		}

		void HandleControl(RTF rtf) {
			switch(rtf.Major) {
				case Major.Unicode: {
					switch(rtf.Minor) {
						case Minor.UnicodeCharBytes: {
							skip_width = rtf.Param;
							break;
						}

						case Minor.UnicodeChar: {
							if (debug)
								Console.Write("[Unicode {0:X4}]", rtf.Param);
							skip_count += skip_width;
							break;
						}
					}
					break;
				}

				case Major.Destination: {
					HandleInfo (rtf);
					break;
				}

				case Major.CharAttr: {
					switch(rtf.Minor) {

					case Minor.Plain:
						if (debug)
							Console.Write("[Normal]");
						GroupIsNotHot ();
						break;

					case Minor.Bold:
						if (rtf.Param == RTF.NoParam) {
							if (debug)
								Console.Write("[Bold]");
							GroupIsHot ();
						} else {
							if (debug)
								Console.Write("[NoBold]");
							GroupIsNotHot ();
						}
						break;

					case Minor.Italic:
						if (rtf.Param == RTF.NoParam) {
							if (debug)
								Console.Write("[Italic]");
							GroupIsHot ();
						} else {
							if (debug)
								Console.Write("[NoItalic]");
							GroupIsNotHot ();
						}
						break;

					case Minor.Underline: {
						if (rtf.Param == RTF.NoParam) {
							if (debug)
								Console.Write("[Underline]");
							GroupIsHot ();
						} else {
							if (debug)
								Console.Write("[NoUnderline]");
							GroupIsNotHot ();
						}
						break;
					}

					default:
						break;
					}
					break;
				}

				case Major.SpecialChar: {
					SpecialChar(rtf);
					break;
				}
			}
		}

		void HandleInfo (RTF rtf)
		{
			switch (rtf.Minor) {

			case Minor.HeaderLeft:
				SkipGroup (rtf);
				break;
			case Minor.HeaderRight:
				SkipGroup (rtf);
				break;
			case Minor.HeaderFirst:
				SkipGroup (rtf);
				break;
			case Minor.FooterLeft:
				SkipGroup (rtf);
				break;
			case Minor.FooterRight:
				SkipGroup (rtf);
				break;
			case Minor.FooterFirst:
				SkipGroup (rtf);
				break;
			case Minor.FieldInst:
				SkipGroup (rtf);
				break;
			case Minor.Info:
				if (debug)
					Console.WriteLine ("[Info started]");
				info_group = 1;
				break;
			case Minor.ITitle:
				AddRTFInfo (rtf, "dc:title", false);
				break;
			case Minor.ISubject:
				AddRTFInfo (rtf, "dc:subject", false);
				break;
			case Minor.IAuthor:
				AddRTFInfo (rtf, "dc:author", false);
				break;
			case Minor.IOperator:
				AddRTFInfo (rtf, "dc:operator", false);
				break;
			case Minor.IKeywords:
				AddRTFInfo (rtf, "dc:keywords", true);
				break;
			case Minor.IComment:
				AddRTFInfo (rtf, "dc:comment", false);
				break;
			case Minor.IVerscomm:
				SkipGroup(rtf);
				break;
			case Minor.IDoccomm:
				SkipGroup (rtf);
				break;
			}
		}

		void SpecialChar(RTF rtf) {
			switch(rtf.Minor) {
				case Minor.Page:
				case Minor.Sect:
				case Minor.Row:
				case Minor.Line:
				case Minor.Par:
					//Console.Write("\n");
					FlushText ();
					AppendStructuralBreak ();
					break;

				case Minor.Cell:
					//Console.Write(" ");
					AppendWhiteSpace ();
					break;

				case Minor.NoBrkSpace:
					//Console.Write(" ");
					AppendWhiteSpace ();
					break;

				case Minor.Tab:
					//Console.Write("\t");
					AppendWhiteSpace ();
					break;

				case Minor.NoBrkHyphen:
					//Console.Write("-");
					AppendTextSmartly ('-');
					break;

				case Minor.Bullet:
					//Console.Write("*");
					AppendTextSmartly ('*');
					break;

				default:
					SkipGroup (rtf);
					break;
			}
		}

		void HandleGroup (RTF rtf)
		{
			switch (rtf.Major) {

			case Major.BeginGroup:
				if (reading_properties)
					info_group = (info_group > 0 ? info_group + 1 : info_group);
				if (debug)
					Console.WriteLine ("[new group] {0}", info_group);
				group_stack.Push (current_is_hot);
				current_is_hot = false;
				break;

			case Major.EndGroup:
				FlushText ();

				if (current_is_hot) {
					if (debug)
						Console.WriteLine ("[end hot]");
					HotDown ();
				}

				try {
					current_is_hot = group_stack.Pop ();
				} catch {
					current_is_hot = false;
				}
				if (reading_properties)
					info_group = (info_group > 0 ? info_group - 1 : info_group);
				if (debug)
					Console.WriteLine ("[end new group] {0}", info_group);
				break;
			}
		}

		void SkipGroup (RTF rtf)
		{
			rtf.SkipGroup ();
			rtf.RouteToken ();
		}

		////////////////////////////////////////////////////////////////////////////

		void AddRTFInfo (RTF rtf, string prop_name, bool is_keyword)
		{
			if (! reading_properties)
				return;

			FlushText ();
			rtf.GetToken ();
			while (rtf.TokenClass == TokenClass.Text) {
				if (debug)
					Console.Write ("{0}", text_map[(StandardCharCode)rtf.Minor]);
				sb.Append (text_map[(StandardCharCode)rtf.Minor]);
				rtf.GetToken ();
			}
			rtf.UngetToken();

			if (! is_keyword)
				AddProperty (Beagle.Property.New (prop_name, sb.ToString ()));
			else {
				foreach (string s in sb.ToString ().Split (' '))
					AddProperty (Beagle.Property.NewKeyword (prop_name, s));
			}

			sb.Length = 0;
		}

		void GroupIsHot ()
		{
			if (current_is_hot)
				return;

			if (debug)
				Console.WriteLine ("[begin hot]");
			current_is_hot = true;
			HotUp ();
		}

		void GroupIsNotHot ()
		{
			if (! current_is_hot)
				return;

			if (debug)
				Console.WriteLine ("[end hot]");
			current_is_hot = false;
			HotDown ();
		}

		void AppendTextSmartly (char c)
		{
			sb.Append (c);
		}

		void AppendTextSmartly (string s)
		{
			if (s.Length == 1) {
				AppendTextSmartly (s [0]);
				return;
			}

			FlushText ();
			if (debug)
				Console.Write ("[{0}]", s);
			AppendText (s);
		}

		void FlushText ()
		{
			if (sb.Length == 0)
				return;

			AppendText (sb.ToString ());
			if (debug)
				Console.Write ("[{0}]", sb.ToString ());
			sb.Length = 0;
		}
	}
}
