//
// ImLogWindow.cs
//
// Lukas Lipka <lukaslipka@gmail.com>
// Raphael  Slinckx <rslinckx@gmail.com>
//
// Copyright (C) 2005 Novell, Inc.
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
using Gtk;
using Glade;
using System.Threading;
using Beagle.Util;
using Mono.Unix;

namespace ImLogViewer {

	public class ImLogWindow {
		[Widget] Window imviewer;
		[Widget] TreeView timelinetree;
		[Widget] Label time_title;
		[Widget] Entry search_entry;
		[Widget] Button search_button;
		[Widget] Button clear_button;
		[Widget] TextView conversation;
		[Widget] ScrolledWindow scrolledwindow;
		
		private string speaking_to;
		private string log_path;
		private string highlight_text;
		private string search_text;

		private TreeStore tree_store;
		private ThreadNotify index_thread_notify;
		private Timeline timeline = new Timeline ();

		private FileInfo initial_select_file;
		private ImLog initial_select;

		private ImClient client;

		public ImLogWindow (ImClient client, string path, string search, string highlight)
		{
			this.client = client;
			
			if (Directory.Exists (path)) {
				log_path = path;
			} else if (File.Exists (path)) {
				log_path = Path.GetDirectoryName (path);
				initial_select_file = new FileInfo (path);
			} else {
				Console.WriteLine ("ERROR: Log path doesn't exist - {0}", path);
				return;
			}

			highlight_text = highlight;
			search_text = search;

			ShowWindow ();
		}

		private void SetStatusTitle (DateTime dt)
		{
			time_title.Markup = String.Format ("<b>{0}</b>", StringFu.DateTimeToPrettyString (dt));
		}
		
		private void SetWindowTitle (string speaker)
		{
			if (String.IsNullOrEmpty (speaker))
				return;

			// Find the buddy
			ImBuddy buddy = null;
			ImBuddyListReader reader = null;

			if (client == ImClient.Pidgin) {
				reader = new PidginBuddyListReader ();
				buddy = reader.Search (speaker);
			} else if (client == ImClient.Kopete) {
				reader = new KopeteBuddyListReader ();
				buddy = reader.Search (speaker);
			} else if (client == ImClient.Konversation) {
				int pos = speaker.IndexOf ('_');

				// speaker of the form irc.gimp.net_#mono-dev
				if (pos == -1)
					imviewer.Title = String.Format (Catalog.GetString ("Conversations in {0}"), speaker);
				else	
					imviewer.Title = String.Format (
						Catalog.GetString ("Conversations in {0} ({1})"),
						speaker.Substring (pos + 1),
						speaker.Substring (0, pos));

				speaking_to = speaker;
				return;
			}
			
			if (speaker.EndsWith (".chat")) {
				imviewer.Title = String.Format (Catalog.GetString ("Conversations in {0}"), speaker.Replace (".chat", String.Empty));
			} else {
				string nick = speaker;

				if (buddy != null && ! String.IsNullOrEmpty (buddy.Alias))
					nick = buddy.Alias;

				imviewer.Title = String.Format (Catalog.GetString ("Conversations with {0}"), nick);
			}

			speaking_to = speaker;
		}

		private void ShowWindow ()
		{
			Application.Init ();
			
			Glade.XML gxml = new Glade.XML (null, "ImLogViewer.glade", "imviewer", null);
			gxml.Autoconnect (this);

			imviewer.Icon = IconTheme.Default.LoadIcon ("system-search", 16, IconLookupFlags.NoSvg);

			conversation.PixelsAboveLines = 3;
			conversation.LeftMargin = 4;
			conversation.RightMargin = 4;

			TextTag boldtag = new TextTag ("bold");
			boldtag.Weight = Pango.Weight.Bold;
			conversation.Buffer.TagTable.Add (boldtag);

			TextTag highlight = new TextTag ("highlight");
			highlight.Background = "yellow";
			conversation.Buffer.TagTable.Add (highlight);

			tree_store = new TreeStore (new Type[] {typeof (string), typeof (string), typeof (object)});

			timelinetree.Model = tree_store;
			timelinetree.AppendColumn ("Date", new CellRendererText(), "markup", 0);
			timelinetree.AppendColumn ("Snippet", new CellRendererText(), "text", 1);
			timelinetree.Selection.Changed += OnConversationSelected; 

			if (highlight_text != null)
				search_entry.Text = highlight_text;

			if (search_text != null)
				Search (search_text);

			search_entry.Activated += OnSearchClicked;
			search_button.Clicked += OnSearchClicked;
			clear_button.Clicked += OnClearClicked;
			imviewer.DeleteEvent += new DeleteEventHandler (OnWindowDelete);

			AccelGroup accel_group = new AccelGroup ();
			GlobalKeybinder global_keys = new GlobalKeybinder (accel_group);
			global_keys.AddAccelerator (OnWindowClose, (uint) Gdk.Key.Escape, 0, Gtk.AccelFlags.Visible);
			imviewer.AddAccelGroup (accel_group);

			// Index the logs
			index_thread_notify = new ThreadNotify (new ReadyEvent (RepopulateTimeline));
			Thread t = new Thread (new ThreadStart (IndexLogs));
			t.Start ();

			Application.Run();
		}

		private void IndexLogs ()
		{
			foreach (string file in Directory.GetFiles (log_path)) {
				ImLog log = null;
				StreamReader reader = new StreamReader (file);
				
				if (client == ImClient.Pidgin)
					log = new PidginLog (new FileInfo (file), reader);
				else if (client == ImClient.Kopete)
					log = new KopeteLog (new FileInfo (file), reader);
				else if (client == ImClient.Konversation)
					log = new KonversationLog (new FileInfo (file));

				reader.Close ();

				if (initial_select_file != null && log.File.FullName == initial_select_file.FullName) {
					initial_select = log;
					initial_select_file = null;
				}
				
				if (speaking_to == null)
					SetWindowTitle (log.SpeakingTo);
				timeline.Add (log, log.StartTime);
			}

			index_thread_notify.WakeupMain ();
		}

		private bool LogContainsString (ImLog log, string text)
 		{
			string [] words = text.Split (null);
			
			//FIXME: This is very crude and EXPENSIVE!
			foreach (string word in words)	{
				bool match = false;

				foreach (ImLog.Utterance utt in log.Utterances)	{
					if (utt.Text.ToLower ().IndexOf (word.ToLower ()) != -1) {
						match = true;
						break;
					}
				}
			
				if (!match) return false;
			}
									
			return true;
		}

		private string GetPreview (ImLog log)
		{
			string preview = null;

			if (log.Utterances.Count == 0)
				return String.Empty;

			foreach (ImLog.Utterance utt in log.Utterances) {
				string snippet = utt.Text;
				int word_count = StringFu.CountWords (snippet, 15);
				if (word_count > 3) {
					preview = snippet;
					break;
				}
			}

			if (preview == null)
				preview = ((ImLog.Utterance) log.Utterances [0]).Text;

			if (preview.Length > 50)
				preview = preview.Substring (0, 50) + "...";

			return preview;
		}

		private void AddCategory (ArrayList list, string name, string date_format)
		{
			if (list.Count == 0)
				return;

			TreeIter parent = TreeIter.Zero;

			foreach (ImLog log in list) {
				if (search_text != null && search_text.Length > 0)
					if (! LogContainsString (log, search_text))
						continue;

				if (parent.Equals(TreeIter.Zero))
					parent = tree_store.AppendValues (String.Format ("<b>{0}</b>", Catalog.GetString (name)), String.Empty, null);

				string date = log.StartTime.ToString (Catalog.GetString (date_format));
				tree_store.AppendValues (parent, date, GetPreview (log), log);
			}
		}

		private void SearchTimeline ()
		{
			// Remove all timeline entries that don't match the search results

			ImLog selected = GetSelectedLog ();

			TreeIter iter;
			if (!tree_store.GetIterFirst (out iter))
				return;

			ArrayList to_remove = new ArrayList ();
			
			do {
				if (tree_store.IterHasChild (iter)) {
					TreeIter child;
					tree_store.IterNthChild (out child, iter, 0);
					
					do {
						ImLog log = tree_store.GetValue (child, 2) as ImLog;
						if (LogContainsString (log, search_text))
							continue;

						to_remove.Add (tree_store.GetPath (child));
						if (log == selected)
							selected = null;
					} while (tree_store.IterNext (ref child));
				}
			} while (tree_store.IterNext (ref iter));

			for (int i = to_remove.Count - 1; i >= 0; i--) {
				if (!tree_store.GetIter (out iter, to_remove [i] as TreePath))
					break;
				tree_store.Remove (ref iter);
			}

			// Remove all the categories that dont have any matches
			tree_store.GetIterFirst (out iter);

			do {
				if (tree_store.IterNChildren (iter) < 1)
					tree_store.Remove (ref iter);
			} while (tree_store.IterNext (ref iter));
			
			ScrollToLog (selected);
			RenderConversation (selected);
		}

		private void RepopulateTimeline ()
		{
			RepopulateTimeline (true, 0);
		}

		private void RepopulateTimeline (bool reset, double vadj)
		{
			ImLog log;

			if (!reset)
				log = GetSelectedLog ();
			else
				log = initial_select;

			tree_store.Clear ();
			AddCategory (timeline.Today, "Today", "HH:mm");
			AddCategory (timeline.Yesterday, "Yesterday", "HH:mm");
			AddCategory (timeline.ThisWeek, "This Week", "dddd");
			AddCategory (timeline.LastWeek, "Last Week", "dddd");
			AddCategory (timeline.ThisMonth, "This Month", "MMM d");
			AddCategory (timeline.ThisYear, "This Year", "MMM d");
			AddCategory (timeline.Older, "Older", "yyy MMM d");
		
			timelinetree.ExpandAll();
			ScrollToLog (log);
			RenderConversation (log);

			if (!reset)
				SetConversationScroll (vadj);
		}

 		private void RenderConversation (ImLog im_log)
 		{
			TextBuffer buffer = conversation.Buffer;
			buffer.Clear ();

 			if (im_log == null) {
				// Find the first (newest) conversation to render
				TreeIter first_parent;
				if (!tree_store.GetIterFirst (out first_parent))
					return;

				TreeIter child;
				if (!tree_store.IterChildren (out child, first_parent))
					return;

				im_log = tree_store.GetValue (child, 2) as ImLog;
			}
 				
 			SetStatusTitle (im_log.StartTime);

			TextTag bold = buffer.TagTable.Lookup ("bold");

			TextIter end = buffer.EndIter;

 			foreach (ImLog.Utterance utt in im_log.Utterances) {
				buffer.InsertWithTags (ref end, utt.Who + ":", new TextTag[] {bold});
				buffer.Insert (ref end, String.Format(" {0}\n", utt.Text));
			}

			if (highlight_text != null)
				HighlightSearchTerms (highlight_text);

			if (search_text != null && search_text.Length > 0)
				HighlightSearchTerms (search_text);
		}

		private void HighlightSearchTerms (string highlight)
		{
			TextBuffer buffer = conversation.Buffer;
			string text = buffer.GetText (buffer.StartIter, buffer.EndIter, false).ToLower ();
			string [] words = highlight.Split (' ');
			bool scrolled = false;

			foreach (string word in words) {
				int idx = 0;

				if (word == String.Empty)
					continue;

				while ((idx = text.IndexOf (word.ToLower (), idx)) != -1) {
					Gtk.TextIter start = buffer.GetIterAtOffset (idx);
					Gtk.TextIter end = start;
					end.ForwardChars (word.Length);
					if (!scrolled) {
						scrolled = true;
						TextMark mark = buffer.CreateMark (null, start, false);
						conversation.ScrollMarkOnscreen (mark);
					}
					buffer.ApplyTag ("highlight", start, end);

					idx += word.Length;
				}
			}
		}

		private void Search (string text)
		{
			search_entry.Text = text;
			search_button.Visible = false;
			clear_button.Visible = true;
			search_entry.Sensitive = false;

			search_text = text;
			highlight_text = null;
		}

		private void OnConversationSelected (object o, EventArgs args) 
		{
			TreeIter iter;
			TreeModel model;
			
			if (((TreeSelection)o).GetSelected (out model, out iter)) {
				ImLog log = model.GetValue (iter, 2) as ImLog;

				if (log == null)
					return;

				RenderConversation (log);
				SetConversationScroll (0);
			}
		}

		private void OnWindowClose (object o, EventArgs args)
		{
			Application.Quit ();
		}

		private void OnWindowDelete (object o, DeleteEventArgs args)
		{
			Application.Quit ();
		}

		private void OnSearchClicked (object o, EventArgs args)
		{
			if (search_entry.Text.Length == 0)
				return;

			Search (search_entry.Text);
			SearchTimeline ();
		}

		private void SetConversationScroll (double vadj)
		{
			scrolledwindow.Vadjustment.Value = vadj;
			scrolledwindow.Vadjustment.ChangeValue ();
			scrolledwindow.Vadjustment.Change ();
		}

		private void ScrollToFirstLog ()
		{
			SelectPath (new TreePath (new int [] {0, 0}));
		}

		private void ScrollToLog (ImLog scroll_log)
		{
			if (scroll_log == null) {
				ScrollToFirstLog ();
				return;
			}

			TreeIter root_iter;
			if (!tree_store.GetIterFirst (out root_iter))
				return;
			
			do {
				if (! tree_store.IterHasChild (root_iter))
					continue;

				TreeIter child;
				tree_store.IterNthChild (out child, root_iter, 0);
					
				do {
					ImLog log = tree_store.GetValue (child, 2) as ImLog;
						
					if (log == scroll_log) {
						SelectPath (tree_store.GetPath (child));
						return;
					}
				} while (tree_store.IterNext (ref child));
			} while (tree_store.IterNext (ref root_iter));
		}

		private void SelectPath (TreePath path)
		{
			timelinetree.Selection.Changed -= OnConversationSelected; 
			timelinetree.ExpandToPath (path);
			timelinetree.Selection.SelectPath (path);
			timelinetree.ScrollToCell (path, null, true, 0.5f, 0.0f);
			timelinetree.Selection.Changed += OnConversationSelected; 
		}

		private ImLog GetSelectedLog ()
		{
			TreeSelection selection = timelinetree.Selection;
			TreeModel model;
			TreeIter iter;

			if (selection.GetSelected (out model, out iter))
				return (ImLog) tree_store.GetValue (iter, 2);
			return null;
		}

		private void OnClearClicked (object o, EventArgs args)
		{
			highlight_text = search_text = null;
			search_button.Visible = true;
			clear_button.Visible = false;
			search_entry.Sensitive = true;

			RepopulateTimeline (false, scrolledwindow.Vadjustment.Value);
			ScrollToLog (GetSelectedLog ());
		}
	}
}
