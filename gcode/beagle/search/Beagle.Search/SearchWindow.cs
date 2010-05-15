//
// SearchWindow.cs
//
// Copyright (c) 2006 Novell, Inc.
// Copyright (C) 2008 Lukas Lipka <lukaslipka@gmail.com>
//

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;

using Gtk;
using Mono.Unix;

using Beagle;
using Beagle.Util;

using Beagle.Search.Tiles;

namespace Beagle.Search {

	public delegate void QueryEventDelegate (string query);

	public class SearchWindow : Window {

		private ISearch search = null;

		private Gtk.Button button;
		private Gtk.Tooltips tips;
		private Gtk.Notebook pages;
		private Gtk.Statusbar statusbar;
		private Gtk.ComboBox scope_list;

		private Beagle.Search.UIManager uim;
		private Beagle.Search.NotificationArea notification_area;
		private Beagle.Search.GroupView view;
		private Beagle.Search.Entry entry;
		private Beagle.Search.Spinner spinner;
		private Beagle.Search.Panes panes;

		private Beagle.Search.Pages.IndexInfo indexinfo;
		private Beagle.Search.Pages.QuickTips quicktips;
		private Beagle.Search.Pages.RootUser rootuser;
		private Beagle.Search.Pages.StartDaemon startdaemon;
		private Beagle.Search.Pages.NoMatch nomatch;

		private Beagle.Search.SortType sort = SortType.Modified;
		private QueryDomain domain = QueryDomain.Local | QueryDomain.System; // default

		// Whether we should grab focus from the text entry
		private bool grab_focus = false;
		private uint timeout_id = 0;

		private Beagle.Query current_query = null;
		private string query_text = null;
		private bool show_details = true;
		private int total_matches = -1;

		public event QueryEventDelegate QueryEvent;

		private struct ScopeMapping {
			internal string label;
			internal string query_mapping;
			internal ScopeMapping (string label, string query_mapping)
			{
				this.label = label;
				this.query_mapping = query_mapping;
			}
		};
		private static List<ScopeMapping> scope_mappings;

		static SearchWindow ()
		{
			// FIXME: Currently hardcoded list is bit too long! Need to hire some usability expert.
			scope_mappings = new List<ScopeMapping> (16);

			/* Translators: This labels are used in a combo-box to allow
			 * the user to select which kind of data he wants to search.
			 */
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("All"), String.Empty)); // Default search scope. Should be at the first
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("My Files"), "source:Files"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Applications"), "filetype:application OR source:applications"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Documents"), "filetype:document"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Pictures"), "filetype:image"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Media"), "filetype:audio OR filetype:video"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Source Code"), "filetype:source"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Archives"), "inarchive:true"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Emails"), "type:MailMessage OR filetype:mail"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("News Feeds"), "type:FeedItem"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Websites"), "type:WebHistory OR type:Bookmark"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Chat Logs"), "type:IMLog"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Contacts"), "type:Contact"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Notes"), "type:Note"));
			scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Calendar Events"), "type:Task OR type:Calendar"));
			//scope_mappings.Add (new ScopeMapping (Catalog.GetString ("Custom"), ""));

			// Allow users to define custom scope choices by defining envirionment var:
			// export BEAGLE_SEARCH_SCOPE="mymusic=source:music;win=tag:windows";
			string user_scopes = Environment.GetEnvironmentVariable ("BEAGLE_SEARCH_SCOPE");
			if (String.IsNullOrEmpty (user_scopes))
				return;

			string[] user_scope_list = user_scopes.Split (';');
			foreach (string s in user_scope_list) {
				string[] key_value = s.Split ('=');
				if (key_value.Length != 2)
					continue;
				scope_mappings.Add (new ScopeMapping (key_value [0], key_value [1]));
			}
		}

		public SearchWindow (ISearch search) : base (WindowType.Toplevel)
		{
			this.search = search;

			base.Title = Catalog.GetString ("Desktop Search");
			base.Icon = WidgetFu.LoadThemeIcon ("system-search", 16);
			base.DefaultWidth = 700;
			base.DefaultHeight = 550;
			base.DeleteEvent += OnWindowDelete;
			
			VBox vbox = new VBox ();
			vbox.Spacing = 3;

			uim = new UIManager (this);
			uim.DomainChanged += OnDomainChanged;
			uim.SortChanged += OnSortChanged;
			uim.ToggleDetails += OnToggleDetails;
			uim.ShowQuickTips += OnShowQuickTips;
			uim.ShowIndexInfo += OnShowIndexInfo;
			uim.StartDaemon += OnStartDaemon;
			uim.StopDaemon += OnStopDaemon;
			vbox.PackStart (uim.MenuBar, false, false, 0);

			HBox hbox = new HBox (false, 6);
			
			Label label = new Label (Catalog.GetString ("_Find in:"));
			hbox.PackStart (label, false, false, 0);
			
			scope_list = ComboBox.NewText ();
			foreach (ScopeMapping mapping in scope_mappings)
				scope_list.AppendText (mapping.label);
			scope_list.Active = 0;

			scope_list.Changed += new EventHandler (delegate (object o, EventArgs args) {
									ComboBox combo = o as ComboBox;
									if (o == null)
										return;
									int active = combo.Active;
									Log.Debug ("Scope changed: {0} maps to '{1}'", combo.ActiveText, scope_mappings [active].query_mapping);
									Query (true);
								});
			hbox.PackStart (scope_list, false, false, 0);

			entry = new Entry ();
			entry.Activated += OnEntryActivated;
			hbox.PackStart (entry, true, true, 0);

			label.MnemonicWidget = entry;
			uim.FocusSearchEntry += delegate () { entry.GrabFocus (); };

			// The auto search after timeout feauture is now optional
			// and can be disabled.

			if (Conf.BeagleSearch.GetOption (Conf.Names.BeagleSearchAutoSearch, true)) {
				entry.Changed += OnEntryResetTimeout;
				entry.MoveCursor += OnEntryResetTimeout;
			}

			button = new Gtk.Button ();
			Gtk.HBox button_hbox = new Gtk.HBox (false, 2);
			Gtk.Image icon = new Gtk.Image (Gtk.Stock.Find, Gtk.IconSize.Button);
			button_hbox.PackStart (icon, false, false, 0);
			label = new Gtk.Label (Catalog.GetString ("Find Now"));
			button_hbox.PackStart (label, false, false, 0);
			button.Add (button_hbox);
			button.Clicked += OnButtonClicked;

			Gtk.VBox buttonVBox = new Gtk.VBox (false, 0);
			buttonVBox.PackStart (button, true, false, 0);
			hbox.PackStart (buttonVBox, false, false, 0);

			spinner = new Spinner ();
			hbox.PackStart (spinner, false, false, 0);

			HBox padding_hbox = new HBox ();
			padding_hbox.PackStart (hbox, true, true, 9);
			vbox.PackStart (padding_hbox, false, true, 6);

			VBox view_box = new VBox (false, 3);
			vbox.PackStart (view_box, true, true, 0);

			HBox na_padding = new HBox ();
			view_box.PackStart (na_padding, false, true, 0);

			notification_area = new NotificationArea ();
			na_padding.PackStart (notification_area, true, true, 3);

			pages = new Gtk.Notebook ();
			pages.ShowTabs = false;
			pages.ShowBorder = false;
			pages.BorderWidth = 3;
			view_box.PackStart (pages, true, true, 0);

			quicktips = new Pages.QuickTips ();
			quicktips.Show ();
			pages.Add (quicktips);

			indexinfo = new Pages.IndexInfo ();
			indexinfo.Show ();
			pages.Add (indexinfo);

			rootuser = new Pages.RootUser ();
			rootuser.Show ();
			pages.Add (rootuser);

			startdaemon = new Pages.StartDaemon ();
			startdaemon.DaemonStarted += OnDaemonStarted;
			startdaemon.Show ();
			pages.Add (startdaemon);

			panes = new Beagle.Search.Panes ();
			panes.Show ();
			pages.Add (panes);

			view = new GroupView ();
			view.TileSelected += ShowInformation;
			panes.MainContents = view;

			this.statusbar = new Gtk.Statusbar ();
			vbox.PackEnd (this.statusbar, false, false, 0);
			
			Add (vbox);

			tips = new Gtk.Tooltips ();
			tips.SetTip (entry, Catalog.GetString ("Type in search terms"), "");
			tips.SetTip (button, Catalog.GetString ("Start searching"), "");
			tips.Enable ();

			if (Environment.UserName == "root" && !Conf.Daemon.GetOption (Conf.Names.AllowRoot, false)) {
				pages.CurrentPage = pages.PageNum (rootuser);
				entry.Sensitive = button.Sensitive = uim.Sensitive = false;
			} else {
				pages.CurrentPage = pages.PageNum (quicktips);
			}

			entry.GrabFocus ();
			StartCheckingIndexingStatus ();
		}

		private void SetWindowTitle (string query)
		{
			Title = String.Format (Catalog.GetString ("Desktop Search: {0}"), query);
		}

		public void GrabEntryFocus ()
		{
			entry.GrabFocus ();
		}

		public void Search (string query)
		{
			entry.Text = query;
			Query (true);
		}

		private void DetachQuery ()
		{
			if (current_query == null)
				return;

			current_query.HitsAddedEvent -= OnHitsAdded;
			current_query.HitsSubtractedEvent -= OnHitsSubtracted;
			current_query.Close ();
			TotalMatches = -1;
		}

		private void Query (bool grab_focus)
		{
			if (timeout_id != 0) {
				GLib.Source.Remove (timeout_id);
				timeout_id = 0;
			}

			string query = query_text = entry.Text;

			if (String.IsNullOrEmpty (query))
				return;

			SetWindowTitle (query);
			ShowInformation (null);

			if (QueryEvent != null)
				QueryEvent (query);

			view.Clear ();
			view.Scope = ScopeType.Everything;
			view.SortType = sort;
			pages.CurrentPage = pages.PageNum (panes);

			this.grab_focus = grab_focus;

			try {
				// Clean up our previous query, if any exists.
				DetachQuery ();

				TotalMatches = 0;

				current_query = new Query ();
				current_query.QueryDomain = domain;

				current_query.AddText (query);
				current_query.HitsAddedEvent += OnHitsAdded;
				current_query.HitsSubtractedEvent += OnHitsSubtracted;
				current_query.FinishedEvent += OnFinished;

				// Don't search documentation by default
				if (!search.DocsEnabled) {
					QueryPart_Property part = new QueryPart_Property ();
					part.Logic = QueryPartLogic.Prohibited;
					part.Type = PropertyType.Keyword;
					part.Key = "beagle:Source";
					part.Value = "documentation";
					current_query.AddPart (part);
				}

				// set scope from scope list
				ScopeMapping mapping = scope_mappings [scope_list.Active];
				if (! String.IsNullOrEmpty (mapping.query_mapping))
					current_query.AddText (mapping.query_mapping);

				current_query.SendAsync ();

				spinner.Start ();
			} catch (Beagle.ResponseMessageException) {
				pages.CurrentPage = pages.PageNum (startdaemon);
			} catch (Exception e) {
				Console.WriteLine ("Querying the Beagle daemon failed: {0}", e.Message);
			}
		}

		private void OnEntryActivated (object obj, EventArgs args)
		{
			Query (true);
		}

		private void OnDaemonStarted ()
		{
			Query (true);
		}

		private void OnEntryResetTimeout (object o, EventArgs args)
		{
			if (timeout_id != 0)
				GLib.Source.Remove (timeout_id);

			timeout_id = GLib.Timeout.Add (1000, OnEntryTimeout);
		}

		private bool OnEntryTimeout ()
		{
			timeout_id = 0;
			Query (false);

			return false;
		}

		private void OnButtonClicked (object obj, EventArgs args)
		{
			Query (true);
		}

		private void OnWindowDelete (object o, Gtk.DeleteEventArgs args)
		{
			// FIXME: Destroy window
			Hide ();
			args.RetVal = true;
		}

		private void OnSortChanged (SortType value)
		{
			view.SortType = sort = value;
		}

		private void OnToggleDetails (bool active)
		{
			show_details = active;
			if (panes.Details != null)
				panes.ToggleDetails (show_details);
			else
				panes.ToggleDetails (false);
		}

		private void OnShowQuickTips ()
		{
			DetachQuery ();
			pages.CurrentPage = pages.PageNum (quicktips);
		}
		
		private void OnShowIndexInfo ()
		{
			DetachQuery ();
			
			if (! indexinfo.Refresh ()) {
				pages.CurrentPage = pages.PageNum (startdaemon);
			} else {
				pages.CurrentPage = pages.PageNum (indexinfo);
			}
		}
		
		private void OnDomainChanged (QueryDomain domain, bool active)
		{
			if (active)
				this.domain |= domain;
			else
				this.domain &= ~domain;

			// FIXME: Most likely refire the query.
		}
		
		private void ShowInformation (Tiles.Tile tile)
		{
			notification_area.Hide ();

			if (tile != null) {
				panes.Details = tile.Details;
				if (tile.Details != null)
					panes.ToggleDetails (show_details);
				else
					panes.ToggleDetails (false);
			} else {
				panes.Details = null;
				panes.ToggleDetails (false);
			}
		}

		private void OnFinished (FinishedResponse response)
		{
			spinner.Stop ();
			view.Finished (grab_focus);
			grab_focus = false;

			CheckNoMatch ();
		}

		private void OnHitsAdded (HitsAddedResponse response)
		{
			int missed_tiles = 0;

			foreach (Hit hit in response.Hits) {
				Tile tile = TileActivatorOrg.MakeTile (hit, current_query);

				if (tile == null) {
					Console.WriteLine ("No tile found for: {0} ({1})", hit.Uri, hit.Type);
					missed_tiles ++;
					continue;
				}

				view.AddHit (tile);

				if (pages.CurrentPageWidget != panes)
					pages.CurrentPage = pages.PageNum (panes);
			}

			if (response.NumMatches != -1)
				TotalMatches += (response.NumMatches - missed_tiles);
		}

		private void OnHitsSubtracted (HitsSubtractedResponse response)
		{
			foreach (Uri uri in response.Uris)
				view.SubtractHit (uri);

			TotalMatches -= response.Uris.Count;

			CheckNoMatch ();
		}

#if ENABLE_AVAHI
                private void OnUnknownHostFound (object sender, AvahiEventArgs args)
                {
			NotificationMessage m = new NotificationMessage ();
			m.Pixbuf = WidgetFu.LoadThemeIcon ("network-workgroup", 48);
			m.Title = Catalog.GetString ("There are computers near you running Beagle");
			m.Message = Catalog.GetString ("You can select to search other computers from the \"Search\" menu.");
			m.AddAction ("Configure", OnNetworkConfigure);

			notification_area.Display (m);
		}

		private void OnNetworkConfigure (object o, EventArgs args)
		{
			SafeProcess p = new SafeProcess ();
			p.Arguments = new string[] { "beagle-settings", "--networking" };

			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Could not start beagle-settings:\n{0}", e);
			}
                }
#endif

		private void OnStartDaemon ()
		{
			notification_area.Hide ();

			DaemonInformationRequest request = new DaemonInformationRequest (true, false, false, false);

			try {
				request.Send ();
			} catch (Beagle.ResponseMessageException) {
				// beagled is not running
				// Start beagled and once it is started, display the quicktips page
				Beagle.Search.Pages.StartDaemon.DoStartDaemon (delegate () {
										this.statusbar.Pop (0);
										this.statusbar.Push (0, Catalog.GetString ("Search service started"));
										});
				pages.CurrentPage = pages.PageNum (quicktips);
				return;
			} catch (Exception e) {
				Console.WriteLine ("Stopping the Beagle daemon failed: {0}", e.Message);
			}

			// beagled is running
			NotificationMessage m = new NotificationMessage ();
			m.Icon = Gtk.Stock.DialogError;
			m.Title = Catalog.GetString ("Starting service failed");
			m.Message = Catalog.GetString ("Service is already running!");
			notification_area.Display (m);
		}

		private void OnStopDaemon ()
		{
			notification_area.Hide ();

			ShutdownRequest request = new ShutdownRequest ();
			try {
				request.Send ();
			} catch (Beagle.ResponseMessageException) {
				// beagled is not running
				NotificationMessage m = new NotificationMessage ();
				m.Icon = Gtk.Stock.DialogError;
				m.Title = Catalog.GetString ("Stopping service failed");
				m.Message = Catalog.GetString ("Service was not running!");
				notification_area.Display (m);

				// show the start daemon if the user wants to start the daemom
				pages.CurrentPage = pages.PageNum (startdaemon);
				return;
			} catch (Exception e) {
				Console.WriteLine ("Stopping the Beagle daemon failed: {0}", e.Message);
			}

			// beagled was running and should be now stopped.
			// Show the start page. The start-daemon page feels as if the user-request failed.
			pages.CurrentPage = pages.PageNum (quicktips);
			this.statusbar.Pop (0);
			this.statusbar.Push (0, Catalog.GetString ("Search service stopped"));
		}

		private void CheckNoMatch ()
		{
			MatchType matches = view.MatchState;

			if (matches == MatchType.Matched) {
				pages.CurrentPage = pages.PageNum (panes);
				return;
			}

			if (nomatch != null)
				nomatch.Destroy ();

			nomatch = new Pages.NoMatch (query_text, matches == MatchType.NoneInScope || scope_list.Active != 0);
			nomatch.Show ();

			pages.Add (nomatch);
			pages.CurrentPage = pages.PageNum (nomatch);

			// Since there is no match, possibly the user wants to modify query; focus the search entry field.
			GrabEntryFocus ();
		}

		/////////////////////////////////////

		private void StartCheckingIndexingStatus ()
		{
			InformationalMessagesRequest msg_request = new InformationalMessagesRequest ();
			msg_request.IndexingStatusEvent += OnIndexingStatusEvent;
			msg_request.SendAsync ();
		}

		private void OnIndexingStatusEvent (IndexingStatus status)
		{
			if (status == IndexingStatus.Running) {
				NotificationMessage m = new NotificationMessage ();
				m.Icon = Gtk.Stock.DialogInfo;
				m.Title = Catalog.GetString ("Your data is being indexed");
				m.Message = Catalog.GetString ("The search service is in the process of indexing your data.  Search results may be incomplete until indexing has finished.");
				notification_area.Display (m);
			} else {
				notification_area.Hide ();
			}
		}

		/////////////////////////////////////

		public bool IconEnabled {
			get { return search.IconEnabled; }
		}

		private int TotalMatches {
			get { return this.total_matches; }
			set {
				if (this.total_matches != -1)
					this.statusbar.Pop (0);

				this.total_matches = value;
				
				if (this.total_matches > -1) {
					string message;
					int tile_count = view.TileCount;

					if (tile_count == this.total_matches)
						message = String.Format (Catalog.GetPluralString ("Showing {0} match", "Showing all {0} matches", this.total_matches), this.total_matches);
					else
						message = String.Format (Catalog.GetString ("Too many matches. Showing latest {0} of total {1}"), view.TileCount, this.total_matches);

					this.statusbar.Push (0, message);
				}
			}
		}
	}
}
