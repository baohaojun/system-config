using Gtk;
using Mono.Unix;
using System;
using System.Diagnostics;

using Beagle;
using Beagle.Util;

namespace Beagle.Search {

	//FIXME: This way of handling scope needs to change since now we provide a direct combo box to set query scope
	[Flags]
	public enum ScopeType : ushort {
		Nothing       = 0,
		Applications  = 1 << 0,
		Calendar      = 1 << 1,
		Contacts      = 1 << 2,
		Documents     = 1 << 3,
		Conversations = 1 << 4,
		Images        = 1 << 5,
		Media         = 1 << 6,
		Folders       = 1 << 7,
		Websites      = 1 << 8,
		Feeds         = 1 << 9,
		Archives      = 1 << 10,
		Everything    = UInt16.MaxValue // Lame but there's no way to do ~0 in a ushort way.
	}

	public enum SortType {
		Relevance,
		Name,
		Modified
	}

	public class UIManager : Gtk.UIManager {

		private SearchWindow search = null;
		
		private Gtk.ActionGroup actions;
		private Gtk.RadioActionEntry[] sort_entries;
		private Gtk.ToggleActionEntry[] view_entries, domain_entries;

		public UIManager (SearchWindow search)
		{
			this.search = search;
			this.actions = new ActionGroup ("Actions");

			ActionEntry quit_action_entry;

			if (search.IconEnabled) {
				quit_action_entry = new ActionEntry ("Quit", Gtk.Stock.Close,
								     null, "<control>Q",
						 		     Catalog.GetString ("Close Desktop Search"),
						 		     Quit);
			} else {
				quit_action_entry = new ActionEntry ("Quit", Gtk.Stock.Quit,
								     null, "<control>Q",
						 		     Catalog.GetString ("Exit Desktop Search"),
						 		     Quit);

			}

			Gtk.ActionEntry[] entries = new ActionEntry[] {
				new ActionEntry ("Search", null,
						 Catalog.GetString ("_Search"),
						 null, null, null),
				new ActionEntry ("Domain", null,
						 Catalog.GetString ("Search _Domains"),
						 null, null, null),
				new ActionEntry ("Actions", null,
						 Catalog.GetString ("_Actions"),
						 null, null, null),
				new ActionEntry ("View", null,
						 Catalog.GetString ("_View"),
						 null, null, null),
				new ActionEntry ("Service", null,
						 Catalog.GetString ("Service _Options"),
						 null, null, null),
				new ActionEntry ("Help", null,
						 Catalog.GetString ("_Help"),
						 null, null, null),
				quit_action_entry,
				new ActionEntry ("Preferences", Gtk.Stock.Preferences,
						 null, null,
						 Catalog.GetString ("Exit Desktop Search"),
						 Preferences),
				new ActionEntry ("StartService", Gtk.Stock.Execute,
						 Catalog.GetString ("Start service"),
						 null, null, StartService),
				new ActionEntry ("StopService", Gtk.Stock.Stop,
						 Catalog.GetString ("Stop service"),
						 null, null, StopService),
				new ActionEntry ("IndexInfo", Gtk.Stock.Index,
						 Catalog.GetString ("Index information"),
						 null, null, IndexInfo),
				new ActionEntry ("Contents", Gtk.Stock.Help,
						 Catalog.GetString ("_Contents"),
						 "F1",
						 Catalog.GetString ("Help - Table of Contents"),
						 Help),
				new ActionEntry ("About", Gnome.Stock.About,
						 null, null,
						 Catalog.GetString ("About Desktop Search"),
						 About),
				new ActionEntry ("QuickTips", null,
						 Catalog.GetString ("Quick Tips"),
						 null, null, QuickTips),
				new ActionEntry ("FocusSearchEntry", null, "",
						 "<control>K", null,
						 OnFocusSearchEntry),
				new ActionEntry ("FocusSearchEntry2", null, "",
						 "<control>L", null,
						 OnFocusSearchEntry),
				new ActionEntry ("HideWindow", null, "",
						 "Escape", null,
						 OnHideWindow),
				new ActionEntry ("HideWindow2", null, "",
						 "<control>W", null,
						 OnHideWindow)
			};
			actions.Add (entries);

			sort_entries = new RadioActionEntry[] {
				new RadioActionEntry ("SortModified", null,
						      Catalog.GetString ("Sort by Date _Modified"), null,
						      Catalog.GetString ("Sort the most-recently-modified matches first"),
						      (int)SortType.Modified),
				new RadioActionEntry ("SortName", null,
						      Catalog.GetString ("Sort by _Name"), null,
						      Catalog.GetString ("Sort matches by name"),
						      (int)SortType.Name),
				new RadioActionEntry ("SortRelevance", null,
						      Catalog.GetString ("Sort by _Relevance"), null,
						      Catalog.GetString ("Sort the best matches first"),
						      (int)SortType.Relevance),
			};
			actions.Add (sort_entries, (int)SortType.Modified, OnSortChanged);

			domain_entries = new ToggleActionEntry [] {
				new ToggleActionEntry ("Local", null,
						       Catalog.GetString ("_Local"),
						       null,
						       Catalog.GetString ("Search in personal data in this computer"), /* personal files, emails */
						       OnDomainChanged,
						       true),
				new ToggleActionEntry ("System", null,
						       Catalog.GetString ("_System"),
						       null,
						       Catalog.GetString ("Search in system data on this computer"), /* system manpages, applications */
						       OnDomainChanged,
						       true),
				new ToggleActionEntry ("Global", null,
						       Catalog.GetString ("_Global"),
						       null,
						       Catalog.GetString ("Search in internet services"), /* gmail and other web services */
						       OnDomainChanged,
						       false),
				new ToggleActionEntry ("Neighborhood", null,
						       Catalog.GetString ("_Neighborhood"),
						       null,
						       Catalog.GetString ("Search on computers near me"), /* remote beagle services */
						       OnDomainChanged,
						       false)
			};
			actions.Add (domain_entries);

			view_entries = new ToggleActionEntry[] {
				new ToggleActionEntry ("ShowDetails", null,
						       Catalog.GetString ("Show Details"), null, null,
						       OnToggleDetails, true)
			};
			actions.Add (view_entries);

			InsertActionGroup (actions, 0);
			search.AddAccelGroup (AccelGroup);
			AddUiFromString (ui_def);
		}

		public Gtk.MenuBar MenuBar {
			get { return (Gtk.MenuBar)GetWidget ("/MenuBar"); }
		}

		private bool sensitive = true;
		public bool Sensitive {
			get { return this.sensitive; }
			set {
				this.sensitive = value;

				actions ["QuickTips"].Sensitive = value;

				foreach (Gtk.RadioActionEntry rae in sort_entries)
					actions [rae.name].Sensitive = value;
			}
		}

		private const string ui_def =
		"<ui>" +
		"  <menubar name='MenuBar'>" +
		"    <menu action='Search'>" +
		"      <menu action='Domain'>" +
		"        <menuitem action='Local'/>" +
		"        <menuitem action='System'/>" +
		"        <menuitem action='Neighborhood'/>" +
#if ENABLE_GOOGLEBACKENDS
		"        <menuitem action='Global'/>" +
#endif
		"      </menu>" +
		"      <menuitem action='Preferences'/>" +
		"      <separator/>" +
		"      <menuitem action='Quit'/>" +
		"    </menu>" +
		"    <menu action='Actions'>" +
		"    </menu>" +
		"    <menu action='View'>" +
		"      <menuitem action='SortModified'/>" +
		"      <menuitem action='SortName'/>" +
		"      <menuitem action='SortRelevance'/>" +
		"      <separator/>" +
		"      <menuitem action='ShowDetails'/>" +
		"    </menu>" +
		"    <menu action='Service'>" +
		"      <menuitem action='StartService'/>" +
		"      <menuitem action='StopService'/>" +
		"      <menuitem action='IndexInfo'/>" +
		"    </menu>" +
		"    <menu action='Help'>" +
		"      <menuitem action='Contents'/>" +
		"      <menuitem action='QuickTips'/>" +
		"      <menuitem action='About'/>" +
		"    </menu>" +
		"  </menubar>" +
		"  <accelerator action='FocusSearchEntry'/>" +
		"  <accelerator action='FocusSearchEntry2'/>" +
		"  <accelerator action='HideWindow'/>" +
		"  <accelerator action='HideWindow2'/>" +
		"</ui>";

		private void Preferences (object obj, EventArgs args)
		{
			Process p = new Process ();
			p.StartInfo.UseShellExecute = false;
			p.StartInfo.FileName = "beagle-settings";

			try {
				p.Start ();
			} catch (Exception e) {
				Console.WriteLine ("Could not start beagle-settings: {0}", e);
			}
		}

		private void OnHideWindow (object obj, EventArgs args)
		{
			if (search.IconEnabled)
				search.Hide ();
		}

		private void Quit (object obj, EventArgs args)
		{
			if (search.IconEnabled) {
				search.Hide ();
				return;
			} 

			Gtk.Application.Quit ();
		}

		private void Help (object obj, EventArgs args)
		{
			string address = "http://www.beagle-project.org/Getting_Started";

			try {
				Gnome.Url.Show (address);
			} catch {
				HigMessageDialog md = new HigMessageDialog (search, Gtk.DialogFlags.DestroyWithParent,
									    Gtk.MessageType.Error, Gtk.ButtonsType.Close,
									    Catalog.GetString ("Couldn't launch web browser"),
									    Catalog.GetString (String.Format ("Please point your web browser to '{0}' manually", address)));
				md.Run ();
				md.Destroy ();
			}
		}

		private void About (object obj, EventArgs args)
		{
			Gdk.Pixbuf logo = WidgetFu.LoadThemeIcon ("system-search", 48);

			string[] people = new string[] { "Anna Dirks <anna@novell.com>",
							 "Dan Winship <danw@novell.com>",
							 "D Bera <dbera.web@gmail.com>",
							 "Fredrik Hedberg <fredrik@avafan.com>",
							 "Joe Shaw <joeshaw@novell.com>", 
							 "Jakub Steiner <jimmac@novell.com>",
							 "Lukas Lipka <lukaslipka@gmail.com>",
					    };

			string translators = Catalog.GetString ("translator-credits");
			if (translators == "translator-credits") // not translated
				translators = null;
			
#pragma warning disable 612 // don't warn that Gnome.About is deprecated
			Gnome.About about = new Gnome.About ("Beagle Search",
							     Beagle.Util.ExternalStringsHack.Version,
							     VersionFu.DefaultCopyright,
							     null, people, null, null,
							     logo);
			about.Run ();
			about.Dispose ();
#pragma warning restore 612
		}

		public delegate void FocusSearchEntryDelegate ();
		public event FocusSearchEntryDelegate FocusSearchEntry;

		private void OnFocusSearchEntry (object obj, EventArgs args)
		{
			if (FocusSearchEntry != null)
				FocusSearchEntry ();
		}

		public delegate void SortChangedDelegate (SortType scope);
		public event SortChangedDelegate SortChanged;

		private void OnSortChanged (object obj, Gtk.ChangedArgs args)
		{
			if (SortChanged != null)
				SortChanged ((SortType)args.Current.CurrentValue);
		}

		public delegate void DomainChangedDelegate (QueryDomain domain, bool active);
		public event DomainChangedDelegate DomainChanged;

		private void OnDomainChanged (object o, EventArgs args)
		{
			QueryDomain domain = (QueryDomain)Enum.Parse (typeof (QueryDomain), ((Gtk.Action) o).Name);

			if (DomainChanged != null)
				DomainChanged (domain, ((ToggleAction)o).Active);
		}

		public delegate void ToggleDetailsDelegate (bool active);
		public event ToggleDetailsDelegate ToggleDetails;

		private void OnToggleDetails (object obj, EventArgs args)
		{
			if (ToggleDetails != null)
				ToggleDetails (((ToggleAction) obj).Active);
		}

		public delegate void ShowQuickTipsDelegate ();
		public event ShowQuickTipsDelegate ShowQuickTips;

		private void QuickTips (object obj, EventArgs args)
		{
			if (ShowQuickTips != null)
				ShowQuickTips ();
		}

		public delegate void ShowIndexInfoDelegate ();
		public event ShowIndexInfoDelegate ShowIndexInfo;

		private void IndexInfo (object obj, EventArgs args)
		{
			if (ShowIndexInfo != null)
				ShowIndexInfo ();
		}

		public delegate void StartDaemonDelegate ();
		public event StartDaemonDelegate StartDaemon;

		private void StartService (object obj, EventArgs args)
		{
			if (StartDaemon != null)
				StartDaemon ();
		}

		public delegate void StopDaemonDelegate ();
		public event StopDaemonDelegate StopDaemon;

		private void StopService (object obj, EventArgs args)
		{
			if (StopDaemon != null)
				StopDaemon ();
		}
	}
}
