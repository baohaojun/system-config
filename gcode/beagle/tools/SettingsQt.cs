//
// SettingsQt.cs
// A Qt based beagle settings program.
//
// Copyright (C) 2008 D Bera <dbera.web@gmail.com>
//

//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

using System;
using System.Reflection;
using System.Collections.Generic;
using Qyoto; 
using Ui;
using Beagle.Util;

// Assembly information
[assembly: AssemblyTitle ("beagle-settings-qt")]
[assembly: AssemblyDescription ("Qt interface to the Beagle configuration options.")]
[assembly: AssemblyCopyright ("Copyright (C) 2007-2008 Debajyoti Bera <dbera.web@gmail.com>")]

public class Settings {
	public static void Main (string[] args)
	{
		if (Array.IndexOf (args, "--help") > -1)
			PrintUsageAndExit ();

		if (Array.IndexOf (args, "--version") > -1) {
			VersionFu.PrintVersion ();
			// Console.WriteLine (Qt.QT_VERSION_STR); // Uncomment when does not crash
			Environment.Exit (0);
		}

		// FIXME: Cant seem to pass the args to Qt!
		new QApplication(args);

		MainForm mf = new MainForm(args);
		mf.Show ();

		QApplication.Exec();
	}

	public static void PrintUsageAndExit () 
	{
		VersionFu.PrintHeader ();

		string usage =
			"Usage: beagle-settings-qt [OPTIONS]\n\n" +
			"Options:\n" +
			"--general    : Open the page with general options.(default)\n" +
			"--indexing   : Open the page with indexing specific options.\n" +
			"--backends   : Open the page with a list of backends to choose.\n" +
			"--networking : Open the page with networking related options.\n\n" +
			"--google     : OPen the page with google backend specific options.\n\n";

		Console.WriteLine (usage);

		System.Environment.Exit (0);
	}

}

// FIXME: Mark for translation

public class MainForm : QDialog
{
	Dialog main;
	Dictionary<string, QCheckBox> backend_checkboxes;

	public MainForm (string[] args) : base ()
	{
		main = new Dialog ();
		main.SetupUi (this);

		// create the checkbox mapping
		backend_checkboxes = new Dictionary<string, QCheckBox> (20);
		backend_checkboxes ["Files"] = main.filesBackend;
		backend_checkboxes ["Locate"] = main.locateBackend;
		backend_checkboxes ["KMail"] = main.kmailBackend;
		backend_checkboxes ["EvolutionMail"] = main.evoBackend;
		backend_checkboxes ["Thunderbird"] = main.tbirdBackend;
		backend_checkboxes ["Kopete"] = main.kopeteBackend;
		backend_checkboxes ["Konversation"] = main.konvBackend;
		backend_checkboxes ["Pidgin"] = main.pidginBackend;
		backend_checkboxes ["Empathy"] = main.empathyBackend;
		backend_checkboxes ["KonquerorHistory"] = main.konqBackend;
		backend_checkboxes ["KonqBookmark"] = main.kBookmarkBackend;
		backend_checkboxes ["Opera"] = main.operaBackend;
		backend_checkboxes ["IndexingService"] = main.ffBackend;
		backend_checkboxes ["KOrganizer"] = main.korgBackend;
		backend_checkboxes ["KNotes"] = main.knotesBackend;
		backend_checkboxes ["KAddressBook"] = main.kaddrbookBackend;
		backend_checkboxes ["Tomboy"] = main.tomboyBackend;
		backend_checkboxes ["Labyrinth"] = main.labyrinthBackend;
		backend_checkboxes ["EvolutionDataServer"] = main.edsBackend;
		backend_checkboxes ["Akregator"] = main.akregatorBackend;
		backend_checkboxes ["Blam"] = main.blamBackend;
		backend_checkboxes ["Liferea"] = main.lifereaBackend;
		backend_checkboxes ["documentation"] = main.docBackend;
		backend_checkboxes ["applications"] = main.appBackend;
		backend_checkboxes ["manpages"] = main.manpagesBackend;
		backend_checkboxes ["monodoc"] = main.monodocBackend;
		backend_checkboxes ["GMailSearch"] = main.gmailLiveBackend;

		Connect (main.CancelButton, SIGNAL ("clicked()"), qApp, SLOT ("quit()"));
		Connect (main.OkButton, SIGNAL ("clicked()"), this, SLOT ("saveSettings()"));
		Connect (main.RootAddButton, SIGNAL ("clicked()"), this, SLOT ("showDirChooser()"));
		Connect (main.RootRemoveButton, SIGNAL ("clicked()"), this, SLOT ("removeChosenDir()"));
		Connect (main.ExcludeDirAddButton, SIGNAL ("clicked()"), this, SLOT ("addExcludeDir()"));
		Connect (main.ExcludeDirRemoveButton, SIGNAL ("clicked()"), this, SLOT ("removeExcludeDir()"));
		Connect (main.PatternAdd, SIGNAL ("clicked()"), this, SLOT ("addNamePattern()"));
		Connect (main.PatternRemove, SIGNAL ("clicked()"), this, SLOT ("removeNamePattern()"));
		Connect (main.MailFolderAdd, SIGNAL ("clicked()"), this, SLOT ("addFolderPattern()"));
		Connect (main.MailFolderRemove, SIGNAL ("clicked()"), this, SLOT ("removeFolderPattern()"));
		Connect (main.IsGoogleAppsAccount, SIGNAL ("stateChanged(int)"), this, SLOT ("isGoogleAppsToggled(int)"));

		LoadSettings ();

		if (args.Length == 0)
			return;

		if (Array.IndexOf (args, "--indexing") != -1)
			main.TabWidget.SetCurrentWidget (main.IndexingTab);
		else if (Array.IndexOf (args, "--backends") != -1)
			main.TabWidget.SetCurrentWidget (main.BackendsTab);
		else if (Array.IndexOf (args, "--networking") != -1)
			main.TabWidget.SetCurrentWidget (main.NetworkingTab);
		else if (Array.IndexOf (args, "--google") != -1)
			main.TabWidget.SetCurrentWidget (main.GoogleTab);
		else
			main.TabWidget.SetCurrentWidget (main.GeneralTab);
	}

	private void LoadSettings ()
	{
		Console.WriteLine ("Loading settings ...");

		Config fsq_config = Conf.Get (Conf.Names.FilesQueryableConfig);
		Config daemon_config = Conf.Get (Conf.Names.DaemonConfig);
		Config networking_config = Conf.Get (Conf.Names.NetworkingConfig);

		main.IndexHomeDirOption.Checked = fsq_config.GetOption (Conf.Names.IndexHomeDir, true);
		main.IndexOnBatteryOption.Checked =  daemon_config.GetOption (Conf.Names.IndexOnBattery, false);
		main.IndexScreenSaverOption.Checked = daemon_config.GetOption (Conf.Names.IndexFasterOnScreensaver, true);
                main.WebinterfaceOption.Checked = networking_config.GetOption ("WebInterface", false);

		List<string[]> values = fsq_config.GetListOptionValues (Conf.Names.Roots);
		if (values != null)
			foreach (string[] root in values)
				main.RootsList.AddItem (root [0]);

		values = fsq_config.GetListOptionValues (Conf.Names.ExcludeSubdirectory);
		if (values != null)
			foreach (string[] subdir in values)
				main.ExcludeDirList.AddItem (subdir [0]);

		values = fsq_config.GetListOptionValues (Conf.Names.ExcludePattern);
		if (values != null)
			foreach (string[] pattern in values)
				main.PatternList.AddItem (pattern [0]);

		values = daemon_config.GetListOptionValues (Conf.Names.ExcludeMailfolder);
		if (values != null)
			foreach (string[] mailfolder in values)
				main.MailFolderList.AddItem (mailfolder [0]);

		values = daemon_config.GetListOptionValues (Conf.Names.DeniedBackends);
		foreach (QCheckBox backend_box in backend_checkboxes.Values)
			backend_box.Checked = true;
		if (values != null) {
			foreach (string[] backend in values) {
				if (! backend_checkboxes.ContainsKey (backend [0]))
					continue;
				backend_checkboxes [backend [0]].Checked = false;
			}
		}

#if ENABLE_GOOGLEBACKENDS
		LoadGoogleSettings ();
#else
		main.GoogleTab.SetDisabled (true);
#endif
	}

	private void LoadGoogleSettings ()
	{
		Config google_config = Conf.Get ("GoogleBackends");
		main.GMailSearchFolder.Text = google_config.GetOption ("GMailSearchFolder", null);

		string username = google_config.GetOption ("GMailUsername", null);
		if (! username.EndsWith ("@gmail.com"))
			username = null;
		else
			main.GoogleUsername.Text = username.Remove (username.Length - 10);

		string google_apps_domain = google_config.GetOption ("GoogleAppsAccountName", null);
		if (google_apps_domain != null)
			google_apps_domain = google_apps_domain.Trim ();
		if (String.IsNullOrEmpty (google_apps_domain)) {
			main.IsGoogleAppsAccount.Checked = false;
			main.GoogleAppsAccountName.SetDisabled (true);
		} else {
			main.IsGoogleAppsAccount.Checked = true;
			main.GoogleAppsAccountName.SetDisabled (false);
			main.GoogleAppsAccountName.Text = google_apps_domain;
		}

		string password_source = google_config.GetOption ("GMailPasswordSource", "conf-file");
		if (password_source == "gnome-keyring") {
			QMessageBox.Critical (this, "Google password storage option", "Gnome keyring support not available from beagle-settings-qt");
			return;
		}

		if (password_source == "kdewallet") {
			if (String.IsNullOrEmpty (username))
				return;

			try {
				string password = KdeUtils.ReadPasswordKDEWallet ("beagle", username);
				main.GooglePassword.Text = password;
				main.OptionStorePasswordKWallet.SetChecked (true);
			} catch (ArgumentException e) {
				QMessageBox.Warning (this, "KDEWallet", e.Message);
			}
			return;
		}

		if (password_source != "conf-file") {
			QMessageBox.Warning (this, "Google password storage option", "Unknown password storage option.");
			return;
		}

		main.GooglePassword.Text = google_config.GetOption ("GMailPassword", null);
		main.OptionStorePasswordConf.SetChecked (true);
	}

	[Q_SLOT ("void saveSettings()")]
	private void SaveSettings ()
	{
		Console.WriteLine ("Saving settings, please wait ...");

		Config fsq_config = Conf.Get (Conf.Names.FilesQueryableConfig);
		Config daemon_config = Conf.Get (Conf.Names.DaemonConfig);
		Config networking_config = Conf.Get (Conf.Names.NetworkingConfig);

		daemon_config.SetOption (Conf.Names.IndexOnBattery, main.IndexOnBatteryOption.Checked);
		daemon_config.SetOption (Conf.Names.IndexFasterOnScreensaver, main.IndexScreenSaverOption.Checked);
		fsq_config.SetOption (Conf.Names.IndexHomeDir, main.IndexHomeDirOption.Checked);
		networking_config.SetOption ("WebInterface", main.WebinterfaceOption.Checked);

		List<string[]> roots = new List<string[]> (main.RootsList.Count);
		List<string[]> denied_backends = new List<string[]> ();
		List<string[]> exclude_dirs = new List<string[]> (main.ExcludeDirList.Count);
		List<string[]> exclude_patterns = new List<string[]> (main.PatternList.Count);
		List<string[]> exclude_folders = new List<string[]> (main.MailFolderList.Count);

		for (int i = 0; i < main.RootsList.Count; ++ i)
			roots.Add (new string[] {main.RootsList.Item (i).Text ()});

		foreach (KeyValuePair<string, QCheckBox> backend_box_pair in backend_checkboxes) {
			if (backend_box_pair.Value.Checked)
				continue;
			denied_backends.Add (new string[] { backend_box_pair.Key });
		}

		for (int i = 0; i < main.ExcludeDirList.Count; ++ i)
			exclude_dirs.Add (new string[] {main.ExcludeDirList.Item (i).Text ()});

		for (int i = 0; i < main.PatternList.Count; ++ i)
			exclude_patterns.Add (new string[] {main.PatternList.Item (i).Text ()});

		for (int i = 0; i < main.MailFolderList.Count; ++ i)
			exclude_folders.Add (new string[] {main.MailFolderList.Item (i).Text ()});

		if (roots.Count > 0)
			fsq_config.SetListOptionValues (Conf.Names.Roots, roots);
		if (denied_backends.Count > 0)
			daemon_config.SetListOptionValues (Conf.Names.DeniedBackends, denied_backends);
		if (exclude_dirs.Count > 0)
			fsq_config.SetListOptionValues (Conf.Names.ExcludeSubdirectory, exclude_dirs);
		if (exclude_patterns.Count > 0)
			fsq_config.SetListOptionValues (Conf.Names.ExcludePattern, exclude_patterns);
		if (exclude_folders.Count > 0)
			daemon_config.SetListOptionValues (Conf.Names.ExcludeMailfolder, exclude_folders);

		Conf.Save (fsq_config);
		Conf.Save (daemon_config);
		Conf.Save (networking_config);

#if ENABLE_GOOGLEBACKENDS
		SaveGoogleSettings ();
#endif

		QApplication.Quit ();
	}

	private void SaveGoogleSettings ()
	{
		Config google_config = Conf.Get ("GoogleBackends");
		google_config.SetOption ("GMailSearchFolder", main.GMailSearchFolder.Text);

		string username = main.GoogleUsername.Text.Trim ();
		if (username != String.Empty) {
			username = username + "@gmail.com";
			google_config.SetOption ("GMailUsername", username);
		}

		if (main.IsGoogleAppsAccount.Checked)
			google_config.SetOption ("GoogleAppsAccountName", main.GoogleAppsAccountName.Text);
		else
			google_config.SetOption ("GoogleAppsAccountName", String.Empty);

		if (main.OptionStorePasswordConf.Checked) {
			google_config.SetOption ("GMailPasswordSource", "conf-file");
			google_config.SetOption ("GMailPassword", main.GooglePassword.Text);
		} else if (main.OptionStorePasswordKWallet.Checked && ! String.IsNullOrEmpty (username)) {
			try {
				KdeUtils.StorePasswordKDEWallet ("beagle", username, main.GooglePassword.Text);
				google_config.SetOption ("GMailPasswordSource", "kdewallet");
			} catch (ArgumentException e) {
				QMessageBox.Warning (this, "KDEWallet", e.Message);
			}
		}

		Conf.Save (google_config);
	}

	[Q_SLOT ("showDirChooser()")]
	private void ShowDirChooser ()
	{
		string new_root = QFileDialog.GetExistingDirectory (this, "Choose new path for beagle to index", "/");
		if (new_root == null)
			return;
		Console.WriteLine ("Selected new root '{0}'", new_root);

		// Check if the new_root is already part of some root in the list
		bool seen = false;
		for (int i = 0; i < main.RootsList.Count; ++ i) {
			Console.WriteLine ("Checking against {0}", main.RootsList.Item (i).Text ());
			if (main.RootsList.Item (i).Text ().StartsWith (new_root)) {
				seen = true;
				QMessageBox.Critical (this, "Invalid directory", "The selected path wasn't added. The list contains a path that is included in the new path.", "OK");
			} else if (new_root.StartsWith (main.RootsList.Item (i).Text ())) {
				seen = true;
				QMessageBox.Critical (this, "Invalid directory", "The selected path wasn't added. The path is already included in one of the paths in the list.", "OK");
			}
		}

		if (! seen)
			main.RootsList.AddItem (new_root);
	}

	[Q_SLOT ("removeChosenDir()")]
	private void RemoveSelectedDir ()
	{
		List<QListWidgetItem> selected_paths = main.RootsList.SelectedItems ();
		if (selected_paths == null || selected_paths.Count == 0)
			return;

		main.RootsList.TakeItem (main.RootsList.Row (selected_paths [0]));
	}

	[Q_SLOT ("addExcludeDir()")]
	private void AddExcludeDir ()
	{
		string dir = QFileDialog.GetExistingDirectory (this, "Select path to exclude", PathFinder.HomeDir);
		if (dir == null)
			return;
		Console.WriteLine ("Excluding '{0}'", dir);

		// Check if the new dir is already part of some excluded directory in the list
		bool seen = false;
		for (int i = 0; i < main.ExcludeDirList.Count; ++ i) {
			if (main.ExcludeDirList.Item (i).Text ().StartsWith (dir)) {
				seen = true;
				QMessageBox.Critical (this, "Invalid directory", "The selected path wasn't added. The list contains a path that is included in the new path.", "OK");
			} else if (dir.StartsWith (main.ExcludeDirList.Item (i).Text ())) {
				seen = true;
				QMessageBox.Critical (this, "Invalid directory", "The selected path wasn't added. The path is already included in one of the paths in the list.", "OK");
			}
		}

		if (! seen)
			main.ExcludeDirList.AddItem (dir);
	}

	[Q_SLOT ("removeExcludeDir()")]
	private void RemoveExcludeDir ()
	{
		List<QListWidgetItem> dirs = main.ExcludeDirList.SelectedItems ();
		if (dirs == null || dirs.Count == 0)
			return;

		main.ExcludeDirList.TakeItem (main.ExcludeDirList.Row (dirs [0]));
	}

	[Q_SLOT ("addNamePattern()")]
	private void AddNamePattern ()
	{
		string pattern = main.PatternLineEdit.Text;
		if (pattern == String.Empty)
			return;
		main.PatternList.AddItem (pattern);
	}

	[Q_SLOT ("removeNamePattern()")]
	private void RemoveNamePattern ()
	{
		List<QListWidgetItem> patterns = main.PatternList.SelectedItems ();
		if (patterns == null || patterns.Count == 0)
			return;

		main.PatternList.TakeItem (main.PatternList.Row (patterns [0]));
	}

	[Q_SLOT ("addFolderPattern()")]
	private void AddFolderPattern ()
	{
		string folder = main.MailFolderLineEdit.Text;
		if (folder == String.Empty)
			return;
		main.MailFolderList.AddItem (folder);
	}

	[Q_SLOT ("removeFolderPattern()")]
	private void RemoveFolderPattern ()
	{
		List<QListWidgetItem> folders = main.MailFolderList.SelectedItems ();
		if (folders == null || folders.Count == 0)
			return;

		main.MailFolderList.TakeItem (main.MailFolderList.Row (folders [0]));
	}

	[Q_SLOT ("isGoogleAppsToggled(int)")]
	private void IsGoogleAppsToggled (int _state)
	{
		Qt.CheckState state = (Qt.CheckState) Enum.ToObject (typeof (Qt.CheckState), _state);

		if (state == Qt.CheckState.Unchecked)
			main.GoogleAppsAccountName.SetDisabled (true);
		else if (state == Qt.CheckState.Checked)
			main.GoogleAppsAccountName.SetDisabled (false);
	}
}

