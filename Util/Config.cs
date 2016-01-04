//
// Conf.cs
//
// Copyright (C) 2005 Novell, Inc.
// Copyright (C) 2007 Debajyoti Bera <dbera.web@gmail.com>
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
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;

namespace Beagrep.Util {

	/**
	 * Manages the configuration files for beagrep.
	 * Each configuration file is defined by the specific class that needs it and is stored in
	 * an XML format. The file is a list of Options, where each Option is a BoolOption, a StringOption
	 * or a ListOption.
	 *
	 * Most of beagrep components are supposed to read values from the files. The easiest way to do that
	 * is:
	 * Config config = Conf.Get (<name of config>);
	 * - To get a bool or a string option:
	 * bool/string opt_val = config.GetOption (<name of option>, default value);
	 * - To get the parameters of a list option:
	 * string[] params = config.GetListOptionParams (<name of option>);
	 * - To get the list of values for a list option:
	 * List<string[]> values = config.GetListOptionValues (<name of option>);
	 *
	 * Both params and values can be null if the option is not found.
	 * SetOption will set existing bool/string options or create new string/bool options.
	 * To create new list option, use NewListOption.
	 *
	 * Some standard config names and option names are listed in Conf.Names to avoid ambiguity.
	 * If Conf.WatchForUpdates() is called, subsequent Conf.Get returns the
	 * latest copy of the configuration and also caches the copy for future
	 * use. It uses inotify to refresh the cached copy if the config file is modified.
	 *
	 * Classes can also listen to changes by saying,
	 * Conf.WatchForUpdates ();
	 * and then subscribing to a particular config for changes,
	 * Conf.Subscribe (<name of config>, ConfigUpdateHandler);
	 *
	 * Of course, if classes do not need such sophisticated behaviour, it can just call
	 * Config config = Conf.Load (<name of config>);
	 * This will return the respective config (can be null if not present) but will not monitor the
	 * config file for changes.
	 *
	 * To save the config, call Conf.Save (config);
	 */
	public static class Conf {

		// A list of names to eliminate typos
		// Every name need not be here, this is just for convenience
		public static class Names {

			internal const int NumConfig = 4;

			// DO NOT change these 4 names - the names should be same as Config.Name and file names
			public const string FilesQueryableConfig = "FilesQueryable";
			public const string BeagrepSearchConfig = "BeagrepSearch";
			public const string DaemonConfig = "Daemon";
			public const string NetworkingConfig = "Networking";

			// Dont change these names either, otherwise old option values cant be read

			// Options for FilesQueryableConfig
			// boolean
			public const string IndexHomeDir = "IndexHomeDir"; // default true
			// list (1 param)
			public const string Roots = "Roots";
			public const string ExcludeSubdirectory = "ExcludeSubdirectory";
			public const string ExcludePattern = "ExcludePattern";

			// Options for SearchingConfig
			// boolean
			public const string KeyBinding_Ctrl = "KeyBinding_Ctrl"; // default false
			public const string KeyBinding_Alt = "KeyBinding_Alt"; // default false
			public const string BeagrepSearchAutoSearch = "BeagrepSearchAutoSearch"; // default true
			// string
			public const string KeyBinding_Key = "KeyBinding_Key"; // default F12
			public const string BeagrepPosX = "BeagrepPosX";
			public const string BeagrepPosY = "BeagrepPosY";
			public const string BeagrepSearchWidth = "BeagrepSearchWidth";
			public const string BeagrepSearchHeight = "BeagrepSearchHeight";
			// list (1 param)
			public const string SearchHistory = "SearchHistory";

			// Options for DaemonConfig
			// bool
			public const string AllowStaticBackend = "AllowStaticBackend"; // default true
			public const string IndexSynchronization = "IndexSynchronization"; // default true
			public const string AllowRoot = "AllowRoot"; // default false
			public const string IndexOnBattery = "IndexOnBattery"; // default false
			public const string IndexFasterOnScreensaver = "IndexFasterOnScreensaver"; // default true
			// list (1 param)
			public const string StaticQueryables = "StaticQueryables";
			public const string DeniedBackends = "DeniedBackends";
			public const string Maildirs = "Maildirs";
			public const string ExcludeMailfolder = "ExcludeMailfolder";

			// Options for NetworkingConfig
			// bool
			public const string ServiceEnabled = "ServiceEnabled"; // default false
			public const string PasswordRequired = "PasswordRequired"; // default true
			// string
			public const string ServiceName = "ServiceName";
			public const string ServicePassword = "ServicePassword";
			// list (4 params)
			public const string NetworkServices = "NetworkServices";
		}

		private static string configs_dir;
		private static Hashtable configs;
		private static Hashtable mtimes;
		private static Hashtable subscriptions;

		private static bool watching_for_updates = false;
		private static bool update_watch_present = false;

		public delegate void ConfigUpdateHandler (Config config);

		static Conf ()
		{
			configs = new Hashtable (Names.NumConfig);
			mtimes = new Hashtable (Names.NumConfig);
			subscriptions = new Hashtable (Names.NumConfig);

			configs_dir = Path.Combine (PathFinder.StorageDir, "config");
			if (!Directory.Exists (configs_dir)) {
				Directory.CreateDirectory (configs_dir);
				return;
			}

			// Else check the directory for old config files
			CheckOldConfig ();
		}

		public static void WatchForUpdates ()
		{
			// Make sure we don't try and watch for updates more than once
			if (update_watch_present)
				return;

			if (Inotify.Enabled) {
				Inotify.Subscribe (configs_dir, OnInotifyEvent, Inotify.EventType.Create | Inotify.EventType.CloseWrite);
			} else {
				// Poll for updates every 60 secs
				GLib.Timeout.Add (60000, new GLib.TimeoutHandler (CheckForUpdates));
			}

			update_watch_present = true;
			watching_for_updates = true;
		}

		private static void OnInotifyEvent (Inotify.Watch watch, string path, string subitem, string srcpath, Inotify.EventType type)
		{
			if (subitem == "" || watching_for_updates == false)
				return;

			Reload ();
		}

		private static bool CheckForUpdates ()
		{
			if (watching_for_updates)
				Reload ();
			return true;
		}

		public static void Subscribe (string name, ConfigUpdateHandler callback)
		{
			if (! update_watch_present)
				WatchForUpdates ();

			if (!subscriptions.ContainsKey (name))
				subscriptions.Add (name, new ArrayList (1));

			ArrayList callbacks = (ArrayList) subscriptions [name];
			callbacks.Add (callback);
		}

		private static void NotifySubscribers (Config config, string name)
		{
			ArrayList callbacks = (ArrayList) subscriptions [name];

			if (callbacks == null)
				return;

			foreach (ConfigUpdateHandler callback in callbacks)
				callback (config);
		}

		// Convenience routine to reload only the subscribed-to configs
		// Apps/Filters/Backends can always load config of their own
		public static void Reload ()
		{
			Config config;

			foreach (string name in subscriptions.Keys) {
				string filename = name + ".xml";
				string filepath = Path.Combine (configs_dir, filename);

				if (! File.Exists (filepath))
					continue;

				// If current_config is loaded and not modified, skip this one
				if (mtimes.ContainsKey (name) &&
				    File.GetLastWriteTimeUtc (filepath).CompareTo ((DateTime) mtimes [name]) <= 0)
					continue;

				config = Load (name);
				NotifySubscribers (config, name);
				mtimes [name] = DateTime.UtcNow;
				configs [name] = config;
			}
		}

		// Returns the config if present or a default config otherwise
		// Caches the config between subsequent calls and refreshes cache if config file is modified
		// Use this instead of Load() if you do not want to read the actual file everytime you fetch
		// the config.
		public static Config Get (string name)
		{
			Config config = (Config) configs [name];
			if (config != null)
				return config;

			config = Load (name);
			if (config != null) {
				configs [name] = config;
				mtimes [name] = DateTime.UtcNow;
				return config;
			}

			return LoadNew (name);
		}

		// Shorthands for these 4 popular configs
		public static Config FilesQueryable {
			get { return Get (Conf.Names.FilesQueryableConfig); }
		}

		public static Config Daemon {
			get { return Get (Conf.Names.DaemonConfig); }
		}

		public static Config BeagrepSearch {
			get { return Get (Conf.Names.BeagrepSearchConfig); }
		}

		public static Config Networking {
			get { return Get (Conf.Names.NetworkingConfig); }
		}

		private static string GlobalConfigDir {
			get { return Path.Combine (PathFinder.ConfigDataDir, "config-files"); }
		}

		// Utility method to check if global config files exist for the main config types
		public static bool CheckGlobalConfig ()
		{
			foreach (string name in new string[] {  Names.FilesQueryableConfig,
								Names.BeagrepSearchConfig,
								Names.DaemonConfig,
								Names.NetworkingConfig}) {
				string filename = (name + ".xml");
				string global_file = Path.Combine (GlobalConfigDir, filename);
				if (! File.Exists (global_file))
					return false;
			}

			return true;
		}

		// This is the core function to load, merge and return a config based on the local
		// and the global config file.
		// This should never return null since at least the global config file should be present
		// Still, its never hurts to check
		public static Config Load (string name)
		{
			string filename = (name + ".xml");

			string global_file = Path.Combine (GlobalConfigDir, filename);
			string local_file = Path.Combine (configs_dir, filename);

			Config merge_from = LoadFrom (local_file);
			Config merge_to = LoadFrom (global_file);

			if (merge_to == null)
				return merge_from;

			foreach (Option option in merge_to.Options.Values)
				option.Global = true;

			if (merge_from == null)
				return merge_to;

			foreach (Option option in merge_from.Options.Values) {
				option.Global = false;
				merge_to.Options [option.Name] = option;
			}

			return merge_to;
		}

		private static XmlSerializer conf_ser = new XmlSerializer (typeof (Config));

		public static Config LoadFrom (string path)
		{
			if (! File.Exists (path))
				return null;

			Config config = null;

			try {
				using (StreamReader reader = new StreamReader (path))
					config = (Config) conf_ser.Deserialize (reader);
			} catch (XmlException e) {
				Log.Error (e, "Unable to parse {0}, possibly corrupt file.", path);
				return null;
			}

			return config;
		}

		public static Config LoadNew (string name)
		{
			Config config = new Config ();
			config.Name = name;
			return config;
		}

		public static void Save (Config config)
		{
			if (config == null)
				return;

			bool to_save = false;
			foreach (Option option in config.Options.Values)
				if (! option.Global)
					to_save = true;

			if (! to_save)
				return;

			bool watching_for_updates_current = watching_for_updates;
			watching_for_updates = false;

			string filename = Path.Combine (configs_dir, (config.Name + ".xml"));

			using (StreamWriter writer = new StreamWriter (filename)) {
				conf_ser.Serialize (writer, config);
				Log.Debug ("Done writing to " + filename);
			}

			watching_for_updates = watching_for_updates_current;
		}

		public static void SaveTo (Config config, string path)
		{
			if (config == null)
				return;

			bool to_save = false;
			foreach (Option option in config.Options.Values)
				if (! option.Global)
					to_save = true;

			if (! to_save)
				return;

			using (StreamWriter writer = new StreamWriter (path)) {
				conf_ser.Serialize (writer, config);
				Console.WriteLine ("Done writing to " + path);
			}
		}

		/* External clients can use the following two xml based methods to read/write xml */

		// Use this method to write the config in its xml format.
		public static void WriteSectionXml (Config config, TextWriter writer)
		{
			if (config == null)
				return;

			// serialize will not serialize global options,
			// so temporarily make every option global
			Dictionary<Option, bool> global_options = new Dictionary<Option, bool> ();
			foreach (Option option in config.Options.Values) {
				global_options [option] = option.Global;
				option.Global = false;
			}

			conf_ser.Serialize (writer, config);

			foreach (Option option in config.Options.Values)
				option.Global = global_options [option];
		}

		// Use this method to send in the xml for a config, and the original config
		// and the changes from the original to the xml will be added to the original config
		// Returns false if the read xml does not correspond to the same config as that
		// was passed, in which case please note that the passed config COULD BE IN A
		// modified state and should be discarded!
		public static bool ReadSectionXml (Config config, TextReader reader)
		{
			if (config == null)
				throw new ArgumentException ("config", "config cannot be null");

			Config new_config = (Config) conf_ser.Deserialize (reader);

			foreach (Option new_option in new_config.Options.Values) {
				Option option = config [new_option.Name] as Option;
				if (option == null || (option.Type != new_option.Type))
					return false;

				switch (option.Type) {
				case OptionType.Bool:
					BoolOption option_bool = (BoolOption) option;
					BoolOption new_option_bool = (BoolOption) new_option;
					option_bool.Value = new_option_bool.Value;
					break;

				case OptionType.String:
					StringOption option_str = (StringOption) option;
					StringOption new_option_str = (StringOption) new_option;
					option_str.Value = new_option_str.Value;
					break;

				case OptionType.List:
					ListOption option_list = (ListOption) option;
					ListOption new_option_list = (ListOption) new_option;
					if (option_list.NumParams != new_option_list.NumParams)
						return false;

					option_list.Values = new_option_list.Values;
					break;
				}
			}

			return true;
		}

		private static void CheckOldConfig ()
		{
			// Load the local config files
			IndexingConfig indexing_config = (IndexingConfig) CreateOldConfig ("indexing.xml", typeof (IndexingConfig));
			SearchingConfig searching_config = (SearchingConfig) CreateOldConfig ("searching.xml", typeof (SearchingConfig));
			DaemonConfig daemon_config = (DaemonConfig) CreateOldConfig ("daemon.xml", typeof (DaemonConfig));
			NetworkingConfig networking_config = (NetworkingConfig) CreateOldConfig ("networking.xml", typeof (NetworkingConfig));

			if (indexing_config == null &&
			    searching_config == null &&
			    daemon_config == null &&
			    networking_config == null)
				return;

			Log.Info ("Old config files found. Moving them to the new format...");

			// Load the global config files
			Config global_fsq_config = LoadFrom (Path.Combine (GlobalConfigDir, Names.FilesQueryableConfig + ".xml"));
			foreach (Option option in global_fsq_config.Options.Values)
				option.Global = true;

			Config global_networking_config = LoadFrom (Path.Combine (GlobalConfigDir, Names.NetworkingConfig + ".xml"));
			foreach (Option option in global_networking_config.Options.Values)
				option.Global = true;

			Config global_bs_config = LoadFrom (Path.Combine (GlobalConfigDir, Names.BeagrepSearchConfig + ".xml"));
			foreach (Option option in global_bs_config.Options.Values)
				option.Global = true;

			Config global_daemon_config = LoadFrom (Path.Combine (GlobalConfigDir, Names.DaemonConfig + ".xml"));
			foreach (Option option in global_daemon_config.Options.Values)
				option.Global = true;

			// From indexing_config
			if (indexing_config != null) {
				List<string[]> roots = new List<string[]> (indexing_config.Roots.Count);
				foreach (string root in indexing_config.Roots)
					roots.Add (new string[1] {root});
				global_fsq_config.SetListOptionValues (Conf.Names.Roots, roots);

				global_fsq_config.SetOption (Conf.Names.IndexHomeDir, indexing_config.IndexHomeDir);
				global_daemon_config.SetOption (Conf.Names.IndexOnBattery, indexing_config.IndexOnBattery);
				global_daemon_config.SetOption (Conf.Names.IndexFasterOnScreensaver, indexing_config.IndexFasterOnScreensaver);

				List<string[]> excludes_path = new List<string[]> ();
				List<string[]> excludes_pattern = new List<string[]> ();
				List<string[]> excludes_mailfolder = new List<string[]> ();

				foreach (ExcludeItem exclude in indexing_config.Excludes) {
					if (exclude.Type == ExcludeType.Path)
						excludes_path.Add (new string[1] {exclude.Value});
					else if (exclude.Type == ExcludeType.Pattern)
						excludes_pattern.Add (new string[1] {exclude.Value});
					else if (exclude.Type == ExcludeType.MailFolder)
						excludes_mailfolder.Add (new string[1] {exclude.Value});
				}

				if (excludes_path.Count > 0)
					global_fsq_config.SetListOptionValues (Conf.Names.ExcludeSubdirectory, excludes_path);

				if (excludes_pattern.Count > 0)
					global_fsq_config.SetListOptionValues (Conf.Names.ExcludePattern, excludes_pattern);

				if (excludes_mailfolder.Count > 0)
					global_daemon_config.SetListOptionValues (Conf.Names.ExcludeMailfolder, excludes_mailfolder);

				List<string[]> maildirs = new List<string[]> (indexing_config.Maildirs.Count);
				foreach (IndexingConfig.Maildir maildir in indexing_config.Maildirs)
					maildirs.Add (new string[1] {maildir.Directory});
				global_daemon_config.SetListOptionValues (Conf.Names.Maildirs, maildirs);

				Log.Info ("Done reading old config: indexing");
			}

			// From networking_config
			if (networking_config != null) {
				global_networking_config.SetOption (Conf.Names.ServiceEnabled, networking_config.ServiceEnabled);
				global_networking_config.SetOption (Conf.Names.PasswordRequired, networking_config.PasswordRequired);
				global_networking_config.SetOption (Conf.Names.ServiceName, networking_config.ServiceName);
				global_networking_config.SetOption (Conf.Names.ServicePassword, networking_config.ServicePassword);

				List<string[]> svcs = new List<string[]> (networking_config.NetworkServices.Count);
				foreach (NetworkService svc in networking_config.NetworkServices)
					svcs.Add (new string[4] {svc.Name, svc.UriString, svc.IsProtected.ToString (), svc.Cookie});
				global_networking_config.SetListOptionValues (Conf.Names.NetworkServices, svcs);

				Log.Info ("Done reading old config: networking");
			}

			// From daemon_config
			if (daemon_config != null) {
				List<string[]> static_queryables = new List<string[]> (daemon_config.StaticQueryables.Count);
				foreach (string name in daemon_config.StaticQueryables)
					static_queryables.Add (new string[1] {name});
				global_daemon_config.SetListOptionValues (Conf.Names.StaticQueryables, static_queryables);

				List<string[]> denied_backends = new List<string[]> (daemon_config.DeniedBackends.Count);
				foreach (string name in daemon_config.DeniedBackends)
					denied_backends.Add (new string[1] {name});
				global_daemon_config.SetListOptionValues (Conf.Names.DeniedBackends, denied_backends);

				global_daemon_config.SetOption (Conf.Names.AllowStaticBackend, daemon_config.AllowStaticBackend);
				global_daemon_config.SetOption (Conf.Names.IndexSynchronization, daemon_config.IndexSynchronization);
				global_daemon_config.SetOption (Conf.Names.AllowRoot, daemon_config.AllowRoot);

				Log.Info ("Done reading old config: daemon");
			}

			// From searching_config
			if (searching_config != null) {
				KeyBinding binding = searching_config.ShowSearchWindowBinding;

				global_bs_config.SetOption (Conf.Names.KeyBinding_Key, binding.Key);
				global_bs_config.SetOption (Conf.Names.KeyBinding_Ctrl, binding.Ctrl);
				global_bs_config.SetOption (Conf.Names.KeyBinding_Alt, binding.Alt);

				global_bs_config.SetOption (Conf.Names.BeagrepPosX, searching_config.BeagrepPosX.ToString ());
				global_bs_config.SetOption (Conf.Names.BeagrepPosY, searching_config.BeagrepPosY.ToString ());
				global_bs_config.SetOption (Conf.Names.BeagrepSearchWidth, searching_config.BeagrepSearchWidth.ToString ());
				global_bs_config.SetOption (Conf.Names.BeagrepSearchHeight, searching_config.BeagrepSearchHeight.ToString ());
				global_bs_config.SetOption (Conf.Names.BeagrepSearchAutoSearch, searching_config.BeagrepSearchAutoSearch);

				List<string[]> history = new List<string[]> (searching_config.SearchHistory.Count);
				foreach (string s in searching_config.SearchHistory)
					history.Add (new string[1] {s});
				global_bs_config.SetListOptionValues (Conf.Names.SearchHistory, history);

				Log.Info ("Done reading old config: searching");
			}

			Conf.Save (global_daemon_config);
			Conf.Save (global_fsq_config);
			Conf.Save (global_networking_config);
			Conf.Save (global_bs_config);
		}

		private static Section CreateOldConfig (string filename, Type type)
		{
			string filepath = Path.Combine (configs_dir, filename);
			if (!File.Exists (filepath))
				return null;

			Log.Debug ("Loading old config {0} from {1}", type, filename);

			Section section = null;

			using (FileStream fs = File.Open (filepath, FileMode.Open, FileAccess.Read, FileShare.Read)) {
				try {
					XmlSerializer serializer = new XmlSerializer (type);
					section = (Section) serializer.Deserialize (fs);
				} catch (Exception e) {
					Log.Error (e, "Could not load configuration from {0}:", filename);
					return null;
				}
			}

			try {
				string backup_path = filepath + ".bak";
				Log.Info ("Done loading. Backing up old config {0} file to {1}", filepath, backup_path);
				Log.Info ("{0} is not needed anymore and can be deleted.", backup_path);
				File.Move (filepath, backup_path);
			} catch (Exception e) {
				Log.Error ("Could not move old config file {0}: {1}", filepath, e.Message);
				Log.Error ("Delete it manually.");
			}

			return section;
		}
	}

	[XmlRoot ("BeagrepConf")]
	public class Config {
		[XmlAttribute]
		public string Name = String.Empty;

		private Hashtable options = new Hashtable ();

		[XmlIgnore]
		public Hashtable Options {
			get { return options; }
		}

		/* Exposed only for serialization. Do not use. */
		public class HashtableEnumerator : IEnumerable {
			public Hashtable options;

			public HashtableEnumerator (Hashtable options)
			{
				this.options = options;
			}

			public void Add (object o)
			{
				Option option = (Option) o;
				options [option.Name] = option;
			}

			public IEnumerator GetEnumerator ()
			{
				ArrayList local_options = new ArrayList (options.Count);
				foreach (Option option in options.Values)
					if (! option.Global)
						local_options.Add (option);

				return local_options.GetEnumerator ();
			}
		}

		[XmlArrayItem (ElementName="BoolOption", Type=typeof (BoolOption))]
		[XmlArrayItem (ElementName="StringOption", Type=typeof (StringOption))]
		[XmlArrayItem (ElementName="ListOption", Type=typeof (ListOption))]
		/* Exposed only for serialization. Do not use. */
		public HashtableEnumerator options_enumerator {
			get { return new HashtableEnumerator (options); }
			set { options = value.options; }
		}

		public Option this [string option_name] {
			get { return (Option) options [option_name]; }
		}

		///////////// Utility Methods : Use Them /////////////

		public bool GetOption (string name, bool default_value)
		{
			BoolOption option = this [name] as BoolOption;
			if (option == null)
				return default_value;

			return option.Value;
		}

		public void SetOption (string name, bool value)
		{
			BoolOption option = this [name] as BoolOption;
			if (option == null) {
				if (name == null)
					throw new ArgumentException ("name cannot be null", "name");
				if (options.Contains (name))
					throw new ArgumentException ("cannot overwrite another option with the same name", "name");
				option = new BoolOption ();
				option.Name = name;
				option.Global = false;
				Options [name] = option;
			}

			option.Value = value;
		}

		public string GetOption (string name, string default_value)
		{
			StringOption option = this [name] as StringOption;
			if (option == null)
				return default_value;

			return option.Value;
		}

		public void SetOption (string name, string value)
		{
			StringOption option = this [name] as StringOption;
			if (option == null) {
				if (name == null)
					throw new ArgumentException ("name cannot be null", "name");
				if (options.Contains (name))
					throw new ArgumentException ("cannot overwrite another option with the same name", "name");
				option = new StringOption ();
				option.Name = name;
				option.Global = false;
				Options [name] = option;
			}

			option.Value = value;
		}

		public string[] GetListOptionParams (string name)
		{
			ListOption option = this [name] as ListOption;
			if (option == null)
				return null;

			return option.ParamNames;
		}

		public List<string[]> GetListOptionValues (string name)
		{
			ListOption option = this [name] as ListOption;
			if (option == null)
				return null;

			return option.Values;
		}

		public bool NewListOption (string name, char separator, string[] param_names)
		{
			ListOption option = this [name] as ListOption;
			if (option != null)
				return false;

			if (name == null)
				throw new ArgumentException ("name cannot be null", "name");
			if (param_names == null)
				throw new ArgumentException ("params cannot be null", "param_names");
			if (param_names.Length == 0)
				throw new ArgumentException ("params should contain non-zero values", "param_names");
			if (this [name] != null)
				throw new ArgumentException ("cannot overwrite another option with the same name", "name");

			option = new ListOption ();
			option.Name = name;
			option.Global = false;
			option.Separator = separator;
			option.Parameter_String = String.Join (separator.ToString (), param_names);
			Options [name] = option;

			return true;
		}

		public bool SetListOptionValues (string name, List<string[]> values)
		{
			ListOption option = this [name] as ListOption;
			if (option == null)
				return false;

			option.Values = values;
			return true;
		}

		public bool AddListOptionValue (string name, string[] values)
		{
			ListOption option = this [name] as ListOption;
			if (option == null)
				return false;

			int num_params = option.NumParams;
			// verify the number of values
			if (values == null || values.Length != num_params)
				throw new ArgumentException (String.Format ("Must be an array of {0} strings", num_params), "values");

			Array.Resize (ref option.Values_String, option.Count + 1);
			option.Values_String [option.Count - 1] = String.Join (option.Separator.ToString (), values);

			option.Global = false;
			return true;
		}

		public bool RemoveListOptionValue (string name, string[] values)
		{
			ListOption option = this [name] as ListOption;
			if (option == null)
				return false;

			int num_params = option.NumParams;
			// verify the number of values
			if (values == null || values.Length != num_params)
				throw new ArgumentException (String.Format ("Must be an array of {0} strings", num_params), "values");

			string value = String.Join (option.Separator.ToString (), values);

			bool found = false;
			for (int i = 0; i < option.Count; ++i) {
				if (found) {
					// FIXME: Assuming no duplicates
					// Shift everything one to the left after the value is found
					option.Values_String [i-1] = option.Values_String [i];
					continue;
				}

				if (option.Values_String [i] == value)
					found = true;
			}

			if (found) {
				// Now remove the last value
				Array.Resize (ref option.Values_String, option.Count - 1);
				option.Global = false;
			}

			return found;
		}
	}

	public enum OptionType {
		Bool = 0,
		String = 1,
		List = 2
	};

	public class Option {
		[XmlAttribute]
		public string Name = String.Empty;

		[XmlAttribute]
		public string Description = String.Empty;

		/* When saving, only the non-global (aka local) options are written to the disk */
		[XmlIgnore]
		internal bool Global = false;

		[XmlIgnore]
		public OptionType Type = OptionType.Bool;
	}

	//////////////////////////////////////////////////////////////////////////////////////////
	/* The classes below are exposed only for serialization. Use responsibly or do not use. */
	//////////////////////////////////////////////////////////////////////////////////////////

	public class BoolOption : Option {
		[XmlText]
		public string Value_String = String.Empty;

		[XmlIgnore]
		public bool Value {
			get {
				if (Value_String == String.Empty)
					return true; // default value
				return Convert.ToBoolean (Value_String);
			}
			set {
				string new_val = value.ToString ();
				if (String.Compare (new_val, Value_String, true) == 0)
					return;

				Value_String = new_val;
				Global = false;
			}
		}

		public BoolOption () : base ()
		{
			Type = OptionType.Bool;
		}
	}

	public class StringOption : Option {
		[XmlText]
		public string Value_String = String.Empty;

		[XmlIgnore]
		public string Value {
			get {
				if (Value_String == null)
					return String.Empty;
				return Value_String;
			}
			set {
				string new_val = value;
				if (new_val == Value_String)
					return;

				Value_String = new_val;
				Global = false;
			}
		}

		public StringOption () : base ()
		{
			Type = OptionType.String;
		}
	}

	public class ListOption : Option {

		[XmlIgnore]
		public char Separator = ',';

		[XmlAttribute (AttributeName = "Separator")]
		public string Separator_String {
			get { return Separator.ToString (); }
			set { if (value != null) Separator = value [0]; }
		}

		[XmlAttribute (AttributeName = "Params")]
		// Separated by "Separator"
		public string Parameter_String = String.Empty;

		public ListOption () : base ()
		{
			Type = OptionType.List;
		}

		[XmlArrayItem (ElementName = "Value", Type = typeof (string))]
		// Each value is separated by "Separator"
		public string[] Values_String = new string [0];

		[XmlIgnore]
		public int Count {
			get {
				if (Values_String == null)
					return 0;
				return Values_String.Length;
			}
		}

		[XmlIgnore]
		public string[] ParamNames {
			get { return Parameter_String.Split (new char [] {Separator}); }
		}

		[XmlIgnore]
		public int NumParams {
			get { return ParamNames.Length; }
		}

		[XmlIgnore]
		public List<string[]> Values {
			get {
				List<string[]> list = new List<string[]> (Count);
				if (Count == 0)
					return list;

				int num_params = NumParams;

				foreach (string value in Values_String) {
					// Skip the bad values
					string[] values = value.Split (new char [] {Separator});
					if (values == null || values.Length != num_params)
						continue;

					list.Add (values);
				}

				return list;
			}
			set {
				if (value == null)
					return;

				int num_params = NumParams;
				string[] values_string = new string[value.Count];

				// Verify that each string[] has num_params values
				for (int i = 0; i < value.Count; ++ i)  {
					string[] list_value = value [i];
					if (list_value == null || list_value.Length != num_params)
						throw new ArgumentException (String.Format ("Each list entry must be arrays of {0} strings", num_params), "values");
					values_string [i] = String.Join (Separator.ToString (), list_value);
				}

				if (ArrayFu.Equal (values_string, Values_String))
					return;

				Values_String = values_string;
				Global = false;
			}
		}
	}

	public class SearchingConfig : Section {

		private KeyBinding show_search_window_binding = new KeyBinding ("F12");
		public KeyBinding ShowSearchWindowBinding {
			get { return show_search_window_binding; }
			set { show_search_window_binding = value; }
		}

		// BeagrepSearch window position and dimension
		// stored as percentage of screen co-ordinates
		// to deal with change of resolution problem - hints from tberman

		private float beagrep_search_pos_x = 0;
		public float BeagrepPosX {
			get { return beagrep_search_pos_x; }
			set { beagrep_search_pos_x = value; }
		}

		private float beagrep_search_pos_y = 0;
		public float BeagrepPosY {
			get { return beagrep_search_pos_y; }
			set { beagrep_search_pos_y = value; }
		}

		private float beagrep_search_width = 0;
		public float BeagrepSearchWidth {
			get { return beagrep_search_width; }
			set { beagrep_search_width = value; }
		}

		private float beagrep_search_height = 0;
		public float BeagrepSearchHeight {
			get { return beagrep_search_height; }
			set { beagrep_search_height = value; }
		}

		// ah!We want a Queue but Queue doesnt serialize *easily*
		private ArrayList search_history = new ArrayList ();
		public ArrayList SearchHistory {
			get { return search_history; }
			set { search_history = value; }
		}

		private bool beagrep_search_auto_search = true;
		public bool BeagrepSearchAutoSearch {
			get { return beagrep_search_auto_search; }
			set { beagrep_search_auto_search = value; }
		}

	}

	public class DaemonConfig : Section {
		private ArrayList static_queryables = new ArrayList ();
		public ArrayList StaticQueryables {
			get { return static_queryables; }
			set { static_queryables = value; }
		}

		// By default, every backend is allowed.
		// Only maintain a list of denied backends.
		private ArrayList denied_backends = new ArrayList ();
		public ArrayList DeniedBackends {
			get { return denied_backends; }
			set { denied_backends = value; }
		}

		private bool allow_static_backend = false; // by default, false
		public bool AllowStaticBackend {
			get { return allow_static_backend; }
			// Don't really want to expose this, but serialization requires it
			set { allow_static_backend = value; }
		}

		private bool index_synchronization = true;
		public bool IndexSynchronization {
			get { return index_synchronization; }
			// Don't really want to expose this, but serialization requires it
			set { index_synchronization = value; }
		}

		private bool allow_root = false;
		public bool AllowRoot {
			get { return allow_root; }
			set { allow_root = value; }
		}
	}

	public class IndexingConfig : Section
	{
		private ArrayList roots = new ArrayList ();
		[XmlArray]
		[XmlArrayItem(ElementName="Root", Type=typeof(string))]
		public ArrayList Roots {
			get { return roots; }
			set { roots = value; }
		}

		private bool index_home_dir = true;
		public bool IndexHomeDir {
			get { return index_home_dir; }
			set { index_home_dir = value; }
		}

		private bool index_on_battery = false;
		public bool IndexOnBattery {
			get { return index_on_battery; }
			set { index_on_battery = value; }
		}

		private bool index_faster_on_screensaver = true;
		public bool IndexFasterOnScreensaver {
			get { return index_faster_on_screensaver; }
			set { index_faster_on_screensaver = value; }
		}

		private ArrayList excludes = new ArrayList ();
		[XmlArray]
		[XmlArrayItem (ElementName="ExcludeItem", Type=typeof(ExcludeItem))]
		public ArrayList Excludes {
			get { return excludes; }
			set { excludes = value; }
		}

		public struct Maildir {
			public string Directory;
			public string Extension;
		}

		private ArrayList maildirs = new ArrayList ();
		[XmlArray]
		[XmlArrayItem (ElementName="Maildir", Type=typeof(Maildir))]
		public ArrayList Maildirs {
			get { return maildirs; }
			set { maildirs = value; }
		}
	}

	public class NetworkingConfig : Section
	{
		// Index sharing service is disabled by default
		private bool service_enabled = false;

		// Password protect our local indexes
		private bool password_required = true;

		// The name and password for the local network service
		private string service_name = String.Format ("{0} ({1})", Environment.UserName, System.Environment.MachineName);
		private string service_password = String.Empty;

		// This is a list of registered and paired nodes which
		// the local client can search
		private ArrayList network_services = new ArrayList ();

		public bool ServiceEnabled {
			get { return service_enabled; }
			set { service_enabled = value; }
		}

		public bool PasswordRequired {
			get { return password_required; }
			set { password_required = value; }
		}

		public string ServiceName {
			get { return service_name; }
			set { service_name = value; }
		}

		public string ServicePassword {
			get { return service_password; }
			set { service_password = value; }
		}

		[XmlArray]
		[XmlArrayItem (ElementName="NetworkService", Type=typeof (NetworkService))]
		public ArrayList NetworkServices {
			get { return network_services; }
			set { network_services = value; }
		}
	}

	public class Section {
		[XmlIgnore]
		public bool SaveNeeded = false;
	}

	//////////////////////////////////////////////////////////////////////

	public enum ExcludeType {
		Path,
		Pattern,
		MailFolder
	}

	public class ExcludeItem {

		private ExcludeType type;
		private string val;

		[XmlAttribute]
		public ExcludeType Type {
			get { return type; }
			set { type = value; }
		}

		private string exactMatch;
		private string prefix;
		private string suffix;
		private Regex  regex;

		[XmlAttribute]
		public string Value {
			get { return val; }
			set {
				switch (type) {
				case ExcludeType.Path:
				case ExcludeType.MailFolder:
					prefix = value;
					break;

				case ExcludeType.Pattern:
					if (value.StartsWith ("/") && value.EndsWith ("/")) {
						regex = new Regex (value.Substring (1, value.Length - 2));
						break;
					}

					int i = value.IndexOf ('*');
					if (i == -1) {
						exactMatch = value;
					} else {
						if (i > 0)
							prefix = value.Substring (0, i);
						if (i < value.Length-1)
							suffix = value.Substring (i+1);
					}
					break;
				}

				val = value;
			}
		}

		public ExcludeItem () {}

		public ExcludeItem (ExcludeType type, string value) {
			this.Type = type;
			this.Value = value;
		}

		public bool IsMatch (string param)
		{
			switch (Type) {
			case ExcludeType.Path:
			case ExcludeType.MailFolder:
				if (prefix != null && ! param.StartsWith (prefix))
					return false;

				return true;

			case ExcludeType.Pattern:
				if (exactMatch != null)
					return param == exactMatch;
				if (prefix != null && ! param.StartsWith (prefix))
					return false;
				if (suffix != null && ! param.EndsWith (suffix))
					return false;
				if (regex != null && ! regex.IsMatch (param))
					return false;

				return true;
			}

			return false;
		}

		public override bool Equals (object obj)
		{
			ExcludeItem exclude = obj as ExcludeItem;
			return (exclude != null && exclude.Type == type && exclude.Value == val);
		}

		public override int GetHashCode ()
		{
			return (this.Value.GetHashCode () ^ (int) this.Type);
		}

	}

	//////////////////////////////////////////////////////////////////////

	public class KeyBinding {
		public string Key;

		[XmlAttribute]
		public bool Ctrl = false;
		[XmlAttribute]
		public bool Alt = false;

		public KeyBinding () {}
		public KeyBinding (string key) : this (key, false, false) {}

		public KeyBinding (string key, bool ctrl, bool alt)
		{
			Key = key;
			Ctrl = ctrl;
			Alt = alt;
		}

		public override string ToString ()
		{
			string result = "";

			if (Ctrl)
				result += "<Ctrl>";
			if (Alt)
				result += "<Alt>";

			result += Key;

			return result;
		}

		public string ToReadableString ()
		{
			return ToString ().Replace (">", "-").Replace ("<", "");
		}
	}
}
