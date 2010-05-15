//
// PathFinder.cs
//
// Copyright (C) 2004 Novell, Inc.
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
using System.IO;

namespace Beagle.Util {

	public class PathFinder {

		private PathFinder () { }

		static public string[] Paths {
			get {
				string env_var = Environment.GetEnvironmentVariable ("PATH");
				return env_var.Split (':');
			}
		}

		static public string PkgLibDir {
			get { return ExternalStringsHack.PkgLibDir; }
		}

		static public string FilterDir {
			get { return Path.Combine (PkgLibDir, "Filters"); }
		}

		static public string BackendDir {
			get { return Path.Combine (PkgLibDir, "Backends"); }
		}

		static private string LocalStateDir {
			get { return ExternalStringsHack.LocalStateDir; }
		}

		static public string SystemDir {
			get { return Path.Combine (Path.Combine (LocalStateDir, "cache"), "beagle"); }
		}

		static public string SystemIndexesDir {
			get { return Path.Combine (SystemDir, "indexes"); }
		}

		// The user's personal files are under this and their dotfiles are in it.
		// It is usually found via HOME, but that can be overridden by setting
		// BEAGLE_HOME
		static string home_dir;
		static object home_dir_lock = new object ();
		static public string HomeDir {
			get {
				lock (home_dir_lock) {
					if (home_dir == null) {
						home_dir = Environment.GetEnvironmentVariable ("BEAGLE_HOME");
						if (home_dir == null)
							home_dir = Environment.GetEnvironmentVariable ("HOME");
						if (home_dir == null)
							throw new Exception ("Couldn't get HOME or BEAGLE_HOME");
						if (home_dir.EndsWith ("/"))
							home_dir = home_dir.Remove (home_dir.Length - 1, 1);
						home_dir = Path.GetFullPath (home_dir);
						if (! Directory.Exists (home_dir))
							throw new Exception ("Home directory '"+home_dir+"' doesn't exist");
					}
				}
				
				return home_dir;
			}

			// You probably don't want to do this.
			set { lock (home_dir_lock) { home_dir = value; } }
		}

		// The storage directory is the place where beagle stores its private data.
		// Fun fact #1: By default this is ~/.beagle
		// Fun fact #2: It can be overridden by setting BEAGLE_STORAGE
		static string storage_dir;
		static object storage_dir_lock = new object ();
		static public string StorageDir {
			get {
				lock (storage_dir_lock) {
					if (storage_dir == null) {
						storage_dir = Environment.GetEnvironmentVariable ("BEAGLE_STORAGE");

						if (storage_dir == null)
							storage_dir = Path.Combine (HomeDir, ".beagle");
						else {
							if (storage_dir.EndsWith ("/"))
								storage_dir = storage_dir.Remove (storage_dir.Length - 1, 1);
							storage_dir = Path.GetFullPath (storage_dir);
						}

						if (! Directory.Exists (storage_dir)) {
							Directory.CreateDirectory (storage_dir);
							// Make sure that the directory is only
							// readable by the owner.
							Mono.Unix.Native.Syscall.chmod (storage_dir, (Mono.Unix.Native.FilePermissions) 448); // 448 == 0700
						}
					}
				}

				return storage_dir;
			}
			set { lock (storage_dir_lock) { storage_dir = value; } }
		}

		static string remote_storage_dir;
		static public string GetRemoteStorageDir (bool create)
		{
			if (remote_storage_dir == null) {
				bool index_synchronization = Conf.Daemon.GetOption (Conf.Names.IndexSynchronization, true);

				if ((! SystemInformation.IsPathOnBlockDevice (PathFinder.StorageDir) && index_synchronization) ||
				    Environment.GetEnvironmentVariable ("BEAGLE_SYNCHRONIZE_LOCALLY") != null) {
					string index_pointer = Path.Combine (StorageDir, "remote_storage_dir");

					if (File.Exists (index_pointer)) {
						StreamReader r = new StreamReader (index_pointer);
						remote_storage_dir = r.ReadLine ();
						r.Close ();

						if (!Directory.Exists (remote_storage_dir))
							remote_storage_dir = null;
					}

					if (create) {
						if (remote_storage_dir == null) {
							do {
								string p = String.Format ("beagle-{0}-{1}", Environment.GetEnvironmentVariable ("USER"),
											  Guid.NewGuid ().ToString ());

								remote_storage_dir = Path.Combine (Path.GetTempPath (), p);
							} while (Directory.Exists (remote_storage_dir) || File.Exists (remote_storage_dir));

							StreamWriter w = new StreamWriter (index_pointer);
							w.WriteLine (remote_storage_dir);
							w.Close ();


						}

						if (! Directory.Exists (remote_storage_dir)) {
							Directory.CreateDirectory (remote_storage_dir);
							// Make sure that the directory is only
							// readable by the owner.
							Mono.Unix.Native.Syscall.chmod (remote_storage_dir, (Mono.Unix.Native.FilePermissions) 448); // 448 == 0700
						}
					}
				} else
					remote_storage_dir = StorageDir;
			}

			return remote_storage_dir;
		}

		// The directory where beagle stores its indexes
		// Fun fact #1: It will be synchronized locally if PathFinder.HomeDir
		// is on a non-block device, or if BEAGLE_SYNCHRONIZE_LOCALLY is set.
		static string index_dir;
		static public string IndexDir {
			get { 
				if (index_dir == null) {
					index_dir = Path.Combine (GetRemoteStorageDir (true), "Indexes");
					
					if (! Directory.Exists (index_dir)) {
						Directory.CreateDirectory (index_dir);

						// Make sure that the directory is only readable by the owner. 
						// Required when using index synchronization as then it resides in /tmp
						Mono.Unix.Native.Syscall.chmod (index_dir, (Mono.Unix.Native.FilePermissions) 448); // 448 == 0700
					}
				}
				
				return index_dir;
			}
		}

		static public string LogDir {
			get {
				string dir = Path.Combine (StorageDir, "Log");
				if (! Directory.Exists (dir))
					Directory.CreateDirectory (dir);
				return dir;
			}
		}

		// Location of the global config files.
		// Once installted, it is Sysconfdir/beagle
		// Otherwise it is $BEAGLE_CONF_DIR
		static string config_data_dir = null;
		static public string ConfigDataDir {
			get {
				if (config_data_dir == null) {
					config_data_dir = Environment.GetEnvironmentVariable ("BEAGLE_CONF_DIR");
					if (config_data_dir == null)
						config_data_dir = Path.Combine (ExternalStringsHack.SysConfDir, "beagle");

					if (config_data_dir.EndsWith ("/"))
						config_data_dir = config_data_dir.Remove (config_data_dir.Length - 1, 1);
					config_data_dir = Path.GetFullPath (config_data_dir);
					if (! Directory.Exists (config_data_dir))
						throw new Exception ("Global config directory '"+config_data_dir+"' doesn't exist");
				}

				return config_data_dir;
			}
		}
	}
}
