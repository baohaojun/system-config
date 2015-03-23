//
// BeagrepDaemon.cs
//
// Copyright (C) 2004-2006 Novell, Inc.
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
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;
using Thread = System.Threading.Thread;
using GLib;

using Beagrep.Util;
using Log = Beagrep.Util.Log;
using Stopwatch = Beagrep.Util.Stopwatch;

namespace Beagrep.Daemon {

        class BeagrepDaemon {

                public static Thread MainLoopThread = null;
                private static MainLoop main_loop = null;

                private static Server server = null;

                private static bool arg_replace = false;
                private static bool arg_disable_scheduler = false;
                private static bool arg_indexing_test_mode = false;
                private static bool arg_heap_shot = false;
                private static bool arg_heap_shot_snapshots = true;

                private static bool disable_textcache = false;
                public static bool DisableTextCache {
                        get { return disable_textcache; }
                        set { disable_textcache = value; }
                }

#if ENABLE_AVAHI
                private static Beagrep.Daemon.Network.Zeroconf zeroconf = null;
#endif

                public static bool StartServer ()
                {
                        Logger.Log.Debug ("Starting messaging server");

                        try {
                                server = new Server ("socket", false, Conf.Networking.GetOption (Conf.Names.ServiceEnabled, false));
                                server.Start ();
                        } catch (InvalidOperationException) {
                                return false;
                        }

                        return true;
                }

                public static void ReplaceExisting ()
                {
                        Log.Always ("Attempting to replace another beagrepd.");

                        do {
                                ShutdownRequest request = new ShutdownRequest ();
                                Logger.Log.Info ("Sending Shutdown");
                                request.Send ();
                                // Give it a second to shut down the messaging server
                                Thread.Sleep (1000);
                        } while (! StartServer ());
                }

                private static int prev_rss = -1;
                private static long prev_gc = -1;
                private static int sigprof_count = 0;

                private static void MaybeSendSigprof (int rss, long gc)
                {
                        bool send_sigprof = false;

                        try {
                                if (prev_rss == -1 || prev_gc == -1)
                                        return;

                                // Log RSS increases of at least 5 megs or 5%
                                if (rss - prev_rss >= 5 * 1024 ||
                                    (double) rss / (double) prev_rss >= 1.05)
                                        send_sigprof = true;

                                // Log total object size increase of at least 10%.
                                if ((double) gc / (double) prev_gc >= 1.1)
                                        send_sigprof = true;
                        } finally {
                                prev_rss = rss;
                                prev_gc = gc;

                                if (send_sigprof) {
                                        Log.Debug ("Suspicious memory size change detected.  Sending SIGPROF to ourself ({0})", sigprof_count++);
                                        Mono.Unix.Native.Syscall.kill (System.Diagnostics.Process.GetCurrentProcess ().Id, Mono.Unix.Native.Signum.SIGPROF);
                                }
                        }
                }

                private static void LogMemoryUsage ()
                {
                        while (! Shutdown.ShutdownRequested) {
                                SystemInformation.LogMemoryUsage ();

                                int vm_rss = SystemInformation.VmRss;

                                if (arg_heap_shot && arg_heap_shot_snapshots)
                                        MaybeSendSigprof (vm_rss, GC.GetTotalMemory (false));

                                if (vm_rss > 300 * 1024) {
                                        Logger.Log.Debug ("VmRss too large --- shutting down");
                                        Shutdown.BeginShutdown ();
                                }

                                Thread.Sleep (5000);
                        }
                }

                private static void PrintUsage ()
                {
                        VersionFu.PrintHeader ();

                        string usage =
                                "Usage: beagrepd [OPTIONS]\n\n" +
                                "Options:\n" +
                                "  --foreground, --fg\tRun the daemon in the foreground.\n" +
                                "  --background, --bg\tRun the daemon in the background.\n" +
                                "  --backend\t\tConfigure which backends to use.  Specifically:\n" +
                                "    --backend <name>\tOnly start backend 'name'\n" +
                                "    --backend +<name>\tAdditionally start backend 'name'\n" +
                                "    --backend -<name>\tDisable backend 'name'\n" +
                                "  --replace\t\tReplace a running daemon with a new instance.\n" +
                                "  --list-backends\tList all the available backends.\n" +
                                "  --add-static-backend\tAdd a static backend by path.\n" +
                                "  --help\t\tPrint this usage message.\n" +
                                "  --version\t\tPrint version information.\n" +
                                "\n" +
                                "Advanced options:\n" +
                                "  --debug\t\tWrite out debugging information.\n" +
                                "  --mono-debug\t\tShow mono related debugging information.\n" +
                                "  --debug-memory\tWrite out debugging information about memory use.\n" +
                                "  --indexing-test-mode\tRun in foreground, and exit when fully indexed.\n" +
                                "  --indexing-delay <t>\tWait 't' seconds before indexing.  (Default 60 seconds)\n" +
                                "  --disable-scheduler\tDisable the use of the scheduler.\n" +
                                "  --disable-text-cache\tDisable the use of the text cache used to provide snippets.\n";

                        Console.WriteLine (usage);
                }

                public static bool StartupProcess ()
                {
                        // Profile our initialization
                        Stopwatch stopwatch = new Stopwatch ();
                        stopwatch.Start ();

                        // Fire up our server
                        if (! StartServer ()) {
                                if (! arg_replace) {
                                        Logger.Log.Error ("Could not set up the listener for beagrep requests.  "
                                                          + "There is probably another beagrepd instance running.  "
                                                          + "Use --replace to replace the running service");
                                        Environment.Exit (1);
                                }

                                ReplaceExisting ();
                        }

                        // Set up out-of-process indexing
                        LuceneQueryable.IndexerHook = new LuceneQueryable.IndexerCreator (RemoteIndexer.NewRemoteIndexer);

                        Config config = Conf.Get (Conf.Names.DaemonConfig);

                        // Initialize synchronization to keep the indexes local if PathFinder.StorageDir
                        // is on a non-block device, or if BEAGREP_SYNCHRONIZE_LOCALLY is set

                        if ((! SystemInformation.IsPathOnBlockDevice (PathFinder.StorageDir) &&
                             config.GetOption (Conf.Names.IndexSynchronization, true)) ||
                            Environment.GetEnvironmentVariable ("BEAGREP_SYNCHRONIZE_LOCALLY") != null)
                                IndexSynchronization.Initialize ();

                        // Start the query driver.
                        Logger.Log.Debug ("Starting QueryDriver");
                        QueryDriver.Start ();

                        // Start our battery monitor so we can shut down the
                        // scheduler if needed.
                        BatteryMonitor.Init ();

                        bool initially_on_battery = ! BatteryMonitor.UsingAC && ! config.GetOption (Conf.Names.IndexOnBattery, false);

                        // Start the Global Scheduler thread
                        if (! arg_disable_scheduler) {
                                if (! initially_on_battery) {
                                        Logger.Log.Debug ("Starting Scheduler thread");
                                        Scheduler.Global.Start ();
                                } else {
                                        Log.Debug ("Beagrep started on battery, not starting scheduler thread");
                                }
                        }

                        // Start our Inotify threads
                        Inotify.Start ();

                        // Test if the FileAdvise stuff is working: This will print a
                        // warning if not.  The actual advice calls will fail silently.
                        FileAdvise.TestAdvise ();

#if ENABLE_AVAHI
                        zeroconf = new Beagrep.Daemon.Network.Zeroconf ();
#endif

                        Conf.WatchForUpdates ();

                        stopwatch.Stop ();

                        Logger.Log.Debug ("Daemon initialization finished after {0}", stopwatch);

                        SystemInformation.LogMemoryUsage ();

                        if (arg_indexing_test_mode) {
                                Thread.Sleep (1000); // Ugly paranoia: wait a second for the backends to settle.
                                Logger.Log.Debug ("Running in indexing test mode");
                                Scheduler.Global.EmptyQueueEvent += OnEmptySchedulerQueue;
                                Scheduler.Global.Add (null); // pulse the scheduler
                        }

                        return false;
                }

                private static void OnEmptySchedulerQueue ()
                {
                        Logger.Log.Debug ("Scheduler queue is empty: terminating immediately");
                        Shutdown.BeginShutdown ();
                        Environment.Exit (0); // Ugly work-around: We need to call Exit here to avoid deadlocking.
                }

                public static void Main (string[] args)
                {
                        try {
                                DoMain (args);
                        } catch (Exception ex) {
                                Logger.Log.Error (ex, "Unhandled exception thrown.  Exiting immediately.");
                                Environment.Exit (1);
                        }
                }

                [DllImport("libgobject-2.0.so.0")]
                static extern void g_type_init ();

                public static void DoMain (string[] args)
                {
                        SystemInformation.InternalCallInitializer.Init ();
                        SystemInformation.SetProcessName ("beagrepd");

                        // Process the command-line arguments
                        bool arg_debug = false;
                        bool arg_debug_memory = false;
                        bool arg_fg = false;

                        int i = 0;
                        while (i < args.Length) {

                                string arg = args [i];
                                ++i;
                                string next_arg = i < args.Length ? args [i] : null;

                                switch (arg) {
                                case "-h":
                                case "--help":
                                        PrintUsage ();
                                        Environment.Exit (0);
                                        break;

                                case "--mdb":
                                case "--mono-debug":
                                        // Silently ignore these arguments: they get handled
                                        // in the wrapper script.
                                        break;

                                case "--list-backends":
                                        Console.WriteLine ("Current available backends:");
                                        Console.Write (QueryDriver.ListBackends ());
                                        Environment.Exit (0);
                                        break;

                                case "--fg":
                                case "--foreground":
                                        arg_fg = true;
                                        break;

                                case "--bg":
                                case "--background":
                                        arg_fg = false;
                                        break;

                                case "--replace":
                                        arg_replace = true;
                                        break;

                                case "--debug":
                                        arg_debug = true;
                                        break;

                                case "--heap-shot":
                                        arg_heap_shot = true;
                                        arg_debug = true;
                                        arg_debug_memory = true;
                                        break;

                                case "--no-snapshots":
                                case "--no-snapshot":
                                        arg_heap_shot_snapshots = false;
                                        break;

                                case "--heap-buddy":
                                case "--debug-memory":
                                        arg_debug = true;
                                        arg_debug_memory = true;
                                        break;

                                case "--indexing-test-mode":
                                        arg_indexing_test_mode = true;
                                        arg_fg = true;
                                        break;

                                case "--backend":
                                        if (next_arg == null) {
                                                Console.WriteLine ("--backend requires a backend name");
                                                Environment.Exit (1);
                                                break;
                                        }

                                        if (next_arg.StartsWith ("--")) {
                                                Console.WriteLine ("--backend requires a backend name. Invalid name '{0}'", next_arg);
                                                Environment.Exit (1);
                                                break;
                                        }

                                        if (next_arg [0] != '+' && next_arg [0] != '-')
                                                QueryDriver.OnlyAllow (next_arg);
                                        else {
                                                if (next_arg [0] == '+')
                                                        QueryDriver.Allow (next_arg.Substring (1));
                                                else
                                                        QueryDriver.Deny (next_arg.Substring (1));
                                        }

                                        ++i; // we used next_arg
                                        break;

                               case "--add-static-backend":
                                        if (next_arg != null)
                                                QueryDriver.AddStaticQueryable (next_arg);
                                        ++i;
                                        break;

                                case "--disable-scheduler":
                                        arg_disable_scheduler = true;
                                        break;

                                case "--indexing-delay":
                                        if (next_arg != null) {
                                                try {
                                                        QueryDriver.IndexingDelay = Int32.Parse (next_arg);
                                                } catch {
                                                        Console.WriteLine ("'{0}' is not a valid number of seconds", next_arg);
                                                        Environment.Exit (1);
                                                }
                                        }

                                        ++i;
                                        break;

                                case "--autostarted":
                                        // FIXME: This option is deprecated and will be removed in a future release.
                                        break;

                                case "--disable-text-cache":
                                        disable_textcache = true;
                                        break;

                                case "--version":
                                        VersionFu.PrintVersion ();
                                        Environment.Exit (0);
                                        break;

                                default:
                                        Console.WriteLine ("Unknown argument '{0}'", arg);
                                        Environment.Exit (1);
                                        break;

                                }
                        }

                        if (Environment.GetEnvironmentVariable ("SABAYON_SESSION_RUNNING") == "yes") {
                                Console.WriteLine ("Beagrep is running underneath Sabayon, exiting.");
                                Environment.Exit (0);
                        }

                        if (arg_indexing_test_mode) {
                                LuceneQueryable.OptimizeRightAway = true;
                        }

                        // Bail out if we are trying to run as root
                        if (Environment.UserName == "root" && Environment.GetEnvironmentVariable ("SUDO_USER") != null) {
                                Console.WriteLine ("You appear to be running beagrep using sudo.  This can cause problems with");
                                Console.WriteLine ("permissions in your .beagrep and .wapi directories if you later try to run");
                                Console.WriteLine ("as an unprivileged user.  If you need to run beagrep as root, please use");
                                Console.WriteLine ("'su -c' instead.");
                                Environment.Exit (-1);
                        }

                        if (Environment.UserName == "root" && ! Conf.Daemon.GetOption (Conf.Names.AllowRoot, false)) {
                                Console.WriteLine ("You can not run beagrep as root.  Beagrep is designed to run from your own");
                                Console.WriteLine ("user account.  If you want to create multiuser or system-wide indexes, use");
                                Console.WriteLine ("the beagrep-build-index tool.");
                                Console.WriteLine ();
                                Console.WriteLine ("You can override this setting using the beagrep-config or beagrep-settings tools.");
                                Environment.Exit (-1);
                        }

                        try {
                                string tmp = PathFinder.HomeDir;
                        } catch (Exception e) {
                                Console.WriteLine ("Unable to start the daemon: {0}", e.Message);
                                Environment.Exit (-1);
                        }

                        MainLoopThread = Thread.CurrentThread;

                        // FIXME: We always turn on full debugging output!  We are still
                        // debugging this code, after all...
                        // arg_debug ? LogLevel.Debug : LogLevel.Warn

                        Log.Initialize (PathFinder.LogDir, "Beagrep", LogLevel.Debug, arg_fg);
                        Log.Always ("Starting Beagrep Daemon (version {0})", ExternalStringsHack.Version);
                        Log.Always ("Running on {0}", SystemInformation.MonoRuntimeVersion);
                        Log.Always ("Command Line: {0}",
                                           Environment.CommandLine != null ? Environment.CommandLine : "(null)");

                        if (! ExtendedAttribute.Supported) {
                                Logger.Log.Warn ("Extended attributes are not supported on this filesystem. " +
                                                 "Performance will suffer as a result.");
                        }

                        if (disable_textcache) {
                                Log.Warn ("Running with text-cache disabled!");
                                Log.Warn ("*** Snippets will not be returned for documents indexed in this session.");
                        }

                        // Check if global configuration files are installed
                        if (! Conf.CheckGlobalConfig ()) {
                                Console.WriteLine ("Global configuration files not found in '{0}'", PathFinder.ConfigDataDir);
                                Environment.Exit (-1);
                        }

                        // Start our memory-logging thread
                        if (arg_debug_memory) {
                                ExceptionHandlingThread.Start (new ThreadStart (LogMemoryUsage));
                        }

                        // Do BEAGREP_EXERCISE_THE_DOG_HARDER-related processing.
                        ExerciseTheDogHarder ();

                        // Initialize GObject type system
                        g_type_init ();

                        QueryDriver.Init ();
                        Server.Init ();

#if MONO_1_9
                        Shutdown.SetupSignalHandlers (new Shutdown.SignalHandler (HandleSignal));
#else
                        SetupSignalHandlers ();
#endif

                        Shutdown.ShutdownEvent += OnShutdown;

                        main_loop = new MainLoop ();
                        Shutdown.RegisterMainLoop (main_loop);

                        // Defer all actual startup until the main loop is
                        // running.  That way shutdowns during the startup
                        // process work correctly.
                        GLib.Idle.Add (new GLib.IdleHandler (StartupProcess));

                        // Start our event loop.
                        main_loop.Run ();

                        // We're out of the main loop now, join all the
                        // running threads so we can exit cleanly.
                        ExceptionHandlingThread.JoinAllThreads ();

                        // If we placed our sockets in a temp directory, try to clean it up
                        // Note: this may fail because the helper is still running
                        if (PathFinder.GetRemoteStorageDir (false) != PathFinder.StorageDir) {
                                try {
                                        Directory.Delete (PathFinder.GetRemoteStorageDir (false));
                                } catch (IOException) { }
                        }

                        Log.Always ("Beagrep daemon process shut down cleanly.");
                }

                /////////////////////////////////////////////////////////////////////////////

                private static void SetupSignalHandlers ()
                {
                        // Force OurSignalHandler to be JITed
                        OurSignalHandler (-1);

                        // Set up our signal handler
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGINT, OurSignalHandler);
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGTERM, OurSignalHandler);
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGUSR1, OurSignalHandler);
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGUSR2, OurSignalHandler);

                        // Ignore SIGPIPE
                        Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGPIPE, Mono.Unix.Native.Stdlib.SIG_IGN);

                        // Work around a mono feature/bug
                        // https://bugzilla.novell.com/show_bug.cgi?id=381928
                        // When beagrep crashes, mono will try to print a stack trace and then call abort()
                        // The abort somehow calls back into beagrep and causes a deadlock
                        if (Environment.GetEnvironmentVariable ("BEAGREP_MONO_DEBUG_FLAG_IS_SET") == null)
                                Mono.Unix.Native.Stdlib.signal (Mono.Unix.Native.Signum.SIGABRT, Mono.Unix.Native.Stdlib.SIG_DFL);
                }

                // Mono signal handler allows setting of global variables;
                // anything else e.g. function calls, even reentrant native methods are risky
                private static void OurSignalHandler (int signal)
                {
                        // This allows us to call OurSignalHandler w/o doing anything.
                        // We want to call it once to ensure that it is pre-JITed.
                        if (signal < 0)
                                return;

                        // Set shutdown flag to true so that other threads can stop initializing
                        if ((Mono.Unix.Native.Signum) signal != Mono.Unix.Native.Signum.SIGUSR1 &&
                            (Mono.Unix.Native.Signum) signal != Mono.Unix.Native.Signum.SIGUSR2)
                                Shutdown.ShutdownRequested = true;

                        // Do all signal handling work in the main loop and not in the signal handler.
                        GLib.Idle.Add (new GLib.IdleHandler (delegate () { HandleSignal (signal); return false; }));
                }

                private static void HandleSignal (int signal)
                {
                        Logger.Log.Debug ("Handling signal {0} ({1})", signal, (Mono.Unix.Native.Signum) signal);

                        // Pass the signals to the helper too.
                        GLib.Idle.Add (new GLib.IdleHandler (delegate ()
                                                            {
                                                                    RemoteIndexer.SignalRemoteIndexer ((Mono.Unix.Native.Signum) signal);
                                                                    return false;
                                                            }));

                        // If we get SIGUSR1, turn the debugging level up.
                        if ((Mono.Unix.Native.Signum) signal == Mono.Unix.Native.Signum.SIGUSR1) {
                                LogLevel old_level = Log.Level;
                                Log.Level = LogLevel.Debug;
                                Log.Debug ("Moving from log level {0} to Debug", old_level);
                                return;
                        } else if ((Mono.Unix.Native.Signum) signal == Mono.Unix.Native.Signum.SIGUSR2) {
                                // Debugging hook for beagrepd
                                QueryDriver.DebugHook ();
                                LuceneCommon.DebugHook ();
                                return;
                        }

                        Logger.Log.Debug ("Initiating shutdown in response to signal.");
                        Shutdown.BeginShutdown ();
                }

                /////////////////////////////////////////////////////////////////////////////

                private static void OnShutdown ()
                {
#if ENABLE_AVAHI
                        zeroconf.Dispose ();
#endif
                        // Stop our Inotify threads
                        Inotify.Stop ();

                        // Stop the global scheduler and ask it to shutdown
                        Scheduler.Global.Stop (true);

                        // Stop the messaging server
                        if (server != null)
                                server.Stop ();
                }

                /////////////////////////////////////////////////////////////////////////////

                private static ArrayList exercise_files = new ArrayList ();

                private static void ExerciseTheDogHarder ()
                {
                        string path;
                        path = Environment.GetEnvironmentVariable ("BEAGREP_EXERCISE_THE_DOG_HARDER");
                        if (path == null)
                                return;

                        DirectoryInfo dir = new DirectoryInfo (path);
                        foreach (FileInfo file in dir.GetFiles ())
                                exercise_files.Add (file);
                        if (exercise_files.Count == 0)
                                return;

                        int N = 5;
                        if (N > exercise_files.Count)
                                N = exercise_files.Count;

                        for (int i = 0; i < N; ++i)
                                ExceptionHandlingThread.Start (new ThreadStart (ExerciseTheDogHarderWorker));
                }

                private static void ExerciseTheDogHarderWorker ()
                {
                        Random rng = new Random ();

                        while (! Shutdown.ShutdownRequested) {

                                FileInfo file = null;
                                int i;


                                lock (exercise_files) {
                                        do {
                                                i = rng.Next (exercise_files.Count);
                                                file = exercise_files [i] as FileInfo;
                                        } while (file == null);
                                        exercise_files [i] = null;
                                }

                                string target;
                                target = Path.Combine (PathFinder.HomeDir, "_HARDER_" + file.Name);

                                Logger.Log.Debug ("ETDH: Copying {0}", file.Name);
                                file.CopyTo (target, true);

                                lock (exercise_files)
                                        exercise_files [i] = file;

                                Thread.Sleep (500 + rng.Next (500));
                        }
                }
        }

}
