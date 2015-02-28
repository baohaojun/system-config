//
// ExtendedAttribute.cs
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

// FIXME: This is not portable to Win32

using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;
using Mono.Unix.Native;

namespace Beagrep.Util {

        public class ExtendedAttribute {
                static byte[] xa_buffer = new byte [4096];

                // FIXME: Deprecate post 0.3.3
                private static string AddPrefix (string name)
                {
                        return "user.Beagrep." + name;
                }

                private const string AttrName = "user.Beagrep";

                static Encoding encoding = new UTF8Encoding ();

                // Set a beagrep attribute
                public static void Set (string path, string value) {
                        Set (path, AttrName, value);
                }

                // Set a non-beagrep attribute
                private static void Set (string path, string name, string value)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        byte[] buffer = encoding.GetBytes (value);
                        int retval = Syscall.lsetxattr (path, name, buffer);
                        if (retval == -1)
                                throw new IOException ("Could not set extended attribute on " + path + ": " + Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError ()));
                }

                // Check if a beagrep attribute exists
                public static bool Exists (string path)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        long size = Syscall.lgetxattr (path, AttrName, null, 0);
                        return size >= 0;
                }

                // FIXME: Deprecate post 0.3.3
                public static bool OldExists (string path, string name)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        name = AddPrefix (name);

                        long size = Syscall.lgetxattr (path, name, null, 0);
                        return size >= 0;
                }

                // Get a beagrep attribute
                public static string Get (string path)
                {
                        return Get (path, AttrName);
                }

                // Get a non-beagrep attribute
                public static string Get (string path, string name)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        long size = Syscall.lgetxattr (path, name, xa_buffer);
                        if (size <= 0)
                                return null;
                        return encoding.GetString (xa_buffer);
                }

                // Remove a beagrep attribute
                public static void Remove (string path)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        int retval = Syscall.lremovexattr (path, AttrName);
                        if (retval != 0)
                                throw new IOException ("Could not remove extended attribute on " + path + ": " + Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError ()));
                }

                // FIXME: Deprecate post 0.3.3
                public static void RemoveOld (string path, string name)
                {
                        Remove (path, AddPrefix (name));
                }

                // Remove a non-beagrep attribute
                private static void Remove (string path, string name)
                {
                        if (! FileSystem.Exists (path))
                                throw new IOException (path);

                        int retval = Syscall.lremovexattr (path, name);
                        if (retval != 0)
                                throw new IOException ("Could not remove extended attribute on " + path + ": " + Mono.Unix.Native.Stdlib.strerror (Mono.Unix.Native.Stdlib.GetLastError ()));
                }

                // Check to see if it is possible to get and set attributes on a given file.
                public static bool Test (string path)
                {
                        const string test_key = "user.Beagrep.__test_key__";
                        const string test_value = "__test_value__";

                        try {
                                Set (path, test_key, test_value);
                                if (Get (path, test_key) != test_value)
                                        return false;
                                Remove (path, test_key);
                                return Get (path, test_key) == null;

                        } catch {
                                return false;
                        }
                }

                private static bool ea_support_tested = false;
                private static bool ea_supported = false;

                public static bool Supported {
                        get {
                                if (ea_support_tested)
                                        return ea_supported;

                                if (Environment.GetEnvironmentVariable ("BEAGREP_DISABLE_XATTR") != null) {
                                        Logger.Log.Debug ("BEAGREP_DISABLE_XATTR is set");
                                        ea_supported = false;
                                        ea_support_tested = true;
                                        return false;
                                }

                                ea_supported = Test (PathFinder.HomeDir);
                                ea_support_tested = true;

                                return ea_supported;
                        }
                }
        }
}
