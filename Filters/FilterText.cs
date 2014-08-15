//
// FilterText.cs
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

using Beagrep.Daemon;

namespace Beagrep.Filters {

        public class FilterText : Beagrep.Daemon.Filter {

                public FilterText ()
                {
                        SnippetMode = true;
                        OriginalIsText = true;
                        SetFileType ("document");
                }

                protected override void RegisterSupportedTypes ()
                {
                        AddSupportedFlavor (Beagrep.Daemon.FilterFlavor.NewFromMimeType ("text/x-bibtex"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xhtml+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-sln"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-perl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-ruby"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-designer"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/html"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/plain"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-authors"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-boo"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-c"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-c++"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-chdr"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-c-header"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-comma-separated-values"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-copying"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-credits"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-csharp"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-c++src"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-csrc"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-fortran"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-install"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-java"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-latex"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-log"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-pascal"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-patch")); // patch files
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-perl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-php"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-python"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-readme"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-scheme"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-emacs-lisp"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-sun-c-file"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-sun-h-file"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-tex"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-texinfo"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-troff"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-troff-man"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-xslt"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-markdown"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-shellscript"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-yaml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xslt+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-php"));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".sci"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-sh"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("image/svg+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".svg"));
                        AddSupportedFlavor(FilterFlavor.NewFromMimeType ("application/x-tex"));
                        AddSupportedFlavor(FilterFlavor.NewFromMimeType ("application/x-latex"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/docbook+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".docbook"));
                        AddSupportedFlavor (new FilterFlavor ("file:///usr/share/doc/*",              ".xml", null, 0));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".js"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-troff-man"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-troff"));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".m"));
                        AddSupportedFlavor (FilterFlavor.NewFromExtension (".ini"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.mozilla.xul+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xml-dtd"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/css"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/troff"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-c++hdr"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-dsrc"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-idl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-makefile"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-matlab"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-ms-regedit"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-objcsrc"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-rpm-spec"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-troff-mm"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-vhdl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/atom+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/rdf+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/rss+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xml-dtd"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-wine-extension-inf"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-csh"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/mbox"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-awk"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-mozilla-bookmarks"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("message/rfc822"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("message/news"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/css"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/troff"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-dsrc"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-gettext-translation"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-makefile"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-matlab"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-ocaml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-sql"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-uri"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/x-vhdl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("addsapplication/xspf+xml"));
                }

                const long LENGTH_CUTOFF = 5 * 1024 * 1024; // 5 Mb

                override protected void DoOpen (FileInfo file)
                {
                        // Extremely large files of type text/plain are usually log files,
                        // data files, or other bits of not-particularly-human-readable junk
                        // that will tend to clog up our indexes.
                        if (file.Length > LENGTH_CUTOFF) {
                                Beagrep.Util.Logger.Log.Debug ("{0} is too large to filter!", file.FullName);
                                Error ();
                                return;
                        }

                        buf = new char [BUFSIZE];
                }

                const int BUFSIZE = 2048;
                char[] buf;
                override protected void DoPull ()
                {
                        bool pull = false;
                        do {
                                int read = TextReader.Read (buf, 0, BUFSIZE);
                                if (read == 0) {
                                        Finished ();
                                        break;
                                } else {
                                        pull = AppendChars (buf, 0, read);
                                }
                        } while (pull);
                }
        }
}
