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
using Beagrep.Util;
using System.Text.RegularExpressions;

namespace Beagrep.Filters {

        public class FilterText : Beagrep.Daemon.Filter {

                static bool sourceCodeTagsMode = false;
                static SafeProcess ctagsPc;
                static StreamReader ctagsOutput;
                static StreamWriter ctagsInput;

                static string getLangMap() {
                        SafeProcess langmapPc = new SafeProcess();
                        string[] args = new string[1];
                        args[0] = "lang-map-for-ctags";
                        langmapPc.Arguments = args;
                        langmapPc.Start();
                        return new StreamReader(langmapPc.StandardOutput).ReadToEnd();
                }

                static FilterText() {
                        if (Environment.GetEnvironmentVariable("SOURCECODETAGSMODE") == "true") {
                                sourceCodeTagsMode = true;
                                ctagsPc = new SafeProcess ();

                                string[] args = new string[7];

                                args[0] = "ctags-ajoke";
                                args[1] = "--langmap=" + getLangMap();
                                args[2] = "-xu";
                                args[3] = "--filter";
                                args[4] = "--filter-terminator=###terminator###\n";
                                args[5] = "--extra=+q";
                                args[6] = "--c-kinds=+p";
                                ctagsPc.Arguments = args;
                                ctagsPc.RedirectStandardOutput = true;
                                ctagsPc.RedirectStandardInput = true;
                                ctagsPc.Start();
                                ctagsInput = new StreamWriter(ctagsPc.StandardInput);
                                ctagsOutput = new StreamReader(ctagsPc.StandardOutput);
                        }
                }

                public FilterText ()
                {
                        SnippetMode = true;
                        OriginalIsText = true;
                        SetFileType ("document");
                }

                protected override void RegisterSupportedTypes ()
                {
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xhtml+xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-sln"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/xml"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-perl"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-ruby"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/x-designer"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("application/vnd.nokia.xml.qt.resource"));
                        AddSupportedFlavor (FilterFlavor.NewFromMimeType ("text/*"));
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
                        if (sourceCodeTagsMode) {

                                if (file.FullName.Contains(" ")) {
                                        Beagrep.Util.Logger.Log.Debug ("{0} is not source code since path contains space!", file.FullName);
                                        Error();
                                        return;
                                }
                                ctagsInput.Write(file.FullName + "\n");
                                ctagsInput.Flush();
                                filepath = file.FullName;
                        }
                }

                const int BUFSIZE = 2048;
                char[] buf;
                string filepath;
                override protected void DoPull ()
                {
                        bool pull = false;
                        if (sourceCodeTagsMode) {
                                Regex rx = new Regex("^(.+?\\s)\\s*\\S+\\s+\\d+\\s+(\\S+)");
                                do {
                                        string s = ctagsOutput.ReadLine();
                                        if (s == "###terminator###") {
                                                Finished ();
                                                break;
                                        } else {
                                                MatchCollection matches = rx.Matches(s);
                                                foreach (Match match in matches) {
                                                        GroupCollection groups = match.Groups;
                                                        pull = AppendChars(groups[1].Value.ToCharArray(), 0, groups[1].Length);
                                                }
                                        }
                                } while (pull);
                        } else {
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
}
