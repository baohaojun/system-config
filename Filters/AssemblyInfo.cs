//
// AssemblyInfo.cs
//
// Copyright (C) 2006 Novell, Inc.
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

using Beagle.Filters;

// All (non-abstract) filter types have to be listed here to be loaded.
[assembly: Beagle.Daemon.FilterTypes (
	 typeof(FilterAbiWord),
	 typeof(FilterApplication),
	 typeof(FilterArchive),
	 typeof(FilterAudio),
	 typeof(FilterBibTex),
	 typeof(FilterBMP),
	 typeof(FilterBoo),
	 typeof(FilterC),
#if HAVE_LIBCHM
	 typeof(FilterChm),
#endif
	 typeof(FilterCpp),
	 typeof(FilterCSharp),
	 typeof(FilterDeb),
	 typeof(FilterDesktop),
#if ENABLE_WV1
	 typeof(FilterDOC),
#endif
	 typeof(FilterDocbook),
	 typeof(FilterEbuild),
	 typeof(FilterExternal),
	 typeof(FilterEmpathyLog),
	 typeof(FilterFortran),
	 typeof(FilterGif),
	 typeof(FilterHtml),
	 typeof(FilterIgnore),
	 typeof(FilterInfo),
	 typeof(FilterJava),
	 typeof(FilterJavascript),
	 typeof(FilterJpeg),
	 typeof(FilterKAddressBook),
	 typeof(FilterKOrganizer),
	 typeof(FilterKnotes),
	 typeof(FilterKonqHistory),
	 typeof(FilterKopeteLog),
	 typeof(FilterLabyrinth),
	 typeof(FilterM3U),
	 typeof(FilterMPlayerVideo),
	 typeof(FilterMail),
	 typeof(FilterMan),
	 typeof(FilterMatlab),
	 typeof(FilterMonodoc),
	 typeof(FilterOpenOffice),
	 typeof(FilterPascal),
	 typeof(FilterPdf),
	 typeof(FilterPerl),
	 typeof(FilterPhp),
	 typeof(FilterPidginLog),
	 typeof(FilterPng),
#if ENABLE_GSF_SHARP
	 typeof(FilterPPT),
#endif
	 typeof(FilterPls),
	 typeof(FilterPython),
	 typeof(FilterRPM),
	 typeof(FilterRTF),
	 typeof(FilterRuby),
	 typeof(FilterScheme),
	 typeof(FilterScilab),
	 typeof(FilterScribus),
	 typeof(FilterShellscript),
	 typeof(FilterSpreadsheet),
	 typeof(FilterSvg),
	 typeof(FilterTeX),
	 typeof(FilterTexi),
	 typeof(FilterText),
	 typeof(FilterTiff),
	 typeof(FilterTotem),
	 typeof(FilterVideo),
	 typeof(FilterXslt)
)]
