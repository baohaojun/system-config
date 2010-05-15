
FILTERDIR = $(pkglibdir)/Filters
CSC = $(MCS) -debug

TARGET = Filters.dll

CSFLAGS = -target:library $(BEAGLE_DEFINES)

# Found at http://sharptoolbox.madgeek.com/
HAP_CSFILES =						\
	$(srcdir)/HtmlAgilityPack/crc32.cs		\
	$(srcdir)/HtmlAgilityPack/Header.cs		\
	$(srcdir)/HtmlAgilityPack/HtmlAttribute.cs	\
	$(srcdir)/HtmlAgilityPack/HtmlDocument.cs	\
	$(srcdir)/HtmlAgilityPack/HtmlEntity.cs		\
	$(srcdir)/HtmlAgilityPack/HtmlNode.cs		\
	$(srcdir)/HtmlAgilityPack/HtmlNodeNavigator.cs	\
	$(srcdir)/HtmlAgilityPack/MixedCodeDocument.cs	\
	$(srcdir)/HtmlAgilityPack/tools.cs

include 7zip/7zip.sources

# Local copy of http://svn.myrealbox.com/viewcvs/trunk/mcs/class/Managed.Windows.Forms/System.Windows.Forms.RTF/
SWF_RTF_CSFILES =						\
	$(srcdir)/System.Windows.Forms.RTF/Charcode.cs		\
	$(srcdir)/System.Windows.Forms.RTF/Charset.cs		\
	$(srcdir)/System.Windows.Forms.RTF/CharsetFlags.cs	\
	$(srcdir)/System.Windows.Forms.RTF/CharsetType.cs	\
	$(srcdir)/System.Windows.Forms.RTF/ClassDelegate.cs	\
	$(srcdir)/System.Windows.Forms.RTF/Color.cs		\
	$(srcdir)/System.Windows.Forms.RTF/DestinationDelegate.cs \
	$(srcdir)/System.Windows.Forms.RTF/Font.cs		\
	$(srcdir)/System.Windows.Forms.RTF/KeysInit.cs		\
	$(srcdir)/System.Windows.Forms.RTF/KeyStruct.cs		\
	$(srcdir)/System.Windows.Forms.RTF/Major.cs		\
	$(srcdir)/System.Windows.Forms.RTF/Minor.cs		\
	$(srcdir)/System.Windows.Forms.RTF/RTF.cs		\
	$(srcdir)/System.Windows.Forms.RTF/RTFException.cs	\
	$(srcdir)/System.Windows.Forms.RTF/StandardCharCode.cs	\
	$(srcdir)/System.Windows.Forms.RTF/StandardCharName.cs	\
	$(srcdir)/System.Windows.Forms.RTF/Style.cs		\
	$(srcdir)/System.Windows.Forms.RTF/StyleElement.cs	\
	$(srcdir)/System.Windows.Forms.RTF/StyleType.cs		\
	$(srcdir)/System.Windows.Forms.RTF/TextMap.cs		\
	$(srcdir)/System.Windows.Forms.RTF/TokenClass.cs

CSFILES = 				\
	$(srcdir)/AssemblyInfo.cs	\
	$(HAP_CSFILES)			\
	$(SWF_RTF_CSFILES)		\
	$(7ZIP_CSFILES)			\
	$(srcdir)/FilterAbiword.cs	\
	$(srcdir)/FilterArchive.cs	\
	$(srcdir)/FilterAudio.cs	\
	$(srcdir)/FilterBibTex.cs	\
	$(srcdir)/FilterBmp.cs		\
	$(srcdir)/FilterBoo.cs		\
	$(srcdir)/FilterC.cs		\
	$(srcdir)/FilterCSharp.cs	\
	$(srcdir)/FilterCpp.cs		\
	$(srcdir)/FilterDeb.cs		\
	$(srcdir)/FilterDesktop.cs	\
	$(srcdir)/FilterDocbook.cs	\
	$(srcdir)/FilterEbuild.cs	\
	$(srcdir)/FilterExternal.cs	\
	$(srcdir)/FilterEmpathyLog.cs	\
	$(srcdir)/FilterFortran.cs	\
	$(srcdir)/FilterGif.cs		\
	$(srcdir)/FilterHtml.cs		\
	$(srcdir)/FilterIgnore.cs	\
	$(srcdir)/FilterImage.cs	\
	$(srcdir)/FilterInfo.cs		\
	$(srcdir)/FilterJava.cs		\
	$(srcdir)/FilterJpeg.cs		\
	$(srcdir)/FilterJs.cs		\
	$(srcdir)/FilterKAddressBook.cs \
	$(srcdir)/FilterKCal.cs		\
	$(srcdir)/FilterKNotes.cs	\
	$(srcdir)/FilterKOrganizer.cs	\
	$(srcdir)/FilterKonqHistory.cs	\
	$(srcdir)/FilterKopeteLog.cs	\
	$(srcdir)/FilterLabyrinth.cs	\
	$(srcdir)/FilterLisp.cs		\
	$(srcdir)/FilterM3U.cs		\
	$(srcdir)/FilterMPlayerVideo.cs	\
	$(srcdir)/FilterMail.cs		\
	$(srcdir)/FilterMan.cs		\
	$(srcdir)/FilterMatlab.cs	\
	$(srcdir)/FilterMonodoc.cs	\
	$(srcdir)/FilterOpenOffice.cs	\
	$(srcdir)/FilterPackage.cs	\
	$(srcdir)/FilterPascal.cs	\
	$(srcdir)/FilterPdf.cs		\
	$(srcdir)/FilterPerl.cs		\
	$(srcdir)/FilterPhp.cs		\
	$(srcdir)/FilterPidginLog.cs	\
	$(srcdir)/FilterPls.cs		\
	$(srcdir)/FilterPng.cs		\
	$(srcdir)/FilterPython.cs	\
	$(srcdir)/FilterRPM.cs		\
	$(srcdir)/FilterRTF.cs		\
	$(srcdir)/FilterRuby.cs		\
	$(srcdir)/FilterScilab.cs	\
	$(srcdir)/FilterScribus.cs	\
	$(srcdir)/FilterShellscript.cs	\
	$(srcdir)/FilterSource.cs	\
	$(srcdir)/FilterSpreadsheet.cs	\
	$(srcdir)/FilterSvg.cs		\
	$(srcdir)/FilterTeX.cs		\
	$(srcdir)/FilterTexi.cs		\
	$(srcdir)/FilterText.cs	        \
	$(srcdir)/FilterTiff.cs		\
	$(srcdir)/FilterTotem.cs	\
	$(srcdir)/FilterVideo.cs	\
	$(srcdir)/FilterXslt.cs

########################################################

if ENABLE_GSF_SHARP
CSFILES += 				\
	$(srcdir)/FilterOle.cs		\
	$(srcdir)/FilterPPT.cs

if ENABLE_WV1
CSFILES += \
	$(srcdir)/FilterDOC.cs		
endif
endif

COND_CS_FILES = \
	$(srcdir)/FilterOle.cs		\
	$(srcdir)/FilterPPT.cs		\
	$(srcdir)/FilterDOC.cs

########################################################

if HAS_LIBCHM
CSFILES += 				\
	$(srcdir)/FilterChm.cs
endif

COND_CS_FILES += $(srcdir)/FilterChm.cs

########################################################

LOCAL_ASSEMBLIES =				\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	../beagled/BeagleDaemonPlugins.dll

ASSEMBLIES =				\
	$(BEAGLED_LIBS)			\
	$(SHARPZIPLIB_LIBS)		\
	$(TAGLIB_SHARP_LIBS)		\
	-r:Mono.Posix			\
	-r:System.Web			\
	$(LOCAL_ASSEMBLIES:%=-r:%)

########################################################

RESOURCES =

$(TARGET): $(CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(CSFILES) $(ASSEMBLIES) $(RESOURCES)

all: $(TARGET)

install-data-local: $(TARGET)
	$(mkinstalldirs) $(DESTDIR)$(FILTERDIR)
	$(INSTALL_DATA) $(TARGET) $(TARGET).mdb $(DESTDIR)$(FILTERDIR)

uninstall-local:
	rm -f $(DESTDIR)$(FILTERDIR)/$(TARGET)
	rm -f $(DESTDIR)$(FILTERDIR)/$(TARGET).mdb

EXTRA_DIST =			\
	$(CSFILES)		\
	$(COND_CS_FILES)

CLEANFILES =		\
	$(TARGET)	\
	$(TARGET).mdb
