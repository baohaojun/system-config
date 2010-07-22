autostartdir = $(sysconfdir)/xdg/autostart
autostart_DATA = beagled-autostart.desktop

############################################################

############################################################

# Warnings we don't want to see.
# Most of these are thanks to DotLucene.  It would be nice if we could use
# "#pragma warning" on these.
#
# 0162 = Unreachable code detected
# 0164 = This label has not been referenced
# 0168 = The variable 'foo' is declared but never used
# 0169 = The private method/field 'foo' is never used
# 0219 = Variable is assigned but never used

CSC = $(MCS) -debug -nowarn:0162,0164,0168,0169,0219,0436

BACKENDDIR = $(pkglibdir)/Backends

PLUGIN_TARGET = BeagleDaemonPlugins.dll

PLUGIN_CSFLAGS = -target:library $(BEAGLE_DEFINES)

PLUGIN_CSFILES =				\
	$(srcdir)/Flavor.cs			\
	$(srcdir)/Filter.cs

PLUGIN_LOCAL_ASSEMBLIES =			\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll

PLUGIN_ASSEMBLIES =				\
	-r:Mono.Posix				\
	$(BEAGLED_LIBS)                 	\
	$(PLUGIN_LOCAL_ASSEMBLIES:%=-r:%)


$(PLUGIN_TARGET): $(PLUGIN_CSFILES) $(PLUGIN_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(PLUGIN_CSFLAGS) $(PLUGIN_CSFILES) $(PLUGIN_ASSEMBLIES)

############################################################

WRAPPER_IN = wrapper.in
WRAPPER_SED = sed 					\
	-e "s|\@prefix\@|$(prefix)|g"			\
	-e "s|\@pkglibdir\@|$(pkglibdir)|g"		\
	-e "s|\@libdir\@|$(libdir)|g"			\
	-e "s|\@evolibdir\@|$(EVO_LIBDIR)|g"		\
	-e "s|\@gacprefix\@|$(GAC_PREFIX)|g"		\
	-e "s|\@bash\@|$(BASH)|"			\
	-e "s|\@wrapper\@|$@|g"

############################################################

include $(srcdir)/Lucene.Net/Makefile.include

LUCENE_CSFILES = $(LUCENE_2_1_CSFILES)

############################################################

DAEMON_DLL_TARGET = BeagleDaemonLib.dll

DAEMON_DLL_CSFLAGS = -target:library $(BEAGLE_DEFINES)

fsqueryabledir = $(srcdir)/FileSystemQueryable
FILE_SYSTEM_QUERYABLE_CSFILES =					\
	$(fsqueryabledir)/DirectoryModel.cs			\
	$(fsqueryabledir)/DirectoryState.cs			\
	$(fsqueryabledir)/TreeCrawlTask.cs			\
	$(fsqueryabledir)/FileCrawlTask.cs			\
	$(fsqueryabledir)/DirectoryIndexableGenerator.cs	\
	$(fsqueryabledir)/FileNameFilter.cs			\
	$(fsqueryabledir)/InternalUriManager.cs			\
	$(fsqueryabledir)/FileSystemQueryable.cs		\
	$(fsqueryabledir)/IFileEventBackend.cs			\
	$(fsqueryabledir)/InotifyBackend.cs			\
	$(fsqueryabledir)/NullFileEventBackend.cs		\
	$(fsqueryabledir)/XmpSidecarStore.cs



indexingservicequeryable = $(srcdir)/IndexingServiceQueryable
INDEXING_SERVICE_QUERYABLE_CSFILES =				\
	$(indexingservicequeryable)/IndexingServiceQueryable.cs

DAEMON_DLL_CSFILES = 						\
	$(LUCENE_CSFILES)					\
	$(SNOWBALL_CSFILES)					\
	$(KMAIL_QUERYABLE_CSFILES)				\
	$(FILE_SYSTEM_QUERYABLE_CSFILES)			\
	$(PIDGIN_QUERYABLE_CSFILES)				\
	$(INDEXING_SERVICE_QUERYABLE_CSFILES)			\
	$(TOMBOY_QUERYABLE_CSFILES)				\
	$(LABYRINTH_QUERYABLE_CSFILES)				\
	$(BLAM_QUERYABLE_CSFILES)				\
	$(LIFEREA_QUERYABLE_CSFILES)				\
	$(AKREGATOR_QUERYABLE_CSFILES)				\
	$(KONQHISTORY_QUERYABLE_CSFILES)			\
	$(KONQBOOKMARK_QUERYABLE_CSFILES)			\
	$(KNOTES_QUERYABLE_CSFILES)				\
	$(KORGANIZER_QUERYABLE_CSFILES)				\
	$(KABC_QUERYABLE_CSFILES)				\
	$(KOPETE_QUERYABLE_CSFILES)				\
	$(KONVERSATION_QUERYABLE_CSFILES)			\
	$(OPERA_QUERYABLE_CSFILES)				\
	$(NAUTILUS_QUERYABLE_CSFILES)				\
	$(NETWORK_SERVICES_QUERYABLE_CSFILES)			\
	$(EMPATHY_QUERYABLE_CSFILES)				\
	$(LOCATE_QUERYABLE_CSFILES)				\
	$(srcdir)/AssemblyInfo.cs				\
	$(srcdir)/ExternalMetadataQueryable.cs			\
	$(srcdir)/FileAttributes.cs				\
	$(srcdir)/FileAttributesStore.cs			\
	$(srcdir)/FileAttributesStore_ExtendedAttribute.cs	\
	$(srcdir)/FileAttributesStore_Mixed.cs			\
	$(srcdir)/FileAttributesStore_Sqlite.cs			\
	$(srcdir)/FilterFactory.cs				\
	$(srcdir)/IFileAttributesStore.cs			\
	$(srcdir)/IIndexer.cs					\
	$(srcdir)/IndexSynchronization.cs			\
	$(srcdir)/IndexerReceipts.cs				\
	$(srcdir)/IndexerRequest.cs				\
	$(srcdir)/InformationalMessagesRequestExecutor.cs	\
	$(srcdir)/IQueryResult.cs				\
	$(srcdir)/IQueryWorker.cs               		\
	$(srcdir)/IQueryable.cs					\
	$(srcdir)/LuceneBitArray.cs				\
	$(srcdir)/LuceneCommon.cs				\
	$(srcdir)/LuceneIndexingDriver.cs			\
	$(srcdir)/LuceneQueryingDriver.cs			\
	$(srcdir)/LuceneFileQueryable.cs			\
	$(srcdir)/LuceneQueryable.cs				\
	$(srcdir)/NoiseFilter.cs				\
	$(srcdir)/PropertyKeywordFu.cs				\
	$(srcdir)/QueryDriver.cs				\
	$(srcdir)/QueryExecutor.cs				\
	$(srcdir)/QueryResult.cs				\
	$(srcdir)/QueryStringParser.cs				\
	$(srcdir)/Queryable.cs					\
	$(srcdir)/QueryableFlavor.cs				\
	$(srcdir)/RemoteControlExecutors.cs			\
	$(srcdir)/RemoteIndexerRequest.cs			\
	$(srcdir)/RemoteIndexerResponse.cs			\
	$(srcdir)/Server.cs					\
	$(srcdir)/Shutdown.cs					\
	$(srcdir)/SnippetExecutor.cs				\
	$(srcdir)/SnippetFu.cs					\
	$(srcdir)/SqliteUtils.cs				\
	$(srcdir)/StaticQueryable.cs				\
	$(srcdir)/TextCache.cs					\
	$(srcdir)/WebServer.cs

DAEMON_DLL_LOCAL_ASSEMBLIES =					\
	../Util/Util.dll					\
	../BeagleClient/Beagle.dll				\
	./$(PLUGIN_TARGET)

DAEMON_DLL_ASSEMBLIES = 					\
	$(BEAGLED_LIBS)                 			\
	$(DAEMON_DLL_LOCAL_ASSEMBLIES:%=-r:%)			\
	$(SHARPZIPLIB_LIBS)					\
	$(NDESK_DBUS_GLIB_LIBS)					\
	-r:Mono.Data.Sqlite					\
	-r:Mono.Posix

if ENABLE_AVAHI
DAEMON_DLL_CSFLAGS += -define:ENABLE_AVAHI
DAEMON_DLL_ASSEMBLIES += $(AVAHI_SHARP_LIBS)
DAEMON_DLL_CSFILES +=						\
	$(srcdir)/Zeroconf.cs
endif

snowballdir = $(srcdir)/Snowball.Net

SNOWBALL_CSFILES =							\
	$(snowballdir)/Lucene.Net/Analysis/Snowball/SnowballAnalyzer.cs	\
	$(snowballdir)/Lucene.Net/Analysis/Snowball/SnowballFilter.cs	\
	$(snowballdir)/SF/Snowball/Ext/ItalianStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/RussianStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/NorwegianStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/EnglishStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/German2Stemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/FinnishStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/PortugueseStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/SpanishStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/FrenchStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/DanishStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/SwedishStemmer.cs		\
	$(snowballdir)/SF/Snowball/Ext/DutchStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/GermanStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/LovinsStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/KpStemmer.cs			\
	$(snowballdir)/SF/Snowball/Ext/PorterStemmer.cs			\
	$(snowballdir)/SF/Snowball/Among.cs				\
	$(snowballdir)/SF/Snowball/SnowballProgram.cs

############################################################


$(DAEMON_DLL_TARGET): $(DAEMON_DLL_CSFILES) $(DAEMON_DLL_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(DAEMON_DLL_CSFLAGS) $(DAEMON_DLL_CSFILES) $(DAEMON_DLL_ASSEMBLIES)

############################################################

###
### Pluggable backend support
###

BACKEND_CSFLAGS = -target:library
BACKEND_LOCAL_ASSEMBLIES =			\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(DAEMON_DLL_TARGET)

BACKEND_ASSEMBLIES =				\
	$(SHARPZIPLIB_LIBS)			\
	$(BACKEND_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

############################################################

DAEMON_WRAPPER = beagled
DAEMON_WRAPPER_IN = beagled.in
DAEMON_TARGET = BeagleDaemon.exe

$(DAEMON_WRAPPER): $(srcdir)/$(DAEMON_WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(DAEMON_TARGET)|g" < $(srcdir)/$(DAEMON_WRAPPER_IN) > $@
	chmod +x $(DAEMON_WRAPPER)

DAEMON_CSFLAGS =				\
	-target:exe				\
	$(BEAGLE_DEFINES)

DAEMON_CSFILES =				\
	$(srcdir)/AssemblyInfo.cs		\
	$(srcdir)/BeagleDaemon.cs		\
	$(srcdir)/RemoteIndexer.cs

DAEMON_LOCAL_ASSEMBLIES =			\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

DAEMON_ASSEMBLIES =				\
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(NDESK_DBUS_GLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

if ENABLE_AVAHI
DAEMON_CSFLAGS += -define:ENABLE_AVAHI
endif

$(DAEMON_TARGET): $(DAEMON_CSFILES) $(DAEMON_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(DAEMON_CSFLAGS) $(DAEMON_CSFILES) $(DAEMON_ASSEMBLIES)

############################################################
############################################################
############################################################

# This next section is a very good example of why we need real mono support in
# automake.  This is a massive wart on our development platform.

INDEX_HELPER_WRAPPER = beagled-index-helper
INDEX_HELPER_WRAPPER_IN = $(INDEX_HELPER_WRAPPER).in
INDEX_HELPER_TARGET = IndexHelper.exe

$(INDEX_HELPER_WRAPPER): $(srcdir)/$(INDEX_HELPER_WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(INDEX_HELPER_TARGET)|g" < $(srcdir)/$(INDEX_HELPER_WRAPPER_IN) > $@
	chmod +x $(INDEX_HELPER_WRAPPER)

INDEX_HELPER_CSFLAGS = -target:exe $(BEAGLE_DEFINES)

indexhelperdir = $(srcdir)/IndexHelper

INDEX_HELPER_CSFILES =	\
	$(indexhelperdir)/IndexHelper.cs		\
	$(indexhelperdir)/RemoteIndexerExecutor.cs


INDEX_HELPER_LOCAL_ASSEMBLIES =		\
	../Util/Util.dll		\
	../BeagleClient/Beagle.dll	\
	./$(DAEMON_DLL_TARGET)		\
	./$(PLUGIN_TARGET)

INDEX_HELPER_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(INDEX_HELPER_LOCAL_ASSEMBLIES:%=-r:%)	\
	$(NDESK_DBUS_GLIB_LIBS)			\
	-r:Mono.Posix

$(INDEX_HELPER_TARGET): $(INDEX_HELPER_CSFILES) $(INDEX_HELPER_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(INDEX_HELPER_CSFLAGS) $(INDEX_HELPER_CSFILES) $(INDEX_HELPER_ASSEMBLIES)

############################################################

EXTRACT_CONTENT_WRAPPER = beagle-extract-content
EXTRACT_CONTENT_TARGET = ExtractContent.exe

$(EXTRACT_CONTENT_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(EXTRACT_CONTENT_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(EXTRACT_CONTENT_WRAPPER)


EXTRACT_CONTENT_CSFLAGS = \
	-target:exe	  \
	$(BEAGLE_DEFINES)

EXTRACT_CONTENT_CSFILES = \
	$(srcdir)/ExtractContent.cs		\
	$(srcdir)/NoiseFilter.cs

EXTRACT_CONTENT_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

EXTRACT_CONTENT_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(EXTRACT_CONTENT_TARGET): $(EXTRACT_CONTENT_CSFILES) $(EXTRACT_CONTENT_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(EXTRACT_CONTENT_CSFLAGS) $(EXTRACT_CONTENT_CSFILES) $(EXTRACT_CONTENT_ASSEMBLIES)

############################################################

DUMP_INDEX_WRAPPER = beagle-dump-index
DUMP_INDEX_TARGET = DumpIndex.exe

$(DUMP_INDEX_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(DUMP_INDEX_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(DUMP_INDEX_WRAPPER)

DUMP_INDEX_CSFLAGS = \
	-target:exe

DUMP_INDEX_CSFILES = \
	$(srcdir)/DumpIndex.cs

DUMP_INDEX_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

DUMP_INDEX_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(DUMP_INDEX_TARGET): $(DUMP_INDEX_CSFILES) $(DUMP_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(DUMP_INDEX_CSFLAGS) $(DUMP_INDEX_CSFILES) $(DUMP_INDEX_ASSEMBLIES)

############################################################

MASTER_DELETE_WRAPPER = beagle-master-delete-button
MASTER_DELETE_TARGET = MasterDelete.exe

$(MASTER_DELETE_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(MASTER_DELETE_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(MASTER_DELETE_WRAPPER)


MASTER_DELETE_CSFLAGS =	\
	-target:exe

MASTER_DELETE_CSFILES =	\
	$(srcdir)/MasterDelete.cs

MASTER_DELETE_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

MASTER_DELETE_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(MASTER_DELETE_TARGET): $(MASTER_DELETE_CSFILES) $(MASTER_DELETE_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(MASTER_DELETE_CSFLAGS) $(MASTER_DELETE_CSFILES) $(MASTER_DELETE_ASSEMBLIES)

############################################################

SNIPPET_TEST_WRAPPER = beagle-snippet-test
SNIPPET_TEST_TARGET = SnippetTest.exe

$(SNIPPET_TEST_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(SNIPPET_TEST_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(SNIPPET_TEST_WRAPPER)

SNIPPET_TEST_CSFLAGS = \
	-target:exe

SNIPPET_TEST_CSFILES = \
	$(srcdir)/SnippetTest.cs

SNIPPET_TEST_LOCAL_ASSEMBLIES =	\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

SNIPPET_TEST_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(SNIPPET_TEST_TARGET): $(SNIPPET_TEST_CSFILES) $(SNIPPET_TEST_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(SNIPPET_TEST_CSFLAGS) $(SNIPPET_TEST_CSFILES) $(SNIPPET_TEST_ASSEMBLIES)

############################################################

BUILD_INDEX_WRAPPER = beagle-build-index
BUILD_INDEX_TARGET = BuildIndex.exe

$(BUILD_INDEX_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(BUILD_INDEX_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(BUILD_INDEX_WRAPPER)

BUILD_INDEX_CSFLAGS = \
	-target:exe

BUILD_INDEX_CSFILES = \
	$(srcdir)/BuildIndex.cs

BUILD_INDEX_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

BUILD_INDEX_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	$(NDESK_DBUS_GLIB_LIBS)			\
	-r:Mono.Posix

$(BUILD_INDEX_TARGET): $(BUILD_INDEX_CSFILES) $(BUILD_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(BUILD_INDEX_CSFLAGS) $(BUILD_INDEX_CSFILES) $(BUILD_INDEX_ASSEMBLIES)

############################################################

MANAGE_INDEX_WRAPPER = beagle-manage-index
MANAGE_INDEX_TARGET = ManageIndex.exe

$(MANAGE_INDEX_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(MANAGE_INDEX_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(MANAGE_INDEX_WRAPPER)

MANAGE_INDEX_CSFLAGS = \
	-target:exe

MANAGE_INDEX_CSFILES = \
	$(srcdir)/ManageIndex.cs

MANAGE_INDEX_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

MANAGE_INDEX_ASSEMBLIES = \
	$(BEAGLED_LIBS)				\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(MANAGE_INDEX_TARGET): $(MANAGE_INDEX_CSFILES) $(MANAGE_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(MANAGE_INDEX_CSFLAGS) $(MANAGE_INDEX_CSFILES) $(MANAGE_INDEX_ASSEMBLIES)

############################################################

REMOVABLE_INDEX_WRAPPER = beagle-removable-index
REMOVABLE_INDEX_TARGET = RemovableIndex.exe

$(REMOVABLE_INDEX_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(REMOVABLE_INDEX_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(REMOVABLE_INDEX_WRAPPER)

REMOVABLE_INDEX_CSFLAGS = \
	-target:exe

REMOVABLE_INDEX_CSFILES = \
	$(srcdir)/RemovableIndexTool.cs

REMOVABLE_INDEX_LOCAL_ASSEMBLIES = 		\
	../Util/Util.dll			\
	../BeagleClient/Beagle.dll

REMOVABLE_INDEX_ASSEMBLIES = \
	$(REMOVABLE_INDEX_LOCAL_ASSEMBLIES:%=-r:%)

$(REMOVABLE_INDEX_TARGET): $(REMOVABLE_INDEX_CSFILES) $(REMOVABLE_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(REMOVABLE_INDEX_CSFLAGS) $(REMOVABLE_INDEX_CSFILES) $(REMOVABLE_INDEX_ASSEMBLIES)

############################################################

CRAWL_SYSTEM_IN = beagle-crawl-system.in
CRAWL_SYSTEM = beagle-crawl-system

$(CRAWL_SYSTEM): $(srcdir)/$(CRAWL_SYSTEM_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(CRAWL_SYSTEM)|g" < $(srcdir)/$(CRAWL_SYSTEM_IN) > $@
	chmod +x $(CRAWL_SYSTEM)

# OK, now back to our regularly scheduled program.  But notice all of the
# EXTRA_DIST and CLEANFILES stuff at the bottom of the file.  Yuck.

############################################################
############################################################
############################################################

ALL_TARGETS = 				\
	$(PLUGIN_TARGET)		\
	$(DAEMON_DLL_TARGET)		\
	$(DAEMON_TARGET)		\
	$(DAEMON_WRAPPER)		\
	$(INDEX_HELPER_TARGET)		\
	$(INDEX_HELPER_WRAPPER)		\
	$(BUILD_INDEX_TARGET)		\
	$(BUILD_INDEX_WRAPPER)		\
	$(EXTRACT_CONTENT_TARGET)	\
	$(EXTRACT_CONTENT_WRAPPER)	\
	$(DUMP_INDEX_TARGET)		\
	$(DUMP_INDEX_WRAPPER)		\
	$(SNIPPET_TEST_TARGET)		\
	$(SNIPPET_TEST_WRAPPET)		\
	$(MANAGE_INDEX_TARGET)		\
	$(MANAGE_INDEX_WRAPPER)		\
	$(MASTER_DELETE_TARGET)		\
	$(MASTER_DELETE_WRAPPER)	\
	$(REMOVABLE_INDEX_TARGET)	\
	$(REMOVABLE_INDEX_WRAPPER)	\
	$(WEBINTERFACE_INDEX_XML)

man_MANS = 				\
	beagled.1			\
	beagle-build-index.8		\
	beagle-dump-index.8		\
	beagle-extract-content.8	\
	beagle-imlogviewer.8		\
	beagle-manage-index.8

all: $(ALL_TARGETS)

install-data-local: $(ALL_TARGETS)
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	$(mkinstalldirs) $(DESTDIR)$(sbindir)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	$(mkinstalldirs) $(DESTDIR)$(BACKENDDIR)
if ENABLE_EVOLUTION
	$(INSTALL_DATA) $(EVO_DLL_TARGET) $(EVO_DLL_TARGET).mdb $(DESTDIR)$(BACKENDDIR)
endif
	@for i in $(BIN_WRAPPERS); do						\
		sed -e "s|\#installed=1|installed=1|" < $$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

	@for i in $(SBIN_WRAPPERS); do						\
		sed -e "s|\#installed=1|installed=1|" < $$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(sbindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(sbindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

	@sed -e "s|\#installed=1|installed=1|" < $(INDEX_HELPER_WRAPPER) > $(INDEX_HELPER_WRAPPER).tmp
	$(INSTALL_SCRIPT) $(INDEX_HELPER_WRAPPER).tmp $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_WRAPPER)
	@rm -f $(INDEX_HELPER_WRAPPER).tmp

	$(INSTALL_DATA) $(PLUGIN_TARGET)                 $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(PLUGIN_TARGET).mdb             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_DLL_TARGET)             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_DLL_TARGET).mdb         $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_TARGET)                 $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_TARGET).mdb             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(EXTRACT_CONTENT_TARGET)        $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(EXTRACT_CONTENT_TARGET).mdb    $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(INDEX_HELPER_TARGET)           $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(INDEX_HELPER_TARGET).mdb       $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(BUILD_INDEX_TARGET)            $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(BUILD_INDEX_TARGET).mdb        $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DUMP_INDEX_TARGET)             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DUMP_INDEX_TARGET).mdb         $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(MANAGE_INDEX_TARGET)           $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(MANAGE_INDEX_TARGET).mdb       $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(REMOVABLE_INDEX_TARGET)            $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(REMOVABLE_INDEX_TARGET).mdb        $(DESTDIR)$(pkglibdir)

uninstall-local:
	@for i in $(BIN_WRAPPERS); do				\
		echo "rm -f $(DESTDIR)$(bindir)/$$i";		\
		rm -f $(DESTDIR)$(bindir)/$$i || exit 1;	\
	done

	@for i in $(SBIN_WRAPPERS); do				\
		echo "rm -f $(DESTDIR)$(sbindir)/$$i";		\
		rm -f $(DESTDIR)$(sbindir)/$$i || exit 1;	\
	done

	rm -f $(DESTDIR)$(pkglibdir)/$(PLUGIN_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(PLUGIN_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(DAEMON_DLL_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(DAEMON_DLL_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(DAEMON_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(DAEMON_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_WRAPPER)
	rm -f $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_WRAPPER).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(EXTRACT_CONTENT_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(EXTRACT_CONTENT_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(BUILD_INDEX_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(BUILD_INDEX_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(DUMP_INDEX_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(DUMP_INDEX_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(MANAGE_INDEX_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(MANAGE_INDEX_TARGET).mdb
	rm -f $(DESTDIR)$(pkglibdir)/$(REMOVABLE_INDEX_TARGET)
	rm -f $(DESTDIR)$(pkglibdir)/$(REMOVABLE_INDEX_TARGET).mdb

BIN_WRAPPERS = \
	$(DAEMON_WRAPPER)		\
	$(EXTRACT_CONTENT_WRAPPER)

SBIN_WRAPPERS = \
	$(BUILD_INDEX_WRAPPER)		\
	$(DUMP_INDEX_WRAPPER)		\
	$(MANAGE_INDEX_WRAPPER)		\
	$(MASTER_DELETE_WRAPPER)	\
	$(REMOVABLE_INDEX_WRAPPER)

noinst_SCRIPTS=	\
	$(SNIPPET_TEST_WRAPPER)

EXTRA_DIST = \
	$(srcdir)/Zeroconf.cs			\
	$(autostart_DATA)			\
	$(WEBINTERFACE_INDEX_XML_IN)		\
	$(webinterface_DATA)			\
	$(webinterfaceimages_DATA)		\
	$(WRAPPER_IN)   			\
	$(DAEMON_WRAPPER_IN)			\
	$(INDEX_HELPER_WRAPPER_IN)		\
	$(man_MANS)				\
	$(PLUGIN_CSFILES)			\
	$(DAEMON_DLL_CSFILES)			\
	$(DAEMON_CSFILES)			\
	$(EVO_DLL_CSFILES)			\
	$(THUNDERBIRD_CSFILES)			\
	$(GOOGLE_BACKEND_CSFILES)		\
	$(EXTRACT_CONTENT_CSFILES)		\
	$(DUMP_INDEX_CSFILES)			\
	$(SNIPPET_TEST_CSFILES)			\
	$(INDEX_HELPER_CSFILES)			\
	$(MASTER_DELETE_CSFILES)		\
	$(MANAGE_INDEX_CSFILES)			\
	$(BUILD_INDEX_CSFILES)			\
	$(REMOVABLE_INDEX_CSFILES)

CLEANFILES = \
	$(PLUGIN_TARGET)       			\
	$(PLUGIN_TARGET).mdb   			\
	$(DAEMON_DLL_TARGET)			\
	$(DAEMON_DLL_TARGET).mdb		\
	$(DAEMON_TARGET)       			\
	$(DAEMON_TARGET).mdb   			\
	$(DAEMON_WRAPPER)      			\
	$(EVO_DLL_TARGET)			\
	$(EVO_DLL_TARGET).mdb			\
	$(INDEX_HELPER_TARGET)			\
	$(INDEX_HELPER_TARGET).mdb		\
	$(INDEX_HELPER_WRAPPER)			\
	$(EXTRACT_CONTENT_TARGET)		\
	$(EXTRACT_CONTENT_TARGET).mdb		\
	$(EXTRACT_CONTENT_WRAPPER)		\
	$(DUMP_INDEX_TARGET)			\
	$(DUMP_INDEX_TARGET).mdb		\
	$(DUMP_INDEX_WRAPPER)			\
	$(MASTER_DELETE_TARGET)			\
	$(MASTER_DELETE_TARGET).mdb		\
	$(MASTER_DELETE_WRAPPER)		\
	$(SNIPPET_TEST_TARGET)			\
	$(SNIPPET_TEST_TARGET).mdb		\
	$(SNIPPET_TEST_WRAPPER)			\
	$(MANAGE_INDEX_TARGET)			\
	$(MANAGE_INDEX_TARGET).mdb		\
	$(MANAGE_INDEX_WRAPPER)			\
	$(BUILD_INDEX_TARGET)			\
	$(BUILD_INDEX_TARGET).mdb		\
	$(BUILD_INDEX_WRAPPER)			\
	$(REMOVABLE_INDEX_TARGET)			\
	$(REMOVABLE_INDEX_TARGET).mdb		\
	$(REMOVABLE_INDEX_WRAPPER)			
