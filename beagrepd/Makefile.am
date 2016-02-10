autostartdir = $(sysconfdir)/xdg/autostart
autostart_DATA = beagrepd-autostart.desktop

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

PLUGIN_TARGET = BeagrepDaemonPlugins.dll

PLUGIN_CSFLAGS = -target:library $(BEAGREP_DEFINES)

PLUGIN_CSFILES =				\
	$(srcdir)/Flavor.cs			\
	$(srcdir)/Filter.cs

PLUGIN_LOCAL_ASSEMBLIES =			\
	../Util/Util.dll			\
	../BeagrepClient/Beagrep.dll

PLUGIN_ASSEMBLIES =				\
	-r:Mono.Posix				\
	$(BEAGREPD_LIBS)                 	\
	$(PLUGIN_LOCAL_ASSEMBLIES:%=-r:%)


$(PLUGIN_TARGET): $(PLUGIN_CSFILES) $(PLUGIN_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(PLUGIN_CSFLAGS) $(PLUGIN_CSFILES) $(PLUGIN_ASSEMBLIES)

############################################################

WRAPPER_IN = wrapper.in
WRAPPER_SED = sed 					\
	-e "s|\@prefix\@|$(prefix)|g"			\
	-e "s|\@pkglibdir\@|$(pkglibdir)|g"		\
	-e "s|\@libdir\@|$(libdir)|g"			\
	-e "s|\@gacprefix\@|$(GAC_PREFIX)|g"		\
	-e "s|\@bash\@|$(BASH)|"			\
	-e "s|\@wrapper\@|$@|g"

############################################################

include $(srcdir)/Lucene.Net/Makefile.include

LUCENE_CSFILES = $(LUCENE_2_1_CSFILES)

############################################################

DAEMON_DLL_TARGET = BeagrepDaemonLib.dll

DAEMON_DLL_CSFLAGS = -target:library $(BEAGREP_DEFINES)

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
	$(FILE_SYSTEM_QUERYABLE_CSFILES)			\
	$(INDEXING_SERVICE_QUERYABLE_CSFILES)			\
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
	../BeagrepClient/Beagrep.dll				\
	./$(PLUGIN_TARGET)

DAEMON_DLL_ASSEMBLIES = 					\
	$(BEAGREPD_LIBS)                 			\
	$(DAEMON_DLL_LOCAL_ASSEMBLIES:%=-r:%)			\
	$(SHARPZIPLIB_LIBS)					\
        -r:System.Data                                          \
	-r:Mono.Data.Sqlite					\
	-r:Mono.Posix


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
	../BeagrepClient/Beagrep.dll		\
	./$(DAEMON_DLL_TARGET)

BACKEND_ASSEMBLIES =				\
	$(SHARPZIPLIB_LIBS)			\
	$(BACKEND_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

############################################################

DAEMON_CSFLAGS =				\
	-target:exe				\
	$(BEAGREP_DEFINES)

DAEMON_CSFILES =				\
	$(srcdir)/AssemblyInfo.cs		\
	$(srcdir)/BeagrepDaemon.cs		\
	$(srcdir)/RemoteIndexer.cs

DAEMON_LOCAL_ASSEMBLIES =			\
	../Util/Util.dll			\
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

DAEMON_ASSEMBLIES =				\
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

############################################################
############################################################
############################################################

# This next section is a very good example of why we need real mono support in
# automake.  This is a massive wart on our development platform.

INDEX_HELPER_WRAPPER = beagrepd-index-helper
INDEX_HELPER_WRAPPER_IN = $(INDEX_HELPER_WRAPPER).in
INDEX_HELPER_TARGET = IndexHelper.exe

$(INDEX_HELPER_WRAPPER): $(srcdir)/$(INDEX_HELPER_WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(INDEX_HELPER_TARGET)|g" < $(srcdir)/$(INDEX_HELPER_WRAPPER_IN) > $@
	chmod +x $(INDEX_HELPER_WRAPPER)

INDEX_HELPER_CSFLAGS = -target:exe $(BEAGREP_DEFINES)

indexhelperdir = $(srcdir)/IndexHelper

INDEX_HELPER_CSFILES =	\
	$(indexhelperdir)/IndexHelper.cs		\
	$(indexhelperdir)/RemoteIndexerExecutor.cs


INDEX_HELPER_LOCAL_ASSEMBLIES =		\
	../Util/Util.dll		\
	../BeagrepClient/Beagrep.dll	\
	./$(DAEMON_DLL_TARGET)		\
	./$(PLUGIN_TARGET)

INDEX_HELPER_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(INDEX_HELPER_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(INDEX_HELPER_TARGET): $(INDEX_HELPER_CSFILES) $(INDEX_HELPER_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(INDEX_HELPER_CSFLAGS) $(INDEX_HELPER_CSFILES) $(INDEX_HELPER_ASSEMBLIES)

############################################################

EXTRACT_CONTENT_WRAPPER = beagrep-extract-content
EXTRACT_CONTENT_TARGET = ExtractContent.exe

$(EXTRACT_CONTENT_WRAPPER): $(srcdir)/$(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(EXTRACT_CONTENT_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(EXTRACT_CONTENT_WRAPPER)


EXTRACT_CONTENT_CSFLAGS = \
	-target:exe	  \
	$(BEAGREP_DEFINES)

EXTRACT_CONTENT_CSFILES = \
	$(srcdir)/ExtractContent.cs

EXTRACT_CONTENT_LOCAL_ASSEMBLIES = \
	../Util/Util.dll			\
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

EXTRACT_CONTENT_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(EXTRACT_CONTENT_TARGET): $(EXTRACT_CONTENT_CSFILES) $(EXTRACT_CONTENT_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(EXTRACT_CONTENT_CSFLAGS) $(EXTRACT_CONTENT_CSFILES) $(EXTRACT_CONTENT_ASSEMBLIES)

############################################################

DUMP_INDEX_WRAPPER = beagrep-dump-index
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
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

DUMP_INDEX_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(DUMP_INDEX_TARGET): $(DUMP_INDEX_CSFILES) $(DUMP_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(DUMP_INDEX_CSFLAGS) $(DUMP_INDEX_CSFILES) $(DUMP_INDEX_ASSEMBLIES)

############################################################

MASTER_DELETE_WRAPPER = beagrep-master-delete-button
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
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

MASTER_DELETE_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(MASTER_DELETE_TARGET): $(MASTER_DELETE_CSFILES) $(MASTER_DELETE_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(MASTER_DELETE_CSFLAGS) $(MASTER_DELETE_CSFILES) $(MASTER_DELETE_ASSEMBLIES)

############################################################

SNIPPET_TEST_WRAPPER = beagrep-snippet-test
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
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

SNIPPET_TEST_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(SHARPZIPLIB_LIBS)			\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(SNIPPET_TEST_TARGET): $(SNIPPET_TEST_CSFILES) $(SNIPPET_TEST_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(SNIPPET_TEST_CSFLAGS) $(SNIPPET_TEST_CSFILES) $(SNIPPET_TEST_ASSEMBLIES)

############################################################

BUILD_INDEX_WRAPPER = beagrep-build-index
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
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

BUILD_INDEX_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(BUILD_INDEX_TARGET): $(BUILD_INDEX_CSFILES) $(BUILD_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(BUILD_INDEX_CSFLAGS) $(BUILD_INDEX_CSFILES) $(BUILD_INDEX_ASSEMBLIES)

############################################################

MANAGE_INDEX_WRAPPER = beagrep-manage-index
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
	../BeagrepClient/Beagrep.dll		\
	./$(PLUGIN_TARGET)			\
	./$(DAEMON_DLL_TARGET)

MANAGE_INDEX_ASSEMBLIES = \
	$(BEAGREPD_LIBS)				\
	$(DAEMON_LOCAL_ASSEMBLIES:%=-r:%)	\
	-r:Mono.Posix

$(MANAGE_INDEX_TARGET): $(MANAGE_INDEX_CSFILES) $(MANAGE_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(MANAGE_INDEX_CSFLAGS) $(MANAGE_INDEX_CSFILES) $(MANAGE_INDEX_ASSEMBLIES)

############################################################

REMOVABLE_INDEX_WRAPPER = beagrep-removable-index
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
	../BeagrepClient/Beagrep.dll

REMOVABLE_INDEX_ASSEMBLIES = \
	$(REMOVABLE_INDEX_LOCAL_ASSEMBLIES:%=-r:%)

$(REMOVABLE_INDEX_TARGET): $(REMOVABLE_INDEX_CSFILES) $(REMOVABLE_INDEX_LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(REMOVABLE_INDEX_CSFLAGS) $(REMOVABLE_INDEX_CSFILES) $(REMOVABLE_INDEX_ASSEMBLIES)

############################################################

CRAWL_SYSTEM_IN = beagrep-crawl-system.in
CRAWL_SYSTEM = beagrep-crawl-system

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
	beagrepd.1			\
	beagrep-build-index.8		\
	beagrep-dump-index.8		\
	beagrep-extract-content.8	\
	beagrep-imlogviewer.8		\
	beagrep-manage-index.8

all: $(ALL_TARGETS)

install-data-local: $(ALL_TARGETS)
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	$(mkinstalldirs) $(DESTDIR)$(sbindir)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	$(mkinstalldirs) $(DESTDIR)$(BACKENDDIR)
	@for i in $(BIN_WRAPPERS); do						\
		sed -e "s|\#installed=1|installed=1|" < $$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

	@sed -e "s|\#installed=1|installed=1|" < $(INDEX_HELPER_WRAPPER) > $(INDEX_HELPER_WRAPPER).tmp
	$(INSTALL_SCRIPT) $(INDEX_HELPER_WRAPPER).tmp $(DESTDIR)$(pkglibdir)/$(INDEX_HELPER_WRAPPER)
	@rm -f $(INDEX_HELPER_WRAPPER).tmp

	$(INSTALL_DATA) $(PLUGIN_TARGET)                 $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(PLUGIN_TARGET).mdb             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_DLL_TARGET)             $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(DAEMON_DLL_TARGET).mdb         $(DESTDIR)$(pkglibdir)
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
	$(EXTRACT_CONTENT_WRAPPER)      \
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
