# Warnings we don't want to see
# 0169 = The private field is never used
# 0414 = The private field is assigned but its value is never used

CSC = $(MCS) -debug -nowarn:0169,0414
CSFLAGS = -target:library

UTIL_TARGET = Util.dll
UTIL_TARGET_CONFIG = $(UTIL_TARGET).config

if OS_LINUX
CSFLAGS += -define:OS_LINUX
endif

if OS_FREEBSD
CSFLAGS += -define:OS_FREEBSD
endif

EXTSTR = ExternalStringsHack.cs
EXTSTR_IN = $(srcdir)/$(EXTSTR).in
$(EXTSTR): $(EXTSTR_IN)
	sed	-e "s|\@prefix\@|$(prefix)|g"			\
		-e "s|\@sysconfdir\@|$(sysconfdir)|g"		\
		-e "s|\@pkglibdir\@|$(pkglibdir)|g"		\
		-e "s|\@pkgdatadir\@|$(pkgdatadir)|g"		\
		-e "s|\@localstatedir\@|$(localstatedir)|g"	\
		-e "s|\@datadir\@|$(datadir)|g"			\
		-e "s|\@VERSION\@|$(VERSION)|g"			\
		-e "s|\@KDE_PREFIX\@|$(KDE_PREFIX)|g"		\
		-e "s|\@SQLITE_MAJ_VER\@|$(SQLITE_MAJ_VER)|g"	\
		< $(EXTSTR_IN) > $@

UTIL_CSFILES = 			        	\
	$(srcdir)/ArrayFu.cs              	\
	$(srcdir)/BetterBitArray.cs		\
	$(srcdir)/Config.cs			\
	$(srcdir)/DateTimeUtil.cs		\
	$(srcdir)/Debug.cs			\
	$(srcdir)/DelayedTextWriter.cs		\
	$(srcdir)/DirectoryWalker.cs		\
	$(srcdir)/ExceptionHandlingThread.cs	\
	$(srcdir)/ExtendedAttribute.cs  	\
	$(srcdir)/FileAdvise.cs			\
	$(srcdir)/FileNameMarshaler.cs		\
	$(srcdir)/FileSystem.cs			\
	$(srcdir)/FrequencyStatistics.cs	\
	$(srcdir)/GuidFu.cs			\
	$(srcdir)/HtmlRemovingReader.cs		\
	$(srcdir)/Inotify.cs			\
	$(srcdir)/LineReader.cs			\
	$(srcdir)/Log.cs			\
	$(srcdir)/Logger.cs             	\
	$(srcdir)/MultiReader.cs        	\
	$(srcdir)/NetworkService.cs		\
	$(srcdir)/Note.cs               	\
	$(srcdir)/Password.cs			\
	$(srcdir)/PathFinder.cs			\
	$(srcdir)/PullingReader.cs      	\
	$(srcdir)/ReflectionFu.cs		\
	$(srcdir)/SafeProcess.cs		\
	$(srcdir)/Scheduler.cs			\
	$(srcdir)/SmallIntArray.cs		\
	$(srcdir)/Stopwatch.cs			\
	$(srcdir)/StringFu.cs           	\
	$(srcdir)/StringMatcher.cs		\
	$(srcdir)/SystemInformation.cs		\
	$(srcdir)/SystemPriorities.cs		\
	$(srcdir)/TeeTextWriter.cs		\
	$(srcdir)/ThreadPond.cs			\
	$(srcdir)/Timeline.cs			\
	$(srcdir)/TopScores.cs			\
	$(srcdir)/TypeCacheAttribute.cs		\
	$(srcdir)/UnclosableStream.cs		\
	$(srcdir)/UriFu.cs			\
	$(srcdir)/VersionFu.cs			\
	$(srcdir)/XdgMime.cs			\
	$(srcdir)/XmlFu.cs			\
	$(srcdir)/QueryKeywordMapping.cs

# To reduce dependencies and other reasons, we don't build this

# Semweb compile flag
CSFLAGS += -define:DOTNET2

UTIL_ASSEMBLIES =				\
	$(BEAGREPD_LIBS)				\
        -r:System.Data                                  \
	-r:Mono.Data.Sqlite			\
	-r:Mono.Posix				\
	-r:ICSharpCode.SharpZipLib

if ENABLE_INOTIFY
INOTIFY_EXE = Inotify.exe
CSFLAGS += -define:ENABLE_INOTIFY
else
INOTIFY_EXE =
endif

UTIL_TARGET_FILES = $(UTIL_CSFILES) $(EXTSTR)

$(UTIL_TARGET): $(UTIL_TARGET_FILES)
	$(CSC) -unsafe -out:$@ $(CSFLAGS) $^ $(UTIL_ASSEMBLIES)

INOTIFY_TEST_CSFILES =				\
	$(srcdir)/Inotify.cs			\
	$(srcdir)/Log.cs			\
	$(srcdir)/Logger.cs			\
	$(srcdir)/DelayedTextWriter.cs		\
	$(srcdir)/TeeTextWriter.cs		\
	$(srcdir)/DirectoryWalker.cs		\
	$(srcdir)/FileSystem.cs			\
	$(srcdir)/FileNameMarshaler.cs		\
	$(srcdir)/ExceptionHandlingThread.cs

Inotify.exe: $(INOTIFY_TEST_CSFILES)
	$(CSC) -unsafe -out:$@ $^ -r:Mono.Posix -define:INOTIFY_TEST -define:ENABLE_INOTIFY

ALL_TARGETS =			\
	$(UTIL_TARGET)		\
	$(INOTIFY_EXE)

all: $(ALL_TARGETS)

install-data-local: $(ALL_TARGETS)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(UTIL_TARGET) $(UTIL_TARGET).mdb $(srcdir)/$(UTIL_TARGET_CONFIG) $(DESTDIR)$(pkglibdir)

uninstall-local:
	rm -f $(DESTDIR)$(pkglibdir)/$(UTIL_TARGET) $(DESTDIR)$(pkglibdir)/$(UTIL_TARGET).mdb $(DESTDIR)$(pkglibdir)/$(UTIL_TARGET_CONFIG)
	rm -f $(DESTDIR)$(pkglibdir)/$(UI_UTIL_TARGET) $(DESTDIR)$(pkglibdir)/$(UI_UTIL_TARGET).mdb $(DESTDIR)$(pkglibdir)/$(UI_UTIL_TARGET_CONFIG)


COND_CS_FILES =

EXTRA_DIST =					\
	$(EXTSTR_IN)				\
	$(UTIL_CSFILES)				\
	$(COND_CS_FILES)			\
	$(srcdir)/$(UTIL_TARGET_CONFIG)		\
	inotify-test

CLEANFILES =			\
	$(EXTSTR)		\
	$(UTIL_TARGET)		\
	$(UTIL_TARGET).mdb	\
	Inotify.exe		\
	Inotify.exe.mdb
