# Warnings we don't want to see

CSC = $(MCS) -debug

LOCAL_ASSEMBLIES =			\
	../Util/Util.dll		\
	../BeagleClient/Beagle.dll	\
	../beagled/BeagleDaemonLib.dll	

ASSEMBLIES =				\
	$(BEAGLED_LIBS)			\
	-r:Mono.Posix			\
	$(LOCAL_ASSEMBLIES:%=-r:%)

WRAPPER_IN = wrapper.in
WRAPPER_SED = sed 					\
	-e "s|\@prefix\@|$(prefix)|g"			\
	-e "s|\@pkglibdir\@|$(pkglibdir)|g"		\
	-e "s|\@libdir\@|$(libdir)|g"			\
	-e "s|\@libexecdir\@|$(libexecdir)|g"		\
	-e "s|\@bindir\@|$(bindir)|g"			\
	-e "s|\@sbindir\@|$(sbindir)|g"			\
	-e "s|\@sysconfdir\@|$(sysconfdir)|g"		\
	-e "s|\@localstatedir\@|$(localstatedir)|g"     \
	-e "s|\@gacprefix\@|$(GAC_PREFIX)|g"		\
	-e "s|\@bash\@|$(BASH)|"

CRAWL_WRAPPER = beagle-crawl-system
CRAWL_WRAPPER_IN = $(CRAWL_WRAPPER).in

$(CRAWL_WRAPPER): $(CRAWL_WRAPPER_IN)
	$(WRAPPER_SED) < $(srcdir)/$(CRAWL_WRAPPER_IN) > $@
	chmod +x $(CRAWL_WRAPPER)

crondir = $(sysconfdir)/cron.daily

DOC_EXTRACTOR_TARGET  = DocExtractor.exe
DOC_EXTRACTOR_WRAPPER = beagle-doc-extractor
DOC_EXTRACTOR_CSFILES = $(srcdir)/DocExtractor.cs

$(DOC_EXTRACTOR_TARGET): $(DOC_EXTRACTOR_CSFILES)
	$(CSC) -out:$@ $(CSFLAGS) $(DOC_EXTRACTOR_CSFILES) $(ASSEMBLIES)

$(DOC_EXTRACTOR_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(DOC_EXTRACTOR_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(DOC_EXTRACTOR_WRAPPER)

INFO_TARGET  = Info.exe
INFO_WRAPPER = beagle-info
INFO_CSFILES = $(srcdir)/Info.cs
INFO_ASSEMBLIES = ../beagled/BeagleDaemonPlugins.dll

$(INFO_TARGET): $(INFO_CSFILES) $(LOCAL_ASSEMBLIES) $(INFO_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(INFO_CSFILES) $(ASSEMBLIES) $(INFO_ASSEMBLIES:%=-r:%)

$(INFO_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(INFO_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(INFO_WRAPPER)

SHUTDOWN_TARGET  = Shutdown.exe
SHUTDOWN_WRAPPER = beagle-shutdown
SHUTDOWN_CSFILES = $(srcdir)/Shutdown.cs

$(SHUTDOWN_TARGET): $(SHUTDOWN_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(SHUTDOWN_CSFILES) $(ASSEMBLIES)

$(SHUTDOWN_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(SHUTDOWN_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(SHUTDOWN_WRAPPER)

QUERY_TARGET  = Query.exe
QUERY_WRAPPER = beagle-query
QUERY_CSFILES = $(srcdir)/Query.cs

$(QUERY_TARGET): $(QUERY_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(QUERY_CSFILES) $(ASSEMBLIES)

$(QUERY_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(QUERY_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(QUERY_WRAPPER)

STATIC_QUERY_TARGET  = StaticQuery.exe
STATIC_QUERY_WRAPPER = beagle-static-query
STATIC_QUERY_CSFILES = $(srcdir)/StaticQuery.cs

$(STATIC_QUERY_TARGET): $(STATIC_QUERY_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(STATIC_QUERY_CSFILES) $(ASSEMBLIES)

$(STATIC_QUERY_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(STATIC_QUERY_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(STATIC_QUERY_WRAPPER)

CONFIG_TARGET = Config.exe
CONFIG_WRAPPER = beagle-config
CONFIG_CSFILES = $(srcdir)/Config.cs

$(CONFIG_TARGET): $(CONFIG_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(CONFIG_CSFILES) $(ASSEMBLIES)

$(CONFIG_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(CONFIG_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(CONFIG_WRAPPER)


TARGETS = 				\
	$(INFO_TARGET)			\
	$(SHUTDOWN_TARGET)		\
	$(QUERY_TARGET)			\
	$(STATIC_QUERY_TARGET)		\
	$(CONFIG_TARGET)

TARGETS_MDB = $(TARGETS:%=%.mdb)

BUILT_WRAPPERS = 			\
	$(INFO_WRAPPER)			\
	$(SHUTDOWN_WRAPPER)		\
	$(QUERY_WRAPPER)		\
	$(STATIC_QUERY_WRAPPER)		\
	$(CONFIG_WRAPPER)

KEYGRABBER =

PREBUILT_WRAPPERS =		\
	beagle-index-info	\
	beagle-ping		\
	beagle-status		\
	blocate

BIN_WRAPPERS = $(BUILT_WRAPPERS) $(PREBUILT_WRAPPERS)

WRAPPERS =			\
	$(BIN_WRAPPERS)		\
	$(CRAWL_WRAPPER)

man_MANS = 			\
	beagle-config.1		\
	beagle-doc-extractor.8	\
	beagle-index-info.8	\
	beagle-info.1		\
	beagle-ping.1		\
	beagle-query.1		\
	beagle-shutdown.1	\
	beagle-status.1

@INTLTOOL_DESKTOP_RULE@


all: $(TARGETS) $(WRAPPERS)

install-data-local: $(TARGETS) $(WRAPPERS)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(TARGETS) $(TARGETS_MDB) $(DESTDIR)$(pkglibdir)

	$(mkinstalldirs) $(DESTDIR)$(bindir)

	@for i in $(BUILT_WRAPPERS); do						\
		sed -e "s|\#installed=1|installed=1|" < $$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

	@for i in $(PREBUILT_WRAPPERS); do					\
		sed -e "s|\#installed=1|installed=1|" < $(srcdir)/$$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

	$(mkinstalldirs) $(DESTDIR)$(crondir)
	$(INSTALL_SCRIPT) $(CRAWL_WRAPPER) $(DESTDIR)$(crondir)

uninstall-local:
	cd $(DESTDIR)$(pkglibdir) && rm -f $(TARGETS) && rm -f $(TARGETS_MDB)

	@for i in $(BIN_WRAPPERS); do				\
		echo "rm -f $(DESTDIR)$(bindir)/$$i";		\
		rm -f $(DESTDIR)$(bindir)/$$i || exit 1;	\
	done

	cd $(DESTDIR)$(crondir) && rm -f $(CRAWL_WRAPPER)

EXTRA_DIST =				\
	$(WRAPPER_IN)			\
	$(CRAWL_WRAPPER_IN)		\
	$(DOC_EXTRACTOR_CSFILES)	\
	$(INFO_CSFILES)			\
	$(SHUTDOWN_CSFILES)		\
	$(QUERY_CSFILES)		\
	$(STATIC_QUERY_CSFILES)		\
	$(CONFIG_CSFILES)		\
	$(man_MANS)			\
	$(cron_SCRIPTS)			\
	$(desktop_in_files)		\
	$(desktop_DATA) 		\
	$(KEYGRABBER)			\
	$(PREBUILT_WRAPPERS)		\
	boot.inotify.init		


CLEANFILES = 				\
	$(TARGETS)			\
	$(TARGETS_MDB)			\
	$(BUILT_WRAPPERS)		\
	$(CRAWL_WRAPPER)		\
	$(desktop_DATA)		 	\
	$(desktop_h_files)





