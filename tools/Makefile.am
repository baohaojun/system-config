# Warnings we don't want to see

CSC = $(MCS) -debug

LOCAL_ASSEMBLIES =			\
	../Util/Util.dll		\
	../BeagrepClient/Beagrep.dll	\
	../beagrepd/BeagrepDaemonLib.dll

ASSEMBLIES =				\
	$(BEAGREPD_LIBS)			\
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

BREAK_WORDS_TARGET = BreakWords.exe
BREAK_WORDS_WRAPPER = beagrep-break-words
BREAK_WORDS_CSFILES = $(foreach cs, BreakWords.cs StandardTokenizerImpl.cs Token.cs, $(srcdir)/$(cs))
$(BREAK_WORDS_TARGET): $(BREAK_WORDS_CSFILES)
	$(CSC) -out:$@ $(CSFLAGS) $(BREAK_WORDS_CSFILES)

$(BREAK_WORDS_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(BREAK_WORDS_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $@

STATIC_QUERY_TARGET  = StaticQuery.exe
STATIC_QUERY_WRAPPER = beagrep-static-query
STATIC_QUERY_CSFILES = $(srcdir)/StaticQuery.cs

$(STATIC_QUERY_TARGET): $(STATIC_QUERY_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -out:$@ $(CSFLAGS) $(STATIC_QUERY_CSFILES) $(ASSEMBLIES)

$(STATIC_QUERY_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(STATIC_QUERY_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $@

TARGETS = 				\
	$(STATIC_QUERY_TARGET)		\
	$(BREAK_WORDS_TARGET)


TARGETS_MDB = $(TARGETS:%=%.mdb)

PREBUILT_WRAPPERS = beagrep beagrep-files beagrep-get-tokens mkbeagrepidx beagrep-unurl-and-filter.pl beagrep-glob-files

BUILT_WRAPPERS = 			\
	$(STATIC_QUERY_WRAPPER)		\
	$(BREAK_WORDS_WRAPPER)


BIN_WRAPPERS = $(BUILT_WRAPPERS) $(PREBUILT_WRAPPERS)

WRAPPERS =			\
	$(BIN_WRAPPERS)

# man_MANS = 			\
# 	beagrep-static-query.1

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

uninstall-local:
	cd $(DESTDIR)$(pkglibdir) && rm -f $(TARGETS) && rm -f $(TARGETS_MDB)

	@for i in $(BIN_WRAPPERS); do				\
		echo "rm -f $(DESTDIR)$(bindir)/$$i";		\
		rm -f $(DESTDIR)$(bindir)/$$i || exit 1;	\
	done

	cd $(DESTDIR)$(crondir) && rm -f $(CRAWL_WRAPPER)

EXTRA_DIST =				\
	$(WRAPPER_IN)			\
	$(STATIC_QUERY_CSFILES)		\
	$(BREAK_WORDS_CSFILES)		\
	$(man_MANS)			\
	$(PREBUILT_WRAPPERS)



CLEANFILES = 				\
	$(TARGETS)			\
	$(TARGETS_MDB)			\
	$(BUILT_WRAPPERS)
