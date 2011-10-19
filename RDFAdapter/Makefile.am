# Warnings we don't want to see

CSC = $(MCS) -debug

CSFLAGS = 			\
	$(BEAGLE_DEFINES)

LOCAL_ASSEMBLIES =			\
	../Util/Util.dll		\
	../BeagleClient/Beagle.dll

ASSEMBLIES =				\
	$(LOCAL_ASSEMBLIES:%=-r:%)

WRAPPER_IN = ../tools/wrapper.in
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

BEAGLESOURCE_TARGET = BeagleSource.dll
BEAGLESOURCE_CSFILES = $(srcdir)/BeagleSource.cs

$(BEAGLESOURCE_TARGET) : $(BEAGLESOURCE_CSFILES) $(LOCAL_ASSEMBLIES)
	$(CSC) -target:library -out:$@ $(CSFLAGS) $(BEAGLESOURCE_CSFILES) $(ASSEMBLIES)

SEMWEB_CLIENT_TARGET = SemWebClient.exe
SEMWEB_CLIENT_CSFILES = $(srcdir)/SemWebClient.cs
SEMWEB_CLIENT_WRAPPER = beagle-semweb-client

$(SEMWEB_CLIENT_TARGET): $(SEMWEB_CLIENT_CSFILES) $(LOCAL_ASSEMBLIES) $(BEAGLESOURCE_TARGET)
	$(CSC) -out:$@ $(CSFLAGS) $(SEMWEB_CLIENT_CSFILES) $(ASSEMBLIES) -r:$(BEAGLESOURCE_TARGET)

$(SEMWEB_CLIENT_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(SEMWEB_CLIENT_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(SEMWEB_CLIENT_WRAPPER)

RDF_QUERY_TOOL_TARGET = RdfQueryTool.exe
RDF_QUERY_TOOL_CSFILES = $(srcdir)/RdfQueryTool.cs
RDF_QUERY_TOOL_WRAPPER = beagle-rdf-query


$(RDF_QUERY_TOOL_TARGET): $(RDF_QUERY_TOOL_CSFILES) $(LOCAL_ASSEMBLIES) $(BEAGLESOURCE_TARGET)
	$(CSC) -out:$@ $(CSFLAGS) $(RDF_QUERY_TOOL_CSFILES) $(ASSEMBLIES) -r:$(BEAGLESOURCE_TARGET)

$(RDF_QUERY_TOOL_WRAPPER): $(WRAPPER_IN)
	$(WRAPPER_SED) -e "s|\@target\@|$(RDF_QUERY_TOOL_TARGET)|g" < $(srcdir)/$(WRAPPER_IN) > $@
	chmod +x $(RDF_QUERY_TOOL_WRAPPER)

RDF_QUERY_TOOL_TARGET_MDB = $(RDF_QUERY_TOOL_TARGET:%=%.mdb)

TARGETS = 				\
	$(BEAGLESOURCE_TARGET)		\
	$(SEMWEB_CLIENT_TARGET)

TARGETS_MDB = $(TARGETS:%=%.mdb)

WRAPPERS = 			\
	$(SEMWEB_CLIENT_WRAPPER)

all: $(TARGETS) $(WRAPPERS) $(RDF_QUERY_TOOL_TARGET) $(RDF_QUERY_TOOL_WRAPPER)

install-data-local: $(TARGETS) $(WRAPPERS)
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) $(TARGETS) $(TARGETS_MDB) $(DESTDIR)$(pkglibdir)

	$(mkinstalldirs) $(DESTDIR)$(bindir)

	@for i in $(WRAPPERS); do						\
		sed -e "s|\#installed=1|installed=1|" < $$i > $$i.tmp;		\
		echo "$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i";	\
		$(INSTALL_SCRIPT) $$i.tmp $(DESTDIR)$(bindir)/$$i || exit 1;	\
		rm -f $$i.tmp;							\
	done

uninstall-local:
	cd $(DESTDIR)$(pkglibdir) && rm -f $(TARGETS) && rm -f $(TARGETS_MDB)

	@for i in $(WRAPPERS); do				\
		echo "rm -f $(DESTDIR)$(bindir)/$$i";		\
		rm -f $(DESTDIR)$(bindir)/$$i || exit 1;	\
	done

EXTRA_DIST =				\
	$(BEAGLESOURCE_CSFILES)		\
	$(SEMWEB_CLIENT_CSFILES)	\
	$(RDF_QUERY_TOOL_CSFILES)	\
	$(WRAPPER_IN)

CLEANFILES = 				\
	$(TARGETS)			\
	$(TARGETS_MDB)			\
	$(RDF_QUERY_TOOL_TARGET)	\
	$(RDF_QUERY_TOOL_TARGET_MDB)	\
	$(RDF_QUERY_TOOL_WRAPPER)	\
	$(WRAPPERS)
