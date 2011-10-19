ASSEMBLIES = 						\
	$(top_builddir)/BeagleClient/Beagle.dll

LOCAL_ASSEMBLIES =					\
	$(top_builddir)/Util/Util.dll

UPDATE_ASSEMBLIES = $(addprefix -assembly:, $(ASSEMBLIES))
UPDATER = $(MONODOCER) -delete -path:BeagleClient -name BeagleClient -pretty $(UPDATE_ASSEMBLIES)

if BUILD_DOCS
SOURCESDIR=$(prefix)/lib/monodoc/sources
TARGETS=beagle-docs.zip beagle-docs.tree beagle-docs.source
else
SOURCESDIR=$(datadir)
TARGETS=
endif

monodocdir=$(SOURCESDIR)
monodoc_DATA=$(TARGETS)

assemble: beagle-docs.zip beagle-docs.tree

DOCFILES =						\
	$(wildcard $(srcdir)/BeagleClient/*.xml)	\
	$(wildcard $(srcdir)/BeagleClient/Beagle/*.xml)

beagle-docs.zip: $(DOCFILES)
	$(MDASSEMBLER) --ecma $(srcdir)/BeagleClient --out beagle-docs

beagle-docs.tree: beagle-docs.zip

update:
	MONO_PATH=$(top_builddir)/Util $(UPDATER)

update-html:
	if [ -d beagleclient-web-docs ]; then \
	        rm -rf beagleclient-web-docs; \
	fi; \
	mkdir beagleclient-web-docs; \
	monodocs2html --source BeagleClient --dest beagleclient-web-docs;

update-delete:
	MONO_PATH=$(top_builddir)/Util $(UPDATER) --delete

CLEANFILES = beagle-docs.zip beagle-docs.tree

EXTRA_DIST = beagle-docs.source

NAMESPACES = Beagle

dist-hook:
	mkdir -p $(distdir)/BeagleClient
	cp $(srcdir)/BeagleClient/*.xml $(distdir)/BeagleClient/
	for i in $(NAMESPACES); do							\
		mkdir -p $(distdir)/BeagleClient/$$i;					\
		cp $(srcdir)/BeagleClient/$$i/*.xml $(distdir)/BeagleClient/$$i;	\
	done

