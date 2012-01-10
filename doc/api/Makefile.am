ASSEMBLIES = 						\
	$(top_builddir)/BeagrepClient/Beagrep.dll

LOCAL_ASSEMBLIES =					\
	$(top_builddir)/Util/Util.dll

UPDATE_ASSEMBLIES = $(addprefix -assembly:, $(ASSEMBLIES))
UPDATER = $(MONODOCER) -delete -path:BeagrepClient -name BeagrepClient -pretty $(UPDATE_ASSEMBLIES)

if BUILD_DOCS
SOURCESDIR=$(prefix)/lib/monodoc/sources
TARGETS=beagrep-docs.zip beagrep-docs.tree beagrep-docs.source
else
SOURCESDIR=$(datadir)
TARGETS=
endif

monodocdir=$(SOURCESDIR)
monodoc_DATA=$(TARGETS)

assemble: beagrep-docs.zip beagrep-docs.tree

DOCFILES =						\
	$(wildcard $(srcdir)/BeagrepClient/*.xml)	\
	$(wildcard $(srcdir)/BeagrepClient/Beagrep/*.xml)

beagrep-docs.zip: $(DOCFILES)
	$(MDASSEMBLER) --ecma $(srcdir)/BeagrepClient --out beagrep-docs

beagrep-docs.tree: beagrep-docs.zip

update:
	MONO_PATH=$(top_builddir)/Util $(UPDATER)

update-html:
	if [ -d beagrepclient-web-docs ]; then \
	        rm -rf beagrepclient-web-docs; \
	fi; \
	mkdir beagrepclient-web-docs; \
	monodocs2html --source BeagrepClient --dest beagrepclient-web-docs;

update-delete:
	MONO_PATH=$(top_builddir)/Util $(UPDATER) --delete

CLEANFILES = beagrep-docs.zip beagrep-docs.tree

EXTRA_DIST = beagrep-docs.source

NAMESPACES = Beagrep

dist-hook:
	mkdir -p $(distdir)/BeagrepClient
	cp $(srcdir)/BeagrepClient/*.xml $(distdir)/BeagrepClient/
	for i in $(NAMESPACES); do							\
		mkdir -p $(distdir)/BeagrepClient/$$i;					\
		cp $(srcdir)/BeagrepClient/$$i/*.xml $(distdir)/BeagrepClient/$$i;	\
	done

