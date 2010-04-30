
PROJECT = beagle
VERSION = 1.1.0

INSTALL_RDF = install.rdf
INSTALL_RDF_IN = $(INSTALL_RDF).in
VERSION_SED = sed -e "s|\@VERSION\@|$(VERSION)|g"

$(INSTALL_RDF) : $(INSTALL_RDF_IN)
	$(VERSION_SED) < $(srcdir)/$(INSTALL_RDF_IN) > $@

all: $(PROJECT).xpi

XPI_FILES = 			\
	chrome.manifest		\
	install.rdf		\
	\
	chrome/content/contents.rdf		\
	chrome/content/beagleOverlay.js		\
	chrome/content/beagleOverlay.xul	\
	chrome/content/beaglePrefs.js		\
	chrome/content/beaglePrefs.xul		\
	chrome/content/beagleAddFilter.xul	\
	chrome/content/beagleAddFilter.js	\
	chrome/content/beagleSearch.js		\
	chrome/content/indexLink.xul		\
	chrome/content/indexLink.js		\
	chrome/content/indexBookmark.js		\
	chrome/content/indexBookmark3.js		\
	chrome/content/md5.js			\
	chrome/content/utils.js			\
	chrome/content/i18n.js			\
	chrome/content/json.js			\
	chrome/content/sidebar.xul		\
	\
	chrome/content/jslib/jslib.js		\
	chrome/content/jslib/modules.js		\
	chrome/content/jslib/debug/debug.js	\
	chrome/content/jslib/io/dir.js		\
	chrome/content/jslib/io/dirUtils.js	\
	chrome/content/jslib/io/file.js		\
	chrome/content/jslib/io/fileUtils.js	\
	chrome/content/jslib/io/filesystem.js	\
	\
	chrome/locale/en-US/contents.rdf	\
	chrome/locale/en-US/beagle.dtd		\
	chrome/locale/en-US/beagle.properties	\
	\
	chrome/locale/sv-SE/contents.rdf	\
	chrome/locale/sv-SE/beagle.dtd		\
	chrome/locale/sv-SE/beagle.properties	\
	\
	chrome/locale/zh-CN/contents.rdf	\
	chrome/locale/zh-CN/beagle.dtd		\
	chrome/locale/zh-CN/beagle.properties	\
	\
	chrome/skin/classic/contents.rdf	\
	chrome/skin/classic/beagle-big.png	\
	chrome/skin/classic/beagle-disabled.png	\
	chrome/skin/classic/beagle-error.png	\
	chrome/skin/classic/beagle.png		\
	chrome/skin/classic/overlay.css		\
	chrome/skin/classic/sidebar.css

$(PROJECT).xpi: $(XPI_FILES)
	zip -q9 $@ $^


EXTRA_DIST =			\
	$(XPI_FILES)		\
	$(INSTALL_RDF_IN)

CLEANFILES =			\
	$(PROJECT).xpi		\
	$(INSTALL_RDF)
