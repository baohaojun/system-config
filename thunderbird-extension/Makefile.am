PROJECT = beagle
VERSION = 0.1.3

all: $(PROJECT).xpi

JAR_FILES =					\
	content/beagle.css			\
	content/beagleIndexer.js		\
	content/beagleMailWindow.xul		\
	content/beagleMessenger.xul		\
	content/beaglePrefs.xul			\
	content/beagleQueue.js			\
	content/beagleService.js		\
	content/beagleSettings.js		\
	content/beagleUnindex.js		\
	content/beagleUnindex.xul		\
	content/beagleUtils.js			\
	content/contents.rdf			\
	locale/en-US/beagle.dtd			\
	locale/en-US/strings.properties		\
	locale/en-US/contents.rdf		\
	locale/sv-SE/beagle.dtd			\
	locale/sv-SE/strings.properties		\
	locale/sv-SE/contents.rdf		\
	skin/classic/overlay.css		\
	skin/classic/beagle.png			\
	skin/classic/beagle-disabled.png	\
	skin/classic/beagle-error.png		\
	skin/classic/contents.rdf

XPI_FILES =					\
	chrome.manifest				\
	install.rdf				\
	chrome/$(PROJECT).jar			\
	defaults/preferences/default.js		\
	components/BeagleIndexer.js		\
	components/BeagleQueue.js		\
	components/BeagleSettings.js		\
	components/BeagleCommandLine.js

chrome/$(PROJECT).jar: $(JAR_FILES)
	-@mkdir -p chrome
	zip -q9 $@ $^

$(PROJECT).xpi: $(XPI_FILES)
	zip -q9 $@ $^

EXTRA_DIST =					\
	$(JAR_FILES)				\
	$(XPI_FILES)

CLEANFILES =					\
	chrome/$(PROJECT).jar			\
	$(PROJECT).xpi				
