# Require at least automake 1.8
AUTOMAKE_OPTIONS = 1.8

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = beagle-0.0.pc beagle-daemon.pc

if ENABLE_GUI
pkgconfig_DATA += beagle-ui-0.0.pc
endif

SUBDIRS =

SUBDIRS +=			\
	Util			\
	glue			\
	BeagleClient            \
	beagled                 \
	Filters

if BUILD_DOCS
SUBDIRS +=			\
	doc
endif

if ENABLE_GUI
SUBDIRS +=			\
	po			\
	search			\
	ImLogViewer
endif

SUBDIRS +=			\
	firefox-extension	\
	tools			\
	conf-data

if ENABLE_EPIPHANY_EXTENSION
SUBDIRS += epiphany-extension
endif

if ENABLE_THUNDERBIRD
SUBDIRS += thunderbird-extension
endif

if ENABLE_RDF_ADAPTER
SUBDIRS += RDFAdapter
endif

EXTRA_DIST = 			\
	HACKING			\
	intltool-extract.in	\
	intltool-merge.in	\
	intltool-update.in	\
	$(pkgconfig_DATA) 	\
	beagle.spec

DISTCHECK_CONFIGURE_FLAGS = --disable-epiphany-extension-install --enable-docs

DISTCLEANFILES = 		\
	intltool-extract	\
	intltool-merge		\
	intltool-update		\
	$(pkgconfig_DATA)
