# Require at least automake 1.8
AUTOMAKE_OPTIONS = 1.8

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = beagle-0.0.pc beagle-daemon.pc

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

SUBDIRS +=			\
	tools			\
	conf-data

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
