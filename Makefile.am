# Require at least automake 1.8
AUTOMAKE_OPTIONS = 1.8

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = beagrep-0.0.pc beagrep-daemon.pc

SUBDIRS =

SUBDIRS +=			\
	Util			\
	glue			\
	BeagrepClient            \
	beagrepd                 \
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
	$(pkgconfig_DATA) 	\
	beagrep.spec

DISTCHECK_CONFIGURE_FLAGS = --disable-epiphany-extension-install --enable-docs

DISTCLEANFILES = 		\
	intltool-extract	\
	intltool-merge		\
	intltool-update		\
	$(pkgconfig_DATA)
