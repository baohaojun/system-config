@EPIPHANY_EXTENSION_RULE@

extension_in_files = beagle.ephy-extension.in
extension_files = $(extension_in_files:.ephy-extension.in=.ephy-extension)

BEAGLE_PY_IN = beagle.py.in

beagle.py: $(BEAGLE_PY_IN)
	sed					\
	-e "s|\@localedir\@|${localedir}|g"	\
	-e "s|\@EPIPHANY_MAJOR\@|${EPIPHANY_MAJOR}|g"	\
	< $(srcdir)/$(BEAGLE_PY_IN) > beagle.py

# Hack to fix distcheck
if INSTALL_EPIPHANY_EXTENSION
extensiondir = $(EPIPHANY_EXTENSIONS_DIR)
extension_DATA = $(extension_files) beagle.py
else
extensiondir = $(libdir)/epiphany/extensions
extension_DATA = $(extension_files) beagle.py
endif

EXTRA_DIST = $(extension_in_files) beagle.py.in

CLEANFILES = $(extension_DATA)
