# We use tomboy icons for displaying notes in beagle-search
ICONS = \
	hicolor_apps_16x16_tomboy.png \
	hicolor_apps_22x22_tomboy.png \
	hicolor_apps_24x24_tomboy.png \
	hicolor_apps_32x32_tomboy.png \
	hicolor_apps_48x48_tomboy.png \
	hicolor_apps_scalable_tomboy.svg

noinst_DATA = \
	hicolor_apps_16x16_tomboy.xcf.bz2 \
	hicolor_apps_22x22_tomboy.xcf.bz2

EXTRA_DIST = \
	$(ICONS)	\
	$(noinst_DATA)

###############################################################################

install-icons:
	for icon in $(ICONS); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		mkdir -p $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT; \
		$(INSTALL_DATA) $(srcdir)/$$icon $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

uninstall-icons:
	-for icon in $(ICONS); do \
		THEME=`echo $$icon | cut -d_ -f1`; \
		CONTEXT=`echo $$icon | cut -d_ -f2`; \
		SIZE=`echo $$icon | cut -d_ -f3`; \
		ICONFILE=`echo $$icon | cut -d_ -f4`; \
		rm -f $(DESTDIR)$(pkgdatadir)/icons/$$THEME/$$SIZE/$$CONTEXT/$$ICONFILE; \
	done

install-data-local: install-icons

uninstall-local: uninstall-icons

