INCLUDES = $(MONO_CFLAGS)

if ENABLE_GUI
INCLUDES += $(UIGLUE_CFLAGS)
endif

EXTRA_GLUE_SOURCES =
EXTRA_GLUE_LIBADD =

BEAGLED_GLUE_SOURCES =		\
	beagled-utils.c

if ENABLE_INOTIFY
EXTRA_GLUE_SOURCES +=		\
	inotify-glue.c
endif

IOPRIO_GLUE_SOURCES =		\
	ioprio-glue.c

SPAWN_GLUE_SOURCES =		\
	spawn-glue.c

SCHEDULER_GLUE_SOURCES =	\
	scheduler-glue.c

SYSINFO_GLUE_SOURCES =		\
	mono-glue.c		\
	screensaver-glue.c	\
	thread-glue.c		\
	vmsize-glue.c

SYSINFO_GLUE_LIBADD = $(MONO_LIBS)

if ENABLE_LIBXSS
SYSINFO_GLUE_LIBADD += $(SYSTEMINFO_GLUE_X_LIBS)
endif

KEY_GLUE_SOURCES =	 	\
	tomboykeybinder.c	\
	tomboykeybinder.h	\
	eggaccelerators.c	\
	eggaccelerators.h

XDGMIME_GLUE_SOURCES =		\
	xdgmime/xdgmime.c	\
	xdgmime/xdgmime.h	\
	xdgmime/xdgmimeglob.c	\
	xdgmime/xdgmimeglob.h	\
	xdgmime/xdgmimeint.c	\
	xdgmime/xdgmimeint.h	\
	xdgmime/xdgmimemagic.c	\
	xdgmime/xdgmimemagic.h	\
	xdgmime/xdgmimealias.c	\
	xdgmime/xdgmimealias.h	\
	xdgmime/xdgmimeparent.c	\
	xdgmime/xdgmimeparent.h	\
	xdgmime/xdgmimecache.c	\
	xdgmime/xdgmimecache.h

UI_GLUE_SOURCES =		\
	search-entry.c		\
	search-entry.h		\
	search-entry-watermark.h\
	ui-glue.c
UI_GLUE_LIBADD = $(UIGLUE_LIBS)

search-entry.c: search-entry-watermark.h

search-entry-watermark.h: search-entry-watermark.svg
	echo '#define SEARCH_ENTRY_WATERMARK_SVG "\' > $@; \
	sed -e 's/"/\\"/g' -e 's/$$/\\/' -e 's/#000000/#%s/g' $< >> $@; \
	echo '"' >> $@

if ENABLE_WV1
INCLUDES += \
	$(WV1_CFLAGS)
EXTRA_GLUE_SOURCES +=	\
	wv1-glue.c
EXTRA_GLUE_LIBADD +=	\
	$(WV1_LIBS)
endif

libbeagleglue_la_SOURCES = 		\
	$(BEAGLED_GLUE_SOURCES)		\
	$(IOPRIO_GLUE_SOURCES)		\
	$(SCHEDULER_GLUE_SOURCES)	\
	$(SPAWN_GLUE_SOURCES)		\
	$(SYSINFO_GLUE_SOURCES)		\
	$(XDGMIME_GLUE_SOURCES)		\
	$(EXTRA_GLUE_SOURCES)

libbeagleglue_la_LIBADD =	\
	$(SYSINFO_GLUE_LIBADD)	\
	$(EXTRA_GLUE_LIBADD)

libbeagleuiglue_la_SOURCES =	\
	$(KEY_GLUE_SOURCES)	\
	$(UI_GLUE_SOURCES)

libbeagleuiglue_la_LIBADD =	\
	$(UI_GLUE_LIBADD)

gluelibdir = $(pkglibdir)
gluelib_LTLIBRARIES = 		\
	libbeagleglue.la

if ENABLE_GUI
gluelib_LTLIBRARIES +=		\
	libbeagleuiglue.la
endif

COND_SOURCES = 			\
	wv1-glue.c		\
	inotify-glue.c

EXTRA_DIST =			\
	$(COND_SOURCES)		\
	search-entry-watermark.svg


