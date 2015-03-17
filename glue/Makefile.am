INCLUDES = @MONO_CFLAGS@ @GLIB_CFLAGS@

EXTRA_GLUE_SOURCES =
EXTRA_GLUE_LIBADD = -lglib-2.0

BEAGREPD_GLUE_SOURCES =		\
	beagrepd-utils.c

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
	thread-glue.c		\
	vmsize-glue.c

SYSINFO_GLUE_LIBADD = $(MONO_LIBS)

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

libbeagrepglue_la_SOURCES = 		\
	$(BEAGREPD_GLUE_SOURCES)		\
	$(IOPRIO_GLUE_SOURCES)		\
	$(SCHEDULER_GLUE_SOURCES)	\
	$(SPAWN_GLUE_SOURCES)		\
	$(SYSINFO_GLUE_SOURCES)		\
	$(XDGMIME_GLUE_SOURCES)		\
	$(EXTRA_GLUE_SOURCES)

libbeagrepglue_la_LIBADD =	\
	$(SYSINFO_GLUE_LIBADD)	\
	$(EXTRA_GLUE_LIBADD)

gluelibdir = $(pkglibdir)
gluelib_LTLIBRARIES = 		\
	libbeagrepglue.la

COND_SOURCES = 			\
	inotify-glue.c

EXTRA_DIST =			\
	$(COND_SOURCES)
