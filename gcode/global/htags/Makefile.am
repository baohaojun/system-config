## Process this file with automake to create Makefile.in
#
# Copyright (c) 2000, 2001, 2003, 2004, 2010 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
bin_PROGRAMS= htags

htags_SOURCES = htags.c defineindex.c dupindex.c fileindex.c cflowindex.c src2html.c \
		anchor.c cache.c common.c incop.c assoc.c path2url.c \
		c.c cpp.c java.c php.c asm.c

noinst_HEADERS = htags.h anchor.h cache.h common.h incop.h assoc.h path2url.h \
		lexcommon.h

INCLUDES = @INCLUDES@ -I$(srcdir)

LDADD = @LDADD@

htags_DEPENDENCIES = $(LDADD)

man_MANS = htags.1

gtagsdir = ${datadir}/gtags
gtags_DATA = global.cgi.tmpl ghtml.cgi.tmpl completion.cgi.tmpl bless.sh.tmpl \
	jscode_suggest.tmpl jscode_treeview.tmpl style.css.tmpl
EXTRA_DIST = $(man_MANS) $(gtags_DATA) manual.in global.cgi.tmpl.in completion.cgi.tmpl.in \
		const.h c.l cpp.l java.l php.l asm.l
install-data-hook:
	dir="$(DESTDIR)$(localstatedir)/gtags/sitekeys"; \
	if [ ! -d $$dir ]; then \
		$(MKDIR_P) $$dir; \
		chmod 755 $$dir; \
	fi
