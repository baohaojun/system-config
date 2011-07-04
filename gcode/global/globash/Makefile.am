## Process this file with automake to create Makefile.in
#
# Copyright (c) 2010 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
bin_SCRIPTS = globash

body = $(srcdir)/$(bin_SCRIPTS).in

$(bin_SCRIPTS): $(body)
	sed -e 's!@DATADIR@!'$(datadir)/gtags'!g' $(body) >$(bin_SCRIPTS).tmp && \
	chmod +x $(bin_SCRIPTS).tmp && mv $(bin_SCRIPTS).tmp $(bin_SCRIPTS)

man_MANS = $(bin_SCRIPTS).1

gtagsdir = ${datadir}/gtags
gtags_DATA = globash.rc

EXTRA_DIST = $(man_MANS) $(bin_SCRIPTS).in $(gtags_DATA)

CLEANFILES = $(bin_SCRIPTS)
