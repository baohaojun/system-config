## Process this file with automake to create Makefile.in
#
# Copyright (c) 2000 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
bin_PROGRAMS= global

global_SOURCES = global.c

INCLUDES = @INCLUDES@

LDADD = @LDADD@

global_LDADD = $(LDADD) $(LIBLTDL)
global_DEPENDENCIES = $(LDADD) $(LTDLDEPS)

man_MANS = global.1

EXTRA_DIST = $(man_MANS) const.h manual.in
