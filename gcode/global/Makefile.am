## Process this file with automake to create Makefile.in
#
# Copyright (c) 2000, 2001, 2002 Tama Communications Corporation
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
SUBDIRS = libglibc libutil libparser libltdl plugin-example
if !USE_DB185_COMPAT
        SUBDIRS += libdb
endif
SUBDIRS += global gozilla gtags htags htags-refkit gtags-cscope globash \
	doc icons jquery

gtagsdir = ${datadir}/gtags
gtags_DATA = AUTHORS COPYING ChangeLog FAQ INSTALL LICENSE NEWS README THANKS \
        gtags.conf gtags.el gtags.pl gtags.vim gtags-cscope.vim \
	BOKIN_MODEL BOKIN_MODEL_FAQ DONORS

EXTRA_DIST = ${gtags_DATA} reconf.sh convert.pl

# If you would like to install gtags.el to the lisp directory then uncomment
# the following line.
# dist_lisp_LISP = gtags.el
