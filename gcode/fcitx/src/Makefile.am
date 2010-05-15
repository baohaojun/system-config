## Makefile.am -- Process this file with automake to produce Makefile.in
## Copyright (C) 2002 James Su
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

MAINTAINERCLEANFILES	= Makefile.in

INCLUDES		= -I$(top_builddir) \
	    		  -I$(top_srcdir) \
			  -I$(top_srcdir)/src\
			  -DSCIM_ICONDIR=\"@SCIM_ICONDIR@\"\
			  -DSCIM_FCITXDATADIR=\"@SCIM_FCITXDATADIR@/\"

noinst_HEADERS = bhjdebug.h ime-socket.h scim_fcitx_imengine.h version.h 
moduledir		= @SCIM_MODULEDIR@/IMEngine
module_LTLIBRARIES	= fcitx.la

fcitx_la_SOURCES = ime-socket.cpp scim_fcitx_imengine.cpp

fcitx_la_CFLAGS = @SCIM_CFLAGS@ @SCIM_DEBUG_FLAGS@
fcitx_la_CXXFLAGS = @SCIM_CFLAGS@ @SCIM_DEBUG_FLAGS@

fcitx_la_LDFLAGS = -avoid-version \
-rpath $(moduledir) \
-module \
@SCIM_LIBS@ 

