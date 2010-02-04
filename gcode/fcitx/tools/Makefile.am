MAINTAINERCLEANFILES    = Makefile.in
toolsdir = $(SCIM_DATADIR)/fcitx/tools

INCLUDES = -I$(top_srcdir)/src

bin_PROGRAMS = mb2txt txt2mb createPYMB
mb2txt_SOURCES = mb2txt.c
txt2mb_SOURCES = txt2mb.c
createPYMB_SOURCES = createPYMB.cpp

createPYMB_CFLAGS		= @SCIM_CFLAGS@ @SCIM_DEBUG_FLAGS@
createPYMB_CXXFLAGS	= @SCIM_CFLAGS@ @SCIM_DEBUG_FLAGS@

createPYMB_LDADD	=@SCIM_LIBS@ 



