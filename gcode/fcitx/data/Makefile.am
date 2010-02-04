MAINTAINERCLEANFILES	= Makefile.in

fcim_tabledir = $(SCIM_FCITXDATADIR)
fcim_table_DATA = cj.mb erbi.mb punc.mb pybase.mb pyphrase.mb pySym.mb\
		  sp.dat wbx.mb tables.conf
CLEANFILES = wbx.mb cj.mb erbi.mb pybase.mb pyphrase.mb pyERROR pyPhrase.ok

wbx.mb:wbx.txt
	$(top_srcdir)/tools/txt2mb wbx.txt wbx.mb
cj.mb:cj.txt
	$(top_srcdir)/tools/txt2mb cj.txt cj.mb
erbi.mb:erbi.txt
	$(top_srcdir)/tools/txt2mb erbi.txt erbi.mb
pybase.mb:gbkpy.org  pyPhrase.org
	$(top_srcdir)/tools/createPYMB gbkpy.org pyPhrase.org

EXTRA_DIST = $(fcim_table_DATA)
