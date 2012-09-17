CRAWL_RULES = \
	$(srcdir)/crawl-rules/crawl-applications	\
	$(srcdir)/crawl-rules/crawl-documentation	\
	$(srcdir)/crawl-rules/crawl-executables		\
	$(srcdir)/crawl-rules/crawl-manpages		\
	$(srcdir)/crawl-rules/crawl-monodoc		\
	$(srcdir)/crawl-rules/crawl-windows

# FIXME: Per-distribution specific crawl rules
crawl_rulesdir = $(sysconfdir)/beagrep/crawl-rules
crawl_rules_DATA = $(CRAWL_RULES)

GLOBAL_CONFIG_FILES = \
	$(srcdir)/config-files/BeagrepSearch.xml		\
	$(srcdir)/config-files/Daemon.xml		\
	$(srcdir)/config-files/FilesQueryable.xml	\
	$(srcdir)/config-files/Networking.xml

global_config_filesdir = $(sysconfdir)/beagrep/config-files
global_config_files_DATA = $(GLOBAL_CONFIG_FILES)

confdir = $(sysconfdir)/beagrep
conf_DATA = $(srcdir)/blocate.conf			\
	    $(srcdir)/external-filters.xml.sample	\
	    $(srcdir)/query-mapping.xml

EXTRA_DIST =				\
	$(CRAWL_RULES)			\
	$(GLOBAL_CONFIG_FILES)		\
	$(conf_DATA)
