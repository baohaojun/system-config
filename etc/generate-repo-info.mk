repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl-all) $(MAKEFILE_LIST) $(shell if test -e local_manifest.xml; then echo local_manifest.xml; fi)
	print-repo-info-as-perl-all > $@
	chmod +x $@
