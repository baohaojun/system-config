repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl-all) $(MAKEFILE_LIST) $(shell if test -e local_manifests/local_manifist.xml; then echo local_manifests/local_manifist.xml; fi)
	print-repo-info-as-perl-all > $@
	chmod +x $@
