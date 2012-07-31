repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl-all) $(MAKEFILE_LIST) $(shell if test -e local_manifest.xml; then echo local_manifest.xml; fi)
	echo '#!/usr/bin/perl' > $@
	echo '$$android_path = $$0;' >> $@
	echo '$$android_path =~ s,/.repo/repo_info.pl,,;' >> $@
	print-repo-info-as-perl-all >> $@
	chmod +x $@
