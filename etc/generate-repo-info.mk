repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl) $(MAKEFILE_LIST)
	echo '#!/usr/bin/perl' > $@
	echo '$$android_path = $$0;' >> $@
	echo '$$android_path =~ s,/.repo/repo_info.pl,,;' >> $@
	repo forall -c print-repo-info-as-perl >> $@
	echo 'foreach (@ARGV) { print $$repo_info{$$ENV{"PWD"}}{$$_} . "\\n"; }' >> $@
	chmod +x $@
