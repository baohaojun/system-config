repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl) $(MAKEFILE_LIST)
	echo '#!/usr/bin/perl' > $@
	echo 'chomp(my $$realpath = qx(readlink -f $$0));' >> $@
	echo 'my $$android_path = $$realpath;' >> $@
	echo '$$android_path =~ s,/.repo/repo_info.pl,,;' >> $@
	repo forall -c print-repo-info-as-perl >> $@
	echo 'foreach (@ARGV) { print $$repo_info{$$ENV{"PWD"}}{$$_} . "\\n"; }' >> $@
	chmod +x $@
