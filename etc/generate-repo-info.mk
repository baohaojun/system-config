repo_info.pl : manifest.xml $(shell which print-repo-info-as-perl) $(MAKEFILE_LIST)
	echo '#!/usr/bin/perl' > $@
	repo forall -c print-repo-info-as-perl >> $@
	echo 'foreach (@ARGV) { print $$repo_info{$$ENV{"PWD"}}{$$_} . "\\n"; }' >> $@
	chmod +x $@
