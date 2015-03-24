#!/bin/bash
#
# Test the commands that use get_*_series, while applying guards
#

source $REG_DIR/scaffold

cmd setup_repo

for x in "" modify add remove mode ; do
	cmd list_files
	[ "$x" != "" ] && guilt push "$x"
	cmd guilt branch br-$x
	cmd list_files
	cmd git checkout master
done
