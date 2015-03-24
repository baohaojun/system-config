#!/bin/bash
#
# Test the applied code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt applied

guilt series | while read n; do
	cmd guilt push

	cmd guilt applied

	cmd guilt applied -c

	cmd list_files
done
