#!/bin/bash
#
# Test the unapplied code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt unapplied

guilt series | while read n; do
	cmd guilt push

	cmd guilt unapplied

	cmd list_files
done
