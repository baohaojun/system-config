#!/bin/bash
#
# Test the top code
#

source $REG_DIR/scaffold

cmd setup_repo

guilt series | while read n ; do
	cmd guilt top

	cmd list_files

	cmd guilt push

	cmd list_files
done

cmd guilt top

cmd list_files
