#!/bin/bash
#
# Test the series code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt series

cmd guilt series -v

guilt series | while read n ; do
	cmd guilt push

	cmd guilt series -v
done

cmd list_files
