#!/bin/bash
#
# Test the commit code
#

source $REG_DIR/scaffold

cmd setup_repo

function opts_to_try
{
	cat << DONE
-n 0
-n 1
-a
--all
DONE
}

shouldfail guilt commit

cmd list_files
 
opts_to_try | while read opt; do
	cmd guilt commit $opt

	cmd list_files
done 


cmd guilt push -a

shouldfail guilt commit

cmd list_files
 
opts_to_try | while read opt; do
	cmd guilt commit $opt

	cmd list_files
done

echo > /dev/null
