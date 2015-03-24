#!/bin/bash
#
# Test the init code
#

source $REG_DIR/scaffold

function opts
{
	cat << DONE

-a
-n
DONE
}

cmd setup_git_repo

opts | while read opt ; do 
	cmd reset_git_repo

	cmd guilt init $opt
	cmd list_files
	shouldfail guilt init $opt
	cmd list_files
done

cmd git branch other
cmd git checkout other

cmd guilt init
cmd list_files
shouldfail guilt init
cmd list_files
