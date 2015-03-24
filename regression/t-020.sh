#!/bin/bash
#
# Test the push code
#

source $REG_DIR/scaffold

cmd setup_repo

#
# incremental push by 1
#
guilt series | while read n ; do
	cmd guilt push

	cmd list_files

	cmd git log -p
done

#
# pop all
#
cmd guilt pop --all

#
# push by name (initially nothing applied)
#
guilt series | while read n ; do
	cmd guilt push $t

	cmd list_files

	cmd git log -p

	cmd guilt pop --all
done

#
# push by name (incrementally)
#
guilt series | while read n ; do
	cmd guilt push $t

	cmd list_files

	cmd git log -p
done

#
# pop all
#
cmd guilt pop --all

npatches=`guilt series | wc -l`
for n in `_seq -2 $npatches`; do
	if [ $n -ge 0 ]; then
		cmd guilt push -n $n
	else
		shouldfail guilt push -n $n
	fi

	cmd list_files

	cmd git log -p

	cmd guilt pop --all
done

cmd list_files

# FIXME:
#   --all
#   -a
#   -n with some patches already applied
