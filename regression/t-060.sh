#!/bin/bash
#
# Test the guilt files code
#

source $REG_DIR/scaffold

cmd setup_repo

function guiltfiles_args
{
	cat << DONE

-v
-l
-v -l
-a
-l -a
-v -a
-v -l -a
DONE
}

# create a patch that contains a file in a subdirectory
cmd guilt new subdir

cmd mkdir blah

cmd touch blah/sub

cmd guilt add blah/sub

cmd guilt refresh

# push em all for tesing
cmd guilt push -a

#
# actual tests
#

guiltfiles_args | while read args; do
	cmd guilt files $args
done

#
# test that changes in the index are also considered
#

cmd dd if=/dev/zero of=file.bin bs=1 count=1024 | filter_dd

guiltfiles_args | while read args; do
	cmd guilt files $args
done

cmd git add file.bin

guiltfiles_args | while read args; do
	cmd guilt files $args
done

cmd git rm def

guiltfiles_args | while read args; do
	cmd guilt files $args
done
