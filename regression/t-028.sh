#!/bin/bash
#
# Test the header code
#

source $REG_DIR/scaffold

cmd setup_repo

function fixup_time_info
{
	touch -a -m -t "$TOUCH_DATE" ".git/patches/master/$1"
}

shouldfail guilt header

cmd guilt push -a

cmd guilt new -s -m "blah blah blah" patch-with-some-desc
cmd guilt pop
fixup_time_info patch-with-some-desc
cmd guilt push

cmd list_files

cmd guilt header

guilt series | while read n; do
	cmd guilt header $n
done

shouldfail guilt header non-existant 2>&1

# FIXME: how do we check that -e works?
