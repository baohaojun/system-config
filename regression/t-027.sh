#!/bin/bash
#
# Test the refresh code
#

source $REG_DIR/scaffold

cmd setup_repo

function fixup_time_info
{
	touch -a -m -t "$TOUCH_DATE" ".git/patches/master/$1"
}

echo abcdef >> def
shouldfail guilt refresh

cmd list_files

cmd git reset --hard HEAD

cmd guilt push modify

echo abcdef >> def
cmd guilt refresh
cmd guilt pop
fixup_time_info modify
cmd guilt push modify

cmd list_files

# FIXME: we should check that the patch actually contains what it should,
# test arguments work the way they should
