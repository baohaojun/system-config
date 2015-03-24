#!/bin/bash
#
# Test the commands that use get_*_series, while applying guards
#

source $REG_DIR/scaffold

cmd setup_repo

function fixup_time_info
{
	touch -a -m -t "$TOUCH_DATE" ".git/patches/master/$1"
}

function std_tests
{
	cmd list_files
	cmd guilt prev
	cmd guilt next
	cmd guilt applied
	cmd guilt unapplied
	cmd list_files
}

# create a couple of dummy (empty) patches strategically placed thoughout
# the series
cmd guilt new first
cmd guilt push modify
cmd guilt new second
cmd guilt push add
cmd guilt new third
cmd guilt push remove
cmd guilt new fourth
cmd guilt push mode
cmd guilt new fifth
cmd guilt pop -a
fixup_time_info first
fixup_time_info second
fixup_time_info third
fixup_time_info fourth
fixup_time_info fifth
fixup_time_info modify
fixup_time_info add
fixup_time_info remove
fixup_time_info mode

p=first

# no guarded patches; no guards selected
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# no guarded patches; one guard selected
cmd guilt select foo
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one positive guarded patch; no guards selected
cmd guilt select -n
cmd guilt guard $p +foo
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one positive guarded patch; that guard selected
cmd guilt select foo
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one positive guarded patch; different guard selected
cmd guilt select -n
cmd guilt select bar
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one negative guarded patch; no guards selected
cmd guilt select -n
cmd guilt guard -l
cmd guilt guard $p -foo
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one negative guarded patch; that guard selected
cmd guilt select foo
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files

# one negative guarded patch; different guard selected
cmd guilt select -n
cmd guilt select bar
cmd guilt guard -l
cmd guilt push -a
std_tests
cmd guilt pop -a
cmd list_files
