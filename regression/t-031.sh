#!/bin/bash
#
# Test the fork code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt push -n 2

cmd list_files
 
shouldfail guilt fork mode

cmd list_files
 
cmd guilt fork foobar

cmd list_files
