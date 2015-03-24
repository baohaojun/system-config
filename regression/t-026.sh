#!/bin/bash
#
# Test the delete code
#

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt delete mode

cmd list_files

cmd guilt delete mode
# FIXME: this should return a non-zero status, no?

cmd list_files

cmd guilt delete add

cmd list_files

# FIXME: test delete -f
