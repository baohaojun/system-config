#!/bin/bash
#
# Test the repair code
#

# FIXME: test status file format upgrade code

source $REG_DIR/scaffold

cmd setup_repo

cmd guilt push -a

cmd list_files

shouldfail guilt repair

cmd list_files

echo | shouldfail guilt repair --full

cmd list_files

yes n | shouldfail guilt repair --full

cmd list_files

yes y | cmd guilt repair --full

cmd list_files

cmd guilt push -a

cmd list_files

yes Y | cmd guilt repair --full

cmd list_files
