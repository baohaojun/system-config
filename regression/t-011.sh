#!/bin/bash
#
# Test that all commands that should fail do indeed fail if the branch was
# not guilt init'ed
#

source $REG_DIR/scaffold

cmd setup_git_repo

tests="add applied branch commit delete diff export files fold fork graph header import import-commit new next pop prev push rebase refresh rm series status top unapplied"
for t in $tests; do
	shouldfail guilt $t
done
