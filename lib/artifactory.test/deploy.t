#!/usr/bin/env bash
set -e

date > $TESTDIR/deploy.txt
artifactory deploy -p hello=world -r test-local -s $TESTDIR/ -t test
