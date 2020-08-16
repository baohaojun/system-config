#!/usr/bin/env bash
set -e

date > $TESTDIR/rm.txt
artifactory upload-dir -u test-local/test -d $TESTDIR/
artifactory rm -u test-local/test/rm.txt
