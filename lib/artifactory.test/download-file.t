#!/usr/bin/env bash
set -e

date > $TESTDIR/download-file.txt
artifactory upload-dir -u test-local/test -d $TESTDIR
artifactory download-file -u test-local/test/download-file.txt -d $TESTDIR -n download-file.txt
artifactory download-file -u ${scm_artifactory_url}artifactory/test-local/test/download-file.txt -d $TESTDIR -n download-file.txt
artifactory download-file -u ${scm_artifactory_url}ui/repos/tree/General/test-local%2Ftest%2Fdownload-file.txt -d $TESTDIR -n download-file.txt
