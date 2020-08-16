#!/usr/bin/env bash
set -e
artifactory download-dir -u ${scm_artifactory_url}artifactory/test-local/test/ -d $TESTDIR
artifactory download-dir -u test-local/test/ -d $TESTDIR
