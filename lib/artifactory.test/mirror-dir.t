#!/usr/bin/env bash
set -e
artifactory mirror-dir -u ${scm_artifactory_url}artifactory/test-local/test/ -d $TESTDIR
artifactory mirror-dir -u test-local/test/ -d $TESTDIR
