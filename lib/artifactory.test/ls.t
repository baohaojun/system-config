#!/usr/bin/env bash
set -e

artifactory ls -u test-local/test -r
artifactory ls -u ${scm_artifactory_url}ui/repos/tree/General/test-local%2Ftest%2Ftest.txt
artifactory ls -u ${scm_artifactory_url}artifactory/test-local/test/test.txt
