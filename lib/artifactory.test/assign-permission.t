#!/usr/bin/env bash
set -e

artifactory assign-permission -n hello-2 -u scmtest -a ${scm_artifactory_url}artifactory/test-local/test/test.txt
