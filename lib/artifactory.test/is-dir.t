#!/usr/bin/env bash
set -e

artifactory is-dir -u test-local/test
if ! artifactory is-dir -u test-local/test/hello; then # non exist
    log hello is non-exist?
fi
if ! artifactory is-dir -u test-local/test/test.txt; then
    log right, test.txt is a file, not a dir
fi
