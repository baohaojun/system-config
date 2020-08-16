#!/usr/bin/env bash
set -e

rm -rf ~/tmp/upload-dir.t
mkdir -p ~/tmp/upload-dir.t
date > ~/tmp/upload-dir.t/test.txt

artifactory upload-dir -u ${scm_artifactory_url}artifactory/test-local/test -d ~/tmp/upload-dir.t
artifactory upload-dir -u test-local/test -d ~/tmp/upload-dir.t
