#!/usr/bin/env bash
set -e

artifactory groups -a list
artifactory groups -a add -g test-group -u scmtest
artifactory groups -a del -g test-group -u scmtest
