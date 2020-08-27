#!/usr/bin/env bash
set -e

for scm_artifactory_url in ${scm_artifactory_url} ${scm_artifactory_url%/}:443/; do
    test "$(artifactory ua2f -u "${scm_artifactory_url}ui/repos/tree/General/hello%2Fworld")" \
         = "${scm_artifactory_url}artifactory/hello/world" &&
        echo ok: ${scm_artifactory_url}
done
