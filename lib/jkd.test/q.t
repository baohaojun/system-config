#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

jiras=$(
    jkd q -q "project = $TESTPROJ and Summary ~ \"hello world\" and key = $jira" -p
     )

if test "$(echo "$jiras" |jq '.issues|.[]|.key' -r)" = $jira; then
    log "Yes, query is correct"
fi
