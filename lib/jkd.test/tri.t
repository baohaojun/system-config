#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

jkd tri -i $jira -t "In Progress"
if test "$(jkd print-issue -i $jira -f status | jq .name -r)" = "In Progress"; then
    log "Yes, tri is ok"
else
    log "No, tri failed: $jira"
    exit 1
fi
