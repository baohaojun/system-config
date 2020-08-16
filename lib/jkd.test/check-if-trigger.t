#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

jkd check-if-trigger -i $jira
jkd tri -i $jira -t "In Progress"
jkd check-if-trigger -i $jira
if jkd check-if-trigger -i $jira; then
    exit 1
elif test $? = 42; then
    log "Yes, we lost trigger fight"
    exit 0
fi
