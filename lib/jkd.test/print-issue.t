#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

jkd print-issue -i $jira
jkd print-issue -i $jira -f Summary
