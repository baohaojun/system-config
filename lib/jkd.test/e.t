#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

jkd e -i $jira --fields-json '{"Summary" : "Hello Summary"}'
