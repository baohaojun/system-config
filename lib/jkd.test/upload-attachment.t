#!/usr/bin/env bash
set -e

jira=$(
    jkd test c |tee /dev/stderr | jq .key -r
    )

date > $TESTDIR/test.txt
jkd upload-attachment -i $jira -f $TESTDIR/test.txt
