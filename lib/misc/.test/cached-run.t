#!/usr/bin/env bash
set -e

export CACHED_RUN_VERBOSE_OUTPUT='echo $r'
rm ~/tmp/cached-run.log

export r=$(random)
res1=$(c -rv bash -c 'echo $r')
log r is $r, res1 is $res1

sleep 1
export r=$(random)
res2=$(c -v bash -c 'echo $r')
log r is $r, res2 is $res2

test $res1 = $res2

sleep 2
res3=$(c -v bash -c 'echo $r')
log r is $r, res3 is $res3

test $res2 != $res3
