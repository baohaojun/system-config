#!/usr/bin/env bash
set -e

(
    touch shit
    . aterr rm -f shit
    exit 20
) || touch fuck.$?

if test -e shit; then
    die "failed to remove shit"
fi

if test ! -e fuck.20; then
    die "Failed to exit with correct exit val"
fi
