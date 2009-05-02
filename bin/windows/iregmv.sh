#!/bin/bash
for x in "$@"; do mv -- "$x" `echo "$x"|perl -npe 'chomp; s/[^a-zA-Z0-9.\/]/-/g; s/-+/-/g'`; done
