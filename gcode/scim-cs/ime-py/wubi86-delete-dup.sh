#!/bin/bash

set -e

word=$1
shift
code=$(select-args -p "Which code to use for $word" "$@")
./wubi86_delete_code.py $code $word "$@" > wubi86-2.py
mv wubi86-2.py wubi86.py
git add wubi86.py
python -c 'import wubi86'
git commit -m "$(echo $word $@)"
