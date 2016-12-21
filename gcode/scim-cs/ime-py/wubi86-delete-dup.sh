#!/bin/bash

set -e

word=$1
shift
code=$(select-args -p "Which code to use for $word" "$@")
for del_code in $(arg1-arg2 "$(echo $@)" $code); do
    ./wubi86_delete_code.py $del_code $word "$@" > wubi86-2.py
    mv wubi86-2.py wubi86.py
done
git add wubi86.py
echo $word $code - $@ >> $(lookup-file-dir -e .git)/.commit-msg-bhj
