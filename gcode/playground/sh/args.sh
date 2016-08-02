#!/bin/bash

bash -c "echo \"\$@\" > ~/tmp/1.txt 2>&1" true "$@"
cat ~/tmp/1.txt
