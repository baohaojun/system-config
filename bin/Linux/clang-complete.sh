#!/bin/bash
exec strace -o ~/tmp/1.txt -s 20000 -f clang-complete.bin "$@"
