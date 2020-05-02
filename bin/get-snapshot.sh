#!/usr/bin/env bash
set -e

## 如果指定了 N 个文件、目录，选出里面最新的一个；
## 如果只指定了一个参数，一般必须是一个目录，然后选出其下最新的一个文件或目录

if test "$#" -gt 1; then
    command ls -t -d "$@" | head -n 1
    exit
fi

cd ${1:-~/shots}

ignore_pat='\.~.*#$'
newest=$(ls -t -A | grep -v -e "$ignore_pat" | head -n 1)
readlink -m "$newest"
EMACS=t mp >/dev/null 2>&1 "$newest" </dev/null
