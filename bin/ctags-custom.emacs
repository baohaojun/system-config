#!/bin/bash
for x in "$@"; do
    if test -e "$x"; then
        grep -Hn '^DEFUN\b' -P "$x" | perl -ne 'if (s/(.*?):(\d+):(DEFUN\s+\("(.*?)".*)/$4 function $2 $1 $3/g) {print}' | tee ~/.logs/$(basename $0).log
    fi
done

#($tag, $type, $line_num, $file) = m/^\s*(\S+)\s+(.*?)\s+(\d+)\s+(\S+)/;
# echo $0 "$@" >> ~/.logs/$(basename $0).log 2>&1
