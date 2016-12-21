#!/bin/bash
# DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",

for x in "$@"; do
    if test -e "$x"; then
        grep -Hn '^DEFUN\b' -P "$x" |
            perl -ne '
                 chomp;
                 if (m/(.*?):(\d+):(DEFUN\s*\("(.*?)", (.*?),.*)/) {
                     print "$4 function $2 $1 $3\n";
                     print "$5 function $2 $1 $3\n";
                 }' | tee ~/.cache/system-config/logs/$(basename $0).log
    fi
done

#($tag, $type, $line_num, $file) = m/^\s*(\S+)\s+(.*?)\s+(\d+)\s+(\S+)/;
# echo $0 "$@" >> ~/.cache/system-config/logs/$(basename $0).log 2>&1
