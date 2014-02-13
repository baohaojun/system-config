#!/system/bin/sh

IFS=$'\t\n'
for y in "$@"; do
    for x in $y*; do
	if test -d "$x"; then
            echo "$x/"; 
	elif test -e "$x"; then
            echo "$x"; 
	fi; 
    done
done
