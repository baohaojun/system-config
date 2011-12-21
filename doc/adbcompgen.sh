#!/system/xbin/bash
bind 'set completion-ignore-case on' >/dev/null 2>&1
IFS=$'\t\n'
for x in `compgen -o filenames -f "${1}"`; do
    if test -d "$x"; then
        echo "$x/"; 
    else
        echo "$x"; 
    fi; 
done
