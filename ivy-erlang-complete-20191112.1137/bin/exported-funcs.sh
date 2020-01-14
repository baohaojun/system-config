#!/bin/bash
module="$1"; shift
find  $@ -name $module.erl | xargs sed -E -n '/-export\(/,/\)./p' | sed -E -e '/%/d' | sed -E -e 's/ //g' | sed -E -e 's/\t//g' | sed -E -e '/^$/d' | sed -E -e '/-export\(\[.*\\]\)./{ n ; d }' | sed -E -e 's/-export.*\(\[//g' | sed -E -e 's/\]\).//g' | sed -E 's/\,/\
/g' | sed -E '/^$/d' | grep /
