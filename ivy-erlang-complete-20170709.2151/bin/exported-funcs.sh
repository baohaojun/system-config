#!/bin/bash
module="$1"; shift
find  $@ -name $module.erl | xargs sed -n '/-export(/,/)./p' | sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\t//g' | sed -e '/^$/d' | sed -e '/-export(\[.*\\])./{ n ; d }' | sed -e 's/-export.*(\[//g' | sed -e 's/\]).//g' | sed 's/\,/\
/g' | sed '/^$/d' | grep /
