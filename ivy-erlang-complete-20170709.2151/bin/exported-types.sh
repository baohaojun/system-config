#!/bin/bash
module="$1"; shift
find $@ -name $module.erl | xargs sed -n '/-export_type(/,/)./p' | sed -e '/%/d' | sed -e 's/ //g' | sed -e 's/\t//g' | sed -e '/^$/d' | sed -e '/-export_type(\[.*\\])./{ n ; d }' | sed -e 's/-export_type.*(\[//g' | sed -n '/^-.*(/,/)./!p' | sed 's/]//' | sed 's/).//' | sed 's/\,/\n/g' | sed '/^$/d' | grep /
