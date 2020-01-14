#!/bin/bash
module="$1"; shift
find $@ -name $module.erl | xargs sed -E -n '/-export_type\(/,/\)./p' | sed -E -e '/%/d' | sed -E -e 's/ //g' | sed -E -e 's/\t//g' | sed -E -e '/^$/d' | sed -E -e '/-export_type\(\[.*\\]\)./{ n ; d }' | sed -E -e 's/-export_type.*\(\[//g' | sed -E -n '/^-.*\(/,/\)./!p' | sed -E 's/]//' | sed -E 's/\).//' | sed -E 's/\,/\n/g' | sed -E '/^$/d' | grep /
