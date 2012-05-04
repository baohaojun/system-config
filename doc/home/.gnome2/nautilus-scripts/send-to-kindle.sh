#!/bin/bash

me=baohaojun@gmail.com
mek=baohaojun@free.kindle.com
for x in "$@"; do
    calibre-smtp \
	-r smtp.gmail.com \
	--port 587 \
	--username $me \
	--password "$(get-authinfo localhost $me)" \
	-vv --subject convert \
	--attachment "$x" \
	$me $mek "email"
done
