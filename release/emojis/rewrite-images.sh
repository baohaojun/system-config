#!/bin/bash

function n_to_unicode_char() {
    for x in "$@"; do
        perl -e 'use Encode; print encode_utf8(chr(hex("0x$ARGV[0]")))' "$x"
    done
}

declare -A skin_color_map

skin_color_map[1f3fb]="white"
skin_color_map[1f3fc]="light brown"
skin_color_map[1f3fd]="olive toned"
skin_color_map[1f3fe]="deep brown"
skin_color_map[1f3ff]="black"

find . -type f -name '*.png' |cut -b 3- |
    grep -P '^[1-9a-f]{4}'|
    sort |
    while read f; do
        u=(
            $(
                echo "$f" | perl -ne '
    while (m/(\b[0-9a-f]{4,})/g) {
        print "$1 ";
    }'
            )
        )

        name=$(unicode-name "${u[0]}")
        if test "${#u[@]}" != 1; then
            if [[ ${u[1]} =~ ^1f3f[b-f] ]]; then
                case ${u[1]} in
                    *b)
                        var=1
                        ;;
                    *c)
                        var=2
                        ;;
                    *d)
                        var=3
                        ;;
                    *e)
                        var=4
                        ;;
                    *f)
                        var=5
                        ;;
                esac
                mv "$f" ${name// /_}.$var.png
                relative-link ${name// /_}.$var.png "$f"
                printf "{'%s', '%s in %s skin unicode', 'emojis/emojione-v2.2.6-22/%s', },\n" \
                       $(n_to_unicode_char "${u[@]}") "$name" "${skin_color_map[${u[1]}]}" ${name// /_}.$var.png
            elif [[ $name =~ 'REGIONAL INDICATOR' ]]; then
                c1=${name#*LETTER }
                c2=$(unicode-name "${u[1]}"|perl -npe 's/.*LETTER //')
                mv "$f" regional-$c1$c2.png
                relative-link regional-$c1$c2.png "$f" -f
                printf "{'%s', 'regional %s unicode', 'emojis/emojione-v2.2.6-22/%s', },\n" \
                       $(n_to_unicode_char "${u[@]}") "$c1$c2" regional-$c1$c2.png
            fi
        else
            mv "$f" ${name// /_}.png
            relative-link ${name// /_}.png "$f" -f
            printf "{'%s', '%s unicode', 'emojis/emojione-v2.2.6-22/%s', },\n" \
                   $(n_to_unicode_char ${u[0]}) "$name" ${name// /_}.png
        fi
    done | tee -a ../new-emojis-$$.lua
