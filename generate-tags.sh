#!/bin/bash

set -e

for tag in $(
    for x in $(find . -name '*.org'|xargs grep '# bhj-tags:' |perl -npe 's/^.*?: //'); do
        echo $x;
    done|sort -u
); do
    mkdir meta/tags/$tag -p;
    descfile=meta/tags/$tag.desc
    if test ! -e $descfile; then
        read -e -p "input desc for tag $tag: " desc;
        echo $desc > $descfile;
    fi
done


for org in $(find blog -name '*.org'); do
    for tag in $(cat $org|grep '# bhj-tags:' |perl -npe 's/^.*?: //'); do
        tagd=meta/tags/$tag
        if test ! -e $tagd/$(basename $org); then
            relative-link $org $tagd
        fi
    done
done

for tagd in meta/tags/*/; do
    tag=$(basename $tagd)
    old_year=0
    old_month=-1
    old_day=-1

    (
        cd $(dirname $(readlink -f $0))
        echo "#+title: $(cat meta/tags/$tag.desc)"
        find $tagd -name '*.org'|xargs bash -c 'for x in "$@"; do readlink -f $x; done' true|sort -n|reverse|while read name; do
            name=$(readlink -f $name)
            name=${name:${#PWD}+1}
            year=${name:5:4}
            month=${name:10:2}
            day=${name:13:2}

            if test $year != $old_year; then
                echo " * $year"
                old_month=-1
                old_day=-1
            fi

            if test $month != $old_month; then
                echo "  - $year-$month"
                old_day=-1
            fi

            if test $day != $old_day; then
                echo "   * $year-$month-$day"
                echo
            fi
            echo "     [[$(relative-path $name $tagd/..)][$(perl -ne 'if (m/#\+title: (.*)/i) {print $1}' $name)]]"
            echo

            old_year=$year
            old_month=$month
            old_day=$day
        done
    ) > $(dirname $tagd)/$tag.org
done


org-export -L $(
    (
        git st -s meta/tags/*.org|pn 2
        for x in meta/tags/*.org; do
            if test ! -e ${x/%.org/.html}; then
                echo $x
            fi
        done
    ) | sort -u
)
