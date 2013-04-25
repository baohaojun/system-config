#!/bin/bash

old_year=0
old_month=-1
old_day=-1

(
    cd $(dirname $(readlink -f $0))
    echo "#+title: Archive"
    find ../blog -name '*.org'|sort -n|reverse|while read name; do
    year=${name:8:4}
    month=${name:13:2}
    day=${name:16:2}

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
    echo "     [[$name][$(perl -ne 'if (m/#\+title: (.*)/i) {print $1}' $name)]]"
    echo

    old_year=$year
    old_month=$month
    old_day=$day

    done
) > $(dirname $(readlink -f $0))/Archive.org

if test -e meta/Archive.org; then
    org-export -L meta/Archive.org
fi
