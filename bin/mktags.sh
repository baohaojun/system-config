#!/bin/bash

export PATH=~/tools/emacswin/bin/:/bin:/usr/bin:"$PATH"

find . \( '-path' '*/CVS' \
    '-o' '-path' '*/.svn' \
    '-o' '-path' '*/autom4te.cache' \
    '-o' '-path' '*/{arch}' \
    '-o' '-path' '*/.hg' \
    '-o' '-path' '*/_darcs' \
    '-o' '-path' '*/.git' \
    '-o' '-path' '*/.bzr' \
    '-o' '-path' '*~*' \
    '-o' '-path' '*#' \
    '-o' '-path' '*/TAGS' \
    '-o' '-path' '*/semantic.cache' \
    '-o' '-iname' '*.o' \
    '-o' '-iname' '*.class' \
    '-o' '-iname' '*.obj' \
    '-o' '-iname' '*.pyc' \
    '-o' '-path' '*/.ignore' \) -prune \
    -o -type f \( -iname '*.cpp' \
    -o -iname '*.hpp' \
    -o -iname '*.s' \
    -o -iname '*.h' \
    -o -iname '*.c' \
    -o -iname '*.cc' \
    -o -iname '*.py' \
    -o -iname '*.java' \
    -o -iname '*.el' \
    -o -iname '*.pl' \
    -o -iname '*.inl' \
    -o -iname '*.aidl' \
    \) -print0 |xargs -0 etags
