#!/bin/bash

set -e
cd ~/Maildir;
for x in */new; do
    test $(ls $x|wc -l) == 0 || exit 0
done

for x in */cur; do
    ls $x|perl -npe 's/.*!//'|grep -v S && exit 0 || true
done

exit 1
