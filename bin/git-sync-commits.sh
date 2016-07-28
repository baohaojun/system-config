#!/bin/bash
y=; for x in $(cat ~/1.txt); do
    (
	set -ex; 
	if test "$y"; then
	    git rm -rf --ignore-unmatch -- $(git ls-tree --name-only $y);
	    git rm 
	else
	    true; 
	fi; 
	
	git checkout $x -- . ;
	git rm -rf --ignore-unmatch -- $(
	    git ls-tree --name-only $y gcode/offline.wikipedia/|xargs bash -c 'for x in "$@"; do basename $x; done' true 
	)
	cp $(git ls-tree --name-only $x gcode/offline.wikipedia/) . -av
	git add $(
	    git ls-tree --name-only $x gcode/offline.wikipedia/|
	    xargs bash -c 'for x in "$@"; do basename $x; done' true
	)
	git rm -rf --ignore-unmatch -- $(git ls-tree --name-only $x)
	git rm .bbdb -f || true;
	git rm gcode/offline.wikipedia -rf || true; 
	git rm $(rgrep bbmzc -l -I) -f||true; 
	git commit --date "$(git-get-date $x)" -m "$(git-get-log $x | grep . || echo no commit message provided)" --allow-empty; 
	if test -z "$y"; then
	    start_recursive_shell check it now
	fi
    ) || ( export REV_1=$y; export REV=$x;  start_recursive_shell $x); 
    if test $? = 5; then
	break;
    fi;
    y=$x; 
done
