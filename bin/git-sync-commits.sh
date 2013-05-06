#!/bin/bash
y=; for x in $(cat ~/1.txt); do
    (
	set -ex; 
	if test "$y"; then
	    git rm -rf --ignore-unmatch -- $(git ls-tree --name-only $y);
	else
	    true; 
	fi; 
	y=$x; 
	git co $x -- . ;
	git rm .bbdb -f || true;
	git rm gcode/offline.wikipedia -rf || true; 
	git rm $(rgrep bbmzc -l -I) -f||true; 
	git commit --date "$(git-get-date $x)" -m "$(git-get-log $x | grep . || echo no commit message provided)" --allow-empty; 
    ) || ( export REV_1=$y; export REV=$x;  start-recursive-shell $x); 
    if test $? = 5; then
	break;
    fi;
    y=$x; 
done
