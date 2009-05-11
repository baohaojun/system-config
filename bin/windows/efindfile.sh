#!/bin/bash

echo path is "`cygpath -alwm \"$1\"`" >> ~/efindfile.log
gnudoit '(find-file "'"`cygpath -alwm \"$1\"`"'")' >>~/efindfile.log 2>&1 &

