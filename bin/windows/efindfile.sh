#!/bin/bash

echo path is "`cygpath -au \"$1\"`" >> ~/efindfile.log
gnudoit '(find-file "'"`cygpath -au \"$1\"`"'")' >>~/efindfile.log 2>&1 &

