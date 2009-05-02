#!/bin/bash

gnudoit '(find-file "'"`cygpath -alwm \"$1\"`"'")' >~/efindfile.log 2>&1 &

