#!/bin/bash
set -e
cd ${1:-~/user/Documents/My\ Pictures}
find . -type f -mtime -.1 -print0|xargs -0 bash -c 'last=$1; shift; for x in "$@"; do if test "$last" -ot "$x"; then last=$x; fi; done; mp 2>/dev/null "$last"' xx
