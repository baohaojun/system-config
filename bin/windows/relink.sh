#!/bin/bash
src="`/bin/readlink \"$1\"`"
lnk="`basename \"$1\"`".lnk
rm "$1"
cd ~/bin/windows/lnks
CreateLink "`cygpath -alw \"$src\"`" "$lnk"
cd - >/dev/null 2>&1
ln -sf "$src" "$1"

