#!/bin/bash
touch ~/tmp/locate/*;
(for x in ~/tmp/locate/*; do locate -d "$x" "$@"; done
locate "$@" )|sort -u|perl -npe 's!^!d:!'
#locate "$@" |perl -npe 's!^(.*)$!cygpath.exe -alw "$1"!'|sh|perl -npe 's!\\!/!g; s!^!c:/!'
