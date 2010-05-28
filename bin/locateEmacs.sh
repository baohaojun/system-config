#!/bin/bash
(for x in ~/tmp/locate/*; do if test -f "$x"; then locate -d "$x" "$@"; fi; done)|sort -u
#locate "$@" |perl -npe 's!^(.*)$!cygpath.exe -alw "$1"!'|sh|perl -npe 's!\\!/!g; s!^!c:/!'
