#!/bin/bash
(for x in ~/.cache/system-config/locate/*; do if test -f "$x"; then locate -d "$x" "$@"; fi; done)|sort -u
#locate "$@" |perl -npe 's!^(.*)$!cygpath.exe -alw "$1"!'|sh|perl -npe 's!\\!/!g; s!^!c:/!'
