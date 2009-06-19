#!/bin/bash
#locate "$@" |perl -npe 's!^!d:!'
. /c/ssh-agent.log
findexec.exe -c emacs-emacs -F 1

/bin/emacsclient "$@"
