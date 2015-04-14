#!/bin/bash --login
rm ~/.ido.last

. ~/system-config/.bashrc-windows
export USER=`whoami`
unset TERM
unset WINDOW
findexec -c emacs-
