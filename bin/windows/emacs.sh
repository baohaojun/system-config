#!/bin/bash --login
rm ~/.ido.last

. ~/.bashrc-windows
export USER=`whoami`
unset TERM
unset WINDOW
findexec -c emacs-
