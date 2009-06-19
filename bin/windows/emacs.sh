#!/bin/bash --login
rm ~/.ido.last

. ~/.bashrc-windows
export USER=`whomai`
unset TERM
unset WINDOW
findexec -c emacs-
