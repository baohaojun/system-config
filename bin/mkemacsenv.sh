#!/usr/bin/env bash

cat <<EOF > ~/.emacs-path.el
(setq exec-path '(
$(for x in $(echo $PATH|tr : '\n'); do echo "    "\"$x\"; done)))
(setenv "PATH" "$PATH")
$(for x in $(grep export .bashrc* |grep PERL |pn 3|perl -npe 's/=.*//'); do
    eval echo \\\(setenv \\\"$x\\\" \\\"\$$x\\\"\\\);
done)
EOF

