#!/usr/bin/env bash
set -e

declare comment_doc=$(
    cat <<'EOFe9576f8668e0' | grep -v '#.*org-mode%'
# {%org-mode%}
此脚本目录用于 Wrench 的封装。

通过使用此脚本集，后续可以：

- 通过 W f xxx@@dd 来查找钉钉中的 xxx
# {%/org-mode%}
EOFe9576f8668e0
        )

me=$(readlink -f $BASH_SOURCE)
if test ! -e "$me"; then
    me=$(readlink -f "$(which $BASH_SOURCE)")
    if test ! -e "$me"; then
        die "Can't find out about me"
        exit 1
    fi
fi

abs0=$BASH_SOURCE
if ! [[ $abs0 =~ ^/ ]]; then
    if [[ $abs0 =~ / ]] && test -e $PWD/$abs0; then
        abs0=$PWD/$abs0
    elif test -e "$(which $BASH_SOURCE)"; then
        abs0=$(which $BASH_SOURCE)
    else
        die "Can't find abs path for $BASH_SOURCE"
    fi
fi

b0=$(basename $BASH_SOURCE)

e "${abs0}"
