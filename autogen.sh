#!/bin/sh
set -e
set -x

autopoint
aclocal -I m4
# autoheader
automake --add-missing --copy
autoconf

prefix_dir="$(pkg-config --variable prefix ibus-1.0)"
if test "$(uname)" = FreeBSD; then
    libexec_dir=$prefix_dir/libexec
else
    libexec_dir=$prefix_dir/lib/ibus
fi

./configure \
    --enable-maintainer-mode --prefix=${prefix_dir} \
    --libexecdir=${libexec_dir} \
    "$@"
