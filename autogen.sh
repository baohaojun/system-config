#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="beagle"
REQUIRED_AUTOMAKE_VERSION=1.8

(test -f $srcdir/configure.in \
  && test -f $srcdir/README) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

GNOME_AUTOGEN=`which gnome-autogen.sh 2>/dev/null`
if [ -z $GNOME_AUTOGEN ]; then
    echo "Using included gnome-autogen script"
    GNOME_AUTOGEN="./gnome-autogen.sh"
else
    echo "Using system-provided gnome-autogen script"
fi

. $GNOME_AUTOGEN
