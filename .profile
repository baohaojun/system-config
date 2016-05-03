#!/bin/bash
if ! . ~/system-config/etc/check-system-config; then
    return 0
fi

if test "$SYSTEM_CONFIG_INITED" != true -a -e ~/.config/system-config/.bashrc-path; then
    . ~/.config/system-config/.bashrc-path
fi

if test -e ~/system-config/.by-user/$USER/.profile; then
    . ~/system-config/.by-user/$USER/.profile
fi

if test "$QT4_IM_MODULE" -a ! "$QT_IM_MODULE"; then
    export QT_IM_MODULE=$QT4_IM_MODULE
fi


