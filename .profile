# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.

# if running bash
# include .bashrc if it exists
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
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

if test "$SSH_CLIENT"; then
    if test -e ~/.config/system-config/.bashrc-path; then
        . ~/.config/system-config/.bashrc-path
    fi
fi
