#!/bin/bash

function ask-for-settings() {
    file=$1
    what=$2
    shift 2
    if test ! -e ~/.config/about_me/$file; then
        read -e -p "What is your $what? "
        if test "$REPLY"; then
            echo -n $REPLY > ~/.config/about_me/$file
            "$@"
        fi
    fi
}

function ask-for-confirmation() {
    file=$1
    what=$2
    shift 2
    if test ! -e ~/.config/about_me/$file; then
        if yes-or-no-p "You have made sure $what? "; then
            touch ~/.config/about_me/$file
        fi
    fi
}

ask-for-settings mail "email address" \
    perl -npe '$mail = '"'$mail'"'; s/(^\s*email\s*=\s*).*/$1$mail/' -i ~/system-config/.gitconfig.bhj

ask-for-settings smtp "email smtp server address"
ask-for-settings smtp-port "email smtp server port"
ask-for-settings conn-type "connection type (nil or ssl)"
ask-for-settings 花名 花名

ask-for-confirmation firefox-config "browser.link.open_newwindow.override.external = 1"
ask-for-confirmation kmail-config "prefer html in kmail? (Set in Configure Kmail -> Security)"
