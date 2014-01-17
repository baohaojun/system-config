#!/bin/bash

function ask-for-settings() {
    file=$1
    what=$2
    shift 2

    local -a selections
    TEMP=$(getopt -o s: --long select: -n $(basename $0) -- "$@")
    eval set -- "$TEMP"
    while true; do
        case "$1" in
            -s|--select)
                selections=("${selections[@]}" "$2")
                shift 2
                ;;
            --)
                shift
                break
                ;;
            *)
                die "internal error"
                ;;
        esac
    done

    if test ! -e ~/.config/about_me/$file; then
        if test ${#selections[@]} != 0; then
            REPLY=$(select-args -p "What is your $what? " -- "${selections[@]}")
        else
            read -e -p "What is your $what? "
        fi
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
    -- perl -npe '$mail = '"'$mail'"'; s/(^\s*email\s*=\s*).*/$1$mail/' -i ~/system-config/.gitconfig.bhj

ask-for-settings smtp "email smtp server address"
ask-for-settings smtp-port "email smtp server port"
ask-for-settings conn-type "connection type" -s nil -s ssl
ask-for-settings 花名 花名
ask-for-settings mach "Machine type" -s macbookair -s thinkpad-t430 -s generic

ask-for-confirmation firefox-config "browser.link.open_newwindow.override.external = 1"
ask-for-confirmation kmail-config "prefer html in kmail? (Set in Configure Kmail -> Security)"
ask-for-confirmation xfce4-ctrl-esc "Have you removed the control escape in xfce4-keyboard-settings"
ask-for-confirmation xfce4-disable-volumed "Have you removed the xfce4-volumed from xfce4-settings-manager"
