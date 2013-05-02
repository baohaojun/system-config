#!/bin/bash
set -e
touch ~/.authinfo
chmod og-r ~/.authinfo
mkdir -p ~/.logs
touch ~/.where.bak
rm -f ~/tmp >/dev/null 2>&1 || true
mkdir -p ~/tmp
cd ~/windows-config/
mkdir -p ~/external/bin/$(uname|perl -npe 's/_.*//')/ext
mkdir -p ~/tmp/notification-manager
mkdir -p ~/external/etc/at
echo ~/external/etc/at >> ~/.where
export PATH=/opt/local/libexec/gnubin:$PATH

function die() {
    echo "$@"
    exit -1
}

function symlink-map() {
    if test $# != 2; then
        die "Error: symlink-map FROM TO"
    fi

    cd "$1"

    for x in `git ls-tree --name-only HEAD`
    do
        if test $(basename $x) = .dir-locals.el; then
            next
        fi

        if test -e "$2"/$x -a "$(readlink -f "$2"/$x)" != "$(readlink -f "$1"/$x)";
        then
            echo "Warning: "$2"/$x already exist and it's not softlink to "$1"/$x"
            mv "$2"/$x "$2"/$x.bak
            ln -s "$1"/$x "$2"/
        elif ! test -e "$2"/$x;
        then
            ln -sf "$1"/$x "$2"/
        fi
    done

}

symlink-map ~/windows-config/ ~/
if test -d ~/.config; then
    symlink-map ~/windows-config/.config.vc/ ~/.config
fi
mkdir -p ~/.local/share/applications
symlink-map ~/windows-config/etc/local-app/ ~/.local/share/applications

if test -e ~/.gitconfig.$USER; then
    ln -sf ~/.gitconfig.$USER ~/.gitconfig
fi

ln -sf .offlineimaprc-$(uname|perl -npe 's/_.*//') ~/.offlineimaprc
if test -e ~/.macbook-air; then
    ln -sf ~/windows-config/etc/.Xmodmap-macbook-air ~/.Xmodmap
else
    ln -sf ~/windows-config/etc/.Xmodmap ~/.Xmodmap
fi
sudo ln -sf ~/etc/rc.local /etc
mkdir -p ~/bin/$(uname|perl -npe 's/_.*//')/ext/`uname -m`/
if test -L ~/.git; then rm -f ~/.git; fi
echo OK
