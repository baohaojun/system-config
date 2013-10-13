#!/bin/bash
set -e
touch ~/.authinfo
chmod og-r ~/.authinfo
mkdir -p ~/.logs
touch ~/.where.bak
rm -f ~/tmp >/dev/null 2>&1 || true
mkdir -p ~/tmp
cd ~/system-config/

uname=$(uname|perl -npe 's/_.*//')
mkdir -p ~/external/bin/$uname/ext
mkdir -p ~/.cache/notification-manager
mkdir -p ~/external/etc/at
echo ~/external/etc/at >> ~/.where
export PATH=/opt/local/libexec/gnubin:$PATH

function die() {
    echo "$@"
    exit -1
}

if test $uname = CYGWIN; then
    if test $(uname) = CYGWIN_NT-5.1; then
        function do_ln_1() {
            junction "$1" "$2"
        }
    else
        function do_ln_1() {
            cmd.exe /c mklink  "$1" "$2"
        }
    fi

    function ln() {
        soft=false
        force=false
        TEMP=$(getopt -o sf --long soft,force -n ln -- "$@")
        eval set -- "$TEMP"
        while true; do
            case "$1" in
                -s|--soft)
                    soft=true
                    shift
                    ;;
                -f|--force)
                    force=true
                    shift
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

        if test $soft = false; then
            die "Can not do hard link";
        fi

        if test $# = 1; then
            set -- $# .
        fi

        args=("$@")

        last=${args[${#args[@]} - 1]}
        n_args=${#args[@]}

        if test $n_args -gt 2 -a ! -d "$last"; then
            die "$last not directory"
        fi

        if test ! -d "$last" -a -e "$last"; then
            if test $force = false; then
                die "$last already exist"
            else
                rm "$last"
            fi
        fi



        for n in $(seq 0 $((${#args[@]} - 2))); do
            dest=$last
            if test -d $last; then
                dest=$last/$(basename ${args[$n]})
            fi

            if test $force = true; then
                rm -f $dest
            elif test -e $dest; then
                die "$dest already exist"
            fi
            do_ln_1 "$(cygpath -wa "$dest")" "$(cygpath -wa "${args[$n]}")"
        done
    }
fi


function symlink-map-files() {
    if test $# != 2; then
        die "Error: symlink-map-files FROM TO"
    fi

    cd "$1"
    for x in $(git ls-tree --name-only HEAD -r); do
        if test $(basename $x) = .dir-locals.el; then
            continue
        fi

        if test -e "$2"/$x -a "$(readlink -f "$2"/$x)" != "$(readlink -f "$1"/$x)"; then
            echo "Warning: "$2"/$x already exist and it's not softlink to "$1"/$x"
            mv "$2"/$x "$2"/$x.bak
            ln -sT "$1"/$x "$2"/$x
        elif test ! -e "$2"/$x; then
            mkdir -p "$(dirname "$2"/$x)"
            ln -sf "$1"/$x "$2"/$x
        fi
    done
}

function symlink-map() {
    if test $# != 2; then
        die "Error: symlink-map FROM TO"
    fi

    cd "$1"

    for x in `git ls-tree --name-only HEAD`
    do
        if test $(basename $x) = .dir-locals.el; then
            continue
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

symlink-map ~/system-config ~
symlink-map-files ~/system-config/.subdir-symlinks ~
mkdir -p ~/.local/share/applications
symlink-map ~/system-config/etc/local-app/ ~/.local/share/applications

if test -e ~/.gitconfig.$USER; then
    ln -sf ~/.gitconfig.$USER ~/.gitconfig
else
    cp ~/.gitconfig.bhj ~/.gitconfig
    name=$(finger $USER | grep Name: | perl -npe 's/.*Name: //')
    if test "$name"; then
        git config --global user.name "$name"
    else
        git config --global user.name "$USER"
    fi
fi

ln -sf .offlineimaprc-$(uname|perl -npe 's/_.*//') ~/.offlineimaprc

if ask-if-not-bhj "Do you want to switch the ctrl/alt, esc/caps_lock keys?"; then
    if test -e ~/.macbook-air; then
        ln -sf ~/system-config/etc/.Xmodmap-macbook-air ~/.Xmodmap
    elif test -e ~/.mach=t430; then
        ln -sf ~/system-config/etc/.Xmodmap-t430 ~/.Xmodmap
    else
        ln -sf ~/system-config/etc/.Xmodmap ~/.Xmodmap
    fi
fi
sudo ln -sf ~/etc/rc.local /etc || true # no sudo on win32
mkdir -p ~/bin/$(uname|perl -npe 's/_.*//')/ext/`uname -m`/
if test -L ~/.git; then rm -f ~/.git; fi
if ! grep -q -P -e 'iface\s+usb0\s+inet\s+manual' /etc/network/interfaces; then
    sudo bash -c 'echo iface usb0 inet manual >> /etc/network/interfaces'
fi

if grep managed=true /etc/NetworkManager/NetworkManager.conf; then
    sudo perl -npe 's/managed=true/managed=false/' -i /etc/NetworkManager/NetworkManager.conf;
fi

if test -e ~/src/github/ahd/ -a ! -e ~/src/ahd; then
    ln -s ~/src/github/ahd/ ~/src/ahd
fi

if test -e ~/src/github/private-config/.bbdb; then
    ln -s ~/src/github/private-config/.bbdb ~/
fi

echo OK

if test ! -d ~/.config/about_me; then
    mkdir -p ~/.config/about_me;
fi

after-co-settings.sh
sudo cp ~/doc/usr/lib/pm-utils/sleep.d/99-switch-touchpad /usr/lib/pm-utils/sleep.d/
sudo update-host-ip phone 192.168.15.244
