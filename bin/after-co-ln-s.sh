#!/bin/bash
set -e
touch ~/.authinfo ~/.netrc
chmod og-rwx ~/.authinfo ~/.netrc

export PATH=/opt/local/libexec/gnubin:~/system-config/bin/Linux:~/system-config/bin:$PATH

if test ! -e /etc/sudoers.d/$USER && ! yes-or-no-p "Do you have sudo power?"; then
    function sudo() {
        true
    }
    export -f sudo
fi

if test -d  /etc/sudoers.d/ -a ! -e /etc/sudoers.d/$USER && ask-if-not-bhj "Make your sudo command not ask for password?"; then
    sudo bash -c "echo $USER ALL=NOPASSWD: ALL > /etc/sudoers.d/$USER; chmod 440 /etc/sudoers.d/$USER"
fi

if ! which sudo >/dev/null 2>&1 ; then
    function sudo() {
        "$@"
    }
fi
if ! which git >/dev/null 2>&1 ; then
    sudo apt-get install -y git || /cygdrive/c/setup-x86_64.exe -q -n -d -A -P git
fi


mkdir -p ~/Downloads/forever

for x in 15m hourly daily weekly; do
    mkdir -p ~/external/etc/cron.d/$x;
done

mkdir -p ~/.logs

if which sudo && test $(uname)  = Linux -a ! -e ~/.logs/offline-is-unstable; then
    (sudo apt-get install -y -t unstable offlineimap >/dev/null 2>&1 && touch ~/.logs/offline-is-unstable) || true
fi

touch ~/.where.bak
rm -f ~/tmp >/dev/null 2>&1 || true
mkdir -p ~/tmp
cd ~/system-config/

uname=$(uname|perl -npe 's/_.*//')
mkdir -p ~/external/bin/$uname/ext
mkdir -p ~/.cache/notification-manager
mkdir -p ~/external/etc/at
echo ~/external/etc/at >> ~/.where

function die() {
    echo "$@"
    exit -1
}

if test $uname = CYGWIN; then
    if test $(uname) = CYGWIN_NT-5.1; then
        export PATH=~/system-config/bin/windows:$PATH
        function do_ln_1() {
            junction "$1" "$2"
        }
    else
        function do_ln_1() {
            if test -d "$2" -o -d "$1"; then
                dflag=/d
            else
                dflag=
            fi
            echo cmd.exe /c mklink $dflag "$1" "$2"
            cmd.exe /c mklink $dflag "$1" "$2" >/dev/null 2>&1
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
symlink-map-files ~/system-config/etc/subdir-symlinks ~
mkdir -p ~/.local/share/applications
symlink-map ~/system-config/etc/local-app/ ~/.local/share/applications

if test -e ~/.gitconfig.$USER; then
    ln -sf ~/.gitconfig.$USER ~/.gitconfig
else
    cp ~/.gitconfig.bhj ~/.gitconfig || true
    name=$(finger $USER | grep Name: | perl -npe 's/.*Name: //')
    if test "$name"; then
        git config --global user.name "$name"
    else
        git config --global user.name "$USER"
    fi
fi

ln -sf .offlineimaprc-$(uname|perl -npe 's/_.*//') ~/.offlineimaprc

if ask-if-not-bhj "Do you want to use bhj's power button behavior (hibernate)?"; then
    sudo cp ~/etc/systemd/logind.conf /etc/systemd/logind.conf
fi

if ask-if-not-bhj "Do you want to use bhj's git-exclude file?"; then
    git config --global core.excludesfile '~/.git-exclude'
fi

if ask-if-not-bhj "Do you want to switch the ctrl/alt, esc/caps_lock keys?"; then
    mach=$(get-about-me mach)
    if test -d ~/etc/hardware-mach/$mach; then
        relative-link -f ~/etc/hardware-mach/$mach/.Xmodmap ~/.Xmodmap
    fi
fi

if test -L /etc/rc.local || yes-or-no-p "Replace /etc/rc.local with system-config's version?"; then
    if ! test -L /etc/rc.local; then
        sudo cp /etc/rc.local /etc/rc.local.bak
    fi
    sudo ln -sf ~/etc/rc.local /etc >/dev/null 2>&1 || true # no sudo on win32
fi
mkdir -p ~/bin/$(uname|perl -npe 's/_.*//')/ext/`uname -m`/
if test -L ~/.git; then rm -f ~/.git; fi
if test $(uname) = Linux; then
    if ! grep -q -P -e 'iface\s+usb0\s+inet\s+manual' /etc/network/interfaces; then
        sudo bash -c 'echo iface usb0 inet manual >> /etc/network/interfaces'
    fi

    if grep managed=true /etc/NetworkManager/NetworkManager.conf; then
        sudo perl -npe 's/managed=true/managed=false/' -i /etc/NetworkManager/NetworkManager.conf;
    fi
fi


if test -e ~/src/github/ahd/ -a ! -e ~/src/ahd; then
    ln -s ~/src/github/ahd/ ~/src/ahd
fi

if test -e ~/src/github/private-config/.bbdb; then
    ln -s ~/src/github/private-config/.bbdb ~/ -f
fi

if test ! -d ~/.config/about_me; then
    mkdir -p ~/.config/about_me;
fi

if uname | grep cygwin -i; then
    exit 0
fi

set-use-my-own-firefox

if test ! -d ~/.config/about_me && yes-or-no-p "You want to configure your about_me?"; then
    after-co-settings.sh
fi
if test -x ~/src/github/private-config/after-co.sh; then
    ~/src/github/private-config/after-co.sh
fi

if which systemctl >/dev/null 2>&1 && test ! -e /etc/systemd/system/rc-local.service; then
    sudo cp ~/etc/systemd/system/rc-local.service /etc/systemd/system/rc-local.service
    sudo chmod a+X /etc/systemd/system/rc-local.service
    sudo systemctl --system daemon-reload
    sudo systemctl enable rc-local.service
    sudo systemctl start rc-local.service
else
    sudo cp ~/etc/systemd/system/rc-local.service /etc/systemd/system/rc-local.service
fi


sudo mkdir -p /etc/acpi/local/

sync-etc-files
mkdir -p ~/.cache # just in case the following command will create
# .cache with root permission.

if test "$USER" = bhj; then
    update-host-ip phone 192.168.15.244
    update-host-ip home 128.199.228.174
fi || true
(
    set +e
    (
        cd ~/src/github/mobileorg-android/
        git remote add up https://github.com/matburt/mobileorg-android
    )
) >/dev/null 2>&1 || true

if which emacs >/dev/null 2>&1 && test ! -e ~/.config/emacs-config-done; then
    touch ~/.config/emacs-config-done
    ~/system-config/bin/Linux/emacs-install-packages
fi || true

check-perl-module String::ShellQuote libstring-shellquote-perl
echo Simple config OK.
