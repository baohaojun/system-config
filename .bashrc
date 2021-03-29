if ! . ~/system-config/etc/check-system-config; then
    return 0
fi

export PATH=/bin:"$PATH"

uname=$(uname)
march=$(uname -m)

if [[ $uname =~ CYGWIN_NT- ]]; then
    uname=${uname%-*}
fi

if test "$uname" = CYGWIN_NT; then
    if test ! "$EMACS"; then
        . ~/system-config/.bashrc-windows
    fi
else
    . ~/system-config/.bashrc-linux
    if test "$uname" = FreeBSD; then
        . ~/system-config/.bashrc-freebsd
    fi
fi

if test "${uname}" = Darwin; then
    # use the HomeBrew version of Gnu Tools
    extra_path=(
        /usr/local/Cellar/coreutils/*/libexec/gnubin
        /usr/local/Cellar/gnu-getopt/*/bin
        /usr/local/Cellar/bash/*/bin
        /usr/local/opt/grep/libexec/gnubin
        /usr/local/opt/findutils/libexec/gnubin
        /usr/local/bin
    )
    export SC_PATH_PREFIX=$(
        for x in "${extra_path[@]}"; do
            echo -n $x:
        done
           )
    if test "$(which grep)" != /usr/local/opt/grep/libexec/gnubin/grep; then
        PATH=${SC_PATH_PREFIX}${PATH}
    fi
fi

if test -e ~/system-config/.bashrc-$uname; then
    . ~/system-config/.bashrc-$uname
fi

if test ~/.config/system-config/.bashrc-path -ot ~/system-config/etc/path/$uname-$march; then
    if test -e ~/.config/system-config/.bashrc-path; then
        echo re-create ~/.config/system-config/.bashrc-path
        rm -f ~/.config/system-config/.bashrc-path || true
    fi
fi

OLD_PATH=$PATH

if test ! -e ~/.config/system-config/.bashrc-path; then
    if test -x /usr/local/opt/coreutils/libexec/gnubin; then
        export PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
    fi
    if test -d ~/system-config/etc/path/$uname-$march; then
        rm -rf ~/external/etc/overide/
        export PATH=$(
            builtin cd ~/system-config/etc/path/$uname-$march ||
                builtin cd ~/system-config/etc/path/$uname ||
                builtin cd ~/system-config/etc/path/$(uname|sed -e 's/-.*//') ||
                builtin cd ~/system-config/etc/path/$(uname|sed -e 's/_.*//')
            (
                for x in $(for d in *; do echo $d; done|sort -n); do
                    if test ! -d "${x}" && (
                            t=$(readlink "$x")
                            [[ $t =~ ^(\.\./){4} ]] &&
                                echo ~/${t#../../../../}
                        ); then
                        true
                    else
                        readlink -m -- $x;
                    fi
                done
                echo $PATH | tr ':' '\n'
            ) | tr '\n' ':'
               )
        export PATH=$(
            echo -n $PATH|perl -npe 's,/+:,:,g'|tr ':' '\n'|
                if (which uniq-even-non-ajacent && which rm-last-nl) >/dev/null 2>&1; then
                    uniq-even-non-ajacent|rm-last-nl
                else
                    cat
                fi | tr '\n' ':')
        if test "$PATH"; then
            printf 'export PATH=%q\n' "$PATH" > ~/.config/system-config/.bashrc-path.$$
            mv ~/.config/system-config/.bashrc-path.$$ ~/.config/system-config/.bashrc-path
        fi
    else
        echo ~/system-config/etc/path/$uname-$march not exist?
    fi
fi

if test -z "$RECURSIVE_SHELL"; then
    . ~/.config/system-config/.bashrc-path

    if test -z "${PERL_LOCAL_LIB_ROOT}" -a -e ~/perl5; then
        export PERL_LOCAL_LIB_ROOT="$HOME/perl5";
        export PERL_MB_OPT="--install_base $HOME/perl5";
        export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
        export PERL5LIB="$HOME/perl5/lib/perl5:$HOME/system-config/etc/perl${PERL5LIB:+:$PERL5LIB}";
        export PATH="$HOME/perl5/bin:$PATH";
    fi
    if test ! -e ~/.local-config/etc/baohaojun; then
        PATH=$PATH:$OLD_PATH
    fi
    if test "${SC_PATH_PREFIX}"; then
        PATH=${SC_PATH_PREFIX}${PATH}
    fi
fi

if test ! "$EMACS"; then
    . ~/system-config/.bashrc-public
fi
if test "$TERM" = dumb; then
    . ~/system-config/.bashrc-in-emacs
else
    . ~/system-config/.bashrc-no-emacs
fi

if test "$USER"x = x; then
    export USER=`whoami`
fi

export USE_CCACHE=1
export MONO_PATH=~/.mono/MimeKit.1.0.3.0/lib/net40

if test -e ~/system-config/.by-user/$USER/.bashrc; then
    . ~/system-config/.by-user/$USER/.bashrc
fi

case $- in
    *i*) . ~/system-config/.bashrc-interactive;;
esac

if test -x ~/external/firefox/firefox; then
    export BROWSER=firefox
fi

for cust in ~/src/github/smartcm/.bashrc-cust \
                ~/system-config/.by-user/$USER/.bashrc-cust \
                ~/.config/system-config/.bashrc-cust \
                ~/system-config/.git/sc-custom/.bashrc \
                ~/.bashrc-cust;
do
    if test -e $cust; then
        . $cust
    fi
done
export SYSTEM_CONFIG_INITED=true
export STAY_OFF_MY_LAWN=true # for android build/envsetup.sh
export scm_common_libdir=~/system-config/lib

export PATH=~/.pyenv/bin:$PATH
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
