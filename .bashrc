if ! . ~/system-config/etc/check-system-config; then
    return 0
fi

export PATH=/bin:"$PATH"

uname=$(uname)
march=$(uname -m)

if test "$uname" = CYGWIN_NT-5.1 -o "$uname" = CYGWIN_NT-6.1; then
    if test ! "$EMACS"; then
        . ~/system-config/.bashrc-windows
    fi
else
    . ~/system-config/.bashrc-linux
fi

if test ~/.config/system-config/.bashrc-path -ot ~/system-config/etc/path/$uname-$march; then
    if test -e ~/.config/system-config/.bashrc-path; then
        echo re-create ~/.config/system-config/.bashrc-path
        rm -f ~/.config/system-config/.bashrc-path || true
    fi
fi

if test -e ~/.config/system-config/.bashrc-path; then
    if test -z "$RECURSIVE_SHELL"; then
        if test "$USER" = bhj; then
            . ~/.config/system-config/.bashrc-path
        else
            OLD_PATH=$PATH
            . ~/.config/system-config/.bashrc-path
            PATH=$PATH:$OLD_PATH
        fi
    fi
else
    if test -x /opt/local/libexec/gnubin/readlink; then
        export PATH=/opt/local/libexec/gnubin:$PATH
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
                    readlink -m -- $x;
                done
                echo $PATH | tr ':' '\n'
            ) | tr '\n' ':'
               )
        export PATH=$(echo -n $PATH|perl -npe 's,/+:,:,g'|tr ':' '\n'|uniq-even-non-ajacent|rm-last-nl|tr '\n' ':')
        if test "$PATH"; then
            printf 'export PATH=%q\n' "$PATH" > ~/.config/system-config/.bashrc-path.$$
            mv ~/.config/system-config/.bashrc-path.$$ ~/.config/system-config/.bashrc-path
        fi
    else
        echo ~/system-config/etc/path/$uname-$march not exist?
    fi

    if test -e ~/system-config/.by-user/$USER/.home-perl5 -o -e ~/perl5; then
        cat <<'EOF' >> ~/.config/system-config/.bashrc-path
    export PERL_LOCAL_LIB_ROOT="$HOME/perl5";
    export PERL_MB_OPT="--install_base $HOME/perl5";
    export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5";
    export PERL5LIB="$HOME/perl5/lib/perl5/x86_64-linux-gnu-thread-multi:$HOME/perl5/lib/perl5:$HOME/system-config/etc/perl${PERL5LIB:+:$PERL5LIB}";
    export PATH="$HOME/perl5/bin:$PATH";
EOF
    fi
    . ~/.config/system-config/.bashrc-path
fi

if test ! "$EMACS"; then
    . ~/system-config/.bashrc-public
fi
if test "$TERM" = dumb; then
    . ~/system-config/.bashrc-in-emacs
else
    . ~/system-config/.bashrc-no-emacs
fi


export HISTCONTROL="ignoredups"
export HISTIGNORE="bg:fg:exit"
export HISTSIZE=2000
export HISTFILESIZE=2000
if test "$USER"x = x; then
    export USER=`whoami`
fi
export USE_CCACHE=1
export MONO_PATH=~/.mono/MimeKit.1.0.3.0/lib/net40

if test -e ~/system-config/.by-user/$USER/.bashrc; then
    . ~/system-config/.by-user/$USER/.bashrc
fi

alias svngdiff='svn diff --diff-cmd ~/system-config/bin/svntkdiff'
alias ls='ls -hF --color=tty --show-control-chars --block-size=1'                 # classify files in colour
alias dir='ls --color=auto --format=vertical --show-control-chars'
alias vdir='ls --color=auto --format=long --show-control-chars'
alias ll='ls -l --show-control-chars'                              # long list
alias la='ls -A --show-control-chars'                              # all but . and ..
alias l='ls -CFl --show-control-chars --block-size=1'                              #
case $- in
    *i*) . ~/system-config/.bashrc-interactive;;
esac

if test -x ~/external/firefox/firefox; then
    export BROWSER=firefox
fi

for cust in ~/src/github/smartcm/.bashrc-cust \
                ~/system-config/.by-user/$USER/.bashrc-cust \
                ~/.config/system-config/.bashrc-cust \
                ~/.bashrc-cust;
do
    if test -e $cust; then
        . $cust
    fi
done
export SYSTEM_CONFIG_INITED=true
export STAY_OFF_MY_LAWN=true
. ~/system-config/.bashrc-aliases
