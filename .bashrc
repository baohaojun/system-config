if [ -e /etc/bash.bashrc ] ; then
  source /etc/bash.bashrc
fi

export PATH=/bin:"$PATH"

uname=$(uname)
if test "$uname" = CYGWIN_NT-5.1 -o "$uname" = CYGWIN_NT-6.1
then
    if test ! "$EMACS"; then
        . ~/system-config/.bashrc-windows
    fi
else
    . ~/system-config/.bashrc-linux
fi

if test -e ~/.config/system-config/.bashrc-path; then
    if test -z "$RECURSIVE_SHELL"; then
        . ~/.config/system-config/.bashrc-path
    fi
else
    if test -x /opt/local/libexec/gnubin/readlink; then
        export PATH=/opt/local/libexec/gnubin:$PATH
    fi
    if test -d ~/system-config/etc/path/$(uname)-$(uname -m); then
        rm -rf ~/external/etc/overide/
        export PATH=$(
            builtin cd ~/system-config/etc/path/$(uname)-$(uname -m) ||
                builtin cd ~/system-config/etc/path/$(uname) ||
                builtin cd ~/system-config/etc/path/$(uname|sed -e 's/-.*//') ||
                builtin cd ~/system-config/etc/path/$(uname|sed -e 's/_.*//')
            (
                for x in $(for d in *; do echo $d; done|sort -n); do
                    readlink -m -- $x;
                done
                echo $PATH | tr ':' '\n'
            ) | tr '\n' ':'
        )
    fi
    export PATH=$(echo -n $PATH|perl -npe 's,/+:,:,g'|tr ':' '\n'|uniq-even-non-ajacent|rm-last-nl|tr '\n' ':')
    printf 'export PATH=%q\n' "$PATH" > ~/.config/system-config/.bashrc-path

if ask-if-not-bhj "Install cpan into your \$HOME/perl5?"; then
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
DEBEMAIL="baohaojun@gmail.com"
DEBFULLNAME="Bao Haojun"
export DEBEMAIL DEBFULLNAME

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US:
export INFOPATH=${HOME}/src/github/org-mode/doc:${INFOPATH}
export ANDROID_HOME=${ANDROID_HOME:-~/external/bin/Linux/ext/android-sdk-linux}
export ANDROIDSDK_HOME=${ANDROIDSDK_HOME:-$ANDROID_HOME}
export ANDROID_NDK_HOME=${ANDROID_NDK_HOME:-~/external/bin/Linux/ext/ndk}
export HISTCONTROL="ignoredups"
export HISTIGNORE="bg:fg:exit"
export HISTSIZE=2000
export HISTFILESIZE=2000
if test "$USER"x = x; then
    export USER=`whoami`
fi
export USE_CCACHE=1
export MONO_PATH=~/.mono/MimeKit.1.0.3.0/lib/net40

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
if test -e ~/.bashrc-cust; then
    . ~/.bashrc-cust;
fi
export SYSTEM_CONFIG_INITED=true
