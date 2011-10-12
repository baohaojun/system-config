export PATH=/bin:"$PATH"
shopt -s nocaseglob
shopt -s histappend
export HISTCONTROL="ignoredups"
export HISTIGNORE="bg:fg:exit"
export PROMPT_COMMAND="history -a"
alias ls='ls -hF --color=tty --show-control-chars --block-size=1'                 # classify files in colour
alias dir='ls --color=auto --format=vertical --show-control-chars'
alias vdir='ls --color=auto --format=long --show-control-chars'
alias ll='ls -l --show-control-chars'                              # long list
alias la='ls -A --show-control-chars'                              # all but . and ..
alias l='ls -CFl --show-control-chars --block-size=1'                              #
alias vi=vim
export HISTSIZE=2000
export HISTFILESIZE=100000
. ~/.bashrc-public

if test `uname` = CYGWIN_NT-5.1 -o `uname` = CYGWIN_NT-6.1
then
    . ~/.bashrc-windows
else
    . ~/.bashrc-linux
fi

case $- in
    *i*) . ~/.bashrc-interactive;;
esac

alias svngdiff='svn diff --diff-cmd ~/bin/svntkdiff'
export DISPLAY=${DISPLAY:-:0} #if it is already set...
export USER=`whoami`
export USE_CCACHE=1
if test -e ~/.bash_path; then
    . ~/.bash_path
else
    export PATH=`echo -n $PATH|perl -a -F: -ne 'for (@F) {print "$_\n"}'|uniq-even-non-ajacent |perl -ne 'chomp; print ":" unless (1..1); print $_'`
    printf 'export PATH=%q' "$PATH" > ~/.bash_path
fi
if test "$TERM" = dumb; then
    . ~/.bashrc-in-emacs
else
    . ~/.bashrc-no-emacs
fi
