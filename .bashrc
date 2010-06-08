export PATH=/bin:"$PATH"
shopt -s nocaseglob
shopt -s histappend
shopt -s cdspell
export HISTCONTROL="ignoredups"
export HISTIGNORE="bg:fg:exit"
export PROMPT_COMMAND="history -a"
alias ls='ls -hF --color=tty --show-control-chars'                 # classify files in colour
alias dir='ls --color=auto --format=vertical --show-control-chars'
alias vdir='ls --color=auto --format=long --show-control-chars'
alias ll='ls -l --show-control-chars'                              # long list
alias la='ls -A --show-control-chars'                              # all but . and ..
alias l='ls -CFl --show-control-chars --block-size=1'                              #
alias vi=vim
export EDITOR=vim
export HISTSIZE=2000
export HISTFILESIZE=100000
if [[ `uname` == CYGWIN_NT-5.1 ]]; 
then
    if . ~/.bashrc-windows; then
        true #nothing
    else #remote login
        return
    fi
else
    . ~/.bashrc-linux
fi
. ~/.bashrc_public

case $- in
    *i*) . ~/.bashrc-interactive;;
esac

alias svngdiff='svn diff --diff-cmd ~/bin/svntkdiff'
export DISPLAY=${DISPLAY:-:0} #if it is already set...
export USER=`whoami`
