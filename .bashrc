export PATH=/bin:"$PATH"
shopt -s nocaseglob
shopt -s histappend
shopt -s cdspell
export HISTCONTROL="ignoredups"
export HISTIGNORE="[   ]*:&:bg:fg:exit"
export PROMPT_COMMAND="history -a"
alias ls='ls -hF --color=tty --show-control-chars'                 # classify files in colour
alias dir='ls --color=auto --format=vertical --show-control-chars'
alias vdir='ls --color=auto --format=long --show-control-chars'
alias ll='ls -l --show-control-chars'                              # long list
alias la='ls -A --show-control-chars'                              # all but . and ..
alias l='ls -CFl --show-control-chars --block-size=1'                              #
alias cd='cd -P >/dev/null'
alias vi=vim
export EDITOR=vim
export HISTSIZE=2000
export HISTFILESIZE=100000
export CDPATH=.:~
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
      *i*) [[ -f /etc/bash_completion ]] && . /etc/bash_completion && . ~/.bash_netcompletion ;;

  esac



alias mail=~/bin/mail
alias svngdiff='svn diff --diff-cmd ~/bin/svntkdiff'

if [[ $TERM == eterm-color ]]; then
    export IN_EMACS=true;
fi
    
    
if [[ $IN_EMACS == true ]]; then
    unset EMACS
    #TERM=xterm will cause screen to act weird
    export TERM=vt100
    stty cols 158 rows 58 
    stty -imaxbel
    stty -echoe -echok -echoctl -echoke
    alias ls='ls --show-control-chars -hF'
    alias slin='TERM=eterm-color slin'
fi
export DISPLAY=${DISPLAY:-:0} #if it is already set...
export USER=`whoami`
