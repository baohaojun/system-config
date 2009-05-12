
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
alias cd='cd -P'

export CDPATH=.:~
alias vi=vim
export EDITOR=vim
export HISTSIZE=2000
export HISTFILESIZE=100000
alias clt='EDITOR="d:\tools\emacswin\emacs-22.1\bin\emacsclient.exe" cleartool'
alias emacsclient='~/emacs/bin/emacsclient.exe -n'
export VC_BUILD_CONFIG=release

. ~/.bashrc-path
alias of=cygstart
alias ps='ps aux -W'
alias reboot=~/bin/reboot
. ~/.bashrc_public
case $- in
      *i*) [[ -f /etc/bash_completion ]] && . /etc/bash_completion && . ~/.bash_netcompletion ;;

  esac
cd
alias klocate='locate -d  ~/knowledge/locatedb'
alias gupdatedb='updatedb --localpaths="/c /d"'
alias mm=mplayer
alias emacs=emacs.sh



function edit_1()
{
    if ! [[ -r "$1" ]]; then
        echo Error: "$1" is not readable
        return 1
    fi

    if ! [[ -w "$1" ]]; then
        echo Warning: "$1" is not writable
    fi
    
    emacsedit.exe "$1" >/dev/null 2>&1

}

function edit()
{
    for x in "$@"; do 
        edit_1 "$x"
    done
}

alias mail=~/bin/mail

function dict() {
    dict.exe -v 2>/dev/null "$@"|iconv -f UTF-8 -t GBK
}
. /c/ssh-agent.log >/dev/null
alias pdfnup='cygexec pdfnup'
alias svngdiff='svn diff --diff-cmd svntkdiff'

function llocate() {
    locateEmacs.sh "$@"|sed -e 's/^.:/"/; s/$/"/'
}
alias locate=llocate

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
export DISPLAY=:0
export USER=`whoami`
function cmd() {
    if [[ -z "$1" ]]; then
        cygstart cmd.exe
    elif [[ -d "$1" ]]; then
        (cd "$1"; cygstart cmd.exe)
    elif [[ -d "`dirname \"$1\"`" ]]; then
        (cd "`dirname \"$1\"`"; cygstart cmd.exe)
    else 
        echo cmd: no such file or directory "$1"
    fi
}
alias mplayer="'/c/Program Files/MPlayer for Windows/MPlayer.exe'"
function lcd()
{
    if [[ -z "$1" ]]; then
        echo Usage: lcd softlink
        return
    fi

    linkTarget=`ls -l "$1"|sed -e 's/.*-> //'`
    if [[ ${linkTarget:0:1} == / ]]; then
        true #nothing to do
    elif [[ ${linkTarget:1:1} == ':' ]]; then #this is a windows pathname (`:' is a special char)
        true
    else
        linkTarget="`dirname \"$1\"`"/"$linkTarget"
    fi
    if [[ -d "$linkTarget" ]]; then
        cd "$linkTarget"
    else
        cd "`dirname \"$linkTarget\"`"
    fi
}

export PYTHON_INCLUDE='c:\python25\include'
export PYTHON_LIB='c:\python25\libs\python25.lib'

export ADBHOST=192.168.100.2
export ADB_HOST=192.168.100.2
