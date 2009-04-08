# base-files version 3.7-1

# To pick up the latest recommended .bashrc content,
# look in /etc/defaults/etc/skel/.bashrc

# Modifying /etc/skel/.bashrc directly will prevent
# setup from updating it.

# The copy in your home directory (~/.bashrc) is yours, please
# feel free to customise it to create a shell
# environment to your liking.  If you feel a change
# would be benificial to all, please feel free to send
# a patch to the cygwin mailing list.

# User dependent .bashrc file


# Shell Options
# #############

# See man bash for more options...

# Don't wait for job termination notification
# set -o notify

# Don't use ^D to exit
# set -o ignoreeof

# Use case-insensitive filename globbing
shopt -s nocaseglob

# Make bash append rather than overwrite the history on disk
 shopt -s histappend

# When changing directory small typos can be ignored by bash
# for example, cd /vr/lgo/apaache would find /var/log/apache
 shopt -s cdspell


# Completion options
# ##################

# These completion tuning parameters change the default behavior of bash_completion:

# Define to access remotely checked-out files over passwordless ssh for CVS
# COMP_CVS_REMOTE=1

# Define to avoid stripping description in --option=description of './configure --help'
# COMP_CONFIGURE_HINTS=1

# Define to avoid flattening internal contents of tar files
# COMP_TAR_INTERNAL_PATHS=1

# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.

# History Options
# ###############

# Don't put duplicate lines in the history.
 export HISTCONTROL="ignoredups"

# Ignore some controlling instructions
 export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Whenever displaying the prompt, write the previous line to disk
 export PROMPT_COMMAND="history -a"


# Aliases
# #######

# Some example alias instructions
# If these are enabled they will be used instead of any instructions
# they may mask.  For example, alias rm='rm -i' will mask the rm
# application.  To override the alias instruction use a \ before, ie
# \rm will call the real rm not the alias.

# Interactive operation...
# alias rm='rm -i'
# alias cp='cp -i'
# alias mv='mv -i'

# Default to human readable figures
# alias df='df -h'
# alias du='du -h'

# Misc :)
# alias less='less -r'                          # raw control characters
# alias whence='type -a'                        # where, of a sort
# alias grep='grep --color'                     # show differences in colour

# Some shortcuts for different directory listings
 alias ls='ls -hF --color=tty --show-control-chars'                 # classify files in colour
 alias dir='ls --color=auto --format=vertical --show-control-chars'
 alias vdir='ls --color=auto --format=long --show-control-chars'
 alias ll='ls -l --show-control-chars'                              # long list
 alias la='ls -A --show-control-chars'                              # all but . and ..
 alias l='ls -CFl --show-control-chars --block-size=1'                              #


# Functions
# #########

# Some example functions
# function settitle() { echo -ne "\e]2;$@\a\e]1;$@\a"; }
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
#alias dict='dict -v 2>/dev/null'
alias mm=mplayer
alias emacs=emacs.sh
#alias edit=emacsedit.bat



function edit_1()
{
    if ! [[ -r "$1" ]]; then
        echo Error: "$1" is not readable
        return 1
    fi

    if ! [[ -w "$1" ]]; then
        echo Warning: "$1" is not writable
    fi
    
    local IFS=$'\n'

    local dir=$( dirname "$1" )
    local file=$( basename "$1" )
    cd -P "$dir" >/dev/null 2>&1
    emacsedit.exe "$file"
    cd - >/dev/null 2>&1
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
#export http_proxy=http://wwwgate0-ch.mot.com:1080
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
export HOME=/q

export PYTHON_INCLUDE='c:\python25\include'
export PYTHON_LIB='c:\python25\libs\python25.lib'
