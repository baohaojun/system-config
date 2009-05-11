#
# .bash_profile
#
##################
#
#
#       DO NOT MODIFY THIS FILE
#
#       If you would like to customize your bash shell environment, then add
#       your .bashrc modifications to the file ~/.bashrc
#
#  LOGIN ENVIRONMENT

echo TERM is $TERM
if [[ $TERM == eterm-color ]]; then
    export TERM=screen
    stty cols 178 rows 50
fi
if [[ $TERM == screen ]]; then 
	export BASH_COMP_INC=true;
        export SCREEN_TERM=true;
fi


[[ -f /site/env/bash_profile.pre ]] && . /site/env/bash_profile.pre
PATH=$PATH:.
export PATH

if [ -f ~/.bashrc ];then . ~/.bashrc
fi

