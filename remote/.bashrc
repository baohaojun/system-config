if [[ $BASH_COMP_INC == true ]]; then
	. ~/.bashrc_not_tramp
else 
    if [[ -z $SCREEN_TERM ]]; then
        export TERM=dumb
    fi
fi
