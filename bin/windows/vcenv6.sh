#!/bin/bash

function vcenv6_func () {
    local vc_bat_file;
    for x in {c..z}; do 
        if test -f  "/cygdrive/$x/Program Files/Microsoft Visual Studio/VC98/Bin/VCVARS32.BAT";
        then
            vc_bat_file="/cygdrive/$x/Program Files/Microsoft Visual Studio/VC98/Bin/VCVARS32.BAT";
            break;
        fi
    done
    if test -z "$vc_bat_file"; 
    then
        echo "Error: can't find vc6\!"
        return
    else
        echo "vc is found at $vc_bat_file"
    fi
    export INCLUDE=""
    export LIB=""
    cp "$vc_bat_file" ~/.vc6.bat
    for x in INCLUDE LIB; do
        export $x=$( (cat ~/.vc6.bat; echo echo %$x%)|u2d|cmd.exe|d2u|tail -n 1 )
    done
    local pathw=$( (cat ~/.vc6.bat; echo echo %PATH%)|u2d|cmd.exe|d2u|tail -n 1 )
    export PATH=$(cygpath -pu "$pathw")
}

vcenv6_func
