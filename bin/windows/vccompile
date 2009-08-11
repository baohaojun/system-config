#!/bin/bash

set -o pipefail
set -e
unset dswSlnFile
for x in *.sln *.dsw ../*.sln ../*.dsw ../../*.sln ../../*.dsw ../../../*.sln ../../../*.dsw; do 
    if [[ -f "$x" ]]; then
        dswSlnFile="$x"
        break
    fi
done

if [[ -z $dswSlnFile ]]; then
    echo "Error: no project file (*.sln|*.dsw) found anywhere!"
    exit -1
fi

cd "`dirname \"$dswSlnFile\"`"
dswSlnFile="`basename \"$dswSlnFile\"`"


function PathW2U()
{
#sample output from vc8, c++ files
#1>.\findexec.cpp(31) : fatal error C1083: Cannot open include file: 'bhjdebug.h': No such file or directory

#sample output from vc9, c# files
#D:\profiles\Program.cs(15,29): error CS0029: Cannot implicitly convert type 'System.Diagnostics.Process[]' to 'System.Diagnostics.Process'

    /bin/perl -ne '
                s/^[0-9]+>//;
                s/\):/) :/g;
                s/\(([0-9]+),[0-9]+\)/($1)/;
                s/error CS([0-9]+):/error C$1:/;
                if (m/(.*?)(\([0-9]+(?:,[0-9]+)?\).*:.*)/) {
                    print `cygpath -au "$1"|tr -d "\n"`.$2."\n"
                } else {
                    print $_
                }
'

}
case $dswSlnFile in 
    *.dsw)
        vc6compile.py $dswSlnFile "$@" 2>&1|PathW2U 
        ;;
    *.sln)
        if grep -q "Format Version 9.00" $dswSlnFile;
        then
            vccompile.py vc8.com $dswSlnFile "$@" 2>&1 | PathW2U 
        elif grep -q "Format Version 10.00" $dswSlnFile
        then
            vccompile.py vc9.com $dswSlnFile "$@" 2>&1 | PathW2U 
        elif grep -q "Format Version 8.00" $dswSlnFile
        then
            vccompile.py vc7.com $dswSlnFile "$@" 2>&1 | PathW2U 
        fi
        ;;
esac 
