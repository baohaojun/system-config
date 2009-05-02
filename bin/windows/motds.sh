#!/bin/bash

if test -f ~/.mail.addr/"$1"; then
    y=`cat ~/.mail.addr/"$1"|perl -npe 's/.*<//; s/@.*//'`
else 
    for x in "$@"; do 
        y=$y+$x
    done
fi

cygstart 'http://motds.mot.com/default.asp?numParms=3&hasSubmitted=1&addParm=0&delParm=0&selMaxRecords=100&selFormat=html&selAndOr=%26&selOutput=nonav&chkTypeE=on&chkTypeN=on&chkTypeO=on&selSort=5&selAttrib1=multiple&selLogic1=contains&txtValue1='$y'&selAttrib2=fullname&selLogic2=contains&txtValue2=&selAttrib3=motCoreID&selLogic3=contains&txtValue3=&cmdSearch=+++Search+++'
