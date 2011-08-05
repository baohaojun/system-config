#!/bin/bash

cat <<EOF
*$1::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls or srs)
    {
        Send {Blind}{$3 DownTemp}
    }
    else
    {
        Send {Blind}{$2 DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{$2 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{$2 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{Shift Down}{$3 DownTemp}{Shift Up} ;
    }
}
return

*$1 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls or srs)
    {
        Send {Blind}{$3 Up}
    }
    else
    {
        Send {Blind}{$2 Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{$2 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{$2 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{Shift Down}{$3 Up}{Shift Up} ;
    }
}
return
EOF
