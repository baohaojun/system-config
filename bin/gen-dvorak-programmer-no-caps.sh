#!/bin/bash

cat <<EOF
*$1::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
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
    Send {Blind}{$3 DownTemp}  ;
}
return

*$1 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
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
        Send {Blind}{$3 Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{$3 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{$3 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{$2 Up}  ;
    }
}
return
EOF
