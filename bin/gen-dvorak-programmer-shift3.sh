#!/bin/bash

cat <<EOF
*$1::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (sls or srs)
{
    Send {Blind}{$3 DownTemp}
}
else
{
    Send {Blind}{$2 DownTemp}  ;
}
return

*$1 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (sls or srs)
{
    Send {Blind}{$3 Up}
}
else
{
    Send {Blind}{$2 Up}  ;
}
return
EOF
