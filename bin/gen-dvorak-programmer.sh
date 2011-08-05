#!/bin/bash

cat <<EOF
*$1::
GetKeyState, ss, Shift
ss := GetKeyState("Shift", "P")
sc := GetKeyState("CapsLock", "T")
if (ss and not sc or not ss and sc)
{

   SetKeyDelay -1   ; If the destination key is a mouse button, SetMouseDelay is used instead.
   Send {Blind}{Shift up}{$3 DownTemp}{Shift Down}  ; DownTemp is like Down except that other Send commands in the script won't assume "b" should stay down during their Send.
}
else
{
   SetKeyDelay -1
   Send {Blind}{$2 DownTemp}  ; 
}
return

*$1 up::
ss := GetKeyState("Shift", "P")
sc := GetKeyState("CapsLock", "T")
if (ss and not sc or not ss and sc)
{
   SetKeyDelay -1   ; If the destination key is a mouse button, SetMouseDelay is used instead.
   Send {Blind}{$3 Up}  ; DownTemp is like Down except that other Send commands in the script won't assume "b" should stay down during their Send.
}
else
{
   SetKeyDelay -1
   Send {Blind}{$2 Up}  ; 
}
return
EOF
