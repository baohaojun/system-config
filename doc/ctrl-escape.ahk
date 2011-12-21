
SendMode Input
#NoEnv
Menu, Tray, Icon, main.cpl, 8

*^Esc::
lctrl := GetKeyState("LControl", "P")
rctrl := GetKeyState("RControl", "P")
SetKeyDelay -1   ;
if (lctrl)
{
    Send {Blind}{LControl up}{Esc DownTemp}{LControl Down}  ;
}
else if (rctrl)
{
    Send {Blind}{RControl up}{Esc DownTemp}{RControl Down}  ;
}
else
{
    Send {Blind}{ DownTemp}  ;
}
return

*^Esc up::
lctrl := GetKeyState("LControl", "P")
rctrl := GetKeyState("RControl", "P")
SetKeyDelay -1   ;
if (lctrl)
{
    Send {Blind}{LControl up}{Esc Up}{LControl Down}  ;
}
else if (rctrl)
{
    Send {Blind}{RControl up}{Esc Up}{RControl Down}  ;
}
else
{
    Send {Blind}{ Up}  ;
}
return
