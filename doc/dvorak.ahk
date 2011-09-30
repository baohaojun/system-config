SendMode Input
#NoEnv
Menu, Tray, Icon, main.cpl, 8

;----------- REMAP TO DVORAK

*`::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{~ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{~ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{$ DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{$ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{$ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{~ DownTemp}  ;
    }
}
return

*` up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{~ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{~ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{$ Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{$ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{$ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{~ Up}  ;
    }
}
return
*SC028::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{_ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{_ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{- DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{- DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{- DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{_ DownTemp}  ;
    }
}
return

*SC028 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{_ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{_ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{- Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{- Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{- Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{_ Up}  ;
    }
}
return
*1::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls or srs)
    {
        Send {Blind}{5 DownTemp}
    }
    else
    {
        Send {Blind}{& DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{& DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{& DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{Shift Down}{5 DownTemp}{Shift Up} ;
    }
}
return

*1 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls or srs)
    {
        Send {Blind}{5 Up}
    }
    else
    {
        Send {Blind}{& Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{& Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{& Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{Shift Down}{5 Up}{Shift Up} ;
    }
}
return
*2::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{7 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{7 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{[ DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{[ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{[ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{7 DownTemp}  ;
    }
}
return

*2 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{7 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{7 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{[ Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{[ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{[ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{7 Up}  ;
    }
}
return
*3::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{5 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{5 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{{ DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{{ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{{ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{5 DownTemp}  ;
    }
}
return

*3 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{5 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{5 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{{ Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{{ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{{ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{5 Up}  ;
    }
}
return
*4::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{3 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{3 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{} DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{} DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{} DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{3 DownTemp}  ;
    }
}
return

*4 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{3 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{3 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{} Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{} Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{} Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{3 Up}  ;
    }
}
return
*5::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{1 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{1 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{( DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{( DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{( DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{1 DownTemp}  ;
    }
}
return

*5 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{1 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{1 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{( Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{( Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{( Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{1 Up}  ;
    }
}
return
*6::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{9 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{9 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{= DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{= DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{= DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{9 DownTemp}  ;
    }
}
return

*6 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{9 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{9 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{= Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{= Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{= Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{9 Up}  ;
    }
}
return
*7::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{0 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{0 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{* DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{* DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{* DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{0 DownTemp}  ;
    }
}
return

*7 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{0 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{0 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{* Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{* Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{* Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{0 Up}  ;
    }
}
return
*8::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{2 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{2 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{) DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{) DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{) DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{2 DownTemp}  ;
    }
}
return

*8 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{2 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{2 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{) Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{) Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{) Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{2 Up}  ;
    }
}
return
*9::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{4 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{4 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{+ DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{+ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{+ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{4 DownTemp}  ;
    }
}
return

*9 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{4 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{4 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{+ Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{+ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{+ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{4 Up}  ;
    }
}
return
*0::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{6 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{6 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{] DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{] DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{] DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{6 DownTemp}  ;
    }
}
return

*0 up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{6 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{6 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{] Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{] Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{] Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{6 Up}  ;
    }
}
return
*-::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{8 DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{8 DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{! DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{! DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{! DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{8 DownTemp}  ;
    }
}
return

*- up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{8 Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{8 Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{! Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{! Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{! Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{8 Up}  ;
    }
}
return
*]::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{^ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{^ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{@ DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{@ DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{@ DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{^ DownTemp}  ;
    }
}
return

*] up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{^ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{^ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{@ Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{@ Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{@ Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{^ Up}  ;
    }
}
return
*=::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{` DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{` DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{# DownTemp}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{# DownTemp}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{# DownTemp}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{` DownTemp}  ;
    }
}
return

*= up::
sls := GetKeyState("LShift", "P")
srs := GetKeyState("RShift", "P")
sc := GetKeyState("CapsLock", "T")
SetKeyDelay -1   ;
if (not sc)
{
    if (sls)
    {
        Send {Blind}{LShift up}{` Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{` Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{# Up}  ;
    }
} 
else
{
    if (sls)
    {
        Send {Blind}{LShift up}{# Up}{LShift Down}  ;
    }
    else if (srs)
    {
        Send {Blind}{RShift up}{# Up}{RShift Down}  ;
    }
    else
    {
        Send {Blind}{` Up}  ;
    }
}
return

q::SC027
w::,
e::.
r::p
t::y
y::f
u::g
i::c
o::r
p::l
[::/
;\::\ ;no change

;a::a ;no change
s::o
d::e
f::u
g::i
h::d
j::h
k::t
l::n
SC027::s

z::SC028
x::q
c::j
v::k
b::x
n::b
m::m
,::w
.::v
/::z


