
cat << "EOF"
SendMode Input
#NoEnv
Menu, Tray, Icon, main.cpl, 8

;----------- REMAP TO DVORAK

EOF

gen-dvorak-programmer.sh \` \$ \~
gen-dvorak-programmer.sh SC028 - _
gen-dvorak-programmer-shift3.sh 1 \& 5 # by 5 we mean %
gen-dvorak-programmer.sh 2 \[ \7
gen-dvorak-programmer.sh 3 \{ \5
gen-dvorak-programmer.sh 4 \} \3
gen-dvorak-programmer.sh 5 \( \1
gen-dvorak-programmer.sh 6 \= \9
gen-dvorak-programmer.sh 7 \* \0
gen-dvorak-programmer.sh 8 \) \2
gen-dvorak-programmer.sh 9 \+ \4
gen-dvorak-programmer.sh 0 \] \6
gen-dvorak-programmer.sh - \! \8



cat << "EOF"

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
]::=
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


EOF
