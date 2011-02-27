@echo off
md obj\i386\%DDKBUILDENV%
copy sources.nt4 sources
call build.exe -cZ
if exist obj\i386\%DDKBUILDENV%\ctrl2cap.sys copy obj\i386\%DDKBUILDENV%\ctrl2cap.sys ctrl2cap.nt4.sys
rebase -b 10000 -x symbols.nt4 -a ctrl2cap.nt4.sys
copy ctrl2cap.nt4.sys ..\release\.
del ctrl2cap.nt4.sys




