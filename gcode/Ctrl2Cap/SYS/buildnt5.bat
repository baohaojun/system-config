@echo off
copy sources.nt5 sources
call build.exe -cZ
if exist obj%BUILD_ALT_DIR%\i386\ctrl2cap.sys copy obj%BUILD_ALT_DIR%\i386\ctrl2cap.sys ctrl2cap.nt5.sys
rebase -b 10000 -x symbols.nt5 -a ctrl2cap.nt5.sys
copy ctrl2cap.nt5.sys ..\release\.
del ctrl2cap.nt5.sys





