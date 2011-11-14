@echo off
setlocal enableextensions enabledelayedexpansion
set WINDDK=C:\WinDDK\7600.16385.1
set PATH=%WINDDK%\bin\x86;%WINDDK%\bin\x86\x86;%WINDIR%\System32;%ProgramFiles%\Suppor~1
set INCLUDE=%WINDDK%\inc\api;%WINDDK%\inc\crt
set LIB=%WINDDK%\lib\wxp\i386;%WINDDK%\lib\crt\i386
nmake -nologo -fkbddvp.mak "WINDDK=%WINDDK%" %*
endlocal
