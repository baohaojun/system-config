@echo off
setlocal enableextensions enabledelayedexpansion
set WINDDK=%ProgramFiles%\WinDDK\3790.1830
set PATH=%WINDDK%\bin\x86;%WINDIR%\System32;%ProgramFiles%\Suppor~1
set INCLUDE=%WINDDK%\inc\wxp;%WINDDK%\inc\crt
set LIB=%WINDDK%\lib\wxp\i386;%WINDDK%\lib\crt\i386
nmake -nologo -fkbddvp.mak "WINDDK=%WINDDK%" %*
endlocal
