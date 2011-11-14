@echo off
setlocal enableextensions enabledelayedexpansion
set PATH=%ProgramFiles%\Suppor~1;%SystemRoot%\System32

rem Find version number from resource file
for /f "tokens=3" %%a in ('type kbddvp.rc ^| %SystemRoot%\System32\find "ProductVersion"') do set ver=%%~a
for /f "tokens=1-3 delims=.,\" %%a in ("%ver%") do (
    set major=%%a
    set major=!major:"=!
    for %%x in (!major!) do set major=%%x
    set minor=%%b
    set minor=!minor:"=!
    for %%y in (!minor!) do set minor=%%y
    set build=%%c
    set build=!build:"=!
    for %%z in (!build!) do set build=%%z
)

rem Do this outside Makefile since NMake has problems extracting version numbers
del /q kbddvp-!major!_!minor!_!build!-i386.exe 2>NUL
del /q kbddvp-!major!_!minor!_!build!-src-winnt.cab 2>NUL

if /i not "%1"=="clean" (
   if not exist kbddvp.exe (
      call build-layout.bat all
   )
   if exist kbddvp.exe (
      copy /y kbddvp.exe kbddvp-!major!_!minor!_!build!-i386.exe
   )

   set files=build-layout.bat build-installer.bat Makefile sources README.txt launcher.c
   for %%a in (c def inf mak rc reg sed zap) do set files=!files! kbddvp.%%a
   cabarc -m LZX:21 n kbddvp-!major!_!minor!_!build!-src-winnt.cab !files!

rem If you want to use makecab instead of cabarc, use these commands:   
rem    copy /y NUL kbddvp-src.ddf
rem    for %%a in (!files!) do echo %%a >> kbddvp-src.ddf
rem    makecab /D CabinetNameTemplate=$@ /D CompressionType=LZX /D CompressionMemory=21 /F kbddvp-src.ddf
)
endlocal
