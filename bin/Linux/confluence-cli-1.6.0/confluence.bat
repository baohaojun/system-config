@echo off
rem remember the directory path to this bat file
set dirPath=%~dp0

rem need to reverse windows names to posix names by changing \ to /
set dirPath=%dirPath:\=/%
rem remove blank at end of string
set dirPath=%dirPath:~0,-1%

rem - Customize for your installation, for instance you might want to add default parameters like the following:
rem   java -jar "%dirPath%"/lib/confluence-cli-2.3.0.jar --server http://my-server --user automation --password automation %*

java -jar "%dirPath%"/lib/confluence-cli-2.3.0.jar %*

rem Exit with the correct error level.
EXIT /B %ERRORLEVEL%
