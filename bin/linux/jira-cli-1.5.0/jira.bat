@echo off

rem Comments
rem - Customize for your installation, for instance you might want to add default parameters like the following:
rem   java -jar release/jira-cli-1.5.0.jar --server http://my-server --user automation --password automation %*

java -jar release/jira-cli-1.5.0.jar %*

rem Exit with the correct error level.
EXIT /B %ERRORLEVEL%
