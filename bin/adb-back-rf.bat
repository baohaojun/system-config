@echo off
REM start code-generator "^\\s *rem\\s *"
REM for x in modemst1 modemst2; do
rem cat << EOF
rem my-adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/$x of=/sdcard/pt-$x"
rem my-adb pull /sdcard/pt-$x
rem EOF
rem done
REM end code-generator
REM start generated code
my-adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/modemst1 of=/sdcard/pt-modemst1"
my-adb pull /sdcard/pt-modemst1
my-adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/modemst2 of=/sdcard/pt-modemst2"
my-adb pull /sdcard/pt-modemst2

REM end generated code
