@echo off
REM start code-generator "^\\s *rem\\s *"
REM for x in modemst1 modemst2; do
rem cat << EOF
rem adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/$x of=/sdcard/pt-$x"
rem adb pull /sdcard/pt-$x
rem EOF
rem done
REM end code-generator
REM start generated code
adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/modemst1 of=/sdcard/pt-modemst1"
adb pull /sdcard/pt-modemst1
adb shell "dd if=/dev/block/platform/msm_sdcc.1/by-name/modemst2 of=/sdcard/pt-modemst2"
adb pull /sdcard/pt-modemst2

REM end generated code
