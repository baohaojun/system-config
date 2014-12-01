cd %userprofile%
md .android
cd .android
echo 0x29a9 > adb_usb.ini
.\the-true-adb kill-server
.\the-true-adb devices
pause
