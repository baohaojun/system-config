#!/bin/bash
URL=`getclip`
echo $URL|perl -npe 's!http://developer.android.com!file:///home/bhj/bin/linux/ext/android-sdk-linux_86/docs!'|putclip
