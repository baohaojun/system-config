#!/bin/bash
URL=`getclip`
#http://www.google.com/url?sa=t&source=web&cd=1&ved=0CBIQFjAA&url=http%3A%2F%2Fdeveloper.android.com%2Fguide%2Fpractices%2Fscreens_support.html&ei=x_4QTNTcDMjQcc6dpf4H&usg=AFQjCNHemvlQ2sNEATp3v6KONvL52P-N7A

echo "$URL"|unurl.pl|perl -npe 's!.*http://developer.android.com!file:///home/bhj/system-config/bin/Linux/ext/android-sdk-linux_86/docs!; s/&.*//;'|putclip
