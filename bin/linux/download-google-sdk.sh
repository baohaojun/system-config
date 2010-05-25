#!/bin/bash
set -e

mkdir ~/bin/linux/ext/android-sdk-linux_86/google -p
cd ~/bin/linux/ext/android-sdk-linux_86/google
rm repository.xml* -f
wget https://dl-ssl.google.com/android/repository/repository.xml

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/2"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n repository.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1;  lftp -c "pget -c -n 10 https://dl-ssl.google.com/android/repository/$1")!g'|sh
