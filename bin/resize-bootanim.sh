#!/bin/bash
cat <<EOF > desc.txt
$1 $2 24
p 1 0 part0
p 0 0 part1
EOF
for x in $(find . -name '*.jpg'); do convert -resize $1x$2 $x $x; done

7z a -mx=0 bootanimation.zip desc.txt $(find part*)
adb_push  bootanimation.zip /system/media/
adb shell bootanimation
