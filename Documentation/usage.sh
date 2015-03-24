#!/bin/sh

for i in `ls ../guilt-*`; do
	name=$(basename $i)
	u=$(grep USAGE $i |  sed 's/USAGE="//' | sed 's/"$//') 
	echo "'$name' $u"  > usage-$name.txt
done
