#!/bin/bash
set -e
cd ~/bin/linux/ext/
ext_download=(
    http://www.antlr.org/download/antlr-3.2.tar.gz
    http://www.antlr.org/download/antlr-3.2.jar
    http://www.antlr.org/download/antlrworks-1.3.1.jar
    http://www.stringtemplate.org/download/stringtemplate-3.2.1.tar.gz
)
for x in "${ext_download[@]}"; do 
    while ! wget --timeout 30 --connect-timeout 30 --tries 1 $x; do rm `basename $x`; done
done
mv antlr-3.2.jar antlr3.jar
tar zxfv stringtemplate*tar.gz
mv stringte*/lib/string*.jar stringtemplate.jar

echo OK
