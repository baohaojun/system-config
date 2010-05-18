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
mv antlrworks-1.3.1.jar antlrworks.jar
cd ~/.emacs_d/lisp/ext/

mkdir -p ~/Downloads/intel
cd ~/Downloads/intel

wget http://download.intel.com/design/PentiumII/manuals/24319002.pdf
wget http://download.intel.com/design/PentiumII/manuals/24319102.pdf
wget http://download.intel.com/design/PentiumII/manuals/24319202.pdf
mv 24319002.pdf "Intel Architecture Software Developer's Manual, Volume 1: Basic Architecture.pdf"
mv 24319102.pdf "Intel Architecture Software Developer's Manual, Volume 2: Instruction Set Reference Manual.pdf"
mv 24319202.pdf "Intel Architecture Software Developer's Manual, Volume 3: System Programming.pdf"

echo OK
