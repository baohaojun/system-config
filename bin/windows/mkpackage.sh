#!/bin/bash
set -e
cd ~
svn update
if [[ `svn st -q|wc -c` != 0 ]]; 
then
    echo "you need check in everything first!"
    false;
fi

svn st -v|grep -v '^\?'|perl -npe 's/.*?baohaojun\s+//' > files.lst
if grep ' ' files.lst; then 
    echo "Error: you have some files that contains a SPACE.."
    exit
fi

#we only take the files, not the directories, becase we don't want to
#roll whole directories into tar-ball, as there may be non-revision
#files

for x in `cat files.lst`; do 
    if ! [[ -d $x ]]; then
        echo $x;
    fi
done > files2.lst

(
    cat files2.lst; 
    echo /c/WINDOWS/Fonts/Monaco.ttf;
    echo ~/tools/emacswin;
    echo ~/tools/emacs-site-lisp/
)|xargs tar czf windows-config.tgz

rm files.lst files2.lst
