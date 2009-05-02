#!/bin/bash
cd ~
svn st -v|grep -v '^\?'|perl -npe 's/.*?baohaojun\s+//' > files.lst
if grep ' ' files.lst; then 
    echo Error: you have some files that contains a SPACE..
    exit
fi

for x in `cat files.lst`; do 
    if ! [[ -d $x ]]; then
        echo $x;
    fi
done > files2.lst

(
    cat files2.lst; 
    echo /c/gnuserv/; 
    echo /c/WINDOWS/Fonts/Monaco.ttf;
    echo /d/tools/emacswin;
    echo /d/tools/emacs-site-lisp/
)|xargs tar czf config.tgz 

rm files.lst files2.lst
