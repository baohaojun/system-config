#!/bin/bash

set -e 
#this script will generate unicode font for pdflatex usage
(mkdir -p ~/.texmf-var/fonts/truetype/;
cp $1.ttf ~/.texmf-var/fonts/truetype/;
cd ~/.texmf-var/fonts/truetype/;
ttf2tfm $1.ttf -w $1@Unicode.sfd@
for x in *.enc; do echo $x; done|perl -npe 's/(.*)\.(.*)/$1 <'$1'.ttf <$1.$2/' >$1.map
mkdir -p ~/.texmf-var/fonts/enc/pdftex/$1
mv *.enc ~/.texmf-var/fonts/enc/pdftex/$1/
mkdir -p ~/.texmf-var/fonts/tfm/$1/
mv *.tfm ~/.texmf-var/fonts/tfm/$1/

(mkdir -p ~/.texmf-var/tex/latex/cjk/utf-8;
cd ~/.texmf-var/tex/latex/cjk/utf-8;
cat <<End|sed -e "s/%fontname%/$1/g" > c70$1.fd
\ProvidesFile{c70%fontname%.fd}
% character set: Unicode U+0080 - U+FFFD
% font encoding: Unicode

\DeclareFontFamily{C70}{%fontname%}{\hyphenchar \font\m@ne}
\DeclareFontShape{C70}{%fontname%}{m}{n}{<-> CJK * %fontname%}{}
\DeclareFontShape{C70}{%fontname%}{bx}{n}{<-> CJKb * %fontname%}{\CJKbold}

\endinput
End
)

updmap --enable Map $1.map
texhash

)

echo 'OK!'
