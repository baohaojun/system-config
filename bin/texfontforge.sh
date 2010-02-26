#!/bin/bash

set -e 
#this script will generate unicode font for pdflatex usage
(mkdir -p ~/.texmf-var/fonts/truetype/;
cp $1.ttf ~/.texmf-var/fonts/truetype/;
cd ~/.texmf-var/fonts/truetype/;
rm -rf tmp
mkdir tmp
cd tmp
cp /usr/share/latex-cjk-common/utils/subfonts/subfonts.pe .
ln -s ../$1.ttf . 

fontforge -script subfonts.pe $1.ttf $1 /usr/share/texmf-texlive/fonts/sfd/Unicode.sfd

for i in *.pfb
do
    echo "$(basename $i .pfb) $(basename $i .pfb) <$i" >> $1.map
done

mkdir -p ~/.texmf-var/fonts/map/dvips/$1/ ~/.texmf-var/fonts/{afm,type1,tfm}/$1

mv $1.map ~/.texmf-var/fonts/map/dvips/$1/
mv *.afm ~/.texmf-var/fonts/afm/$1
mv *.tfm ~/.texmf-var/fonts/tfm/$1
mv *.pfb ~/.texmf-var/fonts/type1/$1

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
