#!/bin/bash

set -e 
TMPD=tmp.$$
#this script will generate unicode font for pdflatex usage
(mkdir -p ~/.texmf-var/fonts/truetype/;
cp $1.ttf ~/.texmf-var/fonts/truetype/;
cd ~/.texmf-var/fonts/truetype/;
rm -rf $TMPD
mkdir $TMPD
cd $TMPD
cp /usr/share/latex-cjk-common/utils/subfonts/subfonts.pe .
ln -s ../$1.ttf . 

kpsewhich Unicode.sfd
fontforge -script subfonts.pe $1.ttf $1 `kpsewhich Unicode.sfd`

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
bold="{<-> CJKb * ${1}}{\CJKbold}"
if test $1 = simsun; then
    bold="{<-> CJK * simhei}{}"
fi
cat <<End> c70$1.fd
\ProvidesFile{c70${1}.fd}
% character set: Unicode U+0080 - U+FFFD
% font encoding: Unicode

\DeclareFontFamily{C70}{${1}}{\hyphenchar \font\m@ne}
\DeclareFontShape{C70}{${1}}{m}{n}{<-> CJK * ${1}}{}
\DeclareFontShape{C70}{${1}}{bx}{n}${bold}

\endinput
End
)

cd ~/.texmf-var/fonts/truetype/
rm $TMPD -rf

texhash
updmap --enable Map $1.map
)

echo 'OK!'
