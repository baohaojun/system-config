#!/bin/bash

set -e
if test "$(readlink -f ~/.texlive2016/texmf-var)" != "$(readlink -f ~/.texmf-var)"; then
    rm -rf ~/.texlive2016
    mkdir ~/.texlive2016
    ln -s ~/.texmf-var ~/.texlive2016/texmf-var
    ln -s ~/.texmf-config ~/.texlive2016/texmf-config
fi
this_script=$(readlink -f $0)
TMPD=tmp.$$
#this script will generate unicode font for pdflatex usage
(
    mkdir -p ~/.texmf-var/fonts/truetype/;

    file=$1.ttf
    if test -e $1.ttc -a ! -e $1.ttf; then
        file=$1.ttc
    fi
    cp $file ~/.texmf-var/fonts/truetype/$1.ttf;
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
    (
        flock 9
        cat ~/.texmf-var/fonts/map/dvips/$1/$1.map >> ~/.texmf-var/fonts/map/dvips/psfonts_t1.map
    ) 9< $this_script
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
    mktexlsr
    updmap --enable Map $1.map
)

echo 'OK!'
