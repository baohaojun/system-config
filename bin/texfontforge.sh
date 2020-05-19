#!/usr/bin/env bash

set -e
this_script=$(readlink -f $0)

tex_config=$(kpsepath base|tr : '\n'|tr -d '!'|grep 'dist/web2c$' -P | tr -d '\n')/texmf.cnf
tex_dir=$(cat "${tex_config}" | grep '^TEXMFVAR\s*=' -P|perl -npe 's/.*=\s*//; s,^~/,$ENV{HOME}/,; s,(.*)/.*,$1,')

var_dir=${tex_dir}/texmf-var
config_dir=${tex_dir}/texmf-config

mkdir -p ${var_dir} ${config_dir}

TMPD=tmp.$$

if test $# = 0; then
    (texfontforge.sh -n simsun -f ~/.fonts/msyh.ttf)
    (texfontforge.sh -n simhei -f ~/.fonts/msyhbd.ttf)
    if test "$(uname)" = FreeBSD; then
        (texfontforge.sh -n cyberb -f ~/.fonts/Cyberbit.ttf)
    fi
    exit
fi

set -e
## start code-generator "^\\s *#\\s *"
# generate-getopt n:font-name f:font-file
## end code-generator
## start generated code
TEMP=$( getopt -o f:n:h \
               --long font-file:,font-name:,help \
               -n $(basename -- $0) -- "$@")
declare font_file=
declare font_name=
eval set -- "$TEMP"
while true; do
    case "$1" in

        -f|--font-file)
            font_file=$2
            shift 2

            ;;
        -n|--font-name)
            font_name=$2
            shift 2

            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %6s '-f, '
            printf %-24s '--font-file=FONT_FILE'
            echo
            printf %6s '-n, '
            printf %-24s '--font-name=FONT_NAME'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error: $(. bt; echo; bt | indent-stdin)"
            ;;
    esac
done


## end generated code

#this script will generate unicode font for pdflatex usage
(
    mkdir -p ${var_dir}/fonts/truetype/;

    cp ${font_file} ${var_dir}/fonts/truetype/$font_name.ttf;
    cd ${var_dir}/fonts/truetype/;
    rm -rf $TMPD
    mkdir $TMPD
    cd $TMPD
    subfonts_pe=/usr/local/share/texmf-dist/source/latex/cjk/utils/subfonts/subfonts.pe
    if test ! -e "${subfonts_pe}"; then
        subfonts_pe=/usr/share/latex-cjk-common/utils/subfonts/subfonts.pe
    fi
    cp "$subfonts_pe" .
    ln -s ../${font_name}.ttf .

    kpsewhich Unicode.sfd
    fontforge -script subfonts.pe ${font_name}.ttf ${font_name} `kpsewhich Unicode.sfd`

    for i in *.pfb
    do
        echo "$(basename $i .pfb) $(basename $i .pfb) <$i" >> ${font_name}.map
    done

    mkdir -p ${var_dir}/fonts/map/dvips/${font_name}/ ${var_dir}/fonts/{afm,type1,tfm}/${font_name}

    mv ${font_name}.map ${var_dir}/fonts/map/dvips/${font_name}/
    (
        flock 9
        cat ${var_dir}/fonts/map/dvips/${font_name}/${font_name}.map >> ${var_dir}/fonts/map/dvips/psfonts_t1.map
    ) 9< $this_script
    mv *.afm ${var_dir}/fonts/afm/${font_name}
    mv *.tfm ${var_dir}/fonts/tfm/${font_name}
    mv *.pfb ${var_dir}/fonts/type1/${font_name}

    (mkdir -p ${var_dir}/tex/latex/cjk/utf-8;
        cd ${var_dir}/tex/latex/cjk/utf-8;
        bold="{<-> CJKb * ${font_name}}{\CJKbold}"
        if test ${font_name} = simsun; then
            bold="{<-> CJK * simhei}{}"
        fi
        cat <<End> c70${font_name}.fd
\ProvidesFile{c70${font_name}.fd}
% character set: Unicode U+0080 - U+FFFD
% font encoding: Unicode

\DeclareFontFamily{C70}{${font_name}}{\hyphenchar \font\m@ne}
\DeclareFontShape{C70}{${font_name}}{m}{n}{<-> CJK * ${font_name}}{}
\DeclareFontShape{C70}{${font_name}}{bx}{n}${bold}

\endinput
End
    )

    cd ${var_dir}/fonts/truetype/
    rm $TMPD -rf

    texhash
    mktexlsr
    if type updmap-user >/dev/null 2>&1; then
        updmap=updmap-user
    else
        updmap=updmap
    fi
    $updmap --enable Map ${font_name}.map
)

echo 'OK!'
