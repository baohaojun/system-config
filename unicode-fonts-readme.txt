Quickstart:

    Configure an extended Latin font for your default face, such
    as Monaco, Consolas, or DejaVu Sans Mono.

    Install these fonts

        http://users.teilar.gr/~g1951d/Symbola.zip
        http://www.quivira-font.com/files/Quivira.ttf   ; or Quivira.otf
        http://sourceforge.net/projects/dejavu/files/dejavu/2.34/dejavu-fonts-ttf-2.34.tar.bz2
        http://noto.googlecode.com/git/fonts/individual/hinted/NotoSans-Regular.ttc
        http://noto.googlecode.com/git/fonts/individual/unhinted/NotoSansSymbols-Regular.ttf

    Remove Unifont from your system.

    (require 'unicode-fonts)

    (unicode-fonts-setup)

Testing:

    C-h h                                         ; M-x view-hello-file
    M-x list-charset-chars RET unicode-bmp RET    ; search for 210x
    M-x list-charset-chars RET unicode-smp RET    ; if your backend supports astral chars
    M-x unicode-fonts-debug-insert-block RET Mathematical_Operators RET

Explanation:

Emacs maintains font mappings on a per-glyph basis, meaning
that multiple fonts are used at the same time (transparently) to
display any character for which you have a font.  Furthermore,
Emacs does this out of the box.

However, font mappings via fontsets are a bit difficult to
configure.  In addition, the default setup does not always pick
the most legible fonts.  As the manual warns, the choice of font
actually displayed for a non-ASCII character is "somewhat random".

The Unicode standard provides a way to organize font mappings: it
divides character ranges into logical groups called "blocks".  This
library configures Emacs in a Unicode-friendly way by providing
mappings from

    each Unicode block  ---to--->   a font with good coverage

and makes the settings available via the customization interface.

This library provides font mappings for 215 of the 245 blocks in
the Unicode 7.0 standard which are public and have displayable
characters.  It assumes that 6 Latin blocks are covered by the
default font.  24/245 blocks are not mapped to any known font.

To use unicode-fonts, place the unicode-fonts.el file somewhere
Emacs can find it, and add the following to your ~/.emacs file:

    (require 'unicode-fonts)
    (unicode-fonts-setup)

See important notes about startup speed below.

To gain any benefit from the library, you must have fonts with good
Unicode support installed on your system.  If you are running a
recent version of OS X or Microsoft Windows, you already own some
good multi-lingual fonts, though you would do very well to download
and install the four items below:

From http://dejavu-fonts.org/wiki/Download

    DejaVu Sans, DejaVu Sans Mono

From http://www.quivira-font.com/downloads.php

    Quivira

From http://users.teilar.gr/~g1951d/

    Symbola

Many non-free fonts are referenced by the default settings.
However, free alternatives are also given wherever possible, and
patches are of course accepted to improve every case.

On the assumption that an extended Latin font such as Monaco,
Consolas, or DejaVu Sans Mono is already being used for the default
face, no separate mappings are provided for the following Unicode
blocks:

    Basic Latin
    Latin Extended Additional
    Latin Extended-A
    Latin Extended-B
    Latin-1 Supplement
    Spacing Modifier Letters

though some of these remain configurable via `customize'.

It is also recommended to remove GNU Unifont from your system.
Unifont is very useful for debugging, but not useful for reading.

The default options favor correctness and completeness over speed,
and can add many seconds to initial startup time in GUI mode.
However, when possible a font cache is kept between sessions.  If
you have persistent-soft.el installed, when you start Emacs the
second time, the startup cost should be negligible.

The disk cache will be rebuilt during Emacs startup whenever a font
is added or removed, or any relevant configuration variables are
changed.  To increase the speed of occasionally building the disk
cache, you may use the customization interface to remove fonts from
`unicode-fonts-block-font-mapping' which are not present on your
system.

If you are using a language written in Chinese or Arabic script,
try customizing `unicode-fonts-skip-font-groups' to control which
script you see, and send a friendly bug report.

Color Emoji are enabled by default when using the Native Mac port
on OS X.  This can be disabled by customizing each relevant mapping,
or by turning off all multicolor glyphs here:

    M-x customize-variable RET unicode-fonts-skip-font-groups RET

See Also

    M-x customize-group RET unicode-fonts RET
    M-x customize-variable RET unicode-fonts-block-font-mapping RET

Notes

Free fonts recognized by this package may be downloaded from the
following locations.  For any language, it is increasingly likely
that Noto Sans provides coverage:

    From http://www.google.com/get/noto/

        Noto Sans and friends         ; 178 Unicode blocks and counting; sole
                                      ; source for these blocks:
                                      ;
                                      ;   Bamum / Bamum Supplement / Kaithi
                                      ;   Mandaic / Meetei Mayek Extensions
                                      ;   Sundanese Supplement
                                      ;
                                      ; Also a good source for recently-added
                                      ; glyphs such as "Turkish Lira Sign".

    From http://scripts.sil.org/cms/scripts/page.php?item_id=CharisSIL_download
      or http://scripts.sil.org/cms/scripts/page.php?item_id=DoulosSIL_download

        Charis SIL or Doulos SIL      ; Extended European and diacritics

    From http://scripts.sil.org/cms/scripts/page.php?item_id=Gentium_download

        Gentium Plus                  ; Greek

    From http://users.teilar.gr/~g1951d/

        Aegean, Aegyptus, Akkadian    ; Ancient languages
        Analecta                      ; Ancient languages, Deseret
        Musica                        ; Musical Symbols
        Nilus                         ; Ancient languages

    From http://www.wazu.jp/gallery/views/View_MPH2BDamase.html

        MPH 2B Damase                 ; Arabic, Armenian, Buginese, Cherokee, Georgian,
                                      ; Glagolitic, Hanunoo, Kharoshthi, Limbu, Osmanya,
                                      ; Shavian, Syloti Nagri, Tai Le, Thaana

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=NamdhinggoSIL

        Namdhinggo SIL                ; Limbu

    From http://wenq.org/wqy2/index.cgi?FontGuide

        WenQuanYi Zen Hei             ; CJK (Simplified Chinese)

    From http://babelstone.co.uk/Fonts/

        BabelStone Han                ; CJK (Simplified Chinese)
        BabelStone Phags-pa Book      ; Phags-pa
        BabelStone Modern             ; Tags / Specials / Selectors

    From http://vietunicode.sourceforge.net/fonts/fonts_hannom.html

        HAN NOM A, HAN NOM B          ; CJK (Nôm Chinese)

    From http://kldp.net/projects/unfonts/

        Un Batang                     ; CJK (Hangul)

    From http://sourceforge.jp/projects/hanazono-font/releases/

        Hana Min A, Hana Min B        ; CJK (Japanese)

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=SILYi_home

        Nuosu SIL                     ; CJK (Yi)

    From http://www.daicing.com/manchu/index.php?page=fonts-downloads

        Daicing Xiaokai               ; Mongolian

    From http://www.library.gov.bt/IT/fonts.html

        Jomolhari                     ; Tibetan

    From http://www.thlib.org/tools/scripts/wiki/tibetan%20machine%20uni.html

        Tibetan Machine Uni           ; Tibetan

    From http://scripts.sil.org/cms/scripts/page.php?item_id=Padauk

        Padauk                        ; Myanmar

    From https://code.google.com/p/myanmar3source/downloads/list

        Myanmar3                      ; Myanmar

    From http://www.yunghkio.com/unicode/

        Yunghkio                      ; Myanmar

    From https://code.google.com/p/tharlon-font/downloads/list

        TharLon                       ; Myanmar

    From http://sourceforge.net/projects/prahita/files/Myanmar%20Unicode%20Fonts/MasterpieceUniSans/

        Masterpiece Uni Sans          ; Myanmar

    From http://sarovar.org/projects/samyak/

        Samyak                        ; Devanagari, Gujarati, Malayalam, Oriya, Tamil

    From http://guca.sourceforge.net/typography/fonts/anmoluni/

        AnmolUni                      ; Gurmukhi

    From http://brahmi.sourceforge.net/downloads2.html

        Kedage                        ; Kannada

    From http://www.omicronlab.com/bangla-fonts.html

        Mukti Narrow                  ; Bengali

    From http://www.kamban.com.au/downloads.html

        Akshar Unicode                ; Sinhala

    From http://tabish.freeshell.org/eeyek/download.html

        Eeyek Unicode                 ; Meetei Mayek

    From http://scripts.sil.org/CMS/scripts/page.php?&item_id=Mondulkiri

        Khmer Mondulkiri              ; Khmer

    From http://www.laoscript.net/downloads/

        Saysettha MX                  ; Lao

    From http://www.geocities.jp/simsheart_alif/taithamunicode.html

        Lanna Alif                    ; Tai Tham

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=DaiBannaSIL

        Dai Banna SIL                 ; New Tai Lue

    From http://scripts.sil.org/cms/scripts/page.php?item_id=TaiHeritage

        Tai Heritage Pro              ; Tai Viet

    From http://sabilulungan.org/aksara/

        Sundanese Unicode             ; Sundanese

    From http://www.amirifont.org/

        Amiri                         ; Arabic (Naskh)

    From http://scripts.sil.org/cms/scripts/page.php?item_id=Scheherazade

        Scheherazade                  ; Arabic (Naskh)

    From http://www.farsiweb.ir/wiki/Persian_fonts

        Koodak                        ; Arabic (Farsi)

    From http://openfontlibrary.org/font/ahuramazda/

        Ahuramzda                     ; Avestan

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=AbyssinicaSIL

        Abyssinica SIL                ; Ethiopic

    From http://www.bethmardutho.org/index.php/resources/fonts.html

        Estrangelo Nisibin            ; Syriac

    From http://www.evertype.com/fonts/nko/

        Conakry                       ; N'ko

    From http://uni.hilledu.com/download-ribenguni

        Ribeng                        ; Chakma

    From http://www.virtualvinodh.com/downloads

        Adinatha Tamil Brahmi         ; Brahmi

    From http://ftp.gnu.org/gnu/freefont/

        FreeMono, etc (FreeFont)      ; Kayah Li (and others)

    From http://ulikozok.com/aksara-batak/batak-font/

        Batak-Unicode                 ; Batak

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=Mingzat

        Mingzat                       ; Lepcha

    From http://phjamr.github.io/lisu.html#install
         http://phjamr.github.io/miao.html#install
         http://phjamr.github.io/mro.html#install

        Miao Unicode                  ; Miao
        Lisu Unicode                  ; Lisu
        Mro Unicode                   ; Mro

    From http://scholarsfonts.net/cardofnt.html

        Cardo                         ; Historical Languages

    From http://sourceforge.net/projects/junicode/files/junicode/

        Junicode                      ; Historical Languages

    From http://www.evertype.com/fonts/vai/

        Dukor                         ; Vai

    From http://sourceforge.net/projects/zhmono/

        ZH Mono                       ; Inscriptional Pahlavi / Parthian

    From http://culmus.sourceforge.net/ancient/index.html

        Aramaic Imperial Yeb          ; Imperial Aramaic

    From http://www.languagegeek.com/font/fontdownload.html

        Aboriginal Sans               ; Aboriginal Languages
        Aboriginal Serif

    From http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=EzraSIL_Home

        Ezra SIL                      ; Hebrew

    From http://www.evertype.com/fonts/coptic/

        Antinoou                      ; Coptic / General Punctuation

    From http://apagreekkeys.org/NAUdownload.html

        New Athena Unicode            ; Ancient Languages / Symbols

Compatibility and Requirements

    GNU Emacs version 24.4-devel     : yes, at the time of writing
    GNU Emacs version 24.3           : yes
    GNU Emacs version 23.3           : yes
    GNU Emacs version 22.3 and lower : no

    Requires font-utils.el, ucs-utils.el

Bugs

    The default choice of font for each code block balances coverage
    versus appearance.  This is necessarily subjective.

    Unicode also defines the notion of a "script" as a higher-level
    abstraction which is independent of "blocks".  Modern fonts can
    report their script coverage, and Emacs may also access that
    information.  However, this library ignores scripts in favor
    of blocks and glyphs.

    Checking for font availability is slow.  This library can
    add anywhere between 0.1 - 10 secs to startup time.  It is
    slowest under X11.  Some per-architecture limitations are
    documented in font-utils.el

    Calling `set-fontset-font' can easily crash Emacs.  There is a
    workaround, but it may not be sufficient on all platforms.
    Tested on Cocoa Emacs, Native Mac Emacs, X11/XQuartz,
    MS Windows XP.

    Glyph-by-glyph fallthrough happens differently depending on the
    font backend.  On Cocoa Emacs, glyph-by-glyph fallthrough does not
    occur, and manual per-glyph overrides are required to maximize
    coverage.  Fallthrough works on MS Windows, but not perfectly.
    X11/FreeType behaves most predictably.

    The following ranges cannot be overridden within the
    "fontset-default" fontset:

        Latin Extended Additional
        Latin Extended-B
        Spacing Modifier Letters

    `unicode-fonts-overrides-mapping' shows some order-dependence,
    which must indicate a bug in this code.

    A number of the entries in `unicode-fonts-overrides-mapping'
    are workarounds for the font Monaco, and therefore specific
    to OS X.

    Widths of alternate fonts do not act as expected on MS Windows.
    For example, DejaVu Sans Mono box-drawing characters may use
    a different width than the default font.

TODO

    provide additional interfaces
    - dump set-fontset-font instructions
    - immediately set font for character/current-character/range
    - recommend font for current character
    - alternatives to customize, which can be called before unicode-fonts-setup
      - eg "prefer this font for this block"
      - also character/range ie overrides

    scripts vs blocks
    - further doc note
    - provide alternative interface via scripts

    reorg font list by language?
    - break down into living/dead/invented

    support MUFI for PUA

    support ConScript for PUA

    Aramaic as a style of Hebrew

    (set-language-environment "UTF-8") ?

    Include all Windows 8 fonts

    Remove very old Microsoft entries (eg Monotype.com which was
    renamed Andale)

    Recognize the default font and make smarter choices when it is
    one of the provided mappings.  (On Cocoa, the default font is
    returned when font-info fails, which is not a good thing
    overall.)

    For every font, list font version and unicode blocks which are
    complete.

    Note all decorative fonts

    Adobe international fonts which are supplied with Reader

    Apple fonts which could not be mapped
        Wawati TC
        Weibei TC
        Weibei SC
        Wawati SC

License

Simplified BSD License:

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

   1. Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.

This software is provided by Roland Walker "AS IS" and any express
or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular
purpose are disclaimed.  In no event shall Roland Walker or
contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of
use, data, or profits; or business interruption) however caused
and on any theory of liability, whether in contract, strict
liability, or tort (including negligence or otherwise) arising in
any way out of the use of this software, even if advised of the
possibility of such damage.

The views and conclusions contained in the software and
documentation are those of the authors and should not be
interpreted as representing official policies, either expressed
or implied, of Roland Walker.

No rights are claimed over data created by the Unicode
Consortium, which are included here under the terms of
the Unicode Terms of Use.
