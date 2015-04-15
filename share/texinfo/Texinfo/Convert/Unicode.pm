# Unicode.pm: handle conversion to unicode.
#
# Copyright 2010, 2011, 2012 Free Software Foundation, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License,
# or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# Original author: Patrice Dumas <pertusus@free.fr>

package Texinfo::Convert::Unicode;

# Seems to be the perl version required for Encode:
# http://cpansearch.perl.org/src/DANKOGAI/Encode-2.47/Encode/README.e2x
# http://coding.derkeiler.com/Archive/Perl/comp.lang.perl.misc/2005-12/msg00833.html
use 5.007_003;
use strict;

use Encode;
use Unicode::Normalize;
use Carp qw(cluck);
use Unicode::EastAsianWidth;

require Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::Unicode ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  unicode_accent
  encoded_accents
  unicode_for_brace_no_arg_command
  unicode_text
  string_width
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

our %unicode_diacritics = (
       'H'          => '030B', 
       'ringaccent' => '030A', 
       "'"          => '0301',
       'v'          => '030C', 
       ','          => '0327', 
       '^'          => '0302', 
       'dotaccent'  => '0307',
       '`'          => '0300',
       '='          => '0304', 
       '~'          => '0303',
       '"'          => '0308', 
       'udotaccent' => '0323', 
       'ubaraccent' => '0332', 
       'u'          => '0306',
       'tieaccent'  => '0361',
       'ogonek'     => '0328'
);

our %diacritics_accent_commands;
foreach my $diacritic(keys(%unicode_diacritics)) {
  $diacritics_accent_commands{$unicode_diacritics{$diacritic}} = $diacritic;
}

our %unicode_accented_letters = (
    'dotaccent' => { # dot above
        'A' => '0226', #C moz-1.2 
        'a' => '0227', #c moz-1.2
        'B' => '1E02',
        'b' => '1E03',
        'C' => '010A',
        'c' => '010B',
        'D' => '1E0A',
        'd' => '1E0B',
        'E' => '0116',
        'e' => '0117',
        'F' => '1E1E',
        'f' => '1E1F',
        'G' => '0120',
        'g' => '0121',
        'H' => '1E22',
        'h' => '1E23',
        'i' => '0069',
        'I' => '0130',
        'N' => '1E44',
        'n' => '1E45',
        'O' => '022E', #Y moz-1.2
        'o' => '022F', #v moz-1.2
        'P' => '1E56',
        'p' => '1E57',
        'R' => '1E58',
        'r' => '1E59',
        'S' => '1E60',
        's' => '1E61',
        'T' => '1E6A',
        't' => '1E6B',
        'W' => '1E86',
        'w' => '1E87',
        'X' => '1E8A',
        'x' => '1E8B',
        'Y' => '1E8E',
        'y' => '1E8F',
        'Z' => '017B',
        'z' => '017C',
    },
    'udotaccent' => { # dot below
        'A' => '1EA0',
        'a' => '1EA1',
        'B' => '1E04',
        'b' => '1E05',
        'D' => '1E0C',
        'd' => '1E0D',
        'E' => '1EB8',
        'e' => '1EB9',
        'H' => '1E24',
        'h' => '1E25',
        'I' => '1ECA',
        'i' => '1ECB',
        'K' => '1E32',
        'k' => '1E33',
        'L' => '1E36',
        'l' => '1E37',
        'M' => '1E42',
        'm' => '1E43',
        'N' => '1E46',
        'n' => '1E47',
        'O' => '1ECC',
        'o' => '1ECD',
        'R' => '1E5A',
        'r' => '1E5B',
        'S' => '1E62',
        's' => '1E63',
        'T' => '1E6C',
        't' => '1E6D',
        'U' => '1EE4',
        'u' => '1EE5',
        'V' => '1E7E',
        'v' => '1E7F',
        'W' => '1E88',
        'w' => '1E89',
        'Y' => '1EF4',
        'y' => '1EF5',
        'Z' => '1E92',
        'z' => '1E93',
    },
    'ubaraccent' => { # line below
        'B' => '1E06',
        'b' => '1E07',
        'D' => '1E0E',
        'd' => '1E0F',
        'h' => '1E96',
        'K' => '1E34',
        'k' => '1E35',
        'L' => '1E3A',
        'l' => '1E3B',
        'N' => '1E48',
        'n' => '1E49',
        'R' => '1E5E',
        'r' => '1E5F',
        'T' => '1E6E',
        't' => '1E6F',
        'Z' => '1E94',
        'z' => '1E95',
    },
    ',' => { # cedilla
        'C' => '00C7',
        'c' => '00E7',
        'D' => '1E10',
        'd' => '1E11',
        'E' => '0228', #C moz-1.2
        'e' => '0229', #c moz-1.2
        'G' => '0122',
        'g' => '0123',
        'H' => '1E28',
        'h' => '1E29',
        'K' => '0136',
        'k' => '0137',
        'L' => '013B',
        'l' => '013C',
        'N' => '0145',
        'n' => '0146',
        'R' => '0156',
        'r' => '0157',
        'S' => '015E',
        's' => '015F',
        'T' => '0162',
        't' => '0163',
    },
    '=' => { # macron
        'A' => '0100',
        'a' => '0101',
        'E' => '0112',
        'e' => '0113',
        'I' => '012A',
        'i' => '012B',
        'G' => '1E20',
        'g' => '1E21',
        'O' => '014C',
        'o' => '014D',
        'U' => '016A',
        'u' => '016B',
        'Y' => '0232', #? moz-1.2
        'y' => '0233', #? moz-1.2
    },
    '"' => { # diaeresis
        'A' => '00C4',
        'a' => '00E4',
        'E' => '00CB',
        'e' => '00EB',
        'H' => '1E26',
        'h' => '1E27',
        'I' => '00CF',
        'i' => '00EF',
        'O' => '00D6',
        'o' => '00F6',
        't' => '1E97',
        'U' => '00DC',
        'u' => '00FC',
        'W' => '1E84',
        'w' => '1E85',
        'X' => '1E8C',
        'x' => '1E8D',
        'y' => '00FF',
        'Y' => '0178',
    },
    'u' => { # breve
        'A' => '0102',
        'a' => '0103',
        'E' => '0114',
        'e' => '0115',
        'G' => '011E',
        'g' => '011F',
        'I' => '012C',
        'i' => '012D',
        'O' => '014E',
        'o' => '014F',
        'U' => '016C',
        'u' => '016D',
    },
    "'" => { # acute
        'A' => '00C1',
        'a' => '00E1',
        'C' => '0106',
        'c' => '0107',
        'E' => '00C9',
        'e' => '00E9',
        'G' => '01F4',
        'g' => '01F5',
        'I' => '00CD',
        'i' => '00ED',
        'K' => '1E30',
        'k' => '1E31',
        'L' => '0139',
        'l' => '013A',
        'M' => '1E3E',
        'm' => '1E3F',
        'N' => '0143',
        'n' => '0144',
        'O' => '00D3',
        'o' => '00F3',
        'P' => '1E54',
        'p' => '1E55',
        'R' => '0154',
        'r' => '0155',
        'S' => '015A',
        's' => '015B',
        'U' => '00DA',
        'u' => '00FA',
        'W' => '1E82',
        'w' => '1E83',
        'Y' => '00DD',
        'y' => '00FD',
        'Z' => '0179',
        'z' => '018A',
    },
    '~' => { # tilde
        'A' => '00C3',
        'a' => '00E3',
        'E' => '1EBC',
        'e' => '1EBD',
        'I' => '0128',
        'i' => '0129',
        'N' => '00D1',
        'n' => '00F1',
        'O' => '00D5',
        'o' => '00F5',
        'U' => '0168',
        'u' => '0169',
        'V' => '1E7C',
        'v' => '1E7D',
        'Y' => '1EF8',
        'y' => '1EF9',
    },
    '`' => { # grave
        'A' => '00C0',
        'a' => '00E0',
        'E' => '00C8',
        'e' => '00E8',
        'I' => '00CC',
        'i' => '00EC',
        'N' => '01F8',
        'n' => '01F9',
        'O' => '00D2',
        'o' => '00F2',
        'U' => '00D9',
        'u' => '00F9',
        'W' => '1E80',
        'w' => '1E81',
        'Y' => '1EF2',
        'y' => '1EF3',
    },
    '^' => { # circumflex
        'A' => '00C2',
        'a' => '00E2',
        'C' => '0108',
        'c' => '0109',
        'E' => '00CA',
        'e' => '00EA',
        'G' => '011C',
        'g' => '011D',
        'H' => '0124',
        'h' => '0125',
        'I' => '00CE',
        'i' => '00EE',
        'J' => '0134',
        'j' => '0135',
        'O' => '00D4',
        'o' => '00F4',
        'S' => '015C',
        's' => '015D',
        'U' => '00DB',
        'u' => '00FB',
        'W' => '0174',
        'w' => '0175',
        'Y' => '0176',
        'y' => '0177',
        'Z' => '1E90',
        'z' => '1E91',
    },
    'ringaccent' => { # ring
        'A' => '00C5',
        'a' => '00E5',
        'U' => '016E',
        'u' => '016F',
        'w' => '1E98',
        'y' => '1E99',
    },
    'v' => { # caron
        'A' => '01CD',
        'a' => '01CE',
        'C' => '010C',
        'c' => '010D',
        'D' => '010E',
        'd' => '010F',
        'E' => '011A',
        'e' => '011B',
        'G' => '01E6',
        'g' => '01E7',
        'H' => '021E', #K with moz-1.2
        'h' => '021F', #k with moz-1.2
        'I' => '01CF',
        'i' => '01D0',
        'K' => '01E8',
        'k' => '01E9',
        'L' => '013D', #L' with moz-1.2
        'l' => '013E', #l' with moz-1.2
        'N' => '0147',
        'n' => '0148',
        'O' => '01D1',
        'o' => '01D2',
        'R' => '0158',
        'r' => '0159',
        'S' => '0160',
        's' => '0161',
        'T' => '0164',
        't' => '0165',
        'U' => '01D3',
        'u' => '01D4',
        'Z' => '017D',
        'z' => '017E',
    },
    'H' => { # double acute
        'O' => '0150',
        'o' => '0151',
        'U' => '0170',
        'u' => '0171',
    },
    'ogonek' => {
        'A' => '0104',
        'a' => '0105',
        'E' => '0118',
        'e' => '0119',
        'I' => '012E',
        'i' => '012F',
        'U' => '0172',
        'u' => '0173',
        'O' => '01EA',
        'o' => '01EB',
    },
);

our %unicode_simple_character_map = (
            ' ' => '0020',
            '!' => '0021',
            '"' => '0022',
            '#' => '0023',
            '$' => '0024',
            '%' => '0025',
            '&' => '0026',
            "'" => '0027',
            '(' => '0028',
            ')' => '0029',
            '*' => '002A',
            '+' => '002B',
            ',' => '002C',
            '-' => '002D',
            '.' => '002E',
            '/' => '002F',
            ':' => '003A',
            ';' => '003B',
            '<' => '003C',
            '=' => '003D',
            '>' => '003E',
            '?' => '003F',
            '@' => '0040',
            '[' => '005B',
            '\\' => '005C',
            ']' => '005D',
            '^' => '005E',
            '_' => '005F',
            '`' => '0060',
            '{' => '007B',
            '|' => '007C',
            '}' => '007D',
            '~' => '007E',
);


# Also discussed on the texinfo list.
# taken from
#Latin extended additionnal
#http://www.alanwood.net/unicode/latin_extended_additional.html
#C1 Controls and Latin-1 Supplement
#http://www.alanwood.net/unicode/latin_1_supplement.html
#Latin Extended-A
#http://www.alanwood.net/unicode/latin_extended_a.html
#Latin Extended-B
#http://www.alanwood.net/unicode/latin_extended_b.html
#dotless i: 0131

#http://www.alanwood.net/unicode/arrows.html 21**
#http://www.alanwood.net/unicode/general_punctuation.html 20**
#http://www.alanwood.net/unicode/mathematical_operators.html 22**

our %unicode_map = (
               'bullet'            => '2022',
               'copyright'         => '00A9',
               'registeredsymbol'  => '00AE',
               'dots'              => '2026',
               'enddots'           => '',
               'equiv'             => '2261',
               'error'             => '',
               'expansion'         => '21A6',
               'arrow'             => '2192',
               'minus'             => '2212', # in mathematical operators
#               'minus'             => '002D', # in latin1
               'point'             => '2605',
               'print'             => '22A3',
               'result'            => '21D2',
               'today'             => '',
               'aa'                => '00E5',
               'AA'                => '00C5',
               'ae'                => '00E6',
               'oe'                => '0153',
               'AE'                => '00C6',
               'OE'                => '0152',
               'o'                 => '00F8',
               'O'                 => '00D8',
               'ss'                => '00DF',
               'DH'                => '00D0',
               'dh'                => '00F0',
               'TH'                => '00DE',
               'th'                => '00FE',
               'l'                 => '0142',
               'L'                 => '0141',
               'exclamdown'        => '00A1',
               'questiondown'      => '00BF',
               'pounds'            => '00A3',
               'ordf'              => '00AA',
               'ordm'              => '00BA',
               'comma'             => '002C',
               'atchar'            => '0040',
               'lbracechar'        => '007B',
               'rbracechar'        => '007D',
               'backslashchar'     => '005C',
               'hashchar'          => '0023',
               'euro'              => '20AC',
               'geq'               => '2265',
               'leq'               => '2264',
               'tie'               => '',
#               'tie'               => '0020',
               'textdegree'        => '00B0',
               'quotedblleft'      => '201C',
               'quotedblright'     => '201D',
               'quoteleft'         => '2018',
               'quoteright'        => '2019',
               'quotedblbase'      => '201E',
               'quotesinglbase'    => '201A',
               'guillemetleft'     => '00AB',
               'guillemetright'    => '00BB',
               'guillemotleft'     => '00AB',
               'guillemotright'    => '00BB',
               'guilsinglleft'     => '2039',
               'guilsinglright'    => '203A',
               # this should only happen if the @clickstyle argument isn't a 
               # command with braces and no argument.
               'click'             => '2192',
             );

# set the %unicode_character_brace_no_arg_commands value to the character
# corresponding to the hex value in %unicode_map.
our %unicode_character_brace_no_arg_commands;
foreach my $command (keys(%unicode_map)) {
  if ($unicode_map{$command} ne '') {
    my $char_nr = hex($unicode_map{$command});
    if ($char_nr > 126 and $char_nr < 255) {
      # this is very strange, indeed.  The reason lies certainly in the 
      # magic backward compatibility support in perl for 8bit encodings.
      $unicode_character_brace_no_arg_commands{$command} = 
         Encode::decode("iso-8859-1", chr($char_nr));
    } else {
      $unicode_character_brace_no_arg_commands{$command} = chr($char_nr);
    }
  }
}

our %unicode_entities;
# set entities corresponding to unicode_map
foreach my $command (keys(%unicode_map)) {
  $unicode_entities{$command}
   = '&#'.hex($unicode_map{$command}).';'
    if ($unicode_map{$command} ne '');
}


our %transliterate_map = (
               '00C5'  => 'AA',
               '00E5'  => 'aa',
               '00D8'  => 'O',
               '00F8'  => 'o',
               '00E6' => 'ae',
               '0153' => 'oe',
               '00C6' => 'AE',
               '0152' => 'OE',
               '00DF' => 'ss',
               '0141' => 'L',
               '0142' => 'l',
               '00D0' => 'D',
               '00F0' => 'd',
               '00DE' => 'TH',
               '00FE' => 'th',
               '0415'  => 'E',
               '0435'  => 'e',
               '0426'  => 'C',
               '042A'  => 'W',
               '044A'  => 'w',
               '042C'  => 'X',
               '044C'  => 'x',
               '042E'  => 'yu',
               '042F'  => 'YA',
               '044F'  => 'ya',
               '0433'  => 'g',
               '0446'  => 'c',
               '04D7'  => 'IO',
               '00DD'  => 'Y', # unidecode gets this wrong ?
               # following appears in tests, this is required to have
               # the same output with and without unidecode
               '4E2D'  => 'Zhong',
               '6587'  => 'Wen',
               '793A'  => 'Shi',
               '4F8B'  => 'Li',
               '7B2C'  => 'Di',
               '7AE0'  => 'Zhang',
               '53E6'  => 'Ling',
               '4E2A'  => 'Ge',
               # in http://www.cantonese.sheik.co.uk/dictionary/characters/7/
               # unidecode certainly gets it wrong
               '4E00'  => 'Yi',
               'FF08' => '(',
               'FF09' => ')',
               'FF0C' => ',',
               '5B66' => 'Xue',
               '7FD2' => 'Xi',
               '30DE' => 'ma',
               '30CB' => 'ni',
               '30E5' => 'yu',
               '30A2' => 'a',
               '30EB' => 'ru',
          );

our %no_transliterate_map;
foreach my $symbol(keys(%unicode_map)) {
  if ($unicode_map{$symbol} ne '' 
      and !exists($transliterate_map{$symbol})) {
    $no_transliterate_map{$unicode_map{$symbol}} = 1;
  }
}

our %transliterate_accent_map;
foreach my $command (keys(%unicode_accented_letters)) {
  foreach my $letter(keys (%{$unicode_accented_letters{$command}})) {
    $transliterate_accent_map{$unicode_accented_letters{$command}->{$letter}} = $letter
      unless (exists($transliterate_map{$unicode_accented_letters{$command}->{$letter}}));
  }
}

my %unicode_to_eight_bit = (
   'iso8859_1' => {
      '00A0' => 'A0',
      '00A1' => 'A1',
      '00A2' => 'A2',
      '00A3' => 'A3',
      '00A4' => 'A4',
      '00A5' => 'A5',
      '00A6' => 'A6',
      '00A7' => 'A7',
      '00A8' => 'A8',
      '00A9' => 'A9',
      '00AA' => 'AA',
      '00AB' => 'AB',
      '00AC' => 'AC',
      '00AD' => 'AD',
      '00AE' => 'AE',
      '00AF' => 'AF',
      '00B0' => 'B0',
      '00B1' => 'B1',
      '00B2' => 'B2',
      '00B3' => 'B3',
      '00B4' => 'B4',
      '00B5' => 'B5',
      '00B6' => 'B6',
      '00B7' => 'B7',
      '00B8' => 'B8',
      '00B9' => 'B9',
      '00BA' => 'BA',
      '00BB' => 'BB',
      '00BC' => 'BC',
      '00BD' => 'BD',
      '00BE' => 'BE',
      '00BF' => 'BF',
      '00C0' => 'C0',
      '00C1' => 'C1',
      '00C2' => 'C2',
      '00C3' => 'C3',
      '00C4' => 'C4',
      '00C5' => 'C5',
      '00C6' => 'C6',
      '00C7' => 'C7',
      '00C7' => 'C7',
      '00C8' => 'C8',
      '00C9' => 'C9',
      '00CA' => 'CA',
      '00CB' => 'CB',
      '00CC' => 'CC',
      '00CD' => 'CD',
      '00CE' => 'CE',
      '00CF' => 'CF',
      '00D0' => 'D0',
      '00D1' => 'D1',
      '00D2' => 'D2',
      '00D3' => 'D3',
      '00D4' => 'D4',
      '00D5' => 'D5',
      '00D6' => 'D6',
      '00D7' => 'D7',
      '00D8' => 'D8',
      '00D9' => 'D9',
      '00DA' => 'DA',
      '00DB' => 'DB',
      '00DC' => 'DC',
      '00DD' => 'DD',
      '00DE' => 'DE',
      '00DF' => 'DF',
      '00E0' => 'E0',
      '00E1' => 'E1',
      '00E2' => 'E2',
      '00E3' => 'E3',
      '00E4' => 'E4',
      '00E5' => 'E5',
      '00E6' => 'E6',
      '00E7' => 'E7',
      '00E8' => 'E8',
      '00E9' => 'E9',
      '00EA' => 'EA',
      '00EB' => 'EB',
      '00EC' => 'EC',
      '00ED' => 'ED',
      '00EE' => 'EE',
      '00EF' => 'EF',
      '00F0' => 'F0',
      '00F1' => 'F1',
      '00F2' => 'F2',
      '00F3' => 'F3',
      '00F4' => 'F4',
      '00F5' => 'F5',
      '00F6' => 'F6',
      '00F7' => 'F7',
      '00F8' => 'F8',
      '00F9' => 'F9',
      '00FA' => 'FA',
      '00FB' => 'FB',
      '00FC' => 'FC',
      '00FD' => 'FD',
      '00FE' => 'FE',
      '00FF' => 'FF',
   },
   'iso8859_15' => {
      '00A0' => 'A0',
      '00A1' => 'A1',
      '00A2' => 'A2',
      '00A3' => 'A3',
      '20AC' => 'A4',
      '00A5' => 'A5',
      '0160' => 'A6',
      '00A7' => 'A7',
      '0161' => 'A8',
      '00A9' => 'A9',
      '00AA' => 'AA',
      '00AB' => 'AB',
      '00AC' => 'AC',
      '00AD' => 'AD',
      '00AE' => 'AE',
      '00AF' => 'AF',
      '00B0' => 'B0',
      '00B1' => 'B1',
      '00B2' => 'B2',
      '00B3' => 'B3',
      '017D' => 'B4',
      '00B5' => 'B5',
      '00B6' => 'B6',
      '00B7' => 'B7',
      '017E' => 'B8',
      '00B9' => 'B9',
      '00BA' => 'BA',
      '00BB' => 'BB',
      '0152' => 'BC',
      '0153' => 'BD',
      '0178' => 'BE',
      '00BF' => 'BF',
      '00C0' => 'C0',
      '00C1' => 'C1',
      '00C2' => 'C2',
      '00C3' => 'C3',
      '00C4' => 'C4',
      '00C5' => 'C5',
      '00C6' => 'C6',
      '00C7' => 'C7',
      '00C8' => 'C8',
      '00C9' => 'C9',
      '00CA' => 'CA',
      '00CB' => 'CB',
      '00CC' => 'CC',
      '00CD' => 'CD',
      '00CE' => 'CE',
      '00CF' => 'CF',
      '00D0' => 'D0',
      '00D1' => 'D1',
      '00D2' => 'D2',
      '00D3' => 'D3',
      '00D4' => 'D4',
      '00D5' => 'D5',
      '00D6' => 'D6',
      '00D7' => 'D7',
      '00D8' => 'D8',
      '00D9' => 'D9',
      '00DA' => 'DA',
      '00DB' => 'DB',
      '00DC' => 'DC',
      '00DD' => 'DD',
      '00DE' => 'DE',
      '00DF' => 'DF',
      '00E0' => 'E0',
      '00E1' => 'E1',
      '00E2' => 'E2',
      '00E3' => 'E3',
      '00E4' => 'E4',
      '00E5' => 'E5',
      '00E6' => 'E6',
      '00E7' => 'E7',
      '00E8' => 'E8',
      '00E9' => 'E9',
      '00EA' => 'EA',
      '00EB' => 'EB',
      '00EC' => 'EC',
      '00ED' => 'ED',
      '00EE' => 'EE',
      '00EF' => 'EF',
      '00F0' => 'F0',
      '00F1' => 'F1',
      '00F2' => 'F2',
      '00F3' => 'F3',
      '00F4' => 'F4',
      '00F5' => 'F5',
      '00F6' => 'F6',
      '00F7' => 'F7',
      '00F8' => 'F8',
      '00F9' => 'F9',
      '00FA' => 'FA',
      '00FB' => 'FB',
      '00FC' => 'FC',
      '00FD' => 'FD',
      '00FE' => 'FE',
      '00FF' => 'FF',
   },
   'iso8859_2' => {
      '00A0' => 'A0',
      '0104' => 'A1',
      '02D8' => 'A2',
      '0141' => 'A3',
      '00A4' => 'A4',
      '013D' => 'A5',
      '015A' => 'A6',
      '00A7' => 'A7',
      '00A8' => 'A8',
      '015E' => 'AA',
      '0164' => 'AB',
      '0179' => 'AC',
      '00AD' => 'AD',
      '017D' => 'AE',
      '017B' => 'AF',
      '00B0' => 'B0',
      '0105' => 'B1',
      '02DB' => 'B2',
      '0142' => 'B3',
      '00B4' => 'B4',
      '013E' => 'B5',
      '015B' => 'B6',
      '02C7' => 'B7',
      '00B8' => 'B8',
      '0161' => 'B9',
      '015F' => 'BA',
      '0165' => 'BB',
      '017A' => 'BC',
      '02DD' => 'BD',
      '017E' => 'BE',
      '017C' => 'BF',
      '0154' => 'C0',
      '00C1' => 'C1',
      '00C2' => 'C2',
      '0102' => 'C3',
      '00C4' => 'C4',
      '0139' => 'C5',
      '0106' => 'C6',
      '00C7' => 'C7',
      '010C' => 'C8',
      '00C9' => 'C9',
      '0118' => 'CA',
      '00CB' => 'CB',
      '011A' => 'CC',
      '00CD' => 'CD',
      '00CE' => 'CE',
      '010E' => 'CF',
      '0110' => 'D0',
      '0143' => 'D1',
      '0147' => 'D2',
      '00D3' => 'D3',
      '00D4' => 'D4',
      '0150' => 'D5',
      '00D6' => 'D6',
      '00D7' => 'D7',
      '0158' => 'D8',
      '016E' => 'D9',
      '00DA' => 'DA',
      '0170' => 'DB',
      '00DC' => 'DC',
      '00DD' => 'DD',
      '0162' => 'DE',
      '00DF' => 'DF',
      '0155' => 'E0',
      '00E1' => 'E1',
      '00E2' => 'E2',
      '0103' => 'E3',
      '00E4' => 'E4',
      '013A' => 'E5',
      '0107' => 'E6',
      '00E7' => 'E7',
      '010D' => 'E8',
      '00E9' => 'E9',
      '0119' => 'EA',
      '00EB' => 'EB',
      '011B' => 'EC',
      '00ED' => 'ED',
      '00EE' => 'EE',
      '010F' => 'EF',
      '0111' => 'F0',
      '0144' => 'F1',
      '0148' => 'F2',
      '00F3' => 'F3',
      '00F4' => 'F4',
      '0151' => 'F5',
      '00F6' => 'F6',
      '00F7' => 'F7',
      '0159' => 'F8',
      '016F' => 'F9',
      '00FA' => 'FA',
      '0171' => 'FB',
      '00FC' => 'FC',
      '00FD' => 'FD',
      '0163' => 'FE',
      '02D9' => 'FF',
   },
   'koi8' => {
      '0415' => 'A3',
      '0454' => 'A4',
      '0456' => 'A6',
      '0457' => 'A7',
      '04D7' => 'B3',
      '0404' => 'B4',
      '0406' => 'B6',
      '0407' => 'B7',
      '042E' => 'C0',
      '0430' => 'C1',
      '0431' => 'C2',
      '0446' => 'C3',
      '0434' => 'C4',
      '0435' => 'C5',
      '0444' => 'C6',
      '0433' => 'C7',
      '0445' => 'C8',
      '0438' => 'C9',
      '0439' => 'CA',
      '043A' => 'CB',
      '043B' => 'CC',
      '043C' => 'CD',
      '043D' => 'CE',
      '043E' => 'CF',
      '043F' => 'D0',
      '044F' => 'D1',
      '0440' => 'D2',
      '0441' => 'D3',
      '0442' => 'D4',
      '0443' => 'D5',
      '0436' => 'D6',
      '0432' => 'D7',
      '044C' => 'D8',
      '044B' => 'D9',
      '0437' => 'DA',
      '0448' => 'DB',
      '044D' => 'DC',
      '0449' => 'DD',
      '0447' => 'DE',
      '044A' => 'DF',
      '042D' => 'E0',
      '0410' => 'E1',
      '0411' => 'E2',
      '0426' => 'E3',
      '0414' => 'E4',
      '0415' => 'E5',
      '0424' => 'E6',
      '0413' => 'E7',
      '0425' => 'E8',
      '0418' => 'E9',
      '0419' => 'EA',
      '041A' => 'EB',
      '041B' => 'EC',
      '041C' => 'ED',
      '041D' => 'EE',
      '041E' => 'EF',
      '041F' => 'F0',
      '042F' => 'F1',
      '0420' => 'F2',
      '0421' => 'F3',
      '0422' => 'F4',
      '0423' => 'F5',
      '0416' => 'F6',
      '0412' => 'F7',
      '042C' => 'F8',
      '042B' => 'F9',
      '0417' => 'FA',
      '0428' => 'FB',
      '042D' => 'FC',
      '0429' => 'FD',
      '0427' => 'FE',
      '042A' => 'FF',
   },
);

# currently unused
my %makeinfo_transliterate_map = (
  '0416' => 'ZH',
  '0447' => 'ch',
  '00EB' => 'e',
  '0414' => 'D',
  '0159' => 'r',
  '00E6' => 'ae',
  '042B' => 'Y',
  '00FA' => 'u',
  '043B' => 'l',
  '00DE' => 'TH',
  '00D9' => 'U',
  '00C4' => 'A',
  '0148' => 'n',
  '00F6' => 'o',
  '0434' => 'd',
  '041E' => 'O',
  '041B' => 'L',
  '044B' => 'y',
  '0107' => 'c',
  '0415' => 'E',
  '00C1' => 'A',
  '00D3' => 'O',
  '00DB' => 'U',
  '016E' => 'U',
  '013A' => 'l',
  '017B' => 'Z',
  '00F1' => 'n',
  '0428' => 'SH',
  '0153' => 'oe',
  '00F4' => 'o',
  '0144' => 'n',
  '0404' => 'IE',
  '0427' => 'CH',
  '0162' => 'T',
  '017A' => 'z',
  '0448' => 'sh',
  '0436' => 'zh',
  '00F9' => 'u',
  '0406' => 'I',
  '0103' => 'a',
  '0422' => 'T',
  '0160' => 'S',
  '0165' => 't',
  '017E' => 'z',
  '00F0' => 'd',
  '043E' => 'o',
  '043D' => 'n',
  '013E' => 'l',
  '0412' => 'V',
  '0111' => 'd',
  '0155' => 's',
  '017C' => 'z',
  '00CE' => 'I',
  '042D' => 'E',
  '00C8' => 'E',
  '00F8' => 'oe',
  '00F2' => 'o',
  '00FF' => 'y',
  '0420' => 'R',
  '0119' => 'e',
  '00D2' => 'O',
  '043C' => 'm',
  '00D0' => 'DH',
  '0179' => 'Z',
  '0110' => 'D',
  '043F' => 'p',
  '0170' => 'U',
  '011A' => 'E',
  '010C' => 'C',
  '015A' => 'S',
  '0433' => 'g',
  '00E1' => 'a',
  '010D' => 'c',
  '00CC' => 'I',
  '016F' => 'u',
  '0457' => 'yi',
  '00C2' => 'A',
  '0438' => 'i',
  '00E3' => 'a',
  '0435' => 'e',
  '0440' => 'r',
  '042A' => 'W',
  '0431' => 'b',
  '00EE' => 'i',
  '0150' => 'O',
  '00E8' => 'e',
  '0418' => 'I',
  '00CF' => 'I',
  '015F' => 's',
  '0142' => 'l',
  '0147' => 'N',
  '00DF' => 'ss',
  '00E5' => 'aa',
  '00C3' => 'A',
  '0106' => 'C',
  '0141' => 'L',
  '0164' => 'T',
  '017D' => 'Z',
  '00EC' => 'i',
  '041C' => 'M',
  '00C9' => 'E',
  '00E0' => 'a',
  '043A' => 'k',
  '00F5' => 'o',
  '042C' => 'X',
  '0449' => 'shch',
  '0444' => 'f',
  '0139' => 'L',
  '0158' => 'R',
  '00F3' => 'o',
  '00FB' => 'u',
  '0424' => 'F',
  '0446' => 'c',
  '0423' => 'U',
  '0442' => 't',
  '00FD' => 'y',
  '0102' => 'A',
  '0104' => 'A',
  '00CB' => 'E',
  '0426' => 'C',
  '00CD' => 'I',
  '0437' => 'z',
  '0178' => 'y',
  '00D4' => 'O',
  '044D' => 'e',
  '0432' => 'v',
  '013D' => 'L',
  '0163' => 't',
  '0456' => 'i',
  '011B' => 'e',
  '044F' => 'ya',
  '0429' => 'SHCH',
  '0411' => 'B',
  '044A' => 'w',
  '00C6' => 'AE',
  '041D' => 'N',
  '00DA' => 'U',
  '00C0' => 'A',
  '0152' => 'OE',
  '00DD' => 'Y',
  '0154' => 'R',
  '00E9' => 'e',
  '00D5' => 'O',
  '041F' => 'P',
  '0161' => 's',
  '0430' => 'a',
  '0445' => 'h',
  '00E2' => 'a',
  '00D6' => 'O',
  '0407' => 'YI',
  '00CA' => 'E',
  '0439' => 'i',
  '0171' => 'u',
  '00DC' => 'U',
  '042F' => 'YA',
  '0425' => 'H',
  '00FE' => 'th',
  '00D1' => 'N',
  '044C' => 'x',
  '010F' => 'd',
  '0410' => 'A',
  '0443' => 'u',
  '00EF' => 'i',
  '0105' => 'a',
  '00EA' => 'e',
  '00E4' => 'a',
  '015E' => 'S',
  '0417' => 'Z',
  '00ED' => 'i',
  '00FC' => 'u',
  '04D7' => 'IO',
  '00D8' => 'OE',
  '0419' => 'I',
  '0421' => 'S',
  '0143' => 'N',
  '010E' => 'D',
  '0413' => 'G',
  '015B' => 's',
  '0151' => 'o',
  '00E7' => 'c',
  '00C5' => 'AA',
  '0441' => 's',
  '0118' => 'E',
  '00C7' => 'C',
  '041A' => 'K',
  '0454' => 'ie',
  '042E' => 'yu',
);


sub unicode_accent($$)
{
  my $text = shift;
  my $command = shift;

  my $accent = $command->{'cmdname'};

  my $result;

  # special handling of @dotless{i}.
  # \x{0131}\x{0308} for @dotless{i} @" doesn't lead to NFC 00ef.
  # so it is set to a real dotless i only if not in an accent command.
  if ($accent eq 'dotless') {
    if ($text eq 'i' and (!$command->{'parent'} 
                         or !$command->{'parent'}->{'parent'}
                         or !$command->{'parent'}->{'parent'}->{'cmdname'}
                         or !$unicode_accented_letters{$command->{'parent'}->{'parent'}->{'cmdname'}})) {
      $result = "\x{0131}";
    } else {
      $result = $text;
    }
    return $result;
  }

  if (defined($unicode_diacritics{$accent})) {
    $result = Unicode::Normalize::NFC($text . chr(hex($unicode_diacritics{$accent})));
    return $result;
  } else {
    return undef;
  }
}

sub unicode_text($$)
{
  my $text = shift;
  my $in_code = shift;

  if (!$in_code) {
    $text =~ s/---/\x{2014}/g;
    $text =~ s/--/\x{2013}/g;
    $text =~ s/``/\x{201C}/g;
    $text =~ s/''/\x{201D}/g;
    $text =~ s/'/\x{2019}/g;
    $text =~ s/`/\x{2018}/g;
  }
  return Unicode::Normalize::NFC($text);
}

# return the 8 bit, if it exists, and the unicode codepoint
sub _eight_bit_and_unicode_point($$)
{
  my $char = shift;
  my $encoding = shift;

  my $encoding_map_name
   = $Texinfo::Encoding::eight_bit_encoding_aliases{$encoding};
  my ($eight_bit, $codepoint);
  if (ord($char) <= 128) {
    # 7bit ascii characters, the same in every 8bit encodings
    $eight_bit = uc(sprintf("%02x",ord($char)));
    $codepoint = uc(sprintf("%04x",ord($char)));
  } elsif (ord($char) <= hex(0xFFFF)) {
    $codepoint = uc(sprintf("%04x",ord($char)));
    if (exists($unicode_to_eight_bit{$encoding_map_name}->{$codepoint})) {
     $eight_bit
         = $unicode_to_eight_bit{$encoding_map_name}->{$codepoint};
    }
  }
  return ($eight_bit, $codepoint);
}

# format a stack of accents as unicode
sub unicode_accents($$$$;$)
{
  my $converter = shift;
  my $result = shift;
  my $stack = shift;
  my $format_accent = shift;
  my $set_case = shift;

  while (@$stack) {
    my $formatted_result = unicode_accent($result, $stack->[-1]);
    last if (!defined($formatted_result));

    $result = $formatted_result;
    pop @$stack;
  }
  if ($set_case) {
    if ($set_case > 0) {
      $result = uc ($result);
    } else {
      $result = lc ($result);
    }
  }
  while (@$stack) {
    my $accent_command = pop @$stack;
    $result = &$format_accent($converter, $result, $accent_command, $set_case);
  }
  return $result;
}

sub eight_bit_accents($$$$$;$)
{
  my $converter = shift;
  my $unicode_formatted = shift;
  my $stack = shift;
  my $encoding = shift;
  my $convert_accent = shift;
  my $set_case = shift;

  my $result = $unicode_formatted;

  my $debug;
  #$debug = 1;

  if ($debug) {
    print STDERR "STACK: ".join('|', map {$_->{'cmdname'}} @$stack)."\n";
  }

  # accents are formatted and the intermediate results are kept, such
  # that we can return the maximum of multiaccented letters that can be
  # rendered with a given eight bit formatting.  undef is stored when 
  # there is no corresponding unicode anymore.
  my @results_stack = ([$unicode_formatted, undef]);

  while (@$stack) {
    if (defined($unicode_formatted)) {
      $unicode_formatted 
         = unicode_accent($unicode_formatted, $stack->[-1]);
      if (defined($unicode_formatted) and $set_case) {
        if ($set_case > 0) {
          $unicode_formatted = uc($unicode_formatted);
        } else {
          $unicode_formatted = lc($unicode_formatted);
        }
      }
    }
    push @results_stack, [$unicode_formatted, $stack->[-1]];
    pop @$stack;
  }

  if ($debug) {
    print STDERR "PARTIAL_RESULTS_STACK:\n";
    foreach my $partial_result (@results_stack) {
      my $command = 'TEXT';
      $command = $partial_result->[1]->{'cmdname'} if ($partial_result->[1]);
      if (defined($partial_result->[0])) {
        print STDERR "   -> ".Encode::encode('utf8', $partial_result->[0])
                            ."|$command\n";
      } else {
        print STDERR "   -> NO UTF8 |$command\n";
      }
    }
  }

  # At this point we have the utf8 encoded results for the accent
  # commands stack, with all the intermediate results.
  # For each one we'll check if it is possible to encode it in the 
  # current eight bit output encoding table and, if so set the result
  # to the character.

  my $eight_bit = '';

  while (@results_stack) {
    my $char = $results_stack[0]->[0];
    last if (!defined($char));

    my ($new_eight_bit, $new_codepoint)
      = _eight_bit_and_unicode_point($char, $encoding);
    if ($debug) {
      my $command = 'TEXT';
      $command = $results_stack[0]->[1]->{'cmdname'} 
        if ($results_stack[0]->[1]);
      my $new_eight_bit_txt = 'UNDEF';
      $new_eight_bit_txt = $new_eight_bit if (defined($new_eight_bit));
      print STDERR "" . Encode::encode('utf8', $char) 
        . " ($command) new_codepoint: $new_codepoint 8bit: $new_eight_bit_txt old: $eight_bit\n";
    }

    # no corresponding eight bit character found for a composed character
    last if (!$new_eight_bit);

    # in that case, the new eight bit character is the same than the one 
    # found with one less character (and it isn't a @dotless{i}). It may
    # hapen in 2 case, both meaning that there is no corresponding 8bit char:
    #
    # -> there are 2 characters in accent. This could happen, for example
    #    if an accent that cannot be rendered is found and it leads to 
    #    appending or prepending a character. For example this happens for
    #    @={@,{@~{n}}}, where @,{@~{n}} is expanded to a 2 character:
    #    n with a tilde, followed by a , 
    #    In that case, the additional utf8 diacritic is appended, which 
    #    means that it is composed with the , and leaves n with a tilde 
    #    untouched. 
    # -> the diacritic is appended but the normal form doesn't lead
    #    to a composed character, such that the first character
    #    of the string is unchanged. This, for example, happens for 
    #    @ubaraccent{a} since there is no composed accent with a and an 
    #    underbar.
    last if ($new_eight_bit eq $eight_bit
             and !($results_stack[0]->[1]->{'cmdname'} eq 'dotless' 
                   and $char eq 'i'));
    $result = $results_stack[0]->[0];
    $eight_bit = $new_eight_bit;
    shift @results_stack;
  }

  # handle the remaining accents, that have not been converted to 8bit
  # compatible unicode
  shift @results_stack if (!defined($results_stack[0]->[1]));
  while (@results_stack) {
    $result = &$convert_accent($converter, $result,
                               $results_stack[0]->[1],
                               $set_case);
    shift @results_stack;
  }

  # An important remark is that the final conversion to 8bit is left to
  # perl.
  return $result;
}

sub encoded_accents($$$$$;$)
{
  my $converter = shift;
  my $text = shift;
  my $stack = shift;
  my $encoding = shift;
  my $format_accent = shift;
  my $set_case = shift;

  if ($encoding) {
    if ($encoding eq 'utf-8') {
      return unicode_accents($converter, $text, $stack, $format_accent, 
                             $set_case);
    } elsif ($Texinfo::Encoding::eight_bit_encoding_aliases{$encoding}) {
      return eight_bit_accents($converter, $text, $stack, $encoding, 
                               $format_accent, $set_case);
    }
  }
  return undef;
}

# returns the unicode for a command with brace and no arg
# if it is known that it is present for the encoding
sub unicode_for_brace_no_arg_command($$) {
  my $command = shift;
  my $encoding = shift;
  
  if ($unicode_character_brace_no_arg_commands{$command}
      and $encoding 
      and ($encoding eq 'utf-8'
           or ($Texinfo::Encoding::eight_bit_encoding_aliases{$encoding}
               and $unicode_to_eight_bit{$Texinfo::Encoding::eight_bit_encoding_aliases{$encoding}}->{$unicode_map{$command}}))) {
    return $unicode_character_brace_no_arg_commands{$command};
  } else {
    return undef;
  }  
}

# string length size taking into account that east asian characters
# may take 2 spaces.
sub string_width($)
{
  my $string = shift;

  if (! defined($string)) {
    Carp::cluck();
  }
  my $width = 0;
  foreach my $character(split '', $string) {
    if ($character =~ /\p{InFullwidth}/) {
      $width += 2;
    } elsif ($character =~ /\pM/) {
      # a mark set at length 0
    } elsif ($character =~ /\p{IsPrint}/ or $character =~ /\p{IsSpace}/) {
      # newlines are not {IsPrint} in v5.14.2
      $width += 1;
    } elsif ($character =~ /\p{IsControl}/) {
      # Control chars may be added, for instance, as part of @image formatting
    #} elsif ($character eq '') { # could that happen?
    } else {
      #print STDERR "unknown char`$character'\n";
    }
  }
  return $width;
}

1;
__END__

=head1 NAME

Texinfo::Convert::Unicode - Handle conversion to Unicode

=head1 SYNOPSIS

  use Texinfo::Convert::Unicode qw(unicode_accent encoded_accents 
                                   unicode_text);

  my ($innermost_contents, $stack)
      = Texinfo::Common::find_innermost_accent_contents($accent);
  
  my $formatted_accents = encoded_accents ($converter, 
                        convert($innermost_contents), $stack, $encoding, 
                        \&Texinfo::Text::ascii_accent_fallback);

  my $accent_text = unicode_accent('e', $accent_command);

=head1 DESCRIPTION

Texinfo::Convert::Unicode provides methods that deals with unicode for
converters. Unicode is important, because it is used internally in perl 
for strings, therefore if converted to Unicode, a string could be output
in other encodings as well when writting out the converted documents.

When an encoding is given as argument of a method of the module, the 
accented letters should only be converted to unicode if it is known that
it will be possible to convert the unicode points to encoded charactes
in the encoding character set.

=head1 METHODS

=over

=item $result = unicode_accent($text, $accent_command)

I<$text> is the text appearing within an accent command.  I<$accent_command>
should be a Texinfo tree element corresponding to an accent command taking
an argument.  The function returns the unicode representation of the accented
character.

=item $result = encoded_accents ($converter, $text, $stack, $encoding, $format_accent, $set_case)

I<$converter> is a converter object.  It may be undef if there is no need of
converter object in I<$format_accent> (I<$format_accent> described below).
I<$text> is the text appearing within nested accent commands.  I<$stack> is
an array reference holding the nested accents texinfo element trees.  For
example, I<$text> could be the formatted content and I<$stack> the stack 
returned by C<Texinfo::Common::find_innermost_accent_contents>.  I<$encoding> 
is the encoding the accented characters should be encoded to.  If 
I<$encoding> not set the I<$result> is set to undef.  I<$format_accent> 
is a function reference that is used to format the accent commands if 
there is no encoded character available for the encoding I<$encoding>
at some point of the conversion of the I<$stack>.  Last, if 
I<$set_case> is positive, the result is upper-cased, while if it is negative, 
the result is lower-cased.

=item $result = unicode_text ($text, $in_code)

Return I<$text> with characters encoded in unicode.  If I<$in_code> 
is set, the text is considered to be in code style.

=item $result = unicode_for_brace_no_arg_command($command_name, $encoding)

Return the unicode representing a command with brace and no argument
I<$command_name> (like C<@bullet{}>, C<@aa{}> or C<@guilsinglleft{}>), 
or undef if there is no available encoded character for encoding 
I<$encoding>. 

=item $width = string_width($string)

Return the string width, taking into account the fact that some characters
have a zero width (like composing accents) while some have a width of 2
(most chinese characters, for example).

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut

