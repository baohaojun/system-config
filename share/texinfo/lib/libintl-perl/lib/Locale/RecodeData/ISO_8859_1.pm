#! /bin/false

# vim: set autoindent shiftwidth=4 tabstop=4:

# Conversion routines for ISO-8859-1.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
                                                                                
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
                                                                                
# You should have received a copy of the GNU Library General Public 
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, 
# USA.

package Locale::RecodeData::ISO_8859_1;

use strict;

require Locale::RecodeData;
use base qw(Locale::RecodeData);

use constant TO_UTF8 => [
    "\x00",
    "\x01",
    "\x02",
    "\x03",
    "\x04",
    "\x05",
    "\x06",
    "\x07",
    "\x08",
    "\x09",
    "\x0a",
    "\x0b",
    "\x0c",
    "\x0d",
    "\x0e",
    "\x0f",
    "\x10",
    "\x11",
    "\x12",
    "\x13",
    "\x14",
    "\x15",
    "\x16",
    "\x17",
    "\x18",
    "\x19",
    "\x1a",
    "\x1b",
    "\x1c",
    "\x1d",
    "\x1e",
    "\x1f",
    "\x20",
    "\x21",
    "\x22",
    "\x23",
    "\x24",
    "\x25",
    "\x26",
    "\x27",
    "\x28",
    "\x29",
    "\x2a",
    "\x2b",
    "\x2c",
    "\x2d",
    "\x2e",
    "\x2f",
    "\x30",
    "\x31",
    "\x32",
    "\x33",
    "\x34",
    "\x35",
    "\x36",
    "\x37",
    "\x38",
    "\x39",
    "\x3a",
    "\x3b",
    "\x3c",
    "\x3d",
    "\x3e",
    "\x3f",
    "\x40",
    "\x41",
    "\x42",
    "\x43",
    "\x44",
    "\x45",
    "\x46",
    "\x47",
    "\x48",
    "\x49",
    "\x4a",
    "\x4b",
    "\x4c",
    "\x4d",
    "\x4e",
    "\x4f",
    "\x50",
    "\x51",
    "\x52",
    "\x53",
    "\x54",
    "\x55",
    "\x56",
    "\x57",
    "\x58",
    "\x59",
    "\x5a",
    "\x5b",
    "\x5c",
    "\x5d",
    "\x5e",
    "\x5f",
    "\x60",
    "\x61",
    "\x62",
    "\x63",
    "\x64",
    "\x65",
    "\x66",
    "\x67",
    "\x68",
    "\x69",
    "\x6a",
    "\x6b",
    "\x6c",
    "\x6d",
    "\x6e",
    "\x6f",
    "\x70",
    "\x71",
    "\x72",
    "\x73",
    "\x74",
    "\x75",
    "\x76",
    "\x77",
    "\x78",
    "\x79",
    "\x7a",
    "\x7b",
    "\x7c",
    "\x7d",
    "\x7e",
    "\x7f",
    "\xc2\x80",
    "\xc2\x81",
    "\xc2\x82",
    "\xc2\x83",
    "\xc2\x84",
    "\xc2\x85",
    "\xc2\x86",
    "\xc2\x87",
    "\xc2\x88",
    "\xc2\x89",
    "\xc2\x8a",
    "\xc2\x8b",
    "\xc2\x8c",
    "\xc2\x8d",
    "\xc2\x8e",
    "\xc2\x8f",
    "\xc2\x90",
    "\xc2\x91",
    "\xc2\x92",
    "\xc2\x93",
    "\xc2\x94",
    "\xc2\x95",
    "\xc2\x96",
    "\xc2\x97",
    "\xc2\x98",
    "\xc2\x99",
    "\xc2\x9a",
    "\xc2\x9b",
    "\xc2\x9c",
    "\xc2\x9d",
    "\xc2\x9e",
    "\xc2\x9f",
    "\xc2\xa0",
    "\xc2\xa1",
    "\xc2\xa2",
    "\xc2\xa3",
    "\xc2\xa4",
    "\xc2\xa5",
    "\xc2\xa6",
    "\xc2\xa7",
    "\xc2\xa8",
    "\xc2\xa9",
    "\xc2\xaa",
    "\xc2\xab",
    "\xc2\xac",
    "\xc2\xad",
    "\xc2\xae",
    "\xc2\xaf",
    "\xc2\xb0",
    "\xc2\xb1",
    "\xc2\xb2",
    "\xc2\xb3",
    "\xc2\xb4",
    "\xc2\xb5",
    "\xc2\xb6",
    "\xc2\xb7",
    "\xc2\xb8",
    "\xc2\xb9",
    "\xc2\xba",
    "\xc2\xbb",
    "\xc2\xbc",
    "\xc2\xbd",
    "\xc2\xbe",
    "\xc2\xbf",
    "\xc3\x80",
    "\xc3\x81",
    "\xc3\x82",
    "\xc3\x83",
    "\xc3\x84",
    "\xc3\x85",
    "\xc3\x86",
    "\xc3\x87",
    "\xc3\x88",
    "\xc3\x89",
    "\xc3\x8a",
    "\xc3\x8b",
    "\xc3\x8c",
    "\xc3\x8d",
    "\xc3\x8e",
    "\xc3\x8f",
    "\xc3\x90",
    "\xc3\x91",
    "\xc3\x92",
    "\xc3\x93",
    "\xc3\x94",
    "\xc3\x95",
    "\xc3\x96",
    "\xc3\x97",
    "\xc3\x98",
    "\xc3\x99",
    "\xc3\x9a",
    "\xc3\x9b",
    "\xc3\x9c",
    "\xc3\x9d",
    "\xc3\x9e",
    "\xc3\x9f",
    "\xc3\xa0",
    "\xc3\xa1",
    "\xc3\xa2",
    "\xc3\xa3",
    "\xc3\xa4",
    "\xc3\xa5",
    "\xc3\xa6",
    "\xc3\xa7",
    "\xc3\xa8",
    "\xc3\xa9",
    "\xc3\xaa",
    "\xc3\xab",
    "\xc3\xac",
    "\xc3\xad",
    "\xc3\xae",
    "\xc3\xaf",
    "\xc3\xb0",
    "\xc3\xb1",
    "\xc3\xb2",
    "\xc3\xb3",
    "\xc3\xb4",
    "\xc3\xb5",
    "\xc3\xb6",
    "\xc3\xb7",
    "\xc3\xb8",
    "\xc3\xb9",
    "\xc3\xba",
    "\xc3\xbb",
    "\xc3\xbc",
    "\xc3\xbd",
    "\xc3\xbe",
    "\xc3\xbf",
];

sub _recode
{
    if ($_[0]->{_from} eq 'INTERNAL') {
		# FIXME: Maybe the lookup is cheaper than the call to chr().
		$_[1] = join '', 
		    map $_ > 255 ? "\x3f" : chr $_,
			    @{$_[1]};
    } elsif ($_[0]->{_to} eq 'UTF-8') {
		$_[1] = join '', map TO_UTF8->[$_], unpack 'C*', $_[1];
    } else {
		$_[1] = [ unpack 'C*', $_[1] ];
    }

    return 1;
}

1;

__END__

=head1 NAME

Locale::RecodeData::ISO_8859_1 - Conversion routines for ISO-8859-1

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module contains the conversion tables and routines for ISO-8859-1.

=head1 CHARACTER TABLE

The following table is sorted in the same order as the original charmap.
All character codes are in hexadecimal.  Please read 'ISO-10646' as
'ISO-10646-UCS4'.

 Local | ISO-10646 | Description
-------+-----------+-------------------------------------------------
    00 |  00000000 | NULL (NUL)
    01 |  00000001 | START OF HEADING (SOH)
    02 |  00000002 | START OF TEXT (STX)
    03 |  00000003 | END OF TEXT (ETX)
    04 |  00000004 | END OF TRANSMISSION (EOT)
    05 |  00000005 | ENQUIRY (ENQ)
    06 |  00000006 | ACKNOWLEDGE (ACK)
    07 |  00000007 | BELL (BEL)
    08 |  00000008 | BACKSPACE (BS)
    09 |  00000009 | CHARACTER TABULATION (HT)
    0A |  0000000A | LINE FEED (LF)
    0B |  0000000B | LINE TABULATION (VT)
    0C |  0000000C | FORM FEED (FF)
    0D |  0000000D | CARRIAGE RETURN (CR)
    0E |  0000000E | SHIFT OUT (SO)
    0F |  0000000F | SHIFT IN (SI)
    10 |  00000010 | DATALINK ESCAPE (DLE)
    11 |  00000011 | DEVICE CONTROL ONE (DC1)
    12 |  00000012 | DEVICE CONTROL TWO (DC2)
    13 |  00000013 | DEVICE CONTROL THREE (DC3)
    14 |  00000014 | DEVICE CONTROL FOUR (DC4)
    15 |  00000015 | NEGATIVE ACKNOWLEDGE (NAK)
    16 |  00000016 | SYNCHRONOUS IDLE (SYN)
    17 |  00000017 | END OF TRANSMISSION BLOCK (ETB)
    18 |  00000018 | CANCEL (CAN)
    19 |  00000019 | END OF MEDIUM (EM)
    1A |  0000001A | SUBSTITUTE (SUB)
    1B |  0000001B | ESCAPE (ESC)
    1C |  0000001C | FILE SEPARATOR (IS4)
    1D |  0000001D | GROUP SEPARATOR (IS3)
    1E |  0000001E | RECORD SEPARATOR (IS2)
    1F |  0000001F | UNIT SEPARATOR (IS1)
    20 |  00000020 | SPACE
    21 |  00000021 | EXCLAMATION MARK
    22 |  00000022 | QUOTATION MARK
    23 |  00000023 | NUMBER SIGN
    24 |  00000024 | DOLLAR SIGN
    25 |  00000025 | PERCENT SIGN
    26 |  00000026 | AMPERSAND
    27 |  00000027 | APOSTROPHE
    28 |  00000028 | LEFT PARENTHESIS
    29 |  00000029 | RIGHT PARENTHESIS
    2A |  0000002A | ASTERISK
    2B |  0000002B | PLUS SIGN
    2C |  0000002C | COMMA
    2D |  0000002D | HYPHEN-MINUS
    2E |  0000002E | FULL STOP
    2F |  0000002F | SOLIDUS
    30 |  00000030 | DIGIT ZERO
    31 |  00000031 | DIGIT ONE
    32 |  00000032 | DIGIT TWO
    33 |  00000033 | DIGIT THREE
    34 |  00000034 | DIGIT FOUR
    35 |  00000035 | DIGIT FIVE
    36 |  00000036 | DIGIT SIX
    37 |  00000037 | DIGIT SEVEN
    38 |  00000038 | DIGIT EIGHT
    39 |  00000039 | DIGIT NINE
    3A |  0000003A | COLON
    3B |  0000003B | SEMICOLON
    3C |  0000003C | LESS-THAN SIGN
    3D |  0000003D | EQUALS SIGN
    3E |  0000003E | GREATER-THAN SIGN
    3F |  0000003F | QUESTION MARK
    40 |  00000040 | COMMERCIAL AT
    41 |  00000041 | LATIN CAPITAL LETTER A
    42 |  00000042 | LATIN CAPITAL LETTER B
    43 |  00000043 | LATIN CAPITAL LETTER C
    44 |  00000044 | LATIN CAPITAL LETTER D
    45 |  00000045 | LATIN CAPITAL LETTER E
    46 |  00000046 | LATIN CAPITAL LETTER F
    47 |  00000047 | LATIN CAPITAL LETTER G
    48 |  00000048 | LATIN CAPITAL LETTER H
    49 |  00000049 | LATIN CAPITAL LETTER I
    4A |  0000004A | LATIN CAPITAL LETTER J
    4B |  0000004B | LATIN CAPITAL LETTER K
    4C |  0000004C | LATIN CAPITAL LETTER L
    4D |  0000004D | LATIN CAPITAL LETTER M
    4E |  0000004E | LATIN CAPITAL LETTER N
    4F |  0000004F | LATIN CAPITAL LETTER O
    50 |  00000050 | LATIN CAPITAL LETTER P
    51 |  00000051 | LATIN CAPITAL LETTER Q
    52 |  00000052 | LATIN CAPITAL LETTER R
    53 |  00000053 | LATIN CAPITAL LETTER S
    54 |  00000054 | LATIN CAPITAL LETTER T
    55 |  00000055 | LATIN CAPITAL LETTER U
    56 |  00000056 | LATIN CAPITAL LETTER V
    57 |  00000057 | LATIN CAPITAL LETTER W
    58 |  00000058 | LATIN CAPITAL LETTER X
    59 |  00000059 | LATIN CAPITAL LETTER Y
    5A |  0000005A | LATIN CAPITAL LETTER Z
    5B |  0000005B | LEFT SQUARE BRACKET
    5C |  0000005C | REVERSE SOLIDUS
    5D |  0000005D | RIGHT SQUARE BRACKET
    5E |  0000005E | CIRCUMFLEX ACCENT
    5F |  0000005F | LOW LINE
    60 |  00000060 | GRAVE ACCENT
    61 |  00000061 | LATIN SMALL LETTER A
    62 |  00000062 | LATIN SMALL LETTER B
    63 |  00000063 | LATIN SMALL LETTER C
    64 |  00000064 | LATIN SMALL LETTER D
    65 |  00000065 | LATIN SMALL LETTER E
    66 |  00000066 | LATIN SMALL LETTER F
    67 |  00000067 | LATIN SMALL LETTER G
    68 |  00000068 | LATIN SMALL LETTER H
    69 |  00000069 | LATIN SMALL LETTER I
    6A |  0000006A | LATIN SMALL LETTER J
    6B |  0000006B | LATIN SMALL LETTER K
    6C |  0000006C | LATIN SMALL LETTER L
    6D |  0000006D | LATIN SMALL LETTER M
    6E |  0000006E | LATIN SMALL LETTER N
    6F |  0000006F | LATIN SMALL LETTER O
    70 |  00000070 | LATIN SMALL LETTER P
    71 |  00000071 | LATIN SMALL LETTER Q
    72 |  00000072 | LATIN SMALL LETTER R
    73 |  00000073 | LATIN SMALL LETTER S
    74 |  00000074 | LATIN SMALL LETTER T
    75 |  00000075 | LATIN SMALL LETTER U
    76 |  00000076 | LATIN SMALL LETTER V
    77 |  00000077 | LATIN SMALL LETTER W
    78 |  00000078 | LATIN SMALL LETTER X
    79 |  00000079 | LATIN SMALL LETTER Y
    7A |  0000007A | LATIN SMALL LETTER Z
    7B |  0000007B | LEFT CURLY BRACKET
    7C |  0000007C | VERTICAL LINE
    7D |  0000007D | RIGHT CURLY BRACKET
    7E |  0000007E | TILDE
    7F |  0000007F | DELETE (DEL)
    80 |  00000080 | PADDING CHARACTER (PAD)
    81 |  00000081 | HIGH OCTET PRESET (HOP)
    82 |  00000082 | BREAK PERMITTED HERE (BPH)
    83 |  00000083 | NO BREAK HERE (NBH)
    84 |  00000084 | INDEX (IND)
    85 |  00000085 | NEXT LINE (NEL)
    86 |  00000086 | START OF SELECTED AREA (SSA)
    87 |  00000087 | END OF SELECTED AREA (ESA)
    88 |  00000088 | CHARACTER TABULATION SET (HTS)
    89 |  00000089 | CHARACTER TABULATION WITH JUSTIFICATION (HTJ)
    8A |  0000008A | LINE TABULATION SET (VTS)
    8B |  0000008B | PARTIAL LINE FORWARD (PLD)
    8C |  0000008C | PARTIAL LINE BACKWARD (PLU)
    8D |  0000008D | REVERSE LINE FEED (RI)
    8E |  0000008E | SINGLE-SHIFT TWO (SS2)
    8F |  0000008F | SINGLE-SHIFT THREE (SS3)
    90 |  00000090 | DEVICE CONTROL STRING (DCS)
    91 |  00000091 | PRIVATE USE ONE (PU1)
    92 |  00000092 | PRIVATE USE TWO (PU2)
    93 |  00000093 | SET TRANSMIT STATE (STS)
    94 |  00000094 | CANCEL CHARACTER (CCH)
    95 |  00000095 | MESSAGE WAITING (MW)
    96 |  00000096 | START OF GUARDED AREA (SPA)
    97 |  00000097 | END OF GUARDED AREA (EPA)
    98 |  00000098 | START OF STRING (SOS)
    99 |  00000099 | SINGLE GRAPHIC CHARACTER INTRODUCER (SGCI)
    9A |  0000009A | SINGLE CHARACTER INTRODUCER (SCI)
    9B |  0000009B | CONTROL SEQUENCE INTRODUCER (CSI)
    9C |  0000009C | STRING TERMINATOR (ST)
    9D |  0000009D | OPERATING SYSTEM COMMAND (OSC)
    9E |  0000009E | PRIVACY MESSAGE (PM)
    9F |  0000009F | APPLICATION PROGRAM COMMAND (APC)
    A0 |  000000A0 | NO-BREAK SPACE
    A1 |  000000A1 | INVERTED EXCLAMATION MARK
    A2 |  000000A2 | CENT SIGN
    A3 |  000000A3 | POUND SIGN
    A4 |  000000A4 | CURRENCY SIGN
    A5 |  000000A5 | YEN SIGN
    A6 |  000000A6 | BROKEN BAR
    A7 |  000000A7 | SECTION SIGN
    A8 |  000000A8 | DIAERESIS
    A9 |  000000A9 | COPYRIGHT SIGN
    AA |  000000AA | FEMININE ORDINAL INDICATOR
    AB |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    AC |  000000AC | NOT SIGN
    AD |  000000AD | SOFT HYPHEN
    AE |  000000AE | REGISTERED SIGN
    AF |  000000AF | MACRON
    B0 |  000000B0 | DEGREE SIGN
    B1 |  000000B1 | PLUS-MINUS SIGN
    B2 |  000000B2 | SUPERSCRIPT TWO
    B3 |  000000B3 | SUPERSCRIPT THREE
    B4 |  000000B4 | ACUTE ACCENT
    B5 |  000000B5 | MICRO SIGN
    B6 |  000000B6 | PILCROW SIGN
    B7 |  000000B7 | MIDDLE DOT
    B8 |  000000B8 | CEDILLA
    B9 |  000000B9 | SUPERSCRIPT ONE
    BA |  000000BA | MASCULINE ORDINAL INDICATOR
    BB |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    BC |  000000BC | VULGAR FRACTION ONE QUARTER
    BD |  000000BD | VULGAR FRACTION ONE HALF
    BE |  000000BE | VULGAR FRACTION THREE QUARTERS
    BF |  000000BF | INVERTED QUESTION MARK
    C0 |  000000C0 | LATIN CAPITAL LETTER A WITH GRAVE
    C1 |  000000C1 | LATIN CAPITAL LETTER A WITH ACUTE
    C2 |  000000C2 | LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    C3 |  000000C3 | LATIN CAPITAL LETTER A WITH TILDE
    C4 |  000000C4 | LATIN CAPITAL LETTER A WITH DIAERESIS
    C5 |  000000C5 | LATIN CAPITAL LETTER A WITH RING ABOVE
    C6 |  000000C6 | LATIN CAPITAL LETTER AE
    C7 |  000000C7 | LATIN CAPITAL LETTER C WITH CEDILLA
    C8 |  000000C8 | LATIN CAPITAL LETTER E WITH GRAVE
    C9 |  000000C9 | LATIN CAPITAL LETTER E WITH ACUTE
    CA |  000000CA | LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    CB |  000000CB | LATIN CAPITAL LETTER E WITH DIAERESIS
    CC |  000000CC | LATIN CAPITAL LETTER I WITH GRAVE
    CD |  000000CD | LATIN CAPITAL LETTER I WITH ACUTE
    CE |  000000CE | LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    CF |  000000CF | LATIN CAPITAL LETTER I WITH DIAERESIS
    D0 |  000000D0 | LATIN CAPITAL LETTER ETH
    D1 |  000000D1 | LATIN CAPITAL LETTER N WITH TILDE
    D2 |  000000D2 | LATIN CAPITAL LETTER O WITH GRAVE
    D3 |  000000D3 | LATIN CAPITAL LETTER O WITH ACUTE
    D4 |  000000D4 | LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    D5 |  000000D5 | LATIN CAPITAL LETTER O WITH TILDE
    D6 |  000000D6 | LATIN CAPITAL LETTER O WITH DIAERESIS
    D7 |  000000D7 | MULTIPLICATION SIGN
    D8 |  000000D8 | LATIN CAPITAL LETTER O WITH STROKE
    D9 |  000000D9 | LATIN CAPITAL LETTER U WITH GRAVE
    DA |  000000DA | LATIN CAPITAL LETTER U WITH ACUTE
    DB |  000000DB | LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    DC |  000000DC | LATIN CAPITAL LETTER U WITH DIAERESIS
    DD |  000000DD | LATIN CAPITAL LETTER Y WITH ACUTE
    DE |  000000DE | LATIN CAPITAL LETTER THORN
    DF |  000000DF | LATIN SMALL LETTER SHARP S
    E0 |  000000E0 | LATIN SMALL LETTER A WITH GRAVE
    E1 |  000000E1 | LATIN SMALL LETTER A WITH ACUTE
    E2 |  000000E2 | LATIN SMALL LETTER A WITH CIRCUMFLEX
    E3 |  000000E3 | LATIN SMALL LETTER A WITH TILDE
    E4 |  000000E4 | LATIN SMALL LETTER A WITH DIAERESIS
    E5 |  000000E5 | LATIN SMALL LETTER A WITH RING ABOVE
    E6 |  000000E6 | LATIN SMALL LETTER AE
    E7 |  000000E7 | LATIN SMALL LETTER C WITH CEDILLA
    E8 |  000000E8 | LATIN SMALL LETTER E WITH GRAVE
    E9 |  000000E9 | LATIN SMALL LETTER E WITH ACUTE
    EA |  000000EA | LATIN SMALL LETTER E WITH CIRCUMFLEX
    EB |  000000EB | LATIN SMALL LETTER E WITH DIAERESIS
    EC |  000000EC | LATIN SMALL LETTER I WITH GRAVE
    ED |  000000ED | LATIN SMALL LETTER I WITH ACUTE
    EE |  000000EE | LATIN SMALL LETTER I WITH CIRCUMFLEX
    EF |  000000EF | LATIN SMALL LETTER I WITH DIAERESIS
    F0 |  000000F0 | LATIN SMALL LETTER ETH
    F1 |  000000F1 | LATIN SMALL LETTER N WITH TILDE
    F2 |  000000F2 | LATIN SMALL LETTER O WITH GRAVE
    F3 |  000000F3 | LATIN SMALL LETTER O WITH ACUTE
    F4 |  000000F4 | LATIN SMALL LETTER O WITH CIRCUMFLEX
    F5 |  000000F5 | LATIN SMALL LETTER O WITH TILDE
    F6 |  000000F6 | LATIN SMALL LETTER O WITH DIAERESIS
    F7 |  000000F7 | DIVISION SIGN
    F8 |  000000F8 | LATIN SMALL LETTER O WITH STROKE
    F9 |  000000F9 | LATIN SMALL LETTER U WITH GRAVE
    FA |  000000FA | LATIN SMALL LETTER U WITH ACUTE
    FB |  000000FB | LATIN SMALL LETTER U WITH CIRCUMFLEX
    FC |  000000FC | LATIN SMALL LETTER U WITH DIAERESIS
    FD |  000000FD | LATIN SMALL LETTER Y WITH ACUTE
    FE |  000000FE | LATIN SMALL LETTER THORN
    FF |  000000FF | LATIN SMALL LETTER Y WITH DIAERESIS

=head1 AUTHOR

Copyright (C) 2002-2009, Guido Flohr E<lt>guido@imperia.netE<gt>, all
rights reserved.  See the source code for details.

This software is contributed to the Perl community by Imperia 
(L<http://www.imperia.net/>).

=head1 SEE ALSO

Locale::RecodeData(3), Locale::Recode(3), perl(1)

=cut
Local Variables:
mode: perl
perl-indent-level: 4
perl-continued-statement-offset: 4
perl-continued-brace-offset: 0
perl-brace-offset: -4
perl-brace-imaginary-offset: 0
perl-label-offset: -4
cperl-indent-level: 4
cperl-continued-statement-offset: 2
tab-width: 4
End:
=cut
