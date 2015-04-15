#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: ASMO_449.pm,v 1.1 2011-10-12 23:51:27 pertusus Exp $

# Conversion routines for ASMO_449.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>, all
# rights reserved.
# This file is generated, do not edit!

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

package Locale::RecodeData::ASMO_449;

use strict;

require Locale::RecodeData;
use base qw(Locale::RecodeData);

my @to_ucs4 = (
    0x0000,
    0x0001,
    0x0002,
    0x0003,
    0x0004,
    0x0005,
    0x0006,
    0x0007,
    0x0008,
    0x0009,
    0x000a,
    0x000b,
    0x000c,
    0x000d,
    0x000e,
    0x000f,
    0x0010,
    0x0011,
    0x0012,
    0x0013,
    0x0014,
    0x0015,
    0x0016,
    0x0017,
    0x0018,
    0x0019,
    0x001a,
    0x001b,
    0x001c,
    0x001d,
    0x001e,
    0x001f,
    0x0020,
    0x0021,
    0x0022,
    0x0023,
    0x00a4,
    0x0025,
    0x0026,
    0x0027,
    0x0028,
    0x0029,
    0x002a,
    0x002b,
    0x060c,
    0x002d,
    0x002e,
    0x002f,
    0x0030,
    0x0031,
    0x0032,
    0x0033,
    0x0034,
    0x0035,
    0x0036,
    0x0037,
    0x0038,
    0x0039,
    0x003a,
    0x061b,
    0x003c,
    0x003d,
    0x003e,
    0x061f,
    0x0040,
    0x0621,
    0x0622,
    0x0623,
    0x0624,
    0x0625,
    0x0626,
    0x0627,
    0x0628,
    0x0629,
    0x062a,
    0x062b,
    0x062c,
    0x062d,
    0x062e,
    0x062f,
    0x0630,
    0x0631,
    0x0632,
    0x0633,
    0x0634,
    0x0635,
    0x0636,
    0x0637,
    0x0638,
    0x0639,
    0x063a,
    0x005b,
    0x005c,
    0x005d,
    0x005e,
    0x005f,
    0x0640,
    0x0641,
    0x0642,
    0x0643,
    0x0644,
    0x0645,
    0x0646,
    0x0647,
    0x0648,
    0x0649,
    0x064a,
    0x064b,
    0x064c,
    0x064d,
    0x064e,
    0x064f,
    0x0650,
    0x0651,
    0x0652,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x007b,
    0x007c,
    0x007d,
    0x203e,
    0x007f,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
);

my @to_utf8 = (
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
    "\xc2\xa4",
    "\x25",
    "\x26",
    "\x27",
    "\x28",
    "\x29",
    "\x2a",
    "\x2b",
    "\xd8\x8c",
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
    "\xd8\x9b",
    "\x3c",
    "\x3d",
    "\x3e",
    "\xd8\x9f",
    "\x40",
    "\xd8\xa1",
    "\xd8\xa2",
    "\xd8\xa3",
    "\xd8\xa4",
    "\xd8\xa5",
    "\xd8\xa6",
    "\xd8\xa7",
    "\xd8\xa8",
    "\xd8\xa9",
    "\xd8\xaa",
    "\xd8\xab",
    "\xd8\xac",
    "\xd8\xad",
    "\xd8\xae",
    "\xd8\xaf",
    "\xd8\xb0",
    "\xd8\xb1",
    "\xd8\xb2",
    "\xd8\xb3",
    "\xd8\xb4",
    "\xd8\xb5",
    "\xd8\xb6",
    "\xd8\xb7",
    "\xd8\xb8",
    "\xd8\xb9",
    "\xd8\xba",
    "\x5b",
    "\x5c",
    "\x5d",
    "\x5e",
    "\x5f",
    "\xd9\x80",
    "\xd9\x81",
    "\xd9\x82",
    "\xd9\x83",
    "\xd9\x84",
    "\xd9\x85",
    "\xd9\x86",
    "\xd9\x87",
    "\xd9\x88",
    "\xd9\x89",
    "\xd9\x8a",
    "\xd9\x8b",
    "\xd9\x8c",
    "\xd9\x8d",
    "\xd9\x8e",
    "\xd9\x8f",
    "\xd9\x90",
    "\xd9\x91",
    "\xd9\x92",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\x7b",
    "\x7c",
    "\x7d",
    "\xe2\x80\xbe",
    "\x7f",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
);

my %from_ucs4 = (
    0x00000000 => "\x00",
    0x00000001 => "\x01",
    0x00000002 => "\x02",
    0x00000003 => "\x03",
    0x00000004 => "\x04",
    0x00000005 => "\x05",
    0x00000006 => "\x06",
    0x00000007 => "\x07",
    0x00000008 => "\x08",
    0x00000009 => "\x09",
    0x0000000a => "\x0a",
    0x0000000b => "\x0b",
    0x0000000c => "\x0c",
    0x0000000d => "\x0d",
    0x0000000e => "\x0e",
    0x0000000f => "\x0f",
    0x00000010 => "\x10",
    0x00000011 => "\x11",
    0x00000012 => "\x12",
    0x00000013 => "\x13",
    0x00000014 => "\x14",
    0x00000015 => "\x15",
    0x00000016 => "\x16",
    0x00000017 => "\x17",
    0x00000018 => "\x18",
    0x00000019 => "\x19",
    0x0000001a => "\x1a",
    0x0000001b => "\x1b",
    0x0000001c => "\x1c",
    0x0000001d => "\x1d",
    0x0000001e => "\x1e",
    0x0000001f => "\x1f",
    0x00000020 => "\x20",
    0x00000021 => "\x21",
    0x00000022 => "\x22",
    0x00000023 => "\x23",
    0x00000025 => "\x25",
    0x00000026 => "\x26",
    0x00000027 => "\x27",
    0x00000028 => "\x28",
    0x00000029 => "\x29",
    0x0000002a => "\x2a",
    0x0000002b => "\x2b",
    0x0000002d => "\x2d",
    0x0000002e => "\x2e",
    0x0000002f => "\x2f",
    0x00000030 => "\x30",
    0x00000031 => "\x31",
    0x00000032 => "\x32",
    0x00000033 => "\x33",
    0x00000034 => "\x34",
    0x00000035 => "\x35",
    0x00000036 => "\x36",
    0x00000037 => "\x37",
    0x00000038 => "\x38",
    0x00000039 => "\x39",
    0x0000003a => "\x3a",
    0x0000003c => "\x3c",
    0x0000003d => "\x3d",
    0x0000003e => "\x3e",
    0x00000040 => "\x40",
    0x0000005b => "\x5b",
    0x0000005c => "\x5c",
    0x0000005d => "\x5d",
    0x0000005e => "\x5e",
    0x0000005f => "\x5f",
    0x0000007b => "\x7b",
    0x0000007c => "\x7c",
    0x0000007d => "\x7d",
    0x0000007f => "\x7f",
    0x000000a4 => "\x24",
    0x0000060c => "\x2c",
    0x0000061b => "\x3b",
    0x0000061f => "\x3f",
    0x00000621 => "\x41",
    0x00000622 => "\x42",
    0x00000623 => "\x43",
    0x00000624 => "\x44",
    0x00000625 => "\x45",
    0x00000626 => "\x46",
    0x00000627 => "\x47",
    0x00000628 => "\x48",
    0x00000629 => "\x49",
    0x0000062a => "\x4a",
    0x0000062b => "\x4b",
    0x0000062c => "\x4c",
    0x0000062d => "\x4d",
    0x0000062e => "\x4e",
    0x0000062f => "\x4f",
    0x00000630 => "\x50",
    0x00000631 => "\x51",
    0x00000632 => "\x52",
    0x00000633 => "\x53",
    0x00000634 => "\x54",
    0x00000635 => "\x55",
    0x00000636 => "\x56",
    0x00000637 => "\x57",
    0x00000638 => "\x58",
    0x00000639 => "\x59",
    0x0000063a => "\x5a",
    0x00000640 => "\x60",
    0x00000641 => "\x61",
    0x00000642 => "\x62",
    0x00000643 => "\x63",
    0x00000644 => "\x64",
    0x00000645 => "\x65",
    0x00000646 => "\x66",
    0x00000647 => "\x67",
    0x00000648 => "\x68",
    0x00000649 => "\x69",
    0x0000064a => "\x6a",
    0x0000064b => "\x6b",
    0x0000064c => "\x6c",
    0x0000064d => "\x6d",
    0x0000064e => "\x6e",
    0x0000064f => "\x6f",
    0x00000650 => "\x70",
    0x00000651 => "\x71",
    0x00000652 => "\x72",
    0x0000203e => "\x7e",
);

sub _recode
{
    if ($_[0]->{_from} eq 'INTERNAL') {
		$_[1] = join '',
	        map $from_ucs4{$_} 
                || (defined $from_ucs4{$_} ? $from_ucs4{$_} : "\x3f"),
		    @{$_[1]};
    } elsif ($_[0]->{_to} eq 'UTF-8',) {
		$_[1] = join '', map $to_utf8[$_], unpack 'C*', $_[1];
    } else {
		$_[1] = [ map 
				  $to_ucs4[$_],
				  unpack 'C*', $_[1] 
				  ];
    }

    return 1;
}

1;

__END__

=head1 NAME

Locale::RecodeData::ASMO_449 - Conversion routines for ASMO_449

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for ASMO_449.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: ECMA registry
 alias ISO_9036
 alias ARABIC7
 alias ISO-IR-89

Please note that aliases listed above are not necessarily valid!

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
    24 |  000000A4 | CURRENCY SIGN
    25 |  00000025 | PERCENT SIGN
    26 |  00000026 | AMPERSAND
    27 |  00000027 | APOSTROPHE
    28 |  00000028 | LEFT PARENTHESIS
    29 |  00000029 | RIGHT PARENTHESIS
    2A |  0000002A | ASTERISK
    2B |  0000002B | PLUS SIGN
    2C |  0000060C | ARABIC COMMA
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
    3B |  0000061B | ARABIC SEMICOLON
    3C |  0000003C | LESS-THAN SIGN
    3D |  0000003D | EQUALS SIGN
    3E |  0000003E | GREATER-THAN SIGN
    3F |  0000061F | ARABIC QUESTION MARK
    40 |  00000040 | COMMERCIAL AT
    41 |  00000621 | ARABIC LETTER HAMZA
    42 |  00000622 | ARABIC LETTER ALEF WITH MADDA ABOVE
    43 |  00000623 | ARABIC LETTER ALEF WITH HAMZA ABOVE
    44 |  00000624 | ARABIC LETTER WAW WITH HAMZA ABOVE
    45 |  00000625 | ARABIC LETTER ALEF WITH HAMZA BELOW
    46 |  00000626 | ARABIC LETTER YEH WITH HAMZA ABOVE
    47 |  00000627 | ARABIC LETTER ALEF
    48 |  00000628 | ARABIC LETTER BEH
    49 |  00000629 | ARABIC LETTER TEH MARBUTA
    4A |  0000062A | ARABIC LETTER TEH
    4B |  0000062B | ARABIC LETTER THEH
    4C |  0000062C | ARABIC LETTER JEEM
    4D |  0000062D | ARABIC LETTER HAH
    4E |  0000062E | ARABIC LETTER KHAH
    4F |  0000062F | ARABIC LETTER DAL
    50 |  00000630 | ARABIC LETTER THAL
    51 |  00000631 | ARABIC LETTER REH
    52 |  00000632 | ARABIC LETTER ZAIN
    53 |  00000633 | ARABIC LETTER SEEN
    54 |  00000634 | ARABIC LETTER SHEEN
    55 |  00000635 | ARABIC LETTER SAD
    56 |  00000636 | ARABIC LETTER DAD
    57 |  00000637 | ARABIC LETTER TAH
    58 |  00000638 | ARABIC LETTER ZAH
    59 |  00000639 | ARABIC LETTER AIN
    5A |  0000063A | ARABIC LETTER GHAIN
    5B |  0000005B | LEFT SQUARE BRACKET
    5C |  0000005C | REVERSE SOLIDUS
    5D |  0000005D | RIGHT SQUARE BRACKET
    5E |  0000005E | CIRCUMFLEX ACCENT
    5F |  0000005F | LOW LINE
    60 |  00000640 | ARABIC TATWEEL
    61 |  00000641 | ARABIC LETTER FEH
    62 |  00000642 | ARABIC LETTER QAF
    63 |  00000643 | ARABIC LETTER KAF
    64 |  00000644 | ARABIC LETTER LAM
    65 |  00000645 | ARABIC LETTER MEEM
    66 |  00000646 | ARABIC LETTER NOON
    67 |  00000647 | ARABIC LETTER HEH
    68 |  00000648 | ARABIC LETTER WAW
    69 |  00000649 | ARABIC LETTER ALEF MAKSURA
    6A |  0000064A | ARABIC LETTER YEH
    6B |  0000064B | ARABIC FATHATAN
    6C |  0000064C | ARABIC DAMMATAN
    6D |  0000064D | ARABIC KASRATAN
    6E |  0000064E | ARABIC FATHA
    6F |  0000064F | ARABIC DAMMA
    70 |  00000650 | ARABIC KASRA
    71 |  00000651 | ARABIC SHADDA
    72 |  00000652 | ARABIC SUKUN
    7B |  0000007B | LEFT CURLY BRACKET
    7C |  0000007C | VERTICAL LINE
    7D |  0000007D | RIGHT CURLY BRACKET
    7E |  0000203E | OVERLINE
    7F |  0000007F | DELETE (DEL)


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
