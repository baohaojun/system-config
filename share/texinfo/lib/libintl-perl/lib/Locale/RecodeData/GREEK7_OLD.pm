#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: GREEK7_OLD.pm,v 1.1 2011-10-12 23:51:34 pertusus Exp $

# Conversion routines for GREEK7-OLD.
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

package Locale::RecodeData::GREEK7_OLD;

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
    0x00a3,
    0x0024,
    0x0025,
    0x0026,
    0x0027,
    0x0028,
    0x0029,
    0x002a,
    0x002b,
    0x002c,
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
    0x003b,
    0x003c,
    0x003d,
    0x003e,
    0x003f,
    0x00b4,
    0x03b1,
    0x03b2,
    0x03c8,
    0x03b4,
    0x03b5,
    0x03c6,
    0x03b3,
    0x03b7,
    0x03b9,
    0x03be,
    0x03ba,
    0x03bb,
    0x03bc,
    0x03bd,
    0x03bf,
    0x03c0,
    0x037a,
    0x03c1,
    0x03c3,
    0x03c4,
    0x03b8,
    0x03c9,
    0x03c2,
    0x03c7,
    0x03c5,
    0x03b6,
    0x1fcf,
    0x1fbf,
    0x1fce,
    0x007e,
    0x005f,
    0x0060,
    0x0391,
    0x0392,
    0x03a8,
    0x0394,
    0x0395,
    0x03a6,
    0x0393,
    0x0397,
    0x0399,
    0x039e,
    0x039a,
    0x039b,
    0x039c,
    0x039d,
    0x039f,
    0x03a0,
    0xfffd,
    0x03a1,
    0x03a3,
    0x03a4,
    0x0398,
    0x03a9,
    0x00b7,
    0x03a7,
    0x03a5,
    0x0396,
    0x1fdf,
    0x1ffe,
    0x1fde,
    0x00a8,
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
    "\xc2\xa3",
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
    "\xc2\xb4",
    "\xce\xb1",
    "\xce\xb2",
    "\xcf\x88",
    "\xce\xb4",
    "\xce\xb5",
    "\xcf\x86",
    "\xce\xb3",
    "\xce\xb7",
    "\xce\xb9",
    "\xce\xbe",
    "\xce\xba",
    "\xce\xbb",
    "\xce\xbc",
    "\xce\xbd",
    "\xce\xbf",
    "\xcf\x80",
    "\xcd\xba",
    "\xcf\x81",
    "\xcf\x83",
    "\xcf\x84",
    "\xce\xb8",
    "\xcf\x89",
    "\xcf\x82",
    "\xcf\x87",
    "\xcf\x85",
    "\xce\xb6",
    "\xe1\xbf\x8f",
    "\xe1\xbe\xbf",
    "\xe1\xbf\x8e",
    "\x7e",
    "\x5f",
    "\x60",
    "\xce\x91",
    "\xce\x92",
    "\xce\xa8",
    "\xce\x94",
    "\xce\x95",
    "\xce\xa6",
    "\xce\x93",
    "\xce\x97",
    "\xce\x99",
    "\xce\x9e",
    "\xce\x9a",
    "\xce\x9b",
    "\xce\x9c",
    "\xce\x9d",
    "\xce\x9f",
    "\xce\xa0",
    "\xef\xbf\xbd",
    "\xce\xa1",
    "\xce\xa3",
    "\xce\xa4",
    "\xce\x98",
    "\xce\xa9",
    "\xc2\xb7",
    "\xce\xa7",
    "\xce\xa5",
    "\xce\x96",
    "\xe1\xbf\x9f",
    "\xe1\xbf\xbe",
    "\xe1\xbf\x9e",
    "\xc2\xa8",
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
    0x00000024 => "\x24",
    0x00000025 => "\x25",
    0x00000026 => "\x26",
    0x00000027 => "\x27",
    0x00000028 => "\x28",
    0x00000029 => "\x29",
    0x0000002a => "\x2a",
    0x0000002b => "\x2b",
    0x0000002c => "\x2c",
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
    0x0000003b => "\x3b",
    0x0000003c => "\x3c",
    0x0000003d => "\x3d",
    0x0000003e => "\x3e",
    0x0000003f => "\x3f",
    0x0000005f => "\x5f",
    0x00000060 => "\x60",
    0x0000007e => "\x5e",
    0x0000007f => "\x7f",
    0x000000a3 => "\x23",
    0x000000a8 => "\x7e",
    0x000000b4 => "\x40",
    0x000000b7 => "\x77",
    0x0000037a => "\x51",
    0x00000391 => "\x61",
    0x00000392 => "\x62",
    0x00000393 => "\x67",
    0x00000394 => "\x64",
    0x00000395 => "\x65",
    0x00000396 => "\x7a",
    0x00000397 => "\x68",
    0x00000398 => "\x75",
    0x00000399 => "\x69",
    0x0000039a => "\x6b",
    0x0000039b => "\x6c",
    0x0000039c => "\x6d",
    0x0000039d => "\x6e",
    0x0000039e => "\x6a",
    0x0000039f => "\x6f",
    0x000003a0 => "\x70",
    0x000003a1 => "\x72",
    0x000003a3 => "\x73",
    0x000003a4 => "\x74",
    0x000003a5 => "\x79",
    0x000003a6 => "\x66",
    0x000003a7 => "\x78",
    0x000003a8 => "\x63",
    0x000003a9 => "\x76",
    0x000003b1 => "\x41",
    0x000003b2 => "\x42",
    0x000003b3 => "\x47",
    0x000003b4 => "\x44",
    0x000003b5 => "\x45",
    0x000003b6 => "\x5a",
    0x000003b7 => "\x48",
    0x000003b8 => "\x55",
    0x000003b9 => "\x49",
    0x000003ba => "\x4b",
    0x000003bb => "\x4c",
    0x000003bc => "\x4d",
    0x000003bd => "\x4e",
    0x000003be => "\x4a",
    0x000003bf => "\x4f",
    0x000003c0 => "\x50",
    0x000003c1 => "\x52",
    0x000003c2 => "\x57",
    0x000003c3 => "\x53",
    0x000003c4 => "\x54",
    0x000003c5 => "\x59",
    0x000003c6 => "\x46",
    0x000003c7 => "\x58",
    0x000003c8 => "\x43",
    0x000003c9 => "\x56",
    0x00001fbf => "\x5c",
    0x00001fce => "\x5d",
    0x00001fcf => "\x5b",
    0x00001fde => "\x7d",
    0x00001fdf => "\x7b",
    0x00001ffe => "\x7c",
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

Locale::RecodeData::GREEK7_OLD - Conversion routines for GREEK7_OLD

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for GREEK7-OLD.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: ECMA registry
 alias ISO-IR-18

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
    23 |  000000A3 | POUND SIGN
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
    40 |  000000B4 | ACUTE ACCENT
    41 |  000003B1 | GREEK SMALL LETTER ALPHA
    42 |  000003B2 | GREEK SMALL LETTER BETA
    43 |  000003C8 | GREEK SMALL LETTER PSI
    44 |  000003B4 | GREEK SMALL LETTER DELTA
    45 |  000003B5 | GREEK SMALL LETTER EPSILON
    46 |  000003C6 | GREEK SMALL LETTER PHI
    47 |  000003B3 | GREEK SMALL LETTER GAMMA
    48 |  000003B7 | GREEK SMALL LETTER ETA
    49 |  000003B9 | GREEK SMALL LETTER IOTA
    4A |  000003BE | GREEK SMALL LETTER XI
    4B |  000003BA | GREEK SMALL LETTER KAPPA
    4C |  000003BB | GREEK SMALL LETTER LAMDA
    4D |  000003BC | GREEK SMALL LETTER MU
    4E |  000003BD | GREEK SMALL LETTER NU
    4F |  000003BF | GREEK SMALL LETTER OMICRON
    50 |  000003C0 | GREEK SMALL LETTER PI
    51 |  0000037A | GREEK YPOGEGRAMMENI
    52 |  000003C1 | GREEK SMALL LETTER RHO
    53 |  000003C3 | GREEK SMALL LETTER SIGMA
    54 |  000003C4 | GREEK SMALL LETTER TAU
    55 |  000003B8 | GREEK SMALL LETTER THETA
    56 |  000003C9 | GREEK SMALL LETTER OMEGA
    57 |  000003C2 | GREEK SMALL LETTER FINAL SIGMA
    58 |  000003C7 | GREEK SMALL LETTER CHI
    59 |  000003C5 | GREEK SMALL LETTER UPSILON
    5A |  000003B6 | GREEK SMALL LETTER ZETA
    5B |  00001FCF | GREEK PSILI AND PERISPOMENI
    5C |  00001FBF | GREEK PSILI
    5D |  00001FCE | GREEK PSILI AND OXIA
    5E |  0000007E | TILDE
    5F |  0000005F | LOW LINE
    60 |  00000060 | GRAVE ACCENT
    61 |  00000391 | GREEK CAPITAL LETTER ALPHA
    62 |  00000392 | GREEK CAPITAL LETTER BETA
    63 |  000003A8 | GREEK CAPITAL LETTER PSI
    64 |  00000394 | GREEK CAPITAL LETTER DELTA
    65 |  00000395 | GREEK CAPITAL LETTER EPSILON
    66 |  000003A6 | GREEK CAPITAL LETTER PHI
    67 |  00000393 | GREEK CAPITAL LETTER GAMMA
    68 |  00000397 | GREEK CAPITAL LETTER ETA
    69 |  00000399 | GREEK CAPITAL LETTER IOTA
    6A |  0000039E | GREEK CAPITAL LETTER XI
    6B |  0000039A | GREEK CAPITAL LETTER KAPPA
    6C |  0000039B | GREEK CAPITAL LETTER LAMDA
    6D |  0000039C | GREEK CAPITAL LETTER MU
    6E |  0000039D | GREEK CAPITAL LETTER NU
    6F |  0000039F | GREEK CAPITAL LETTER OMICRON
    70 |  000003A0 | GREEK CAPITAL LETTER PI
    72 |  000003A1 | GREEK CAPITAL LETTER RHO
    73 |  000003A3 | GREEK CAPITAL LETTER SIGMA
    74 |  000003A4 | GREEK CAPITAL LETTER TAU
    75 |  00000398 | GREEK CAPITAL LETTER THETA
    76 |  000003A9 | GREEK CAPITAL LETTER OMEGA
    77 |  000000B7 | MIDDLE DOT
    78 |  000003A7 | GREEK CAPITAL LETTER CHI
    79 |  000003A5 | GREEK CAPITAL LETTER UPSILON
    7A |  00000396 | GREEK CAPITAL LETTER ZETA
    7B |  00001FDF | GREEK DASIA AND PERISPOMENI
    7C |  00001FFE | GREEK DASIA
    7D |  00001FDE | GREEK DASIA AND OXIA
    7E |  000000A8 | DIAERESIS
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
