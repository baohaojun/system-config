#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: INIS_CYRILLIC.pm,v 1.1 2011-10-12 23:51:41 pertusus Exp $

# Conversion routines for INIS-CYRILLIC.
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

package Locale::RecodeData::INIS_CYRILLIC;

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
    0x221a,
    0xfffd,
    0x2192,
    0x222b,
    0x03b1,
    0x03b2,
    0x03b3,
    0x03b4,
    0x03a3,
    0x03bc,
    0x03bd,
    0x03c9,
    0x03c0,
    0x039e,
    0x0394,
    0x039b,
    0x03a9,
    0x042a,
    0x207b,
    0x207a,
    0x044e,
    0x0430,
    0x0431,
    0x0446,
    0x0434,
    0x0435,
    0x0444,
    0x0433,
    0x0445,
    0x0438,
    0x0439,
    0x043a,
    0x043b,
    0x043c,
    0x043d,
    0x043e,
    0x043f,
    0x044f,
    0x0440,
    0x0441,
    0x0442,
    0x0443,
    0x0436,
    0x0432,
    0x044c,
    0x044b,
    0x0437,
    0x0448,
    0x044d,
    0x0449,
    0x0447,
    0x044a,
    0x042e,
    0x0410,
    0x0411,
    0x0426,
    0x0414,
    0x0415,
    0x0424,
    0x0413,
    0x0425,
    0x0418,
    0x0419,
    0x041a,
    0x041b,
    0x041c,
    0x041d,
    0x041e,
    0x041f,
    0x042f,
    0x0420,
    0x0421,
    0x0422,
    0x0423,
    0x0416,
    0x0412,
    0x042c,
    0x042b,
    0x0417,
    0x0428,
    0x042d,
    0x0429,
    0x0427,
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
    "\xe2\x88\x9a",
    "\xef\xbf\xbd",
    "\xe2\x86\x92",
    "\xe2\x88\xab",
    "\xce\xb1",
    "\xce\xb2",
    "\xce\xb3",
    "\xce\xb4",
    "\xce\xa3",
    "\xce\xbc",
    "\xce\xbd",
    "\xcf\x89",
    "\xcf\x80",
    "\xce\x9e",
    "\xce\x94",
    "\xce\x9b",
    "\xce\xa9",
    "\xd0\xaa",
    "\xe2\x81\xbb",
    "\xe2\x81\xba",
    "\xd1\x8e",
    "\xd0\xb0",
    "\xd0\xb1",
    "\xd1\x86",
    "\xd0\xb4",
    "\xd0\xb5",
    "\xd1\x84",
    "\xd0\xb3",
    "\xd1\x85",
    "\xd0\xb8",
    "\xd0\xb9",
    "\xd0\xba",
    "\xd0\xbb",
    "\xd0\xbc",
    "\xd0\xbd",
    "\xd0\xbe",
    "\xd0\xbf",
    "\xd1\x8f",
    "\xd1\x80",
    "\xd1\x81",
    "\xd1\x82",
    "\xd1\x83",
    "\xd0\xb6",
    "\xd0\xb2",
    "\xd1\x8c",
    "\xd1\x8b",
    "\xd0\xb7",
    "\xd1\x88",
    "\xd1\x8d",
    "\xd1\x89",
    "\xd1\x87",
    "\xd1\x8a",
    "\xd0\xae",
    "\xd0\x90",
    "\xd0\x91",
    "\xd0\xa6",
    "\xd0\x94",
    "\xd0\x95",
    "\xd0\xa4",
    "\xd0\x93",
    "\xd0\xa5",
    "\xd0\x98",
    "\xd0\x99",
    "\xd0\x9a",
    "\xd0\x9b",
    "\xd0\x9c",
    "\xd0\x9d",
    "\xd0\x9e",
    "\xd0\x9f",
    "\xd0\xaf",
    "\xd0\xa0",
    "\xd0\xa1",
    "\xd0\xa2",
    "\xd0\xa3",
    "\xd0\x96",
    "\xd0\x92",
    "\xd0\xac",
    "\xd0\xab",
    "\xd0\x97",
    "\xd0\xa8",
    "\xd0\xad",
    "\xd0\xa9",
    "\xd0\xa7",
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
    0x0000007f => "\x7f",
    0x00000394 => "\x3a",
    0x0000039b => "\x3b",
    0x0000039e => "\x39",
    0x000003a3 => "\x34",
    0x000003a9 => "\x3c",
    0x000003b1 => "\x30",
    0x000003b2 => "\x31",
    0x000003b3 => "\x32",
    0x000003b4 => "\x33",
    0x000003bc => "\x35",
    0x000003bd => "\x36",
    0x000003c0 => "\x38",
    0x000003c9 => "\x37",
    0x00000410 => "\x61",
    0x00000411 => "\x62",
    0x00000412 => "\x77",
    0x00000413 => "\x67",
    0x00000414 => "\x64",
    0x00000415 => "\x65",
    0x00000416 => "\x76",
    0x00000417 => "\x7a",
    0x00000418 => "\x69",
    0x00000419 => "\x6a",
    0x0000041a => "\x6b",
    0x0000041b => "\x6c",
    0x0000041c => "\x6d",
    0x0000041d => "\x6e",
    0x0000041e => "\x6f",
    0x0000041f => "\x70",
    0x00000420 => "\x72",
    0x00000421 => "\x73",
    0x00000422 => "\x74",
    0x00000423 => "\x75",
    0x00000424 => "\x66",
    0x00000425 => "\x68",
    0x00000426 => "\x63",
    0x00000427 => "\x7e",
    0x00000428 => "\x7b",
    0x00000429 => "\x7d",
    0x0000042a => "\x3d",
    0x0000042b => "\x79",
    0x0000042c => "\x78",
    0x0000042d => "\x7c",
    0x0000042e => "\x60",
    0x0000042f => "\x71",
    0x00000430 => "\x41",
    0x00000431 => "\x42",
    0x00000432 => "\x57",
    0x00000433 => "\x47",
    0x00000434 => "\x44",
    0x00000435 => "\x45",
    0x00000436 => "\x56",
    0x00000437 => "\x5a",
    0x00000438 => "\x49",
    0x00000439 => "\x4a",
    0x0000043a => "\x4b",
    0x0000043b => "\x4c",
    0x0000043c => "\x4d",
    0x0000043d => "\x4e",
    0x0000043e => "\x4f",
    0x0000043f => "\x50",
    0x00000440 => "\x52",
    0x00000441 => "\x53",
    0x00000442 => "\x54",
    0x00000443 => "\x55",
    0x00000444 => "\x46",
    0x00000445 => "\x48",
    0x00000446 => "\x43",
    0x00000447 => "\x5e",
    0x00000448 => "\x5b",
    0x00000449 => "\x5d",
    0x0000044a => "\x5f",
    0x0000044b => "\x59",
    0x0000044c => "\x58",
    0x0000044d => "\x5c",
    0x0000044e => "\x40",
    0x0000044f => "\x51",
    0x0000207a => "\x3f",
    0x0000207b => "\x3e",
    0x00002192 => "\x2e",
    0x0000221a => "\x2c",
    0x0000222b => "\x2f",
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

Locale::RecodeData::INIS_CYRILLIC - Conversion routines for INIS_CYRILLIC

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for INIS-CYRILLIC.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: ECMA registry
 alias ISO-IR-51

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
    2C |  0000221A | SQUARE ROOT
    2E |  00002192 | RIGHTWARDS ARROW
    2F |  0000222B | INTEGRAL
    30 |  000003B1 | GREEK SMALL LETTER ALPHA
    31 |  000003B2 | GREEK SMALL LETTER BETA
    32 |  000003B3 | GREEK SMALL LETTER GAMMA
    33 |  000003B4 | GREEK SMALL LETTER DELTA
    34 |  000003A3 | GREEK CAPITAL LETTER SIGMA
    35 |  000003BC | GREEK SMALL LETTER MU
    36 |  000003BD | GREEK SMALL LETTER NU
    37 |  000003C9 | GREEK SMALL LETTER OMEGA
    38 |  000003C0 | GREEK SMALL LETTER PI
    39 |  0000039E | GREEK CAPITAL LETTER XI
    3A |  00000394 | GREEK CAPITAL LETTER DELTA
    3B |  0000039B | GREEK CAPITAL LETTER LAMDA
    3C |  000003A9 | GREEK CAPITAL LETTER OMEGA
    3D |  0000042A | CYRILLIC CAPITAL LETTER HARD SIGN
    3E |  0000207B | SUPERSCRIPT MINUS
    3F |  0000207A | SUPERSCRIPT PLUS SIGN
    40 |  0000044E | CYRILLIC SMALL LETTER YU
    41 |  00000430 | CYRILLIC SMALL LETTER A
    42 |  00000431 | CYRILLIC SMALL LETTER BE
    43 |  00000446 | CYRILLIC SMALL LETTER TSE
    44 |  00000434 | CYRILLIC SMALL LETTER DE
    45 |  00000435 | CYRILLIC SMALL LETTER IE
    46 |  00000444 | CYRILLIC SMALL LETTER EF
    47 |  00000433 | CYRILLIC SMALL LETTER GHE
    48 |  00000445 | CYRILLIC SMALL LETTER HA
    49 |  00000438 | CYRILLIC SMALL LETTER I
    4A |  00000439 | CYRILLIC SMALL LETTER SHORT I
    4B |  0000043A | CYRILLIC SMALL LETTER KA
    4C |  0000043B | CYRILLIC SMALL LETTER EL
    4D |  0000043C | CYRILLIC SMALL LETTER EM
    4E |  0000043D | CYRILLIC SMALL LETTER EN
    4F |  0000043E | CYRILLIC SMALL LETTER O
    50 |  0000043F | CYRILLIC SMALL LETTER PE
    51 |  0000044F | CYRILLIC SMALL LETTER YA
    52 |  00000440 | CYRILLIC SMALL LETTER ER
    53 |  00000441 | CYRILLIC SMALL LETTER ES
    54 |  00000442 | CYRILLIC SMALL LETTER TE
    55 |  00000443 | CYRILLIC SMALL LETTER U
    56 |  00000436 | CYRILLIC SMALL LETTER ZHE
    57 |  00000432 | CYRILLIC SMALL LETTER VE
    58 |  0000044C | CYRILLIC SMALL LETTER SOFT SIGN
    59 |  0000044B | CYRILLIC SMALL LETTER YERU
    5A |  00000437 | CYRILLIC SMALL LETTER ZE
    5B |  00000448 | CYRILLIC SMALL LETTER SHA
    5C |  0000044D | CYRILLIC SMALL LETTER E
    5D |  00000449 | CYRILLIC SMALL LETTER SHCHA
    5E |  00000447 | CYRILLIC SMALL LETTER CHE
    5F |  0000044A | CYRILLIC SMALL LETTER HARD SIGN
    60 |  0000042E | CYRILLIC CAPITAL LETTER YU
    61 |  00000410 | CYRILLIC CAPITAL LETTER A
    62 |  00000411 | CYRILLIC CAPITAL LETTER BE
    63 |  00000426 | CYRILLIC CAPITAL LETTER TSE
    64 |  00000414 | CYRILLIC CAPITAL LETTER DE
    65 |  00000415 | CYRILLIC CAPITAL LETTER IE
    66 |  00000424 | CYRILLIC CAPITAL LETTER EF
    67 |  00000413 | CYRILLIC CAPITAL LETTER GHE
    68 |  00000425 | CYRILLIC CAPITAL LETTER HA
    69 |  00000418 | CYRILLIC CAPITAL LETTER I
    6A |  00000419 | CYRILLIC CAPITAL LETTER SHORT I
    6B |  0000041A | CYRILLIC CAPITAL LETTER KA
    6C |  0000041B | CYRILLIC CAPITAL LETTER EL
    6D |  0000041C | CYRILLIC CAPITAL LETTER EM
    6E |  0000041D | CYRILLIC CAPITAL LETTER EN
    6F |  0000041E | CYRILLIC CAPITAL LETTER O
    70 |  0000041F | CYRILLIC CAPITAL LETTER PE
    71 |  0000042F | CYRILLIC CAPITAL LETTER YA
    72 |  00000420 | CYRILLIC CAPITAL LETTER ER
    73 |  00000421 | CYRILLIC CAPITAL LETTER ES
    74 |  00000422 | CYRILLIC CAPITAL LETTER TE
    75 |  00000423 | CYRILLIC CAPITAL LETTER U
    76 |  00000416 | CYRILLIC CAPITAL LETTER ZHE
    77 |  00000412 | CYRILLIC CAPITAL LETTER VE
    78 |  0000042C | CYRILLIC CAPITAL LETTER SOFT SIGN
    79 |  0000042B | CYRILLIC CAPITAL LETTER YERU
    7A |  00000417 | CYRILLIC CAPITAL LETTER ZE
    7B |  00000428 | CYRILLIC CAPITAL LETTER SHA
    7C |  0000042D | CYRILLIC CAPITAL LETTER E
    7D |  00000429 | CYRILLIC CAPITAL LETTER SHCHA
    7E |  00000427 | CYRILLIC CAPITAL LETTER CHE
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
