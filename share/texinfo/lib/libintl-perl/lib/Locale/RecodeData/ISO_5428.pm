#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: ISO_5428.pm,v 1.1 2011-10-12 23:51:42 pertusus Exp $

# Conversion routines for ISO_5428.
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

package Locale::RecodeData::ISO_5428;

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
    0xe002,
    0xe003,
    0xe009,
    0xe005,
    0xe012,
    0xe013,
    0xe014,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x00ab,
    0x00bb,
    0x201d,
    0x201c,
    0x0374,
    0x0375,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x00b7,
    0xfffd,
    0xfffd,
    0xfffd,
    0x003b,
    0xfffd,
    0x0391,
    0x0392,
    0xfffd,
    0x0393,
    0x0394,
    0x0395,
    0x03da,
    0x03dc,
    0x0396,
    0x0397,
    0x0398,
    0x0399,
    0x039a,
    0x039b,
    0x039c,
    0x039d,
    0x039e,
    0x039f,
    0x03a0,
    0x03de,
    0x03a1,
    0x03a3,
    0xfffd,
    0x03a4,
    0x03a5,
    0x03a6,
    0x03a7,
    0x03a8,
    0x03a9,
    0x03e0,
    0xfffd,
    0xfffd,
    0x03b1,
    0x03b2,
    0x03d0,
    0x03b3,
    0x03b4,
    0x03b5,
    0xe01a,
    0xe01b,
    0x03b6,
    0x03b7,
    0x03b8,
    0x03b9,
    0x03ba,
    0x03bb,
    0x03bc,
    0x03bd,
    0x03be,
    0x03bf,
    0x03c0,
    0xe01c,
    0x03c1,
    0x03c3,
    0x03c2,
    0x03c4,
    0x03c5,
    0x03c6,
    0x03c7,
    0x03c8,
    0x03c9,
    0xe01d,
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
    "\xee\x80\x82",
    "\xee\x80\x83",
    "\xee\x80\x89",
    "\xee\x80\x85",
    "\xee\x80\x92",
    "\xee\x80\x93",
    "\xee\x80\x94",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xc2\xab",
    "\xc2\xbb",
    "\xe2\x80\x9d",
    "\xe2\x80\x9c",
    "\xcd\xb4",
    "\xcd\xb5",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xc2\xb7",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\x3b",
    "\xef\xbf\xbd",
    "\xce\x91",
    "\xce\x92",
    "\xef\xbf\xbd",
    "\xce\x93",
    "\xce\x94",
    "\xce\x95",
    "\xcf\x9a",
    "\xcf\x9c",
    "\xce\x96",
    "\xce\x97",
    "\xce\x98",
    "\xce\x99",
    "\xce\x9a",
    "\xce\x9b",
    "\xce\x9c",
    "\xce\x9d",
    "\xce\x9e",
    "\xce\x9f",
    "\xce\xa0",
    "\xcf\x9e",
    "\xce\xa1",
    "\xce\xa3",
    "\xef\xbf\xbd",
    "\xce\xa4",
    "\xce\xa5",
    "\xce\xa6",
    "\xce\xa7",
    "\xce\xa8",
    "\xce\xa9",
    "\xcf\xa0",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xce\xb1",
    "\xce\xb2",
    "\xcf\x90",
    "\xce\xb3",
    "\xce\xb4",
    "\xce\xb5",
    "\xee\x80\x9a",
    "\xee\x80\x9b",
    "\xce\xb6",
    "\xce\xb7",
    "\xce\xb8",
    "\xce\xb9",
    "\xce\xba",
    "\xce\xbb",
    "\xce\xbc",
    "\xce\xbd",
    "\xce\xbe",
    "\xce\xbf",
    "\xcf\x80",
    "\xee\x80\x9c",
    "\xcf\x81",
    "\xcf\x83",
    "\xcf\x82",
    "\xcf\x84",
    "\xcf\x85",
    "\xcf\x86",
    "\xcf\x87",
    "\xcf\x88",
    "\xcf\x89",
    "\xee\x80\x9d",
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
    0x0000003b => "\x3f",
    0x0000007f => "\x7f",
    0x000000ab => "\x30",
    0x000000b7 => "\x3b",
    0x000000bb => "\x31",
    0x00000374 => "\x34",
    0x00000375 => "\x35",
    0x00000391 => "\x41",
    0x00000392 => "\x42",
    0x00000393 => "\x44",
    0x00000394 => "\x45",
    0x00000395 => "\x46",
    0x00000396 => "\x49",
    0x00000397 => "\x4a",
    0x00000398 => "\x4b",
    0x00000399 => "\x4c",
    0x0000039a => "\x4d",
    0x0000039b => "\x4e",
    0x0000039c => "\x4f",
    0x0000039d => "\x50",
    0x0000039e => "\x51",
    0x0000039f => "\x52",
    0x000003a0 => "\x53",
    0x000003a1 => "\x55",
    0x000003a3 => "\x56",
    0x000003a4 => "\x58",
    0x000003a5 => "\x59",
    0x000003a6 => "\x5a",
    0x000003a7 => "\x5b",
    0x000003a8 => "\x5c",
    0x000003a9 => "\x5d",
    0x000003b1 => "\x61",
    0x000003b2 => "\x62",
    0x000003b3 => "\x64",
    0x000003b4 => "\x65",
    0x000003b5 => "\x66",
    0x000003b6 => "\x69",
    0x000003b7 => "\x6a",
    0x000003b8 => "\x6b",
    0x000003b9 => "\x6c",
    0x000003ba => "\x6d",
    0x000003bb => "\x6e",
    0x000003bc => "\x6f",
    0x000003bd => "\x70",
    0x000003be => "\x71",
    0x000003bf => "\x72",
    0x000003c0 => "\x73",
    0x000003c1 => "\x75",
    0x000003c2 => "\x77",
    0x000003c3 => "\x76",
    0x000003c4 => "\x78",
    0x000003c5 => "\x79",
    0x000003c6 => "\x7a",
    0x000003c7 => "\x7b",
    0x000003c8 => "\x7c",
    0x000003c9 => "\x7d",
    0x000003d0 => "\x63",
    0x000003da => "\x47",
    0x000003dc => "\x48",
    0x000003de => "\x54",
    0x000003e0 => "\x5e",
    0x0000201c => "\x33",
    0x0000201d => "\x32",
    0x0000e002 => "\x21",
    0x0000e003 => "\x22",
    0x0000e005 => "\x24",
    0x0000e009 => "\x23",
    0x0000e012 => "\x25",
    0x0000e013 => "\x26",
    0x0000e014 => "\x27",
    0x0000e01a => "\x67",
    0x0000e01b => "\x68",
    0x0000e01c => "\x74",
    0x0000e01d => "\x7e",
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

Locale::RecodeData::ISO_5428 - Conversion routines for ISO_5428

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for ISO_5428.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: ECMA registry
 alias ISO-IR-55
 alias ISO_5428:1980

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
    21 |  0000E002 | NON-SPACING GRAVE ACCENT E<lt>ISO-IR-103_C1E<gt> (not a real character)
    22 |  0000E003 | NON-SPACING ACUTE ACCENT E<lt>ISO-IR-103_C2E<gt> (not a real character)
    23 |  0000E009 | NON-SPACING DIAERESIS E<lt>ISO-IR-103_C8E<gt> (not a real character)
    24 |  0000E005 | NON-SPACING TILDE E<lt>ISO-IR-103_C4E<gt> (not a real character)
    25 |  0000E012 | GREEK NON-SPACING PSILI PNEUMATA E<lt>ISO-IR-55_25E<gt> (not a real character)
    26 |  0000E013 | GREEK NON-SPACING DASIA PNEUMATA E<lt>ISO-IR-55_26E<gt> (not a real character)
    27 |  0000E014 | GREEK NON-SPACING IOTA BELOW E<lt>ISO-IR-55_27E<gt> (not a real character)
    30 |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    31 |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    32 |  0000201D | RIGHT DOUBLE QUOTATION MARK
    33 |  0000201C | LEFT DOUBLE QUOTATION MARK
    34 |  00000374 | GREEK NUMERAL SIGN (Dexia keraia)
    35 |  00000375 | GREEK LOWER NUMERAL SIGN (Aristeri keraia)
    3B |  000000B7 | MIDDLE DOT
    3F |  0000003B | SEMICOLON
    41 |  00000391 | GREEK CAPITAL LETTER ALPHA
    42 |  00000392 | GREEK CAPITAL LETTER BETA
    44 |  00000393 | GREEK CAPITAL LETTER GAMMA
    45 |  00000394 | GREEK CAPITAL LETTER DELTA
    46 |  00000395 | GREEK CAPITAL LETTER EPSILON
    47 |  000003DA | GREEK LETTER STIGMA
    48 |  000003DC | GREEK LETTER DIGAMMA
    49 |  00000396 | GREEK CAPITAL LETTER ZETA
    4A |  00000397 | GREEK CAPITAL LETTER ETA
    4B |  00000398 | GREEK CAPITAL LETTER THETA
    4C |  00000399 | GREEK CAPITAL LETTER IOTA
    4D |  0000039A | GREEK CAPITAL LETTER KAPPA
    4E |  0000039B | GREEK CAPITAL LETTER LAMDA
    4F |  0000039C | GREEK CAPITAL LETTER MU
    50 |  0000039D | GREEK CAPITAL LETTER NU
    51 |  0000039E | GREEK CAPITAL LETTER XI
    52 |  0000039F | GREEK CAPITAL LETTER OMICRON
    53 |  000003A0 | GREEK CAPITAL LETTER PI
    54 |  000003DE | GREEK LETTER KOPPA
    55 |  000003A1 | GREEK CAPITAL LETTER RHO
    56 |  000003A3 | GREEK CAPITAL LETTER SIGMA
    58 |  000003A4 | GREEK CAPITAL LETTER TAU
    59 |  000003A5 | GREEK CAPITAL LETTER UPSILON
    5A |  000003A6 | GREEK CAPITAL LETTER PHI
    5B |  000003A7 | GREEK CAPITAL LETTER CHI
    5C |  000003A8 | GREEK CAPITAL LETTER PSI
    5D |  000003A9 | GREEK CAPITAL LETTER OMEGA
    5E |  000003E0 | GREEK LETTER SAMPI
    61 |  000003B1 | GREEK SMALL LETTER ALPHA
    62 |  000003B2 | GREEK SMALL LETTER BETA
    63 |  000003D0 | GREEK BETA SYMBOL
    64 |  000003B3 | GREEK SMALL LETTER GAMMA
    65 |  000003B4 | GREEK SMALL LETTER DELTA
    66 |  000003B5 | GREEK SMALL LETTER EPSILON
    67 |  0000E01A | GREEK SMALL LETTER STIGMA E<lt>ISO-IR-55_47E<gt>
    68 |  0000E01B | GREEK SMALL LETTER DIGAMMA E<lt>ISO-IR-55_48E<gt>
    69 |  000003B6 | GREEK SMALL LETTER ZETA
    6A |  000003B7 | GREEK SMALL LETTER ETA
    6B |  000003B8 | GREEK SMALL LETTER THETA
    6C |  000003B9 | GREEK SMALL LETTER IOTA
    6D |  000003BA | GREEK SMALL LETTER KAPPA
    6E |  000003BB | GREEK SMALL LETTER LAMDA
    6F |  000003BC | GREEK SMALL LETTER MU
    70 |  000003BD | GREEK SMALL LETTER NU
    71 |  000003BE | GREEK SMALL LETTER XI
    72 |  000003BF | GREEK SMALL LETTER OMICRON
    73 |  000003C0 | GREEK SMALL LETTER PI
    74 |  0000E01C | GREEK SMALL LETTER KOPPA E<lt>ISO-IR-55_54E<gt>
    75 |  000003C1 | GREEK SMALL LETTER RHO
    76 |  000003C3 | GREEK SMALL LETTER SIGMA
    77 |  000003C2 | GREEK SMALL LETTER FINAL SIGMA
    78 |  000003C4 | GREEK SMALL LETTER TAU
    79 |  000003C5 | GREEK SMALL LETTER UPSILON
    7A |  000003C6 | GREEK SMALL LETTER PHI
    7B |  000003C7 | GREEK SMALL LETTER CHI
    7C |  000003C8 | GREEK SMALL LETTER PSI
    7D |  000003C9 | GREEK SMALL LETTER OMEGA
    7E |  0000E01D | GREEK SMALL LETTER SAMPI E<lt>ISO-IR-55_5EE<gt>
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
