#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM290.pm,v 1.1 2011-10-12 23:51:37 pertusus Exp $

# Conversion routines for IBM290.
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

package Locale::RecodeData::IBM290;

use strict;

require Locale::RecodeData;
use base qw(Locale::RecodeData);

my @to_ucs4 = (
    0x0000,
    0x0001,
    0x0002,
    0x0003,
    0x009c,
    0x0009,
    0x0086,
    0x007f,
    0x0097,
    0x008d,
    0x008e,
    0x000b,
    0x000c,
    0x000d,
    0x000e,
    0x000f,
    0x0010,
    0x0011,
    0x0012,
    0x0013,
    0x009d,
    0x0085,
    0x0008,
    0x0087,
    0x0018,
    0x0019,
    0x0092,
    0x008f,
    0x001c,
    0x001d,
    0x001e,
    0x001f,
    0x0080,
    0x0081,
    0x0082,
    0x0083,
    0x0084,
    0x000a,
    0x0017,
    0x001b,
    0x0088,
    0x0089,
    0x008a,
    0x008b,
    0x008c,
    0x0005,
    0x0006,
    0x0007,
    0x0090,
    0x0091,
    0x0016,
    0x0093,
    0x0094,
    0x0095,
    0x0096,
    0x0004,
    0x0098,
    0x0099,
    0x009a,
    0x009b,
    0x0014,
    0x0015,
    0x009e,
    0x001a,
    0x0020,
    0x3002,
    0x300c,
    0x300d,
    0x3001,
    0x30fb,
    0x30f2,
    0x30a1,
    0x30a3,
    0x30a5,
    0x00a3,
    0x002e,
    0x003c,
    0x0028,
    0x002b,
    0x007c,
    0x0026,
    0x30a7,
    0x30a9,
    0x30e3,
    0x30e5,
    0x30e7,
    0x30c3,
    0xfffd,
    0x30fc,
    0xfffd,
    0x0021,
    0x00a5,
    0x002a,
    0x0029,
    0x003b,
    0x00ac,
    0x002d,
    0x002f,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x00a6,
    0x002c,
    0x0025,
    0x005f,
    0x003e,
    0x003f,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x0060,
    0x003a,
    0x0023,
    0x0040,
    0x0027,
    0x003d,
    0x0022,
    0xfffd,
    0x30a2,
    0x30a4,
    0x30a6,
    0x30a8,
    0x30aa,
    0x30ab,
    0x30ad,
    0x30af,
    0x30b1,
    0x30b3,
    0xfffd,
    0x30b5,
    0x30b7,
    0x30b9,
    0x30bb,
    0x30bd,
    0x30bf,
    0x30c1,
    0x30c4,
    0x30c6,
    0x30c8,
    0x30ca,
    0x30cb,
    0x30cc,
    0x30cd,
    0x30ce,
    0xfffd,
    0xfffd,
    0x30cf,
    0x30d2,
    0x30d5,
    0xfffd,
    0x203e,
    0x30d8,
    0x30db,
    0x30de,
    0x30df,
    0x30e0,
    0x30e1,
    0x30e2,
    0x30e4,
    0x30e6,
    0xfffd,
    0x30e8,
    0x30e9,
    0x30ea,
    0x30eb,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x30ec,
    0x30ed,
    0x30ef,
    0x30f3,
    0x309b,
    0x309c,
    0xfffd,
    0x0041,
    0x0042,
    0x0043,
    0x0044,
    0x0045,
    0x0046,
    0x0047,
    0x0048,
    0x0049,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x004a,
    0x004b,
    0x004c,
    0x004d,
    0x004e,
    0x004f,
    0x0050,
    0x0051,
    0x0052,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x0024,
    0xfffd,
    0x0053,
    0x0054,
    0x0055,
    0x0056,
    0x0057,
    0x0058,
    0x0059,
    0x005a,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
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
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x009f,
);

my @to_utf8 = (
    "\x00",
    "\x01",
    "\x02",
    "\x03",
    "\xc2\x9c",
    "\x09",
    "\xc2\x86",
    "\x7f",
    "\xc2\x97",
    "\xc2\x8d",
    "\xc2\x8e",
    "\x0b",
    "\x0c",
    "\x0d",
    "\x0e",
    "\x0f",
    "\x10",
    "\x11",
    "\x12",
    "\x13",
    "\xc2\x9d",
    "\xc2\x85",
    "\x08",
    "\xc2\x87",
    "\x18",
    "\x19",
    "\xc2\x92",
    "\xc2\x8f",
    "\x1c",
    "\x1d",
    "\x1e",
    "\x1f",
    "\xc2\x80",
    "\xc2\x81",
    "\xc2\x82",
    "\xc2\x83",
    "\xc2\x84",
    "\x0a",
    "\x17",
    "\x1b",
    "\xc2\x88",
    "\xc2\x89",
    "\xc2\x8a",
    "\xc2\x8b",
    "\xc2\x8c",
    "\x05",
    "\x06",
    "\x07",
    "\xc2\x90",
    "\xc2\x91",
    "\x16",
    "\xc2\x93",
    "\xc2\x94",
    "\xc2\x95",
    "\xc2\x96",
    "\x04",
    "\xc2\x98",
    "\xc2\x99",
    "\xc2\x9a",
    "\xc2\x9b",
    "\x14",
    "\x15",
    "\xc2\x9e",
    "\x1a",
    "\x20",
    "\xe3\x80\x82",
    "\xe3\x80\x8c",
    "\xe3\x80\x8d",
    "\xe3\x80\x81",
    "\xe3\x83\xbb",
    "\xe3\x83\xb2",
    "\xe3\x82\xa1",
    "\xe3\x82\xa3",
    "\xe3\x82\xa5",
    "\xc2\xa3",
    "\x2e",
    "\x3c",
    "\x28",
    "\x2b",
    "\x7c",
    "\x26",
    "\xe3\x82\xa7",
    "\xe3\x82\xa9",
    "\xe3\x83\xa3",
    "\xe3\x83\xa5",
    "\xe3\x83\xa7",
    "\xe3\x83\x83",
    "\xef\xbf\xbd",
    "\xe3\x83\xbc",
    "\xef\xbf\xbd",
    "\x21",
    "\xc2\xa5",
    "\x2a",
    "\x29",
    "\x3b",
    "\xc2\xac",
    "\x2d",
    "\x2f",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xc2\xa6",
    "\x2c",
    "\x25",
    "\x5f",
    "\x3e",
    "\x3f",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\x60",
    "\x3a",
    "\x23",
    "\x40",
    "\x27",
    "\x3d",
    "\x22",
    "\xef\xbf\xbd",
    "\xe3\x82\xa2",
    "\xe3\x82\xa4",
    "\xe3\x82\xa6",
    "\xe3\x82\xa8",
    "\xe3\x82\xaa",
    "\xe3\x82\xab",
    "\xe3\x82\xad",
    "\xe3\x82\xaf",
    "\xe3\x82\xb1",
    "\xe3\x82\xb3",
    "\xef\xbf\xbd",
    "\xe3\x82\xb5",
    "\xe3\x82\xb7",
    "\xe3\x82\xb9",
    "\xe3\x82\xbb",
    "\xe3\x82\xbd",
    "\xe3\x82\xbf",
    "\xe3\x83\x81",
    "\xe3\x83\x84",
    "\xe3\x83\x86",
    "\xe3\x83\x88",
    "\xe3\x83\x8a",
    "\xe3\x83\x8b",
    "\xe3\x83\x8c",
    "\xe3\x83\x8d",
    "\xe3\x83\x8e",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xe3\x83\x8f",
    "\xe3\x83\x92",
    "\xe3\x83\x95",
    "\xef\xbf\xbd",
    "\xe2\x80\xbe",
    "\xe3\x83\x98",
    "\xe3\x83\x9b",
    "\xe3\x83\x9e",
    "\xe3\x83\x9f",
    "\xe3\x83\xa0",
    "\xe3\x83\xa1",
    "\xe3\x83\xa2",
    "\xe3\x83\xa4",
    "\xe3\x83\xa6",
    "\xef\xbf\xbd",
    "\xe3\x83\xa8",
    "\xe3\x83\xa9",
    "\xe3\x83\xaa",
    "\xe3\x83\xab",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xe3\x83\xac",
    "\xe3\x83\xad",
    "\xe3\x83\xaf",
    "\xe3\x83\xb3",
    "\xe3\x82\x9b",
    "\xe3\x82\x9c",
    "\xef\xbf\xbd",
    "\x41",
    "\x42",
    "\x43",
    "\x44",
    "\x45",
    "\x46",
    "\x47",
    "\x48",
    "\x49",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\x4a",
    "\x4b",
    "\x4c",
    "\x4d",
    "\x4e",
    "\x4f",
    "\x50",
    "\x51",
    "\x52",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\x24",
    "\xef\xbf\xbd",
    "\x53",
    "\x54",
    "\x55",
    "\x56",
    "\x57",
    "\x58",
    "\x59",
    "\x5a",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
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
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xc2\x9f",
);

my %from_ucs4 = (
    0x00000000 => "\x00",
    0x00000001 => "\x01",
    0x00000002 => "\x02",
    0x00000003 => "\x03",
    0x00000004 => "\x37",
    0x00000005 => "\x2d",
    0x00000006 => "\x2e",
    0x00000007 => "\x2f",
    0x00000008 => "\x16",
    0x00000009 => "\x05",
    0x0000000a => "\x25",
    0x0000000b => "\x0b",
    0x0000000c => "\x0c",
    0x0000000d => "\x0d",
    0x0000000e => "\x0e",
    0x0000000f => "\x0f",
    0x00000010 => "\x10",
    0x00000011 => "\x11",
    0x00000012 => "\x12",
    0x00000013 => "\x13",
    0x00000014 => "\x3c",
    0x00000015 => "\x3d",
    0x00000016 => "\x32",
    0x00000017 => "\x26",
    0x00000018 => "\x18",
    0x00000019 => "\x19",
    0x0000001a => "\x3f",
    0x0000001b => "\x27",
    0x0000001c => "\x1c",
    0x0000001d => "\x1d",
    0x0000001e => "\x1e",
    0x0000001f => "\x1f",
    0x00000020 => "\x40",
    0x00000021 => "\x5a",
    0x00000022 => "\x7f",
    0x00000023 => "\x7b",
    0x00000024 => "\xe0",
    0x00000025 => "\x6c",
    0x00000026 => "\x50",
    0x00000027 => "\x7d",
    0x00000028 => "\x4d",
    0x00000029 => "\x5d",
    0x0000002a => "\x5c",
    0x0000002b => "\x4e",
    0x0000002c => "\x6b",
    0x0000002d => "\x60",
    0x0000002e => "\x4b",
    0x0000002f => "\x61",
    0x00000030 => "\xf0",
    0x00000031 => "\xf1",
    0x00000032 => "\xf2",
    0x00000033 => "\xf3",
    0x00000034 => "\xf4",
    0x00000035 => "\xf5",
    0x00000036 => "\xf6",
    0x00000037 => "\xf7",
    0x00000038 => "\xf8",
    0x00000039 => "\xf9",
    0x0000003a => "\x7a",
    0x0000003b => "\x5e",
    0x0000003c => "\x4c",
    0x0000003d => "\x7e",
    0x0000003e => "\x6e",
    0x0000003f => "\x6f",
    0x00000040 => "\x7c",
    0x00000041 => "\xc1",
    0x00000042 => "\xc2",
    0x00000043 => "\xc3",
    0x00000044 => "\xc4",
    0x00000045 => "\xc5",
    0x00000046 => "\xc6",
    0x00000047 => "\xc7",
    0x00000048 => "\xc8",
    0x00000049 => "\xc9",
    0x0000004a => "\xd1",
    0x0000004b => "\xd2",
    0x0000004c => "\xd3",
    0x0000004d => "\xd4",
    0x0000004e => "\xd5",
    0x0000004f => "\xd6",
    0x00000050 => "\xd7",
    0x00000051 => "\xd8",
    0x00000052 => "\xd9",
    0x00000053 => "\xe2",
    0x00000054 => "\xe3",
    0x00000055 => "\xe4",
    0x00000056 => "\xe5",
    0x00000057 => "\xe6",
    0x00000058 => "\xe7",
    0x00000059 => "\xe8",
    0x0000005a => "\xe9",
    0x0000005f => "\x6d",
    0x00000060 => "\x79",
    0x0000007c => "\x4f",
    0x0000007f => "\x07",
    0x00000080 => "\x20",
    0x00000081 => "\x21",
    0x00000082 => "\x22",
    0x00000083 => "\x23",
    0x00000084 => "\x24",
    0x00000085 => "\x15",
    0x00000086 => "\x06",
    0x00000087 => "\x17",
    0x00000088 => "\x28",
    0x00000089 => "\x29",
    0x0000008a => "\x2a",
    0x0000008b => "\x2b",
    0x0000008c => "\x2c",
    0x0000008d => "\x09",
    0x0000008e => "\x0a",
    0x0000008f => "\x1b",
    0x00000090 => "\x30",
    0x00000091 => "\x31",
    0x00000092 => "\x1a",
    0x00000093 => "\x33",
    0x00000094 => "\x34",
    0x00000095 => "\x35",
    0x00000096 => "\x36",
    0x00000097 => "\x08",
    0x00000098 => "\x38",
    0x00000099 => "\x39",
    0x0000009a => "\x3a",
    0x0000009b => "\x3b",
    0x0000009c => "\x04",
    0x0000009d => "\x14",
    0x0000009e => "\x3e",
    0x0000009f => "\xff",
    0x000000a3 => "\x4a",
    0x000000a5 => "\x5b",
    0x000000a6 => "\x6a",
    0x000000ac => "\x5f",
    0x0000203e => "\xa1",
    0x00003001 => "\x44",
    0x00003002 => "\x41",
    0x0000300c => "\x42",
    0x0000300d => "\x43",
    0x0000309b => "\xbe",
    0x0000309c => "\xbf",
    0x000030a1 => "\x47",
    0x000030a2 => "\x81",
    0x000030a3 => "\x48",
    0x000030a4 => "\x82",
    0x000030a5 => "\x49",
    0x000030a6 => "\x83",
    0x000030a7 => "\x51",
    0x000030a8 => "\x84",
    0x000030a9 => "\x52",
    0x000030aa => "\x85",
    0x000030ab => "\x86",
    0x000030ad => "\x87",
    0x000030af => "\x88",
    0x000030b1 => "\x89",
    0x000030b3 => "\x8a",
    0x000030b5 => "\x8c",
    0x000030b7 => "\x8d",
    0x000030b9 => "\x8e",
    0x000030bb => "\x8f",
    0x000030bd => "\x90",
    0x000030bf => "\x91",
    0x000030c1 => "\x92",
    0x000030c3 => "\x56",
    0x000030c4 => "\x93",
    0x000030c6 => "\x94",
    0x000030c8 => "\x95",
    0x000030ca => "\x96",
    0x000030cb => "\x97",
    0x000030cc => "\x98",
    0x000030cd => "\x99",
    0x000030ce => "\x9a",
    0x000030cf => "\x9d",
    0x000030d2 => "\x9e",
    0x000030d5 => "\x9f",
    0x000030d8 => "\xa2",
    0x000030db => "\xa3",
    0x000030de => "\xa4",
    0x000030df => "\xa5",
    0x000030e0 => "\xa6",
    0x000030e1 => "\xa7",
    0x000030e2 => "\xa8",
    0x000030e3 => "\x53",
    0x000030e4 => "\xa9",
    0x000030e5 => "\x54",
    0x000030e6 => "\xaa",
    0x000030e7 => "\x55",
    0x000030e8 => "\xac",
    0x000030e9 => "\xad",
    0x000030ea => "\xae",
    0x000030eb => "\xaf",
    0x000030ec => "\xba",
    0x000030ed => "\xbb",
    0x000030ef => "\xbc",
    0x000030f2 => "\x46",
    0x000030f3 => "\xbd",
    0x000030fb => "\x45",
    0x000030fc => "\x58",
);

sub _recode
{
    if ($_[0]->{_from} eq 'INTERNAL') {
		$_[1] = join '',
	        map $from_ucs4{$_} 
                || (defined $from_ucs4{$_} ? $from_ucs4{$_} : "\x6f"),
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

Locale::RecodeData::IBM290 - Conversion routines for IBM290

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM290.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990
 alias CP290
 alias EBCDIC-JP-KANA

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
    04 |  0000009C | STRING TERMINATOR (ST)
    05 |  00000009 | CHARACTER TABULATION (HT)
    06 |  00000086 | START OF SELECTED AREA (SSA)
    07 |  0000007F | DELETE (DEL)
    08 |  00000097 | END OF GUARDED AREA (EPA)
    09 |  0000008D | REVERSE LINE FEED (RI)
    0A |  0000008E | SINGLE-SHIFT TWO (SS2)
    0B |  0000000B | LINE TABULATION (VT)
    0C |  0000000C | FORM FEED (FF)
    0D |  0000000D | CARRIAGE RETURN (CR)
    0E |  0000000E | SHIFT OUT (SO)
    0F |  0000000F | SHIFT IN (SI)
    10 |  00000010 | DATALINK ESCAPE (DLE)
    11 |  00000011 | DEVICE CONTROL ONE (DC1)
    12 |  00000012 | DEVICE CONTROL TWO (DC2)
    13 |  00000013 | DEVICE CONTROL THREE (DC3)
    14 |  0000009D | OPERATING SYSTEM COMMAND (OSC)
    15 |  00000085 | NEXT LINE (NEL)
    16 |  00000008 | BACKSPACE (BS)
    17 |  00000087 | END OF SELECTED AREA (ESA)
    18 |  00000018 | CANCEL (CAN)
    19 |  00000019 | END OF MEDIUM (EM)
    1A |  00000092 | PRIVATE USE TWO (PU2)
    1B |  0000008F | SINGLE-SHIFT THREE (SS3)
    1C |  0000001C | FILE SEPARATOR (IS4)
    1D |  0000001D | GROUP SEPARATOR (IS3)
    1E |  0000001E | RECORD SEPARATOR (IS2)
    1F |  0000001F | UNIT SEPARATOR (IS1)
    20 |  00000080 | PADDING CHARACTER (PAD)
    21 |  00000081 | HIGH OCTET PRESET (HOP)
    22 |  00000082 | BREAK PERMITTED HERE (BPH)
    23 |  00000083 | NO BREAK HERE (NBH)
    24 |  00000084 | INDEX (IND)
    25 |  0000000A | LINE FEED (LF)
    26 |  00000017 | END OF TRANSMISSION BLOCK (ETB)
    27 |  0000001B | ESCAPE (ESC)
    28 |  00000088 | CHARACTER TABULATION SET (HTS)
    29 |  00000089 | CHARACTER TABULATION WITH JUSTIFICATION (HTJ)
    2A |  0000008A | LINE TABULATION SET (VTS)
    2B |  0000008B | PARTIAL LINE FORWARD (PLD)
    2C |  0000008C | PARTIAL LINE BACKWARD (PLU)
    2D |  00000005 | ENQUIRY (ENQ)
    2E |  00000006 | ACKNOWLEDGE (ACK)
    2F |  00000007 | BELL (BEL)
    30 |  00000090 | DEVICE CONTROL STRING (DCS)
    31 |  00000091 | PRIVATE USE ONE (PU1)
    32 |  00000016 | SYNCHRONOUS IDLE (SYN)
    33 |  00000093 | SET TRANSMIT STATE (STS)
    34 |  00000094 | CANCEL CHARACTER (CCH)
    35 |  00000095 | MESSAGE WAITING (MW)
    36 |  00000096 | START OF GUARDED AREA (SPA)
    37 |  00000004 | END OF TRANSMISSION (EOT)
    38 |  00000098 | START OF STRING (SOS)
    39 |  00000099 | SINGLE GRAPHIC CHARACTER INTRODUCER (SGCI)
    3A |  0000009A | SINGLE CHARACTER INTRODUCER (SCI)
    3B |  0000009B | CONTROL SEQUENCE INTRODUCER (CSI)
    3C |  00000014 | DEVICE CONTROL FOUR (DC4)
    3D |  00000015 | NEGATIVE ACKNOWLEDGE (NAK)
    3E |  0000009E | PRIVACY MESSAGE (PM)
    3F |  0000001A | SUBSTITUTE (SUB)
    40 |  00000020 | SPACE
    41 |  00003002 | IDEOGRAPHIC FULL STOP
    42 |  0000300C | LEFT CORNER BRACKET
    43 |  0000300D | RIGHT CORNER BRACKET
    44 |  00003001 | IDEOGRAPHIC COMMA
    45 |  000030FB | KATAKANA MIDDLE DOT
    46 |  000030F2 | KATAKANA LETTER WO
    47 |  000030A1 | KATAKANA LETTER SMALL A
    48 |  000030A3 | KATAKANA LETTER SMALL I
    49 |  000030A5 | KATAKANA LETTER SMALL U
    4A |  000000A3 | POUND SIGN
    4B |  0000002E | FULL STOP
    4C |  0000003C | LESS-THAN SIGN
    4D |  00000028 | LEFT PARENTHESIS
    4E |  0000002B | PLUS SIGN
    4F |  0000007C | VERTICAL LINE
    50 |  00000026 | AMPERSAND
    51 |  000030A7 | KATAKANA LETTER SMALL E
    52 |  000030A9 | KATAKANA LETTER SMALL O
    53 |  000030E3 | KATAKANA LETTER SMALL YA
    54 |  000030E5 | KATAKANA LETTER SMALL YU
    55 |  000030E7 | KATAKANA LETTER SMALL YO
    56 |  000030C3 | KATAKANA LETTER SMALL TU
    58 |  000030FC | KATAKANA-HIRAGANA PROLONGED SOUND MARK
    5A |  00000021 | EXCLAMATION MARK
    5B |  000000A5 | YEN SIGN
    5C |  0000002A | ASTERISK
    5D |  00000029 | RIGHT PARENTHESIS
    5E |  0000003B | SEMICOLON
    5F |  000000AC | NOT SIGN
    60 |  0000002D | HYPHEN-MINUS
    61 |  0000002F | SOLIDUS
    6A |  000000A6 | BROKEN BAR
    6B |  0000002C | COMMA
    6C |  00000025 | PERCENT SIGN
    6D |  0000005F | LOW LINE
    6E |  0000003E | GREATER-THAN SIGN
    6F |  0000003F | QUESTION MARK
    79 |  00000060 | GRAVE ACCENT
    7A |  0000003A | COLON
    7B |  00000023 | NUMBER SIGN
    7C |  00000040 | COMMERCIAL AT
    7D |  00000027 | APOSTROPHE
    7E |  0000003D | EQUALS SIGN
    7F |  00000022 | QUOTATION MARK
    81 |  000030A2 | KATAKANA LETTER A
    82 |  000030A4 | KATAKANA LETTER I
    83 |  000030A6 | KATAKANA LETTER U
    84 |  000030A8 | KATAKANA LETTER E
    85 |  000030AA | KATAKANA LETTER O
    86 |  000030AB | KATAKANA LETTER KA
    87 |  000030AD | KATAKANA LETTER KI
    88 |  000030AF | KATAKANA LETTER KU
    89 |  000030B1 | KATAKANA LETTER KE
    8A |  000030B3 | KATAKANA LETTER KO
    8C |  000030B5 | KATAKANA LETTER SA
    8D |  000030B7 | KATAKANA LETTER SI
    8E |  000030B9 | KATAKANA LETTER SU
    8F |  000030BB | KATAKANA LETTER SE
    90 |  000030BD | KATAKANA LETTER SO
    91 |  000030BF | KATAKANA LETTER TA
    92 |  000030C1 | KATAKANA LETTER TI
    93 |  000030C4 | KATAKANA LETTER TU
    94 |  000030C6 | KATAKANA LETTER TE
    95 |  000030C8 | KATAKANA LETTER TO
    96 |  000030CA | KATAKANA LETTER NA
    97 |  000030CB | KATAKANA LETTER NI
    98 |  000030CC | KATAKANA LETTER NU
    99 |  000030CD | KATAKANA LETTER NE
    9A |  000030CE | KATAKANA LETTER NO
    9D |  000030CF | KATAKANA LETTER HA
    9E |  000030D2 | KATAKANA LETTER HI
    9F |  000030D5 | KATAKANA LETTER HU
    A1 |  0000203E | OVERLINE
    A2 |  000030D8 | KATAKANA LETTER HE
    A3 |  000030DB | KATAKANA LETTER HO
    A4 |  000030DE | KATAKANA LETTER MA
    A5 |  000030DF | KATAKANA LETTER MI
    A6 |  000030E0 | KATAKANA LETTER MU
    A7 |  000030E1 | KATAKANA LETTER ME
    A8 |  000030E2 | KATAKANA LETTER MO
    A9 |  000030E4 | KATAKANA LETTER YA
    AA |  000030E6 | KATAKANA LETTER YU
    AC |  000030E8 | KATAKANA LETTER YO
    AD |  000030E9 | KATAKANA LETTER RA
    AE |  000030EA | KATAKANA LETTER RI
    AF |  000030EB | KATAKANA LETTER RU
    BA |  000030EC | KATAKANA LETTER RE
    BB |  000030ED | KATAKANA LETTER RO
    BC |  000030EF | KATAKANA LETTER WA
    BD |  000030F3 | KATAKANA LETTER N
    BE |  0000309B | KATAKANA-HIRAGANA VOICED SOUND MARK
    BF |  0000309C | KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
    C1 |  00000041 | LATIN CAPITAL LETTER A
    C2 |  00000042 | LATIN CAPITAL LETTER B
    C3 |  00000043 | LATIN CAPITAL LETTER C
    C4 |  00000044 | LATIN CAPITAL LETTER D
    C5 |  00000045 | LATIN CAPITAL LETTER E
    C6 |  00000046 | LATIN CAPITAL LETTER F
    C7 |  00000047 | LATIN CAPITAL LETTER G
    C8 |  00000048 | LATIN CAPITAL LETTER H
    C9 |  00000049 | LATIN CAPITAL LETTER I
    D1 |  0000004A | LATIN CAPITAL LETTER J
    D2 |  0000004B | LATIN CAPITAL LETTER K
    D3 |  0000004C | LATIN CAPITAL LETTER L
    D4 |  0000004D | LATIN CAPITAL LETTER M
    D5 |  0000004E | LATIN CAPITAL LETTER N
    D6 |  0000004F | LATIN CAPITAL LETTER O
    D7 |  00000050 | LATIN CAPITAL LETTER P
    D8 |  00000051 | LATIN CAPITAL LETTER Q
    D9 |  00000052 | LATIN CAPITAL LETTER R
    E0 |  00000024 | DOLLAR SIGN
    E2 |  00000053 | LATIN CAPITAL LETTER S
    E3 |  00000054 | LATIN CAPITAL LETTER T
    E4 |  00000055 | LATIN CAPITAL LETTER U
    E5 |  00000056 | LATIN CAPITAL LETTER V
    E6 |  00000057 | LATIN CAPITAL LETTER W
    E7 |  00000058 | LATIN CAPITAL LETTER X
    E8 |  00000059 | LATIN CAPITAL LETTER Y
    E9 |  0000005A | LATIN CAPITAL LETTER Z
    F0 |  00000030 | DIGIT ZERO
    F1 |  00000031 | DIGIT ONE
    F2 |  00000032 | DIGIT TWO
    F3 |  00000033 | DIGIT THREE
    F4 |  00000034 | DIGIT FOUR
    F5 |  00000035 | DIGIT FIVE
    F6 |  00000036 | DIGIT SIX
    F7 |  00000037 | DIGIT SEVEN
    F8 |  00000038 | DIGIT EIGHT
    F9 |  00000039 | DIGIT NINE
    FF |  0000009F | APPLICATION PROGRAM COMMAND (APC)


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
