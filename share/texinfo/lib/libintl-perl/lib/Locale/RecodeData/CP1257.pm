#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: CP1257.pm,v 1.1 2011-10-12 23:51:29 pertusus Exp $

# Conversion routines for CP1257.
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

package Locale::RecodeData::CP1257;

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
    0x0040,
    0x0041,
    0x0042,
    0x0043,
    0x0044,
    0x0045,
    0x0046,
    0x0047,
    0x0048,
    0x0049,
    0x004a,
    0x004b,
    0x004c,
    0x004d,
    0x004e,
    0x004f,
    0x0050,
    0x0051,
    0x0052,
    0x0053,
    0x0054,
    0x0055,
    0x0056,
    0x0057,
    0x0058,
    0x0059,
    0x005a,
    0x005b,
    0x005c,
    0x005d,
    0x005e,
    0x005f,
    0x0060,
    0x0061,
    0x0062,
    0x0063,
    0x0064,
    0x0065,
    0x0066,
    0x0067,
    0x0068,
    0x0069,
    0x006a,
    0x006b,
    0x006c,
    0x006d,
    0x006e,
    0x006f,
    0x0070,
    0x0071,
    0x0072,
    0x0073,
    0x0074,
    0x0075,
    0x0076,
    0x0077,
    0x0078,
    0x0079,
    0x007a,
    0x007b,
    0x007c,
    0x007d,
    0x007e,
    0x007f,
    0x20ac,
    0xfffd,
    0x201a,
    0xfffd,
    0x201e,
    0x2026,
    0x2020,
    0x2021,
    0xfffd,
    0x2030,
    0xfffd,
    0x2039,
    0xfffd,
    0x00a8,
    0x02c7,
    0x00b8,
    0xfffd,
    0x2018,
    0x2019,
    0x201c,
    0x201d,
    0x2022,
    0x2013,
    0x2014,
    0xfffd,
    0x2122,
    0xfffd,
    0x203a,
    0xfffd,
    0x00af,
    0x02db,
    0xfffd,
    0x00a0,
    0xfffd,
    0x00a2,
    0x00a3,
    0x00a4,
    0xfffd,
    0x00a6,
    0x00a7,
    0x00d8,
    0x00a9,
    0x0156,
    0x00ab,
    0x00ac,
    0x00ad,
    0x00ae,
    0x00c6,
    0x00b0,
    0x00b1,
    0x00b2,
    0x00b3,
    0x00b4,
    0x00b5,
    0x00b6,
    0x00b7,
    0x00f8,
    0x00b9,
    0x0157,
    0x00bb,
    0x00bc,
    0x00bd,
    0x00be,
    0x00e6,
    0x0104,
    0x012e,
    0x0100,
    0x0106,
    0x00c4,
    0x00c5,
    0x0118,
    0x0112,
    0x010c,
    0x00c9,
    0x0179,
    0x0116,
    0x0122,
    0x0136,
    0x012a,
    0x013b,
    0x0160,
    0x0143,
    0x0145,
    0x00d3,
    0x014c,
    0x00d5,
    0x00d6,
    0x00d7,
    0x0172,
    0x0141,
    0x015a,
    0x016a,
    0x00dc,
    0x017b,
    0x017d,
    0x00df,
    0x0105,
    0x012f,
    0x0101,
    0x0107,
    0x00e4,
    0x00e5,
    0x0119,
    0x0113,
    0x010d,
    0x00e9,
    0x017a,
    0x0117,
    0x0123,
    0x0137,
    0x012b,
    0x013c,
    0x0161,
    0x0144,
    0x0146,
    0x00f3,
    0x014d,
    0x00f5,
    0x00f6,
    0x00f7,
    0x0173,
    0x0142,
    0x015b,
    0x016b,
    0x00fc,
    0x017c,
    0x017e,
    0x02d9,
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
    "\xe2\x82\xac",
    "\xef\xbf\xbd",
    "\xe2\x80\x9a",
    "\xef\xbf\xbd",
    "\xe2\x80\x9e",
    "\xe2\x80\xa6",
    "\xe2\x80\xa0",
    "\xe2\x80\xa1",
    "\xef\xbf\xbd",
    "\xe2\x80\xb0",
    "\xef\xbf\xbd",
    "\xe2\x80\xb9",
    "\xef\xbf\xbd",
    "\xc2\xa8",
    "\xcb\x87",
    "\xc2\xb8",
    "\xef\xbf\xbd",
    "\xe2\x80\x98",
    "\xe2\x80\x99",
    "\xe2\x80\x9c",
    "\xe2\x80\x9d",
    "\xe2\x80\xa2",
    "\xe2\x80\x93",
    "\xe2\x80\x94",
    "\xef\xbf\xbd",
    "\xe2\x84\xa2",
    "\xef\xbf\xbd",
    "\xe2\x80\xba",
    "\xef\xbf\xbd",
    "\xc2\xaf",
    "\xcb\x9b",
    "\xef\xbf\xbd",
    "\xc2\xa0",
    "\xef\xbf\xbd",
    "\xc2\xa2",
    "\xc2\xa3",
    "\xc2\xa4",
    "\xef\xbf\xbd",
    "\xc2\xa6",
    "\xc2\xa7",
    "\xc3\x98",
    "\xc2\xa9",
    "\xc5\x96",
    "\xc2\xab",
    "\xc2\xac",
    "\xc2\xad",
    "\xc2\xae",
    "\xc3\x86",
    "\xc2\xb0",
    "\xc2\xb1",
    "\xc2\xb2",
    "\xc2\xb3",
    "\xc2\xb4",
    "\xc2\xb5",
    "\xc2\xb6",
    "\xc2\xb7",
    "\xc3\xb8",
    "\xc2\xb9",
    "\xc5\x97",
    "\xc2\xbb",
    "\xc2\xbc",
    "\xc2\xbd",
    "\xc2\xbe",
    "\xc3\xa6",
    "\xc4\x84",
    "\xc4\xae",
    "\xc4\x80",
    "\xc4\x86",
    "\xc3\x84",
    "\xc3\x85",
    "\xc4\x98",
    "\xc4\x92",
    "\xc4\x8c",
    "\xc3\x89",
    "\xc5\xb9",
    "\xc4\x96",
    "\xc4\xa2",
    "\xc4\xb6",
    "\xc4\xaa",
    "\xc4\xbb",
    "\xc5\xa0",
    "\xc5\x83",
    "\xc5\x85",
    "\xc3\x93",
    "\xc5\x8c",
    "\xc3\x95",
    "\xc3\x96",
    "\xc3\x97",
    "\xc5\xb2",
    "\xc5\x81",
    "\xc5\x9a",
    "\xc5\xaa",
    "\xc3\x9c",
    "\xc5\xbb",
    "\xc5\xbd",
    "\xc3\x9f",
    "\xc4\x85",
    "\xc4\xaf",
    "\xc4\x81",
    "\xc4\x87",
    "\xc3\xa4",
    "\xc3\xa5",
    "\xc4\x99",
    "\xc4\x93",
    "\xc4\x8d",
    "\xc3\xa9",
    "\xc5\xba",
    "\xc4\x97",
    "\xc4\xa3",
    "\xc4\xb7",
    "\xc4\xab",
    "\xc4\xbc",
    "\xc5\xa1",
    "\xc5\x84",
    "\xc5\x86",
    "\xc3\xb3",
    "\xc5\x8d",
    "\xc3\xb5",
    "\xc3\xb6",
    "\xc3\xb7",
    "\xc5\xb3",
    "\xc5\x82",
    "\xc5\x9b",
    "\xc5\xab",
    "\xc3\xbc",
    "\xc5\xbc",
    "\xc5\xbe",
    "\xcb\x99",
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
    0x00000040 => "\x40",
    0x00000041 => "\x41",
    0x00000042 => "\x42",
    0x00000043 => "\x43",
    0x00000044 => "\x44",
    0x00000045 => "\x45",
    0x00000046 => "\x46",
    0x00000047 => "\x47",
    0x00000048 => "\x48",
    0x00000049 => "\x49",
    0x0000004a => "\x4a",
    0x0000004b => "\x4b",
    0x0000004c => "\x4c",
    0x0000004d => "\x4d",
    0x0000004e => "\x4e",
    0x0000004f => "\x4f",
    0x00000050 => "\x50",
    0x00000051 => "\x51",
    0x00000052 => "\x52",
    0x00000053 => "\x53",
    0x00000054 => "\x54",
    0x00000055 => "\x55",
    0x00000056 => "\x56",
    0x00000057 => "\x57",
    0x00000058 => "\x58",
    0x00000059 => "\x59",
    0x0000005a => "\x5a",
    0x0000005b => "\x5b",
    0x0000005c => "\x5c",
    0x0000005d => "\x5d",
    0x0000005e => "\x5e",
    0x0000005f => "\x5f",
    0x00000060 => "\x60",
    0x00000061 => "\x61",
    0x00000062 => "\x62",
    0x00000063 => "\x63",
    0x00000064 => "\x64",
    0x00000065 => "\x65",
    0x00000066 => "\x66",
    0x00000067 => "\x67",
    0x00000068 => "\x68",
    0x00000069 => "\x69",
    0x0000006a => "\x6a",
    0x0000006b => "\x6b",
    0x0000006c => "\x6c",
    0x0000006d => "\x6d",
    0x0000006e => "\x6e",
    0x0000006f => "\x6f",
    0x00000070 => "\x70",
    0x00000071 => "\x71",
    0x00000072 => "\x72",
    0x00000073 => "\x73",
    0x00000074 => "\x74",
    0x00000075 => "\x75",
    0x00000076 => "\x76",
    0x00000077 => "\x77",
    0x00000078 => "\x78",
    0x00000079 => "\x79",
    0x0000007a => "\x7a",
    0x0000007b => "\x7b",
    0x0000007c => "\x7c",
    0x0000007d => "\x7d",
    0x0000007e => "\x7e",
    0x0000007f => "\x7f",
    0x000000a0 => "\xa0",
    0x000000a2 => "\xa2",
    0x000000a3 => "\xa3",
    0x000000a4 => "\xa4",
    0x000000a6 => "\xa6",
    0x000000a7 => "\xa7",
    0x000000a8 => "\x8d",
    0x000000a9 => "\xa9",
    0x000000ab => "\xab",
    0x000000ac => "\xac",
    0x000000ad => "\xad",
    0x000000ae => "\xae",
    0x000000af => "\x9d",
    0x000000b0 => "\xb0",
    0x000000b1 => "\xb1",
    0x000000b2 => "\xb2",
    0x000000b3 => "\xb3",
    0x000000b4 => "\xb4",
    0x000000b5 => "\xb5",
    0x000000b6 => "\xb6",
    0x000000b7 => "\xb7",
    0x000000b8 => "\x8f",
    0x000000b9 => "\xb9",
    0x000000bb => "\xbb",
    0x000000bc => "\xbc",
    0x000000bd => "\xbd",
    0x000000be => "\xbe",
    0x000000c4 => "\xc4",
    0x000000c5 => "\xc5",
    0x000000c6 => "\xaf",
    0x000000c9 => "\xc9",
    0x000000d3 => "\xd3",
    0x000000d5 => "\xd5",
    0x000000d6 => "\xd6",
    0x000000d7 => "\xd7",
    0x000000d8 => "\xa8",
    0x000000dc => "\xdc",
    0x000000df => "\xdf",
    0x000000e4 => "\xe4",
    0x000000e5 => "\xe5",
    0x000000e6 => "\xbf",
    0x000000e9 => "\xe9",
    0x000000f3 => "\xf3",
    0x000000f5 => "\xf5",
    0x000000f6 => "\xf6",
    0x000000f7 => "\xf7",
    0x000000f8 => "\xb8",
    0x000000fc => "\xfc",
    0x00000100 => "\xc2",
    0x00000101 => "\xe2",
    0x00000104 => "\xc0",
    0x00000105 => "\xe0",
    0x00000106 => "\xc3",
    0x00000107 => "\xe3",
    0x0000010c => "\xc8",
    0x0000010d => "\xe8",
    0x00000112 => "\xc7",
    0x00000113 => "\xe7",
    0x00000116 => "\xcb",
    0x00000117 => "\xeb",
    0x00000118 => "\xc6",
    0x00000119 => "\xe6",
    0x00000122 => "\xcc",
    0x00000123 => "\xec",
    0x0000012a => "\xce",
    0x0000012b => "\xee",
    0x0000012e => "\xc1",
    0x0000012f => "\xe1",
    0x00000136 => "\xcd",
    0x00000137 => "\xed",
    0x0000013b => "\xcf",
    0x0000013c => "\xef",
    0x00000141 => "\xd9",
    0x00000142 => "\xf9",
    0x00000143 => "\xd1",
    0x00000144 => "\xf1",
    0x00000145 => "\xd2",
    0x00000146 => "\xf2",
    0x0000014c => "\xd4",
    0x0000014d => "\xf4",
    0x00000156 => "\xaa",
    0x00000157 => "\xba",
    0x0000015a => "\xda",
    0x0000015b => "\xfa",
    0x00000160 => "\xd0",
    0x00000161 => "\xf0",
    0x0000016a => "\xdb",
    0x0000016b => "\xfb",
    0x00000172 => "\xd8",
    0x00000173 => "\xf8",
    0x00000179 => "\xca",
    0x0000017a => "\xea",
    0x0000017b => "\xdd",
    0x0000017c => "\xfd",
    0x0000017d => "\xde",
    0x0000017e => "\xfe",
    0x000002c7 => "\x8e",
    0x000002d9 => "\xff",
    0x000002db => "\x9e",
    0x00002013 => "\x96",
    0x00002014 => "\x97",
    0x00002018 => "\x91",
    0x00002019 => "\x92",
    0x0000201a => "\x82",
    0x0000201c => "\x93",
    0x0000201d => "\x94",
    0x0000201e => "\x84",
    0x00002020 => "\x86",
    0x00002021 => "\x87",
    0x00002022 => "\x95",
    0x00002026 => "\x85",
    0x00002030 => "\x89",
    0x00002039 => "\x8b",
    0x0000203a => "\x9b",
    0x000020ac => "\x80",
    0x00002122 => "\x99",
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

Locale::RecodeData::CP1257 - Conversion routines for CP1257

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for CP1257.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: CEN/TC304 N283
 alias WINBALTRIM

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
    80 |  000020AC | EURO SIGN
    82 |  0000201A | SINGLE LOW-9 QUOTATION MARK
    84 |  0000201E | DOUBLE LOW-9 QUOTATION MARK
    85 |  00002026 | HORIZONTAL ELLIPSIS
    86 |  00002020 | DAGGER
    87 |  00002021 | DOUBLE DAGGER
    89 |  00002030 | PER MILLE SIGN
    8B |  00002039 | SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    8D |  000000A8 | DIAERESIS
    8E |  000002C7 | CARON (Mandarin Chinese third tone)
    8F |  000000B8 | CEDILLA
    91 |  00002018 | LEFT SINGLE QUOTATION MARK
    92 |  00002019 | RIGHT SINGLE QUOTATION MARK
    93 |  0000201C | LEFT DOUBLE QUOTATION MARK
    94 |  0000201D | RIGHT DOUBLE QUOTATION MARK
    95 |  00002022 | BULLET
    96 |  00002013 | EN DASH
    97 |  00002014 | EM DASH
    99 |  00002122 | TRADE MARK SIGN
    9B |  0000203A | SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    9D |  000000AF | MACRON
    9E |  000002DB | OGONEK
    A0 |  000000A0 | NO-BREAK SPACE
    A2 |  000000A2 | CENT SIGN
    A3 |  000000A3 | POUND SIGN
    A4 |  000000A4 | CURRENCY SIGN
    A6 |  000000A6 | BROKEN BAR
    A7 |  000000A7 | SECTION SIGN
    A8 |  000000D8 | LATIN CAPITAL LETTER O WITH STROKE
    A9 |  000000A9 | COPYRIGHT SIGN
    AA |  00000156 | LATIN CAPITAL LETTER R WITH CEDILLA
    AB |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    AC |  000000AC | NOT SIGN
    AD |  000000AD | SOFT HYPHEN
    AE |  000000AE | REGISTERED SIGN
    AF |  000000C6 | LATIN CAPITAL LETTER AE
    B0 |  000000B0 | DEGREE SIGN
    B1 |  000000B1 | PLUS-MINUS SIGN
    B2 |  000000B2 | SUPERSCRIPT TWO
    B3 |  000000B3 | SUPERSCRIPT THREE
    B4 |  000000B4 | ACUTE ACCENT
    B5 |  000000B5 | MICRO SIGN
    B6 |  000000B6 | PILCROW SIGN
    B7 |  000000B7 | MIDDLE DOT
    B8 |  000000F8 | LATIN SMALL LETTER O WITH STROKE
    B9 |  000000B9 | SUPERSCRIPT ONE
    BA |  00000157 | LATIN SMALL LETTER R WITH CEDILLA
    BB |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    BC |  000000BC | VULGAR FRACTION ONE QUARTER
    BD |  000000BD | VULGAR FRACTION ONE HALF
    BE |  000000BE | VULGAR FRACTION THREE QUARTERS
    BF |  000000E6 | LATIN SMALL LETTER AE
    C0 |  00000104 | LATIN CAPITAL LETTER A WITH OGONEK
    C1 |  0000012E | LATIN CAPITAL LETTER I WITH OGONEK
    C2 |  00000100 | LATIN CAPITAL LETTER A WITH MACRON
    C3 |  00000106 | LATIN CAPITAL LETTER C WITH ACUTE
    C4 |  000000C4 | LATIN CAPITAL LETTER A WITH DIAERESIS
    C5 |  000000C5 | LATIN CAPITAL LETTER A WITH RING ABOVE
    C6 |  00000118 | LATIN CAPITAL LETTER E WITH OGONEK
    C7 |  00000112 | LATIN CAPITAL LETTER E WITH MACRON
    C8 |  0000010C | LATIN CAPITAL LETTER C WITH CARON
    C9 |  000000C9 | LATIN CAPITAL LETTER E WITH ACUTE
    CA |  00000179 | LATIN CAPITAL LETTER Z WITH ACUTE
    CB |  00000116 | LATIN CAPITAL LETTER E WITH DOT ABOVE
    CC |  00000122 | LATIN CAPITAL LETTER G WITH CEDILLA
    CD |  00000136 | LATIN CAPITAL LETTER K WITH CEDILLA
    CE |  0000012A | LATIN CAPITAL LETTER I WITH MACRON
    CF |  0000013B | LATIN CAPITAL LETTER L WITH CEDILLA
    D0 |  00000160 | LATIN CAPITAL LETTER S WITH CARON
    D1 |  00000143 | LATIN CAPITAL LETTER N WITH ACUTE
    D2 |  00000145 | LATIN CAPITAL LETTER N WITH CEDILLA
    D3 |  000000D3 | LATIN CAPITAL LETTER O WITH ACUTE
    D4 |  0000014C | LATIN CAPITAL LETTER O WITH MACRON
    D5 |  000000D5 | LATIN CAPITAL LETTER O WITH TILDE
    D6 |  000000D6 | LATIN CAPITAL LETTER O WITH DIAERESIS
    D7 |  000000D7 | MULTIPLICATION SIGN
    D8 |  00000172 | LATIN CAPITAL LETTER U WITH OGONEK
    D9 |  00000141 | LATIN CAPITAL LETTER L WITH STROKE
    DA |  0000015A | LATIN CAPITAL LETTER S WITH ACUTE
    DB |  0000016A | LATIN CAPITAL LETTER U WITH MACRON
    DC |  000000DC | LATIN CAPITAL LETTER U WITH DIAERESIS
    DD |  0000017B | LATIN CAPITAL LETTER Z WITH DOT ABOVE
    DE |  0000017D | LATIN CAPITAL LETTER Z WITH CARON
    DF |  000000DF | LATIN SMALL LETTER SHARP S (German)
    E0 |  00000105 | LATIN SMALL LETTER A WITH OGONEK
    E1 |  0000012F | LATIN SMALL LETTER I WITH OGONEK
    E2 |  00000101 | LATIN SMALL LETTER A WITH MACRON
    E3 |  00000107 | LATIN SMALL LETTER C WITH ACUTE
    E4 |  000000E4 | LATIN SMALL LETTER A WITH DIAERESIS
    E5 |  000000E5 | LATIN SMALL LETTER A WITH RING ABOVE
    E6 |  00000119 | LATIN SMALL LETTER E WITH OGONEK
    E7 |  00000113 | LATIN SMALL LETTER E WITH MACRON
    E8 |  0000010D | LATIN SMALL LETTER C WITH CARON
    E9 |  000000E9 | LATIN SMALL LETTER E WITH ACUTE
    EA |  0000017A | LATIN SMALL LETTER Z WITH ACUTE
    EB |  00000117 | LATIN SMALL LETTER E WITH DOT ABOVE
    EC |  00000123 | LATIN SMALL LETTER G WITH CEDILLA
    ED |  00000137 | LATIN SMALL LETTER K WITH CEDILLA
    EE |  0000012B | LATIN SMALL LETTER I WITH MACRON
    EF |  0000013C | LATIN SMALL LETTER L WITH CEDILLA
    F0 |  00000161 | LATIN SMALL LETTER S WITH CARON
    F1 |  00000144 | LATIN SMALL LETTER N WITH ACUTE
    F2 |  00000146 | LATIN SMALL LETTER N WITH CEDILLA
    F3 |  000000F3 | LATIN SMALL LETTER O WITH ACUTE
    F4 |  0000014D | LATIN SMALL LETTER O WITH MACRON
    F5 |  000000F5 | LATIN SMALL LETTER O WITH TILDE
    F6 |  000000F6 | LATIN SMALL LETTER O WITH DIAERESIS
    F7 |  000000F7 | DIVISION SIGN
    F8 |  00000173 | LATIN SMALL LETTER U WITH OGONEK
    F9 |  00000142 | LATIN SMALL LETTER L WITH STROKE
    FA |  0000015B | LATIN SMALL LETTER S WITH ACUTE
    FB |  0000016B | LATIN SMALL LETTER U WITH MACRON
    FC |  000000FC | LATIN SMALL LETTER U WITH DIAERESIS
    FD |  0000017C | LATIN SMALL LETTER Z WITH DOT ABOVE
    FE |  0000017E | LATIN SMALL LETTER Z WITH CARON
    FF |  000002D9 | DOT ABOVE (Mandarin Chinese light tone)


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
