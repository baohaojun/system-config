#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM1047.pm,v 1.1 2011-10-12 23:51:36 pertusus Exp $

# Conversion routines for IBM1047.
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

package Locale::RecodeData::IBM1047;

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
    0x00a0,
    0x00e2,
    0x00e4,
    0x00e0,
    0x00e1,
    0x00e3,
    0x00e5,
    0x00e7,
    0x00f1,
    0x00a2,
    0x002e,
    0x003c,
    0x0028,
    0x002b,
    0x007c,
    0x0026,
    0x00e9,
    0x00ea,
    0x00eb,
    0x00e8,
    0x00ed,
    0x00ee,
    0x00ef,
    0x00ec,
    0x00df,
    0x0021,
    0x0024,
    0x002a,
    0x0029,
    0x003b,
    0x005e,
    0x002d,
    0x002f,
    0x00c2,
    0x00c4,
    0x00c0,
    0x00c1,
    0x00c3,
    0x00c5,
    0x00c7,
    0x00d1,
    0x00a6,
    0x002c,
    0x0025,
    0x005f,
    0x003e,
    0x003f,
    0x00f8,
    0x00c9,
    0x00ca,
    0x00cb,
    0x00c8,
    0x00cd,
    0x00ce,
    0x00cf,
    0x00cc,
    0x0060,
    0x003a,
    0x0023,
    0x0040,
    0x0027,
    0x003d,
    0x0022,
    0x00d8,
    0x0061,
    0x0062,
    0x0063,
    0x0064,
    0x0065,
    0x0066,
    0x0067,
    0x0068,
    0x0069,
    0x00ab,
    0x00bb,
    0x00f0,
    0x00fd,
    0x00fe,
    0x00b1,
    0x00b0,
    0x006a,
    0x006b,
    0x006c,
    0x006d,
    0x006e,
    0x006f,
    0x0070,
    0x0071,
    0x0072,
    0x00aa,
    0x00ba,
    0x00e6,
    0x00b8,
    0x00c6,
    0x00a4,
    0x00b5,
    0x007e,
    0x0073,
    0x0074,
    0x0075,
    0x0076,
    0x0077,
    0x0078,
    0x0079,
    0x007a,
    0x00a1,
    0x00bf,
    0x00d0,
    0x005b,
    0x00de,
    0x00ae,
    0x00ac,
    0x00a3,
    0x00a5,
    0x00b7,
    0x00a9,
    0x00a7,
    0x00b6,
    0x00bc,
    0x00bd,
    0x00be,
    0x00dd,
    0x00a8,
    0x00af,
    0x005d,
    0x00b4,
    0x00d7,
    0x007b,
    0x0041,
    0x0042,
    0x0043,
    0x0044,
    0x0045,
    0x0046,
    0x0047,
    0x0048,
    0x0049,
    0x00ad,
    0x00f4,
    0x00f6,
    0x00f2,
    0x00f3,
    0x00f5,
    0x007d,
    0x004a,
    0x004b,
    0x004c,
    0x004d,
    0x004e,
    0x004f,
    0x0050,
    0x0051,
    0x0052,
    0x00b9,
    0x00fb,
    0x00fc,
    0x00f9,
    0x00fa,
    0x00ff,
    0x005c,
    0x00f7,
    0x0053,
    0x0054,
    0x0055,
    0x0056,
    0x0057,
    0x0058,
    0x0059,
    0x005a,
    0x00b2,
    0x00d4,
    0x00d6,
    0x00d2,
    0x00d3,
    0x00d5,
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
    0x00b3,
    0x00db,
    0x00dc,
    0x00d9,
    0x00da,
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
    "\xc2\xa0",
    "\xc3\xa2",
    "\xc3\xa4",
    "\xc3\xa0",
    "\xc3\xa1",
    "\xc3\xa3",
    "\xc3\xa5",
    "\xc3\xa7",
    "\xc3\xb1",
    "\xc2\xa2",
    "\x2e",
    "\x3c",
    "\x28",
    "\x2b",
    "\x7c",
    "\x26",
    "\xc3\xa9",
    "\xc3\xaa",
    "\xc3\xab",
    "\xc3\xa8",
    "\xc3\xad",
    "\xc3\xae",
    "\xc3\xaf",
    "\xc3\xac",
    "\xc3\x9f",
    "\x21",
    "\x24",
    "\x2a",
    "\x29",
    "\x3b",
    "\x5e",
    "\x2d",
    "\x2f",
    "\xc3\x82",
    "\xc3\x84",
    "\xc3\x80",
    "\xc3\x81",
    "\xc3\x83",
    "\xc3\x85",
    "\xc3\x87",
    "\xc3\x91",
    "\xc2\xa6",
    "\x2c",
    "\x25",
    "\x5f",
    "\x3e",
    "\x3f",
    "\xc3\xb8",
    "\xc3\x89",
    "\xc3\x8a",
    "\xc3\x8b",
    "\xc3\x88",
    "\xc3\x8d",
    "\xc3\x8e",
    "\xc3\x8f",
    "\xc3\x8c",
    "\x60",
    "\x3a",
    "\x23",
    "\x40",
    "\x27",
    "\x3d",
    "\x22",
    "\xc3\x98",
    "\x61",
    "\x62",
    "\x63",
    "\x64",
    "\x65",
    "\x66",
    "\x67",
    "\x68",
    "\x69",
    "\xc2\xab",
    "\xc2\xbb",
    "\xc3\xb0",
    "\xc3\xbd",
    "\xc3\xbe",
    "\xc2\xb1",
    "\xc2\xb0",
    "\x6a",
    "\x6b",
    "\x6c",
    "\x6d",
    "\x6e",
    "\x6f",
    "\x70",
    "\x71",
    "\x72",
    "\xc2\xaa",
    "\xc2\xba",
    "\xc3\xa6",
    "\xc2\xb8",
    "\xc3\x86",
    "\xc2\xa4",
    "\xc2\xb5",
    "\x7e",
    "\x73",
    "\x74",
    "\x75",
    "\x76",
    "\x77",
    "\x78",
    "\x79",
    "\x7a",
    "\xc2\xa1",
    "\xc2\xbf",
    "\xc3\x90",
    "\x5b",
    "\xc3\x9e",
    "\xc2\xae",
    "\xc2\xac",
    "\xc2\xa3",
    "\xc2\xa5",
    "\xc2\xb7",
    "\xc2\xa9",
    "\xc2\xa7",
    "\xc2\xb6",
    "\xc2\xbc",
    "\xc2\xbd",
    "\xc2\xbe",
    "\xc3\x9d",
    "\xc2\xa8",
    "\xc2\xaf",
    "\x5d",
    "\xc2\xb4",
    "\xc3\x97",
    "\x7b",
    "\x41",
    "\x42",
    "\x43",
    "\x44",
    "\x45",
    "\x46",
    "\x47",
    "\x48",
    "\x49",
    "\xc2\xad",
    "\xc3\xb4",
    "\xc3\xb6",
    "\xc3\xb2",
    "\xc3\xb3",
    "\xc3\xb5",
    "\x7d",
    "\x4a",
    "\x4b",
    "\x4c",
    "\x4d",
    "\x4e",
    "\x4f",
    "\x50",
    "\x51",
    "\x52",
    "\xc2\xb9",
    "\xc3\xbb",
    "\xc3\xbc",
    "\xc3\xb9",
    "\xc3\xba",
    "\xc3\xbf",
    "\x5c",
    "\xc3\xb7",
    "\x53",
    "\x54",
    "\x55",
    "\x56",
    "\x57",
    "\x58",
    "\x59",
    "\x5a",
    "\xc2\xb2",
    "\xc3\x94",
    "\xc3\x96",
    "\xc3\x92",
    "\xc3\x93",
    "\xc3\x95",
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
    "\xc2\xb3",
    "\xc3\x9b",
    "\xc3\x9c",
    "\xc3\x99",
    "\xc3\x9a",
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
    0x00000024 => "\x5b",
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
    0x0000005b => "\xad",
    0x0000005c => "\xe0",
    0x0000005d => "\xbd",
    0x0000005e => "\x5f",
    0x0000005f => "\x6d",
    0x00000060 => "\x79",
    0x00000061 => "\x81",
    0x00000062 => "\x82",
    0x00000063 => "\x83",
    0x00000064 => "\x84",
    0x00000065 => "\x85",
    0x00000066 => "\x86",
    0x00000067 => "\x87",
    0x00000068 => "\x88",
    0x00000069 => "\x89",
    0x0000006a => "\x91",
    0x0000006b => "\x92",
    0x0000006c => "\x93",
    0x0000006d => "\x94",
    0x0000006e => "\x95",
    0x0000006f => "\x96",
    0x00000070 => "\x97",
    0x00000071 => "\x98",
    0x00000072 => "\x99",
    0x00000073 => "\xa2",
    0x00000074 => "\xa3",
    0x00000075 => "\xa4",
    0x00000076 => "\xa5",
    0x00000077 => "\xa6",
    0x00000078 => "\xa7",
    0x00000079 => "\xa8",
    0x0000007a => "\xa9",
    0x0000007b => "\xc0",
    0x0000007c => "\x4f",
    0x0000007d => "\xd0",
    0x0000007e => "\xa1",
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
    0x000000a0 => "\x41",
    0x000000a1 => "\xaa",
    0x000000a2 => "\x4a",
    0x000000a3 => "\xb1",
    0x000000a4 => "\x9f",
    0x000000a5 => "\xb2",
    0x000000a6 => "\x6a",
    0x000000a7 => "\xb5",
    0x000000a8 => "\xbb",
    0x000000a9 => "\xb4",
    0x000000aa => "\x9a",
    0x000000ab => "\x8a",
    0x000000ac => "\xb0",
    0x000000ad => "\xca",
    0x000000ae => "\xaf",
    0x000000af => "\xbc",
    0x000000b0 => "\x90",
    0x000000b1 => "\x8f",
    0x000000b2 => "\xea",
    0x000000b3 => "\xfa",
    0x000000b4 => "\xbe",
    0x000000b5 => "\xa0",
    0x000000b6 => "\xb6",
    0x000000b7 => "\xb3",
    0x000000b8 => "\x9d",
    0x000000b9 => "\xda",
    0x000000ba => "\x9b",
    0x000000bb => "\x8b",
    0x000000bc => "\xb7",
    0x000000bd => "\xb8",
    0x000000be => "\xb9",
    0x000000bf => "\xab",
    0x000000c0 => "\x64",
    0x000000c1 => "\x65",
    0x000000c2 => "\x62",
    0x000000c3 => "\x66",
    0x000000c4 => "\x63",
    0x000000c5 => "\x67",
    0x000000c6 => "\x9e",
    0x000000c7 => "\x68",
    0x000000c8 => "\x74",
    0x000000c9 => "\x71",
    0x000000ca => "\x72",
    0x000000cb => "\x73",
    0x000000cc => "\x78",
    0x000000cd => "\x75",
    0x000000ce => "\x76",
    0x000000cf => "\x77",
    0x000000d0 => "\xac",
    0x000000d1 => "\x69",
    0x000000d2 => "\xed",
    0x000000d3 => "\xee",
    0x000000d4 => "\xeb",
    0x000000d5 => "\xef",
    0x000000d6 => "\xec",
    0x000000d7 => "\xbf",
    0x000000d8 => "\x80",
    0x000000d9 => "\xfd",
    0x000000da => "\xfe",
    0x000000db => "\xfb",
    0x000000dc => "\xfc",
    0x000000dd => "\xba",
    0x000000de => "\xae",
    0x000000df => "\x59",
    0x000000e0 => "\x44",
    0x000000e1 => "\x45",
    0x000000e2 => "\x42",
    0x000000e3 => "\x46",
    0x000000e4 => "\x43",
    0x000000e5 => "\x47",
    0x000000e6 => "\x9c",
    0x000000e7 => "\x48",
    0x000000e8 => "\x54",
    0x000000e9 => "\x51",
    0x000000ea => "\x52",
    0x000000eb => "\x53",
    0x000000ec => "\x58",
    0x000000ed => "\x55",
    0x000000ee => "\x56",
    0x000000ef => "\x57",
    0x000000f0 => "\x8c",
    0x000000f1 => "\x49",
    0x000000f2 => "\xcd",
    0x000000f3 => "\xce",
    0x000000f4 => "\xcb",
    0x000000f5 => "\xcf",
    0x000000f6 => "\xcc",
    0x000000f7 => "\xe1",
    0x000000f8 => "\x70",
    0x000000f9 => "\xdd",
    0x000000fa => "\xde",
    0x000000fb => "\xdb",
    0x000000fc => "\xdc",
    0x000000fd => "\x8d",
    0x000000fe => "\x8e",
    0x000000ff => "\xdf",
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

Locale::RecodeData::IBM1047 - Conversion routines for IBM1047

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM1047.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM Character Data Representation Architecture
  Registry SC09-1391-00 p 150.
 alias CP1047
 alias 1047

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
    41 |  000000A0 | NO-BREAK SPACE
    42 |  000000E2 | LATIN SMALL LETTER A WITH CIRCUMFLEX
    43 |  000000E4 | LATIN SMALL LETTER A WITH DIAERESIS
    44 |  000000E0 | LATIN SMALL LETTER A WITH GRAVE
    45 |  000000E1 | LATIN SMALL LETTER A WITH ACUTE
    46 |  000000E3 | LATIN SMALL LETTER A WITH TILDE
    47 |  000000E5 | LATIN SMALL LETTER A WITH RING ABOVE
    48 |  000000E7 | LATIN SMALL LETTER C WITH CEDILLA
    49 |  000000F1 | LATIN SMALL LETTER N WITH TILDE
    4A |  000000A2 | CENT SIGN
    4B |  0000002E | FULL STOP
    4C |  0000003C | LESS-THAN SIGN
    4D |  00000028 | LEFT PARENTHESIS
    4E |  0000002B | PLUS SIGN
    4F |  0000007C | VERTICAL LINE
    50 |  00000026 | AMPERSAND
    51 |  000000E9 | LATIN SMALL LETTER E WITH ACUTE
    52 |  000000EA | LATIN SMALL LETTER E WITH CIRCUMFLEX
    53 |  000000EB | LATIN SMALL LETTER E WITH DIAERESIS
    54 |  000000E8 | LATIN SMALL LETTER E WITH GRAVE
    55 |  000000ED | LATIN SMALL LETTER I WITH ACUTE
    56 |  000000EE | LATIN SMALL LETTER I WITH CIRCUMFLEX
    57 |  000000EF | LATIN SMALL LETTER I WITH DIAERESIS
    58 |  000000EC | LATIN SMALL LETTER I WITH GRAVE
    59 |  000000DF | LATIN SMALL LETTER SHARP S (German)
    5A |  00000021 | EXCLAMATION MARK
    5B |  00000024 | DOLLAR SIGN
    5C |  0000002A | ASTERISK
    5D |  00000029 | RIGHT PARENTHESIS
    5E |  0000003B | SEMICOLON
    5F |  0000005E | CIRCUMFLEX ACCENT
    60 |  0000002D | HYPHEN-MINUS
    61 |  0000002F | SOLIDUS
    62 |  000000C2 | LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    63 |  000000C4 | LATIN CAPITAL LETTER A WITH DIAERESIS
    64 |  000000C0 | LATIN CAPITAL LETTER A WITH GRAVE
    65 |  000000C1 | LATIN CAPITAL LETTER A WITH ACUTE
    66 |  000000C3 | LATIN CAPITAL LETTER A WITH TILDE
    67 |  000000C5 | LATIN CAPITAL LETTER A WITH RING ABOVE
    68 |  000000C7 | LATIN CAPITAL LETTER C WITH CEDILLA
    69 |  000000D1 | LATIN CAPITAL LETTER N WITH TILDE
    6A |  000000A6 | BROKEN BAR
    6B |  0000002C | COMMA
    6C |  00000025 | PERCENT SIGN
    6D |  0000005F | LOW LINE
    6E |  0000003E | GREATER-THAN SIGN
    6F |  0000003F | QUESTION MARK
    70 |  000000F8 | LATIN SMALL LETTER O WITH STROKE
    71 |  000000C9 | LATIN CAPITAL LETTER E WITH ACUTE
    72 |  000000CA | LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    73 |  000000CB | LATIN CAPITAL LETTER E WITH DIAERESIS
    74 |  000000C8 | LATIN CAPITAL LETTER E WITH GRAVE
    75 |  000000CD | LATIN CAPITAL LETTER I WITH ACUTE
    76 |  000000CE | LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    77 |  000000CF | LATIN CAPITAL LETTER I WITH DIAERESIS
    78 |  000000CC | LATIN CAPITAL LETTER I WITH GRAVE
    79 |  00000060 | GRAVE ACCENT
    7A |  0000003A | COLON
    7B |  00000023 | NUMBER SIGN
    7C |  00000040 | COMMERCIAL AT
    7D |  00000027 | APOSTROPHE
    7E |  0000003D | EQUALS SIGN
    7F |  00000022 | QUOTATION MARK
    80 |  000000D8 | LATIN CAPITAL LETTER O WITH STROKE
    81 |  00000061 | LATIN SMALL LETTER A
    82 |  00000062 | LATIN SMALL LETTER B
    83 |  00000063 | LATIN SMALL LETTER C
    84 |  00000064 | LATIN SMALL LETTER D
    85 |  00000065 | LATIN SMALL LETTER E
    86 |  00000066 | LATIN SMALL LETTER F
    87 |  00000067 | LATIN SMALL LETTER G
    88 |  00000068 | LATIN SMALL LETTER H
    89 |  00000069 | LATIN SMALL LETTER I
    8A |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    8B |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    8C |  000000F0 | LATIN SMALL LETTER ETH (Icelandic)
    8D |  000000FD | LATIN SMALL LETTER Y WITH ACUTE
    8E |  000000FE | LATIN SMALL LETTER THORN (Icelandic)
    8F |  000000B1 | PLUS-MINUS SIGN
    90 |  000000B0 | DEGREE SIGN
    91 |  0000006A | LATIN SMALL LETTER J
    92 |  0000006B | LATIN SMALL LETTER K
    93 |  0000006C | LATIN SMALL LETTER L
    94 |  0000006D | LATIN SMALL LETTER M
    95 |  0000006E | LATIN SMALL LETTER N
    96 |  0000006F | LATIN SMALL LETTER O
    97 |  00000070 | LATIN SMALL LETTER P
    98 |  00000071 | LATIN SMALL LETTER Q
    99 |  00000072 | LATIN SMALL LETTER R
    9A |  000000AA | FEMININE ORDINAL INDICATOR
    9B |  000000BA | MASCULINE ORDINAL INDICATOR
    9C |  000000E6 | LATIN SMALL LETTER AE
    9D |  000000B8 | CEDILLA
    9E |  000000C6 | LATIN CAPITAL LETTER AE
    9F |  000000A4 | CURRENCY SIGN
    A0 |  000000B5 | MICRO SIGN
    A1 |  0000007E | TILDE
    A2 |  00000073 | LATIN SMALL LETTER S
    A3 |  00000074 | LATIN SMALL LETTER T
    A4 |  00000075 | LATIN SMALL LETTER U
    A5 |  00000076 | LATIN SMALL LETTER V
    A6 |  00000077 | LATIN SMALL LETTER W
    A7 |  00000078 | LATIN SMALL LETTER X
    A8 |  00000079 | LATIN SMALL LETTER Y
    A9 |  0000007A | LATIN SMALL LETTER Z
    AA |  000000A1 | INVERTED EXCLAMATION MARK
    AB |  000000BF | INVERTED QUESTION MARK
    AC |  000000D0 | LATIN CAPITAL LETTER ETH (Icelandic)
    AD |  0000005B | LEFT SQUARE BRACKET
    AE |  000000DE | LATIN CAPITAL LETTER THORN (Icelandic)
    AF |  000000AE | REGISTERED SIGN
    B0 |  000000AC | NOT SIGN
    B1 |  000000A3 | POUND SIGN
    B2 |  000000A5 | YEN SIGN
    B3 |  000000B7 | MIDDLE DOT
    B4 |  000000A9 | COPYRIGHT SIGN
    B5 |  000000A7 | SECTION SIGN
    B6 |  000000B6 | PILCROW SIGN
    B7 |  000000BC | VULGAR FRACTION ONE QUARTER
    B8 |  000000BD | VULGAR FRACTION ONE HALF
    B9 |  000000BE | VULGAR FRACTION THREE QUARTERS
    BA |  000000DD | LATIN CAPITAL LETTER Y WITH ACUTE
    BB |  000000A8 | DIAERESIS
    BC |  000000AF | MACRON
    BD |  0000005D | RIGHT SQUARE BRACKET
    BE |  000000B4 | ACUTE ACCENT
    BF |  000000D7 | MULTIPLICATION SIGN
    C0 |  0000007B | LEFT CURLY BRACKET
    C1 |  00000041 | LATIN CAPITAL LETTER A
    C2 |  00000042 | LATIN CAPITAL LETTER B
    C3 |  00000043 | LATIN CAPITAL LETTER C
    C4 |  00000044 | LATIN CAPITAL LETTER D
    C5 |  00000045 | LATIN CAPITAL LETTER E
    C6 |  00000046 | LATIN CAPITAL LETTER F
    C7 |  00000047 | LATIN CAPITAL LETTER G
    C8 |  00000048 | LATIN CAPITAL LETTER H
    C9 |  00000049 | LATIN CAPITAL LETTER I
    CA |  000000AD | SOFT HYPHEN
    CB |  000000F4 | LATIN SMALL LETTER O WITH CIRCUMFLEX
    CC |  000000F6 | LATIN SMALL LETTER O WITH DIAERESIS
    CD |  000000F2 | LATIN SMALL LETTER O WITH GRAVE
    CE |  000000F3 | LATIN SMALL LETTER O WITH ACUTE
    CF |  000000F5 | LATIN SMALL LETTER O WITH TILDE
    D0 |  0000007D | RIGHT CURLY BRACKET
    D1 |  0000004A | LATIN CAPITAL LETTER J
    D2 |  0000004B | LATIN CAPITAL LETTER K
    D3 |  0000004C | LATIN CAPITAL LETTER L
    D4 |  0000004D | LATIN CAPITAL LETTER M
    D5 |  0000004E | LATIN CAPITAL LETTER N
    D6 |  0000004F | LATIN CAPITAL LETTER O
    D7 |  00000050 | LATIN CAPITAL LETTER P
    D8 |  00000051 | LATIN CAPITAL LETTER Q
    D9 |  00000052 | LATIN CAPITAL LETTER R
    DA |  000000B9 | SUPERSCRIPT ONE
    DB |  000000FB | LATIN SMALL LETTER U WITH CIRCUMFLEX
    DC |  000000FC | LATIN SMALL LETTER U WITH DIAERESIS
    DD |  000000F9 | LATIN SMALL LETTER U WITH GRAVE
    DE |  000000FA | LATIN SMALL LETTER U WITH ACUTE
    DF |  000000FF | LATIN SMALL LETTER Y WITH DIAERESIS
    E0 |  0000005C | REVERSE SOLIDUS
    E1 |  000000F7 | DIVISION SIGN
    E2 |  00000053 | LATIN CAPITAL LETTER S
    E3 |  00000054 | LATIN CAPITAL LETTER T
    E4 |  00000055 | LATIN CAPITAL LETTER U
    E5 |  00000056 | LATIN CAPITAL LETTER V
    E6 |  00000057 | LATIN CAPITAL LETTER W
    E7 |  00000058 | LATIN CAPITAL LETTER X
    E8 |  00000059 | LATIN CAPITAL LETTER Y
    E9 |  0000005A | LATIN CAPITAL LETTER Z
    EA |  000000B2 | SUPERSCRIPT TWO
    EB |  000000D4 | LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    EC |  000000D6 | LATIN CAPITAL LETTER O WITH DIAERESIS
    ED |  000000D2 | LATIN CAPITAL LETTER O WITH GRAVE
    EE |  000000D3 | LATIN CAPITAL LETTER O WITH ACUTE
    EF |  000000D5 | LATIN CAPITAL LETTER O WITH TILDE
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
    FA |  000000B3 | SUPERSCRIPT THREE
    FB |  000000DB | LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    FC |  000000DC | LATIN CAPITAL LETTER U WITH DIAERESIS
    FD |  000000D9 | LATIN CAPITAL LETTER U WITH GRAVE
    FE |  000000DA | LATIN CAPITAL LETTER U WITH ACUTE
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
