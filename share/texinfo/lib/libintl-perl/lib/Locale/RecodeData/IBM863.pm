#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM863.pm,v 1.1 2011-10-12 23:51:39 pertusus Exp $

# Conversion routines for IBM863.
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

package Locale::RecodeData::IBM863;

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
    0x00c7,
    0x00fc,
    0x00e9,
    0x00e2,
    0x00c2,
    0x00e0,
    0x00b6,
    0x00e7,
    0x00ea,
    0x00eb,
    0x00e8,
    0x00ef,
    0x00ee,
    0x2017,
    0x00c0,
    0x00a7,
    0x00c9,
    0x00c8,
    0x00ca,
    0x00f4,
    0x00cb,
    0x00cf,
    0x00fb,
    0x00f9,
    0x00a4,
    0x00d4,
    0x00dc,
    0x00a2,
    0x00a3,
    0x00d9,
    0x00db,
    0x0192,
    0x00a6,
    0x00b4,
    0x00f3,
    0x00fa,
    0x00a8,
    0x00b8,
    0x00b3,
    0x00af,
    0x00ce,
    0x2310,
    0x00ac,
    0x00bd,
    0x00bc,
    0x00be,
    0x00ab,
    0x00bb,
    0x2591,
    0x2592,
    0x2593,
    0x2502,
    0x2524,
    0x2561,
    0x2562,
    0x2556,
    0x2555,
    0x2563,
    0x2551,
    0x2557,
    0x255d,
    0x255c,
    0x255b,
    0x2510,
    0x2514,
    0x2534,
    0x252c,
    0x251c,
    0x2500,
    0x253c,
    0x255e,
    0x255f,
    0x255a,
    0x2554,
    0x2569,
    0x2566,
    0x2560,
    0x2550,
    0x256c,
    0x2567,
    0x2568,
    0x2564,
    0x2565,
    0x2559,
    0x2558,
    0x2552,
    0x2553,
    0x256b,
    0x256a,
    0x2518,
    0x250c,
    0x2588,
    0x2584,
    0x258c,
    0x2590,
    0x2580,
    0x03b1,
    0x00df,
    0x0393,
    0x03c0,
    0x03a3,
    0x03c3,
    0x00b5,
    0x03c4,
    0x03a6,
    0x0398,
    0x03a9,
    0x03b4,
    0x221e,
    0x03c6,
    0x03b5,
    0x2229,
    0x2261,
    0x00b1,
    0x2265,
    0x2264,
    0x2320,
    0x2321,
    0x00f7,
    0x2248,
    0x00b0,
    0x2219,
    0x00b7,
    0x221a,
    0x207f,
    0x00b2,
    0x25a0,
    0x00a0,
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
    "\xc3\x87",
    "\xc3\xbc",
    "\xc3\xa9",
    "\xc3\xa2",
    "\xc3\x82",
    "\xc3\xa0",
    "\xc2\xb6",
    "\xc3\xa7",
    "\xc3\xaa",
    "\xc3\xab",
    "\xc3\xa8",
    "\xc3\xaf",
    "\xc3\xae",
    "\xe2\x80\x97",
    "\xc3\x80",
    "\xc2\xa7",
    "\xc3\x89",
    "\xc3\x88",
    "\xc3\x8a",
    "\xc3\xb4",
    "\xc3\x8b",
    "\xc3\x8f",
    "\xc3\xbb",
    "\xc3\xb9",
    "\xc2\xa4",
    "\xc3\x94",
    "\xc3\x9c",
    "\xc2\xa2",
    "\xc2\xa3",
    "\xc3\x99",
    "\xc3\x9b",
    "\xc6\x92",
    "\xc2\xa6",
    "\xc2\xb4",
    "\xc3\xb3",
    "\xc3\xba",
    "\xc2\xa8",
    "\xc2\xb8",
    "\xc2\xb3",
    "\xc2\xaf",
    "\xc3\x8e",
    "\xe2\x8c\x90",
    "\xc2\xac",
    "\xc2\xbd",
    "\xc2\xbc",
    "\xc2\xbe",
    "\xc2\xab",
    "\xc2\xbb",
    "\xe2\x96\x91",
    "\xe2\x96\x92",
    "\xe2\x96\x93",
    "\xe2\x94\x82",
    "\xe2\x94\xa4",
    "\xe2\x95\xa1",
    "\xe2\x95\xa2",
    "\xe2\x95\x96",
    "\xe2\x95\x95",
    "\xe2\x95\xa3",
    "\xe2\x95\x91",
    "\xe2\x95\x97",
    "\xe2\x95\x9d",
    "\xe2\x95\x9c",
    "\xe2\x95\x9b",
    "\xe2\x94\x90",
    "\xe2\x94\x94",
    "\xe2\x94\xb4",
    "\xe2\x94\xac",
    "\xe2\x94\x9c",
    "\xe2\x94\x80",
    "\xe2\x94\xbc",
    "\xe2\x95\x9e",
    "\xe2\x95\x9f",
    "\xe2\x95\x9a",
    "\xe2\x95\x94",
    "\xe2\x95\xa9",
    "\xe2\x95\xa6",
    "\xe2\x95\xa0",
    "\xe2\x95\x90",
    "\xe2\x95\xac",
    "\xe2\x95\xa7",
    "\xe2\x95\xa8",
    "\xe2\x95\xa4",
    "\xe2\x95\xa5",
    "\xe2\x95\x99",
    "\xe2\x95\x98",
    "\xe2\x95\x92",
    "\xe2\x95\x93",
    "\xe2\x95\xab",
    "\xe2\x95\xaa",
    "\xe2\x94\x98",
    "\xe2\x94\x8c",
    "\xe2\x96\x88",
    "\xe2\x96\x84",
    "\xe2\x96\x8c",
    "\xe2\x96\x90",
    "\xe2\x96\x80",
    "\xce\xb1",
    "\xc3\x9f",
    "\xce\x93",
    "\xcf\x80",
    "\xce\xa3",
    "\xcf\x83",
    "\xc2\xb5",
    "\xcf\x84",
    "\xce\xa6",
    "\xce\x98",
    "\xce\xa9",
    "\xce\xb4",
    "\xe2\x88\x9e",
    "\xcf\x86",
    "\xce\xb5",
    "\xe2\x88\xa9",
    "\xe2\x89\xa1",
    "\xc2\xb1",
    "\xe2\x89\xa5",
    "\xe2\x89\xa4",
    "\xe2\x8c\xa0",
    "\xe2\x8c\xa1",
    "\xc3\xb7",
    "\xe2\x89\x88",
    "\xc2\xb0",
    "\xe2\x88\x99",
    "\xc2\xb7",
    "\xe2\x88\x9a",
    "\xe2\x81\xbf",
    "\xc2\xb2",
    "\xe2\x96\xa0",
    "\xc2\xa0",
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
    0x000000a0 => "\xff",
    0x000000a2 => "\x9b",
    0x000000a3 => "\x9c",
    0x000000a4 => "\x98",
    0x000000a6 => "\xa0",
    0x000000a7 => "\x8f",
    0x000000a8 => "\xa4",
    0x000000ab => "\xae",
    0x000000ac => "\xaa",
    0x000000af => "\xa7",
    0x000000b0 => "\xf8",
    0x000000b1 => "\xf1",
    0x000000b2 => "\xfd",
    0x000000b3 => "\xa6",
    0x000000b4 => "\xa1",
    0x000000b5 => "\xe6",
    0x000000b6 => "\x86",
    0x000000b7 => "\xfa",
    0x000000b8 => "\xa5",
    0x000000bb => "\xaf",
    0x000000bc => "\xac",
    0x000000bd => "\xab",
    0x000000be => "\xad",
    0x000000c0 => "\x8e",
    0x000000c2 => "\x84",
    0x000000c7 => "\x80",
    0x000000c8 => "\x91",
    0x000000c9 => "\x90",
    0x000000ca => "\x92",
    0x000000cb => "\x94",
    0x000000ce => "\xa8",
    0x000000cf => "\x95",
    0x000000d4 => "\x99",
    0x000000d9 => "\x9d",
    0x000000db => "\x9e",
    0x000000dc => "\x9a",
    0x000000df => "\xe1",
    0x000000e0 => "\x85",
    0x000000e2 => "\x83",
    0x000000e7 => "\x87",
    0x000000e8 => "\x8a",
    0x000000e9 => "\x82",
    0x000000ea => "\x88",
    0x000000eb => "\x89",
    0x000000ee => "\x8c",
    0x000000ef => "\x8b",
    0x000000f3 => "\xa2",
    0x000000f4 => "\x93",
    0x000000f7 => "\xf6",
    0x000000f9 => "\x97",
    0x000000fa => "\xa3",
    0x000000fb => "\x96",
    0x000000fc => "\x81",
    0x00000192 => "\x9f",
    0x00000393 => "\xe2",
    0x00000398 => "\xe9",
    0x000003a3 => "\xe4",
    0x000003a6 => "\xe8",
    0x000003a9 => "\xea",
    0x000003b1 => "\xe0",
    0x000003b4 => "\xeb",
    0x000003b5 => "\xee",
    0x000003c0 => "\xe3",
    0x000003c3 => "\xe5",
    0x000003c4 => "\xe7",
    0x000003c6 => "\xed",
    0x00002017 => "\x8d",
    0x0000207f => "\xfc",
    0x00002219 => "\xf9",
    0x0000221a => "\xfb",
    0x0000221e => "\xec",
    0x00002229 => "\xef",
    0x00002248 => "\xf7",
    0x00002261 => "\xf0",
    0x00002264 => "\xf3",
    0x00002265 => "\xf2",
    0x00002310 => "\xa9",
    0x00002320 => "\xf4",
    0x00002321 => "\xf5",
    0x00002500 => "\xc4",
    0x00002502 => "\xb3",
    0x0000250c => "\xda",
    0x00002510 => "\xbf",
    0x00002514 => "\xc0",
    0x00002518 => "\xd9",
    0x0000251c => "\xc3",
    0x00002524 => "\xb4",
    0x0000252c => "\xc2",
    0x00002534 => "\xc1",
    0x0000253c => "\xc5",
    0x00002550 => "\xcd",
    0x00002551 => "\xba",
    0x00002552 => "\xd5",
    0x00002553 => "\xd6",
    0x00002554 => "\xc9",
    0x00002555 => "\xb8",
    0x00002556 => "\xb7",
    0x00002557 => "\xbb",
    0x00002558 => "\xd4",
    0x00002559 => "\xd3",
    0x0000255a => "\xc8",
    0x0000255b => "\xbe",
    0x0000255c => "\xbd",
    0x0000255d => "\xbc",
    0x0000255e => "\xc6",
    0x0000255f => "\xc7",
    0x00002560 => "\xcc",
    0x00002561 => "\xb5",
    0x00002562 => "\xb6",
    0x00002563 => "\xb9",
    0x00002564 => "\xd1",
    0x00002565 => "\xd2",
    0x00002566 => "\xcb",
    0x00002567 => "\xcf",
    0x00002568 => "\xd0",
    0x00002569 => "\xca",
    0x0000256a => "\xd8",
    0x0000256b => "\xd7",
    0x0000256c => "\xce",
    0x00002580 => "\xdf",
    0x00002584 => "\xdc",
    0x00002588 => "\xdb",
    0x0000258c => "\xdd",
    0x00002590 => "\xde",
    0x00002591 => "\xb0",
    0x00002592 => "\xb1",
    0x00002593 => "\xb2",
    0x000025a0 => "\xfe",
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

Locale::RecodeData::IBM863 - Conversion routines for IBM863

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM863.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991
 alias CP863
 alias 863

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
    80 |  000000C7 | LATIN CAPITAL LETTER C WITH CEDILLA
    81 |  000000FC | LATIN SMALL LETTER U WITH DIAERESIS
    82 |  000000E9 | LATIN SMALL LETTER E WITH ACUTE
    83 |  000000E2 | LATIN SMALL LETTER A WITH CIRCUMFLEX
    84 |  000000C2 | LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    85 |  000000E0 | LATIN SMALL LETTER A WITH GRAVE
    86 |  000000B6 | PILCROW SIGN
    87 |  000000E7 | LATIN SMALL LETTER C WITH CEDILLA
    88 |  000000EA | LATIN SMALL LETTER E WITH CIRCUMFLEX
    89 |  000000EB | LATIN SMALL LETTER E WITH DIAERESIS
    8A |  000000E8 | LATIN SMALL LETTER E WITH GRAVE
    8B |  000000EF | LATIN SMALL LETTER I WITH DIAERESIS
    8C |  000000EE | LATIN SMALL LETTER I WITH CIRCUMFLEX
    8D |  00002017 | DOUBLE LOW LINE
    8E |  000000C0 | LATIN CAPITAL LETTER A WITH GRAVE
    8F |  000000A7 | SECTION SIGN
    90 |  000000C9 | LATIN CAPITAL LETTER E WITH ACUTE
    91 |  000000C8 | LATIN CAPITAL LETTER E WITH GRAVE
    92 |  000000CA | LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    93 |  000000F4 | LATIN SMALL LETTER O WITH CIRCUMFLEX
    94 |  000000CB | LATIN CAPITAL LETTER E WITH DIAERESIS
    95 |  000000CF | LATIN CAPITAL LETTER I WITH DIAERESIS
    96 |  000000FB | LATIN SMALL LETTER U WITH CIRCUMFLEX
    97 |  000000F9 | LATIN SMALL LETTER U WITH GRAVE
    98 |  000000A4 | CURRENCY SIGN
    99 |  000000D4 | LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    9A |  000000DC | LATIN CAPITAL LETTER U WITH DIAERESIS
    9B |  000000A2 | CENT SIGN
    9C |  000000A3 | POUND SIGN
    9D |  000000D9 | LATIN CAPITAL LETTER U WITH GRAVE
    9E |  000000DB | LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    9F |  00000192 | LATIN SMALL LETTER F WITH HOOK
    A0 |  000000A6 | BROKEN BAR
    A1 |  000000B4 | ACUTE ACCENT
    A2 |  000000F3 | LATIN SMALL LETTER O WITH ACUTE
    A3 |  000000FA | LATIN SMALL LETTER U WITH ACUTE
    A4 |  000000A8 | DIAERESIS
    A5 |  000000B8 | CEDILLA
    A6 |  000000B3 | SUPERSCRIPT THREE
    A7 |  000000AF | MACRON
    A8 |  000000CE | LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    A9 |  00002310 | REVERSED NOT SIGN
    AA |  000000AC | NOT SIGN
    AB |  000000BD | VULGAR FRACTION ONE HALF
    AC |  000000BC | VULGAR FRACTION ONE QUARTER
    AD |  000000BE | VULGAR FRACTION THREE QUARTERS
    AE |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    AF |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    B0 |  00002591 | LIGHT SHADE
    B1 |  00002592 | MEDIUM SHADE
    B2 |  00002593 | DARK SHADE
    B3 |  00002502 | BOX DRAWINGS LIGHT VERTICAL
    B4 |  00002524 | BOX DRAWINGS LIGHT VERTICAL AND LEFT
    B5 |  00002561 | BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
    B6 |  00002562 | BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
    B7 |  00002556 | BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
    B8 |  00002555 | BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
    B9 |  00002563 | BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    BA |  00002551 | BOX DRAWINGS DOUBLE VERTICAL
    BB |  00002557 | BOX DRAWINGS DOUBLE DOWN AND LEFT
    BC |  0000255D | BOX DRAWINGS DOUBLE UP AND LEFT
    BD |  0000255C | BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
    BE |  0000255B | BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
    BF |  00002510 | BOX DRAWINGS LIGHT DOWN AND LEFT
    C0 |  00002514 | BOX DRAWINGS LIGHT UP AND RIGHT
    C1 |  00002534 | BOX DRAWINGS LIGHT UP AND HORIZONTAL
    C2 |  0000252C | BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    C3 |  0000251C | BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    C4 |  00002500 | BOX DRAWINGS LIGHT HORIZONTAL
    C5 |  0000253C | BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    C6 |  0000255E | BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
    C7 |  0000255F | BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
    C8 |  0000255A | BOX DRAWINGS DOUBLE UP AND RIGHT
    C9 |  00002554 | BOX DRAWINGS DOUBLE DOWN AND RIGHT
    CA |  00002569 | BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    CB |  00002566 | BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    CC |  00002560 | BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    CD |  00002550 | BOX DRAWINGS DOUBLE HORIZONTAL
    CE |  0000256C | BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    CF |  00002567 | BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
    D0 |  00002568 | BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
    D1 |  00002564 | BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
    D2 |  00002565 | BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
    D3 |  00002559 | BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
    D4 |  00002558 | BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
    D5 |  00002552 | BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
    D6 |  00002553 | BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
    D7 |  0000256B | BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
    D8 |  0000256A | BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
    D9 |  00002518 | BOX DRAWINGS LIGHT UP AND LEFT
    DA |  0000250C | BOX DRAWINGS LIGHT DOWN AND RIGHT
    DB |  00002588 | FULL BLOCK
    DC |  00002584 | LOWER HALF BLOCK
    DD |  0000258C | LEFT HALF BLOCK
    DE |  00002590 | RIGHT HALF BLOCK
    DF |  00002580 | UPPER HALF BLOCK
    E0 |  000003B1 | GREEK SMALL LETTER ALPHA
    E1 |  000000DF | LATIN SMALL LETTER SHARP S (German)
    E2 |  00000393 | GREEK CAPITAL LETTER GAMMA
    E3 |  000003C0 | GREEK SMALL LETTER PI
    E4 |  000003A3 | GREEK CAPITAL LETTER SIGMA
    E5 |  000003C3 | GREEK SMALL LETTER SIGMA
    E6 |  000000B5 | MICRO SIGN
    E7 |  000003C4 | GREEK SMALL LETTER TAU
    E8 |  000003A6 | GREEK CAPITAL LETTER PHI
    E9 |  00000398 | GREEK CAPITAL LETTER THETA
    EA |  000003A9 | GREEK CAPITAL LETTER OMEGA
    EB |  000003B4 | GREEK SMALL LETTER DELTA
    EC |  0000221E | INFINITY
    ED |  000003C6 | GREEK SMALL LETTER PHI
    EE |  000003B5 | GREEK SMALL LETTER EPSILON
    EF |  00002229 | INTERSECTION
    F0 |  00002261 | IDENTICAL TO
    F1 |  000000B1 | PLUS-MINUS SIGN
    F2 |  00002265 | GREATER-THAN OR EQUAL TO
    F3 |  00002264 | LESS-THAN OR EQUAL TO
    F4 |  00002320 | TOP HALF INTEGRAL
    F5 |  00002321 | BOTTOM HALF INTEGRAL
    F6 |  000000F7 | DIVISION SIGN
    F7 |  00002248 | ALMOST EQUAL TO
    F8 |  000000B0 | DEGREE SIGN
    F9 |  00002219 | BULLET OPERATOR
    FA |  000000B7 | MIDDLE DOT
    FB |  0000221A | SQUARE ROOT
    FC |  0000207F | SUPERSCRIPT LATIN SMALL LETTER N
    FD |  000000B2 | SUPERSCRIPT TWO
    FE |  000025A0 | BLACK SQUARE
    FF |  000000A0 | NO-BREAK SPACE


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
