#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM869.pm,v 1.1 2011-10-12 23:51:39 pertusus Exp $

# Conversion routines for IBM869.
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

package Locale::RecodeData::IBM869;

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
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0xfffd,
    0x0386,
    0xfffd,
    0x00b7,
    0x00ac,
    0x00a6,
    0x2018,
    0x2019,
    0x0388,
    0x2015,
    0x0389,
    0x038a,
    0x03aa,
    0x038c,
    0xfffd,
    0xfffd,
    0x038e,
    0x03ab,
    0x00a9,
    0x038f,
    0x00b2,
    0x00b3,
    0x03ac,
    0x00a3,
    0x03ad,
    0x03ae,
    0x03af,
    0x03ca,
    0x0390,
    0x03cc,
    0x03cd,
    0x0391,
    0x0392,
    0x0393,
    0x0394,
    0x0395,
    0x0396,
    0x0397,
    0x00bd,
    0x0398,
    0x0399,
    0x00ab,
    0x00bb,
    0x2591,
    0x2592,
    0x2593,
    0x2502,
    0x2524,
    0x039a,
    0x039b,
    0x039c,
    0x039d,
    0x2563,
    0x2551,
    0x2557,
    0x255d,
    0x039e,
    0x039f,
    0x2510,
    0x2514,
    0x2534,
    0x252c,
    0x251c,
    0x2500,
    0x253c,
    0x03a0,
    0x03a1,
    0x255a,
    0x2554,
    0x2569,
    0x2566,
    0x2560,
    0x2550,
    0x256c,
    0x03a3,
    0x03a4,
    0x03a5,
    0x03a6,
    0x03a7,
    0x03a8,
    0x03a9,
    0x03b1,
    0x03b2,
    0x03b3,
    0x2518,
    0x250c,
    0x2588,
    0x2584,
    0x03b4,
    0x03b5,
    0x2580,
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
    0x03c1,
    0x03c3,
    0x03c2,
    0x03c4,
    0x0384,
    0x00ad,
    0x00b1,
    0x03c5,
    0x03c6,
    0x03c7,
    0x00a7,
    0x03c8,
    0x0385,
    0x00b0,
    0x00a8,
    0x03c9,
    0x03cb,
    0x03b0,
    0x03ce,
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
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xce\x86",
    "\xef\xbf\xbd",
    "\xc2\xb7",
    "\xc2\xac",
    "\xc2\xa6",
    "\xe2\x80\x98",
    "\xe2\x80\x99",
    "\xce\x88",
    "\xe2\x80\x95",
    "\xce\x89",
    "\xce\x8a",
    "\xce\xaa",
    "\xce\x8c",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xce\x8e",
    "\xce\xab",
    "\xc2\xa9",
    "\xce\x8f",
    "\xc2\xb2",
    "\xc2\xb3",
    "\xce\xac",
    "\xc2\xa3",
    "\xce\xad",
    "\xce\xae",
    "\xce\xaf",
    "\xcf\x8a",
    "\xce\x90",
    "\xcf\x8c",
    "\xcf\x8d",
    "\xce\x91",
    "\xce\x92",
    "\xce\x93",
    "\xce\x94",
    "\xce\x95",
    "\xce\x96",
    "\xce\x97",
    "\xc2\xbd",
    "\xce\x98",
    "\xce\x99",
    "\xc2\xab",
    "\xc2\xbb",
    "\xe2\x96\x91",
    "\xe2\x96\x92",
    "\xe2\x96\x93",
    "\xe2\x94\x82",
    "\xe2\x94\xa4",
    "\xce\x9a",
    "\xce\x9b",
    "\xce\x9c",
    "\xce\x9d",
    "\xe2\x95\xa3",
    "\xe2\x95\x91",
    "\xe2\x95\x97",
    "\xe2\x95\x9d",
    "\xce\x9e",
    "\xce\x9f",
    "\xe2\x94\x90",
    "\xe2\x94\x94",
    "\xe2\x94\xb4",
    "\xe2\x94\xac",
    "\xe2\x94\x9c",
    "\xe2\x94\x80",
    "\xe2\x94\xbc",
    "\xce\xa0",
    "\xce\xa1",
    "\xe2\x95\x9a",
    "\xe2\x95\x94",
    "\xe2\x95\xa9",
    "\xe2\x95\xa6",
    "\xe2\x95\xa0",
    "\xe2\x95\x90",
    "\xe2\x95\xac",
    "\xce\xa3",
    "\xce\xa4",
    "\xce\xa5",
    "\xce\xa6",
    "\xce\xa7",
    "\xce\xa8",
    "\xce\xa9",
    "\xce\xb1",
    "\xce\xb2",
    "\xce\xb3",
    "\xe2\x94\x98",
    "\xe2\x94\x8c",
    "\xe2\x96\x88",
    "\xe2\x96\x84",
    "\xce\xb4",
    "\xce\xb5",
    "\xe2\x96\x80",
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
    "\xcf\x81",
    "\xcf\x83",
    "\xcf\x82",
    "\xcf\x84",
    "\xce\x84",
    "\xc2\xad",
    "\xc2\xb1",
    "\xcf\x85",
    "\xcf\x86",
    "\xcf\x87",
    "\xc2\xa7",
    "\xcf\x88",
    "\xce\x85",
    "\xc2\xb0",
    "\xc2\xa8",
    "\xcf\x89",
    "\xcf\x8b",
    "\xce\xb0",
    "\xcf\x8e",
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
    0x000000a3 => "\x9c",
    0x000000a6 => "\x8a",
    0x000000a7 => "\xf5",
    0x000000a8 => "\xf9",
    0x000000a9 => "\x97",
    0x000000ab => "\xae",
    0x000000ac => "\x89",
    0x000000ad => "\xf0",
    0x000000b0 => "\xf8",
    0x000000b1 => "\xf1",
    0x000000b2 => "\x99",
    0x000000b3 => "\x9a",
    0x000000b7 => "\x88",
    0x000000bb => "\xaf",
    0x000000bd => "\xab",
    0x00000384 => "\xef",
    0x00000385 => "\xf7",
    0x00000386 => "\x86",
    0x00000388 => "\x8d",
    0x00000389 => "\x8f",
    0x0000038a => "\x90",
    0x0000038c => "\x92",
    0x0000038e => "\x95",
    0x0000038f => "\x98",
    0x00000390 => "\xa1",
    0x00000391 => "\xa4",
    0x00000392 => "\xa5",
    0x00000393 => "\xa6",
    0x00000394 => "\xa7",
    0x00000395 => "\xa8",
    0x00000396 => "\xa9",
    0x00000397 => "\xaa",
    0x00000398 => "\xac",
    0x00000399 => "\xad",
    0x0000039a => "\xb5",
    0x0000039b => "\xb6",
    0x0000039c => "\xb7",
    0x0000039d => "\xb8",
    0x0000039e => "\xbd",
    0x0000039f => "\xbe",
    0x000003a0 => "\xc6",
    0x000003a1 => "\xc7",
    0x000003a3 => "\xcf",
    0x000003a4 => "\xd0",
    0x000003a5 => "\xd1",
    0x000003a6 => "\xd2",
    0x000003a7 => "\xd3",
    0x000003a8 => "\xd4",
    0x000003a9 => "\xd5",
    0x000003aa => "\x91",
    0x000003ab => "\x96",
    0x000003ac => "\x9b",
    0x000003ad => "\x9d",
    0x000003ae => "\x9e",
    0x000003af => "\x9f",
    0x000003b0 => "\xfc",
    0x000003b1 => "\xd6",
    0x000003b2 => "\xd7",
    0x000003b3 => "\xd8",
    0x000003b4 => "\xdd",
    0x000003b5 => "\xde",
    0x000003b6 => "\xe0",
    0x000003b7 => "\xe1",
    0x000003b8 => "\xe2",
    0x000003b9 => "\xe3",
    0x000003ba => "\xe4",
    0x000003bb => "\xe5",
    0x000003bc => "\xe6",
    0x000003bd => "\xe7",
    0x000003be => "\xe8",
    0x000003bf => "\xe9",
    0x000003c0 => "\xea",
    0x000003c1 => "\xeb",
    0x000003c2 => "\xed",
    0x000003c3 => "\xec",
    0x000003c4 => "\xee",
    0x000003c5 => "\xf2",
    0x000003c6 => "\xf3",
    0x000003c7 => "\xf4",
    0x000003c8 => "\xf6",
    0x000003c9 => "\xfa",
    0x000003ca => "\xa0",
    0x000003cb => "\xfb",
    0x000003cc => "\xa2",
    0x000003cd => "\xa3",
    0x000003ce => "\xfd",
    0x00002015 => "\x8e",
    0x00002018 => "\x8b",
    0x00002019 => "\x8c",
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
    0x00002554 => "\xc9",
    0x00002557 => "\xbb",
    0x0000255a => "\xc8",
    0x0000255d => "\xbc",
    0x00002560 => "\xcc",
    0x00002563 => "\xb9",
    0x00002566 => "\xcb",
    0x00002569 => "\xca",
    0x0000256c => "\xce",
    0x00002580 => "\xdf",
    0x00002584 => "\xdc",
    0x00002588 => "\xdb",
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

Locale::RecodeData::IBM869 - Conversion routines for IBM869

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM869.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991
 alias CP869
 alias 869
 alias CP-GR

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
    86 |  00000386 | GREEK CAPITAL LETTER ALPHA WITH TONOS
    88 |  000000B7 | MIDDLE DOT
    89 |  000000AC | NOT SIGN
    8A |  000000A6 | BROKEN BAR
    8B |  00002018 | LEFT SINGLE QUOTATION MARK
    8C |  00002019 | RIGHT SINGLE QUOTATION MARK
    8D |  00000388 | GREEK CAPITAL LETTER EPSILON WITH TONOS
    8E |  00002015 | HORIZONTAL BAR
    8F |  00000389 | GREEK CAPITAL LETTER ETA WITH TONOS
    90 |  0000038A | GREEK CAPITAL LETTER IOTA WITH TONOS
    91 |  000003AA | GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    92 |  0000038C | GREEK CAPITAL LETTER OMICRON WITH TONOS
    95 |  0000038E | GREEK CAPITAL LETTER UPSILON WITH TONOS
    96 |  000003AB | GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    97 |  000000A9 | COPYRIGHT SIGN
    98 |  0000038F | GREEK CAPITAL LETTER OMEGA WITH TONOS
    99 |  000000B2 | SUPERSCRIPT TWO
    9A |  000000B3 | SUPERSCRIPT THREE
    9B |  000003AC | GREEK SMALL LETTER ALPHA WITH TONOS
    9C |  000000A3 | POUND SIGN
    9D |  000003AD | GREEK SMALL LETTER EPSILON WITH TONOS
    9E |  000003AE | GREEK SMALL LETTER ETA WITH TONOS
    9F |  000003AF | GREEK SMALL LETTER IOTA WITH TONOS
    A0 |  000003CA | GREEK SMALL LETTER IOTA WITH DIALYTIKA
    A1 |  00000390 | GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    A2 |  000003CC | GREEK SMALL LETTER OMICRON WITH TONOS
    A3 |  000003CD | GREEK SMALL LETTER UPSILON WITH TONOS
    A4 |  00000391 | GREEK CAPITAL LETTER ALPHA
    A5 |  00000392 | GREEK CAPITAL LETTER BETA
    A6 |  00000393 | GREEK CAPITAL LETTER GAMMA
    A7 |  00000394 | GREEK CAPITAL LETTER DELTA
    A8 |  00000395 | GREEK CAPITAL LETTER EPSILON
    A9 |  00000396 | GREEK CAPITAL LETTER ZETA
    AA |  00000397 | GREEK CAPITAL LETTER ETA
    AB |  000000BD | VULGAR FRACTION ONE HALF
    AC |  00000398 | GREEK CAPITAL LETTER THETA
    AD |  00000399 | GREEK CAPITAL LETTER IOTA
    AE |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    AF |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    B0 |  00002591 | LIGHT SHADE
    B1 |  00002592 | MEDIUM SHADE
    B2 |  00002593 | DARK SHADE
    B3 |  00002502 | BOX DRAWINGS LIGHT VERTICAL
    B4 |  00002524 | BOX DRAWINGS LIGHT VERTICAL AND LEFT
    B5 |  0000039A | GREEK CAPITAL LETTER KAPPA
    B6 |  0000039B | GREEK CAPITAL LETTER LAMDA
    B7 |  0000039C | GREEK CAPITAL LETTER MU
    B8 |  0000039D | GREEK CAPITAL LETTER NU
    B9 |  00002563 | BOX DRAWINGS DOUBLE VERTICAL AND LEFT
    BA |  00002551 | BOX DRAWINGS DOUBLE VERTICAL
    BB |  00002557 | BOX DRAWINGS DOUBLE DOWN AND LEFT
    BC |  0000255D | BOX DRAWINGS DOUBLE UP AND LEFT
    BD |  0000039E | GREEK CAPITAL LETTER XI
    BE |  0000039F | GREEK CAPITAL LETTER OMICRON
    BF |  00002510 | BOX DRAWINGS LIGHT DOWN AND LEFT
    C0 |  00002514 | BOX DRAWINGS LIGHT UP AND RIGHT
    C1 |  00002534 | BOX DRAWINGS LIGHT UP AND HORIZONTAL
    C2 |  0000252C | BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    C3 |  0000251C | BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    C4 |  00002500 | BOX DRAWINGS LIGHT HORIZONTAL
    C5 |  0000253C | BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    C6 |  000003A0 | GREEK CAPITAL LETTER PI
    C7 |  000003A1 | GREEK CAPITAL LETTER RHO
    C8 |  0000255A | BOX DRAWINGS DOUBLE UP AND RIGHT
    C9 |  00002554 | BOX DRAWINGS DOUBLE DOWN AND RIGHT
    CA |  00002569 | BOX DRAWINGS DOUBLE UP AND HORIZONTAL
    CB |  00002566 | BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
    CC |  00002560 | BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
    CD |  00002550 | BOX DRAWINGS DOUBLE HORIZONTAL
    CE |  0000256C | BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
    CF |  000003A3 | GREEK CAPITAL LETTER SIGMA
    D0 |  000003A4 | GREEK CAPITAL LETTER TAU
    D1 |  000003A5 | GREEK CAPITAL LETTER UPSILON
    D2 |  000003A6 | GREEK CAPITAL LETTER PHI
    D3 |  000003A7 | GREEK CAPITAL LETTER CHI
    D4 |  000003A8 | GREEK CAPITAL LETTER PSI
    D5 |  000003A9 | GREEK CAPITAL LETTER OMEGA
    D6 |  000003B1 | GREEK SMALL LETTER ALPHA
    D7 |  000003B2 | GREEK SMALL LETTER BETA
    D8 |  000003B3 | GREEK SMALL LETTER GAMMA
    D9 |  00002518 | BOX DRAWINGS LIGHT UP AND LEFT
    DA |  0000250C | BOX DRAWINGS LIGHT DOWN AND RIGHT
    DB |  00002588 | FULL BLOCK
    DC |  00002584 | LOWER HALF BLOCK
    DD |  000003B4 | GREEK SMALL LETTER DELTA
    DE |  000003B5 | GREEK SMALL LETTER EPSILON
    DF |  00002580 | UPPER HALF BLOCK
    E0 |  000003B6 | GREEK SMALL LETTER ZETA
    E1 |  000003B7 | GREEK SMALL LETTER ETA
    E2 |  000003B8 | GREEK SMALL LETTER THETA
    E3 |  000003B9 | GREEK SMALL LETTER IOTA
    E4 |  000003BA | GREEK SMALL LETTER KAPPA
    E5 |  000003BB | GREEK SMALL LETTER LAMDA
    E6 |  000003BC | GREEK SMALL LETTER MU
    E7 |  000003BD | GREEK SMALL LETTER NU
    E8 |  000003BE | GREEK SMALL LETTER XI
    E9 |  000003BF | GREEK SMALL LETTER OMICRON
    EA |  000003C0 | GREEK SMALL LETTER PI
    EB |  000003C1 | GREEK SMALL LETTER RHO
    EC |  000003C3 | GREEK SMALL LETTER SIGMA
    ED |  000003C2 | GREEK SMALL LETTER FINAL SIGMA
    EE |  000003C4 | GREEK SMALL LETTER TAU
    EF |  00000384 | GREEK TONOS
    F0 |  000000AD | SOFT HYPHEN
    F1 |  000000B1 | PLUS-MINUS SIGN
    F2 |  000003C5 | GREEK SMALL LETTER UPSILON
    F3 |  000003C6 | GREEK SMALL LETTER PHI
    F4 |  000003C7 | GREEK SMALL LETTER CHI
    F5 |  000000A7 | SECTION SIGN
    F6 |  000003C8 | GREEK SMALL LETTER PSI
    F7 |  00000385 | GREEK DIALYTIKA TONOS
    F8 |  000000B0 | DEGREE SIGN
    F9 |  000000A8 | DIAERESIS
    FA |  000003C9 | GREEK SMALL LETTER OMEGA
    FB |  000003CB | GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    FC |  000003B0 | GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    FD |  000003CE | GREEK SMALL LETTER OMEGA WITH TONOS
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
