#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM880.pm,v 1.1 2011-10-12 23:51:40 pertusus Exp $

# Conversion routines for IBM880.
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

package Locale::RecodeData::IBM880;

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
    0xfffd,
    0x0452,
    0x0453,
    0x0451,
    0xfffd,
    0x0455,
    0x0456,
    0x0457,
    0x0458,
    0x005b,
    0x002e,
    0x003c,
    0x0028,
    0x002b,
    0x0021,
    0x0026,
    0x0459,
    0x045a,
    0x045b,
    0x045c,
    0xfffd,
    0x045f,
    0x042a,
    0x2116,
    0x0402,
    0x005d,
    0x0024,
    0x002a,
    0x0029,
    0x003b,
    0x005e,
    0x002d,
    0x002f,
    0x0403,
    0x0401,
    0xfffd,
    0x0405,
    0x0406,
    0x0407,
    0x0408,
    0x0409,
    0x00a6,
    0x002c,
    0x0025,
    0x005f,
    0x003e,
    0x003f,
    0x040a,
    0x040b,
    0x040c,
    0xfffd,
    0xfffd,
    0x040f,
    0x044e,
    0x0430,
    0x0431,
    0xfffd,
    0x003a,
    0x0023,
    0x0040,
    0x0027,
    0x003d,
    0x0022,
    0x0446,
    0x0061,
    0x0062,
    0x0063,
    0x0064,
    0x0065,
    0x0066,
    0x0067,
    0x0068,
    0x0069,
    0x0434,
    0x0435,
    0x0444,
    0x0433,
    0x0445,
    0x0438,
    0x0439,
    0x006a,
    0x006b,
    0x006c,
    0x006d,
    0x006e,
    0x006f,
    0x0070,
    0x0071,
    0x0072,
    0x043a,
    0x043b,
    0x043c,
    0x043d,
    0x043e,
    0x043f,
    0x044f,
    0xfffd,
    0x0073,
    0x0074,
    0x0075,
    0x0076,
    0x0077,
    0x0078,
    0x0079,
    0x007a,
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
    0x0425,
    0x0418,
    0x0419,
    0x041a,
    0x041b,
    0x041c,
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
    0x041d,
    0x041e,
    0x041f,
    0x042f,
    0x0420,
    0x0421,
    0x005c,
    0x00a4,
    0x0053,
    0x0054,
    0x0055,
    0x0056,
    0x0057,
    0x0058,
    0x0059,
    0x005a,
    0x0422,
    0x0423,
    0x0416,
    0x0412,
    0x042c,
    0x042b,
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
    0x0417,
    0x0428,
    0x042d,
    0x0429,
    0x0427,
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
    "\xef\xbf\xbd",
    "\xd1\x92",
    "\xd1\x93",
    "\xd1\x91",
    "\xef\xbf\xbd",
    "\xd1\x95",
    "\xd1\x96",
    "\xd1\x97",
    "\xd1\x98",
    "\x5b",
    "\x2e",
    "\x3c",
    "\x28",
    "\x2b",
    "\x21",
    "\x26",
    "\xd1\x99",
    "\xd1\x9a",
    "\xd1\x9b",
    "\xd1\x9c",
    "\xef\xbf\xbd",
    "\xd1\x9f",
    "\xd0\xaa",
    "\xe2\x84\x96",
    "\xd0\x82",
    "\x5d",
    "\x24",
    "\x2a",
    "\x29",
    "\x3b",
    "\x5e",
    "\x2d",
    "\x2f",
    "\xd0\x83",
    "\xd0\x81",
    "\xef\xbf\xbd",
    "\xd0\x85",
    "\xd0\x86",
    "\xd0\x87",
    "\xd0\x88",
    "\xd0\x89",
    "\xc2\xa6",
    "\x2c",
    "\x25",
    "\x5f",
    "\x3e",
    "\x3f",
    "\xd0\x8a",
    "\xd0\x8b",
    "\xd0\x8c",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xd0\x8f",
    "\xd1\x8e",
    "\xd0\xb0",
    "\xd0\xb1",
    "\xef\xbf\xbd",
    "\x3a",
    "\x23",
    "\x40",
    "\x27",
    "\x3d",
    "\x22",
    "\xd1\x86",
    "\x61",
    "\x62",
    "\x63",
    "\x64",
    "\x65",
    "\x66",
    "\x67",
    "\x68",
    "\x69",
    "\xd0\xb4",
    "\xd0\xb5",
    "\xd1\x84",
    "\xd0\xb3",
    "\xd1\x85",
    "\xd0\xb8",
    "\xd0\xb9",
    "\x6a",
    "\x6b",
    "\x6c",
    "\x6d",
    "\x6e",
    "\x6f",
    "\x70",
    "\x71",
    "\x72",
    "\xd0\xba",
    "\xd0\xbb",
    "\xd0\xbc",
    "\xd0\xbd",
    "\xd0\xbe",
    "\xd0\xbf",
    "\xd1\x8f",
    "\xef\xbf\xbd",
    "\x73",
    "\x74",
    "\x75",
    "\x76",
    "\x77",
    "\x78",
    "\x79",
    "\x7a",
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
    "\xd0\xa5",
    "\xd0\x98",
    "\xd0\x99",
    "\xd0\x9a",
    "\xd0\x9b",
    "\xd0\x9c",
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
    "\xd0\x9d",
    "\xd0\x9e",
    "\xd0\x9f",
    "\xd0\xaf",
    "\xd0\xa0",
    "\xd0\xa1",
    "\x5c",
    "\xc2\xa4",
    "\x53",
    "\x54",
    "\x55",
    "\x56",
    "\x57",
    "\x58",
    "\x59",
    "\x5a",
    "\xd0\xa2",
    "\xd0\xa3",
    "\xd0\x96",
    "\xd0\x92",
    "\xd0\xac",
    "\xd0\xab",
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
    "\xd0\x97",
    "\xd0\xa8",
    "\xd0\xad",
    "\xd0\xa9",
    "\xd0\xa7",
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
    0x00000021 => "\x4f",
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
    0x0000005b => "\x4a",
    0x0000005c => "\xe0",
    0x0000005d => "\x5a",
    0x0000005e => "\x5f",
    0x0000005f => "\x6d",
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
    0x000000a4 => "\xe1",
    0x000000a6 => "\x6a",
    0x00000401 => "\x63",
    0x00000402 => "\x59",
    0x00000403 => "\x62",
    0x00000405 => "\x65",
    0x00000406 => "\x66",
    0x00000407 => "\x67",
    0x00000408 => "\x68",
    0x00000409 => "\x69",
    0x0000040a => "\x70",
    0x0000040b => "\x71",
    0x0000040c => "\x72",
    0x0000040f => "\x75",
    0x00000410 => "\xb9",
    0x00000411 => "\xba",
    0x00000412 => "\xed",
    0x00000413 => "\xbf",
    0x00000414 => "\xbc",
    0x00000415 => "\xbd",
    0x00000416 => "\xec",
    0x00000417 => "\xfa",
    0x00000418 => "\xcb",
    0x00000419 => "\xcc",
    0x0000041a => "\xcd",
    0x0000041b => "\xce",
    0x0000041c => "\xcf",
    0x0000041d => "\xda",
    0x0000041e => "\xdb",
    0x0000041f => "\xdc",
    0x00000420 => "\xde",
    0x00000421 => "\xdf",
    0x00000422 => "\xea",
    0x00000423 => "\xeb",
    0x00000424 => "\xbe",
    0x00000425 => "\xca",
    0x00000426 => "\xbb",
    0x00000427 => "\xfe",
    0x00000428 => "\xfb",
    0x00000429 => "\xfd",
    0x0000042a => "\x57",
    0x0000042b => "\xef",
    0x0000042c => "\xee",
    0x0000042d => "\xfc",
    0x0000042e => "\xb8",
    0x0000042f => "\xdd",
    0x00000430 => "\x77",
    0x00000431 => "\x78",
    0x00000432 => "\xaf",
    0x00000433 => "\x8d",
    0x00000434 => "\x8a",
    0x00000435 => "\x8b",
    0x00000436 => "\xae",
    0x00000437 => "\xb2",
    0x00000438 => "\x8f",
    0x00000439 => "\x90",
    0x0000043a => "\x9a",
    0x0000043b => "\x9b",
    0x0000043c => "\x9c",
    0x0000043d => "\x9d",
    0x0000043e => "\x9e",
    0x0000043f => "\x9f",
    0x00000440 => "\xaa",
    0x00000441 => "\xab",
    0x00000442 => "\xac",
    0x00000443 => "\xad",
    0x00000444 => "\x8c",
    0x00000445 => "\x8e",
    0x00000446 => "\x80",
    0x00000447 => "\xb6",
    0x00000448 => "\xb3",
    0x00000449 => "\xb5",
    0x0000044a => "\xb7",
    0x0000044b => "\xb1",
    0x0000044c => "\xb0",
    0x0000044d => "\xb4",
    0x0000044e => "\x76",
    0x0000044f => "\xa0",
    0x00000451 => "\x44",
    0x00000452 => "\x42",
    0x00000453 => "\x43",
    0x00000455 => "\x46",
    0x00000456 => "\x47",
    0x00000457 => "\x48",
    0x00000458 => "\x49",
    0x00000459 => "\x51",
    0x0000045a => "\x52",
    0x0000045b => "\x53",
    0x0000045c => "\x54",
    0x0000045f => "\x56",
    0x00002116 => "\x58",
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

Locale::RecodeData::IBM880 - Conversion routines for IBM880

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM880.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM NLS RM Vol2 SE09-8002-01, March 1990
 alias CP880
 alias EBCDIC-CYRILLIC

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
    42 |  00000452 | CYRILLIC SMALL LETTER DJE (Serbocroatian)
    43 |  00000453 | CYRILLIC SMALL LETTER GJE
    44 |  00000451 | CYRILLIC SMALL LETTER IO
    46 |  00000455 | CYRILLIC SMALL LETTER DZE
    47 |  00000456 | CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    48 |  00000457 | CYRILLIC SMALL LETTER YI (Ukrainian)
    49 |  00000458 | CYRILLIC SMALL LETTER JE
    4A |  0000005B | LEFT SQUARE BRACKET
    4B |  0000002E | FULL STOP
    4C |  0000003C | LESS-THAN SIGN
    4D |  00000028 | LEFT PARENTHESIS
    4E |  0000002B | PLUS SIGN
    4F |  00000021 | EXCLAMATION MARK
    50 |  00000026 | AMPERSAND
    51 |  00000459 | CYRILLIC SMALL LETTER LJE
    52 |  0000045A | CYRILLIC SMALL LETTER NJE
    53 |  0000045B | CYRILLIC SMALL LETTER TSHE (Serbocroatian)
    54 |  0000045C | CYRILLIC SMALL LETTER KJE
    56 |  0000045F | CYRILLIC SMALL LETTER DZHE
    57 |  0000042A | CYRILLIC CAPITAL LETTER HARD SIGN
    58 |  00002116 | NUMERO SIGN
    59 |  00000402 | CYRILLIC CAPITAL LETTER DJE (Serbocroatian)
    5A |  0000005D | RIGHT SQUARE BRACKET
    5B |  00000024 | DOLLAR SIGN
    5C |  0000002A | ASTERISK
    5D |  00000029 | RIGHT PARENTHESIS
    5E |  0000003B | SEMICOLON
    5F |  0000005E | CIRCUMFLEX ACCENT
    60 |  0000002D | HYPHEN-MINUS
    61 |  0000002F | SOLIDUS
    62 |  00000403 | CYRILLIC CAPITAL LETTER GJE
    63 |  00000401 | CYRILLIC CAPITAL LETTER IO
    65 |  00000405 | CYRILLIC CAPITAL LETTER DZE
    66 |  00000406 | CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    67 |  00000407 | CYRILLIC CAPITAL LETTER YI (Ukrainian)
    68 |  00000408 | CYRILLIC CAPITAL LETTER JE
    69 |  00000409 | CYRILLIC CAPITAL LETTER LJE
    6A |  000000A6 | BROKEN BAR
    6B |  0000002C | COMMA
    6C |  00000025 | PERCENT SIGN
    6D |  0000005F | LOW LINE
    6E |  0000003E | GREATER-THAN SIGN
    6F |  0000003F | QUESTION MARK
    70 |  0000040A | CYRILLIC CAPITAL LETTER NJE
    71 |  0000040B | CYRILLIC CAPITAL LETTER TSHE (Serbocroatian)
    72 |  0000040C | CYRILLIC CAPITAL LETTER KJE
    75 |  0000040F | CYRILLIC CAPITAL LETTER DZHE
    76 |  0000044E | CYRILLIC SMALL LETTER YU
    77 |  00000430 | CYRILLIC SMALL LETTER A
    78 |  00000431 | CYRILLIC SMALL LETTER BE
    7A |  0000003A | COLON
    7B |  00000023 | NUMBER SIGN
    7C |  00000040 | COMMERCIAL AT
    7D |  00000027 | APOSTROPHE
    7E |  0000003D | EQUALS SIGN
    7F |  00000022 | QUOTATION MARK
    80 |  00000446 | CYRILLIC SMALL LETTER TSE
    81 |  00000061 | LATIN SMALL LETTER A
    82 |  00000062 | LATIN SMALL LETTER B
    83 |  00000063 | LATIN SMALL LETTER C
    84 |  00000064 | LATIN SMALL LETTER D
    85 |  00000065 | LATIN SMALL LETTER E
    86 |  00000066 | LATIN SMALL LETTER F
    87 |  00000067 | LATIN SMALL LETTER G
    88 |  00000068 | LATIN SMALL LETTER H
    89 |  00000069 | LATIN SMALL LETTER I
    8A |  00000434 | CYRILLIC SMALL LETTER DE
    8B |  00000435 | CYRILLIC SMALL LETTER IE
    8C |  00000444 | CYRILLIC SMALL LETTER EF
    8D |  00000433 | CYRILLIC SMALL LETTER GHE
    8E |  00000445 | CYRILLIC SMALL LETTER HA
    8F |  00000438 | CYRILLIC SMALL LETTER I
    90 |  00000439 | CYRILLIC SMALL LETTER SHORT I
    91 |  0000006A | LATIN SMALL LETTER J
    92 |  0000006B | LATIN SMALL LETTER K
    93 |  0000006C | LATIN SMALL LETTER L
    94 |  0000006D | LATIN SMALL LETTER M
    95 |  0000006E | LATIN SMALL LETTER N
    96 |  0000006F | LATIN SMALL LETTER O
    97 |  00000070 | LATIN SMALL LETTER P
    98 |  00000071 | LATIN SMALL LETTER Q
    99 |  00000072 | LATIN SMALL LETTER R
    9A |  0000043A | CYRILLIC SMALL LETTER KA
    9B |  0000043B | CYRILLIC SMALL LETTER EL
    9C |  0000043C | CYRILLIC SMALL LETTER EM
    9D |  0000043D | CYRILLIC SMALL LETTER EN
    9E |  0000043E | CYRILLIC SMALL LETTER O
    9F |  0000043F | CYRILLIC SMALL LETTER PE
    A0 |  0000044F | CYRILLIC SMALL LETTER YA
    A2 |  00000073 | LATIN SMALL LETTER S
    A3 |  00000074 | LATIN SMALL LETTER T
    A4 |  00000075 | LATIN SMALL LETTER U
    A5 |  00000076 | LATIN SMALL LETTER V
    A6 |  00000077 | LATIN SMALL LETTER W
    A7 |  00000078 | LATIN SMALL LETTER X
    A8 |  00000079 | LATIN SMALL LETTER Y
    A9 |  0000007A | LATIN SMALL LETTER Z
    AA |  00000440 | CYRILLIC SMALL LETTER ER
    AB |  00000441 | CYRILLIC SMALL LETTER ES
    AC |  00000442 | CYRILLIC SMALL LETTER TE
    AD |  00000443 | CYRILLIC SMALL LETTER U
    AE |  00000436 | CYRILLIC SMALL LETTER ZHE
    AF |  00000432 | CYRILLIC SMALL LETTER VE
    B0 |  0000044C | CYRILLIC SMALL LETTER SOFT SIGN
    B1 |  0000044B | CYRILLIC SMALL LETTER YERU
    B2 |  00000437 | CYRILLIC SMALL LETTER ZE
    B3 |  00000448 | CYRILLIC SMALL LETTER SHA
    B4 |  0000044D | CYRILLIC SMALL LETTER E
    B5 |  00000449 | CYRILLIC SMALL LETTER SHCHA
    B6 |  00000447 | CYRILLIC SMALL LETTER CHE
    B7 |  0000044A | CYRILLIC SMALL LETTER HARD SIGN
    B8 |  0000042E | CYRILLIC CAPITAL LETTER YU
    B9 |  00000410 | CYRILLIC CAPITAL LETTER A
    BA |  00000411 | CYRILLIC CAPITAL LETTER BE
    BB |  00000426 | CYRILLIC CAPITAL LETTER TSE
    BC |  00000414 | CYRILLIC CAPITAL LETTER DE
    BD |  00000415 | CYRILLIC CAPITAL LETTER IE
    BE |  00000424 | CYRILLIC CAPITAL LETTER EF
    BF |  00000413 | CYRILLIC CAPITAL LETTER GHE
    C1 |  00000041 | LATIN CAPITAL LETTER A
    C2 |  00000042 | LATIN CAPITAL LETTER B
    C3 |  00000043 | LATIN CAPITAL LETTER C
    C4 |  00000044 | LATIN CAPITAL LETTER D
    C5 |  00000045 | LATIN CAPITAL LETTER E
    C6 |  00000046 | LATIN CAPITAL LETTER F
    C7 |  00000047 | LATIN CAPITAL LETTER G
    C8 |  00000048 | LATIN CAPITAL LETTER H
    C9 |  00000049 | LATIN CAPITAL LETTER I
    CA |  00000425 | CYRILLIC CAPITAL LETTER HA
    CB |  00000418 | CYRILLIC CAPITAL LETTER I
    CC |  00000419 | CYRILLIC CAPITAL LETTER SHORT I
    CD |  0000041A | CYRILLIC CAPITAL LETTER KA
    CE |  0000041B | CYRILLIC CAPITAL LETTER EL
    CF |  0000041C | CYRILLIC CAPITAL LETTER EM
    D1 |  0000004A | LATIN CAPITAL LETTER J
    D2 |  0000004B | LATIN CAPITAL LETTER K
    D3 |  0000004C | LATIN CAPITAL LETTER L
    D4 |  0000004D | LATIN CAPITAL LETTER M
    D5 |  0000004E | LATIN CAPITAL LETTER N
    D6 |  0000004F | LATIN CAPITAL LETTER O
    D7 |  00000050 | LATIN CAPITAL LETTER P
    D8 |  00000051 | LATIN CAPITAL LETTER Q
    D9 |  00000052 | LATIN CAPITAL LETTER R
    DA |  0000041D | CYRILLIC CAPITAL LETTER EN
    DB |  0000041E | CYRILLIC CAPITAL LETTER O
    DC |  0000041F | CYRILLIC CAPITAL LETTER PE
    DD |  0000042F | CYRILLIC CAPITAL LETTER YA
    DE |  00000420 | CYRILLIC CAPITAL LETTER ER
    DF |  00000421 | CYRILLIC CAPITAL LETTER ES
    E0 |  0000005C | REVERSE SOLIDUS
    E1 |  000000A4 | CURRENCY SIGN
    E2 |  00000053 | LATIN CAPITAL LETTER S
    E3 |  00000054 | LATIN CAPITAL LETTER T
    E4 |  00000055 | LATIN CAPITAL LETTER U
    E5 |  00000056 | LATIN CAPITAL LETTER V
    E6 |  00000057 | LATIN CAPITAL LETTER W
    E7 |  00000058 | LATIN CAPITAL LETTER X
    E8 |  00000059 | LATIN CAPITAL LETTER Y
    E9 |  0000005A | LATIN CAPITAL LETTER Z
    EA |  00000422 | CYRILLIC CAPITAL LETTER TE
    EB |  00000423 | CYRILLIC CAPITAL LETTER U
    EC |  00000416 | CYRILLIC CAPITAL LETTER ZHE
    ED |  00000412 | CYRILLIC CAPITAL LETTER VE
    EE |  0000042C | CYRILLIC CAPITAL LETTER SOFT SIGN
    EF |  0000042B | CYRILLIC CAPITAL LETTER YERU
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
    FA |  00000417 | CYRILLIC CAPITAL LETTER ZE
    FB |  00000428 | CYRILLIC CAPITAL LETTER SHA
    FC |  0000042D | CYRILLIC CAPITAL LETTER E
    FD |  00000429 | CYRILLIC CAPITAL LETTER SHCHA
    FE |  00000427 | CYRILLIC CAPITAL LETTER CHE
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
