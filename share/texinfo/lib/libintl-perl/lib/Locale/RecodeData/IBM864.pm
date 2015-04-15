#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM864.pm,v 1.1 2011-10-12 23:51:39 pertusus Exp $

# Conversion routines for IBM864.
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

package Locale::RecodeData::IBM864;

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
    0x066a,
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
    0x00b0,
    0x00b7,
    0x2219,
    0x221a,
    0x2592,
    0x2500,
    0x2502,
    0x253c,
    0x2524,
    0x252c,
    0x251c,
    0x2534,
    0x2510,
    0x250c,
    0x2514,
    0x2518,
    0x03b2,
    0x221e,
    0x03c6,
    0x00b1,
    0x00bd,
    0x00bc,
    0x2248,
    0x00ab,
    0x00bb,
    0xfef7,
    0xfef8,
    0xfffd,
    0xfffd,
    0xfefb,
    0xfefc,
    0xfffd,
    0x00a0,
    0x00ad,
    0xfe82,
    0x00a3,
    0x00a4,
    0xfe84,
    0xfffd,
    0xfffd,
    0xfe8e,
    0xfe8f,
    0xfe95,
    0xfe99,
    0x060c,
    0xfe9d,
    0xfea1,
    0xfea5,
    0x0660,
    0x0661,
    0x0662,
    0x0663,
    0x0664,
    0x0665,
    0x0666,
    0x0667,
    0x0668,
    0x0669,
    0xfed1,
    0x061b,
    0xfeb1,
    0xfeb5,
    0xfeb9,
    0x061f,
    0x00a2,
    0xfe80,
    0xfe81,
    0xfe83,
    0xfe85,
    0xfeca,
    0xfe8b,
    0xfe8d,
    0xfe91,
    0xfe93,
    0xfe97,
    0xfe9b,
    0xfe9f,
    0xfea3,
    0xfea7,
    0xfea9,
    0xfeab,
    0xfead,
    0xfeaf,
    0xfeb3,
    0xfeb7,
    0xfebb,
    0xfebf,
    0xfec1,
    0xfec5,
    0xfecb,
    0xfecf,
    0x00a6,
    0x00ac,
    0x00f7,
    0x00d7,
    0xfec9,
    0x0640,
    0xfed3,
    0xfed7,
    0xfedb,
    0xfedf,
    0xfee3,
    0xfee7,
    0xfeeb,
    0xfeed,
    0xfeef,
    0xfef3,
    0xfebd,
    0xfecc,
    0xfece,
    0xfecd,
    0xfee1,
    0xfe7d,
    0x0651,
    0xfee5,
    0xfee9,
    0xfeec,
    0xfef0,
    0xfef2,
    0xfed0,
    0xfed5,
    0xfef5,
    0xfef6,
    0xfedd,
    0xfed9,
    0xfef1,
    0x25a0,
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
    "\x24",
    "\xd9\xaa",
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
    "\xc2\xb0",
    "\xc2\xb7",
    "\xe2\x88\x99",
    "\xe2\x88\x9a",
    "\xe2\x96\x92",
    "\xe2\x94\x80",
    "\xe2\x94\x82",
    "\xe2\x94\xbc",
    "\xe2\x94\xa4",
    "\xe2\x94\xac",
    "\xe2\x94\x9c",
    "\xe2\x94\xb4",
    "\xe2\x94\x90",
    "\xe2\x94\x8c",
    "\xe2\x94\x94",
    "\xe2\x94\x98",
    "\xce\xb2",
    "\xe2\x88\x9e",
    "\xcf\x86",
    "\xc2\xb1",
    "\xc2\xbd",
    "\xc2\xbc",
    "\xe2\x89\x88",
    "\xc2\xab",
    "\xc2\xbb",
    "\xef\xbb\xb7",
    "\xef\xbb\xb8",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbb\xbb",
    "\xef\xbb\xbc",
    "\xef\xbf\xbd",
    "\xc2\xa0",
    "\xc2\xad",
    "\xef\xba\x82",
    "\xc2\xa3",
    "\xc2\xa4",
    "\xef\xba\x84",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xba\x8e",
    "\xef\xba\x8f",
    "\xef\xba\x95",
    "\xef\xba\x99",
    "\xd8\x8c",
    "\xef\xba\x9d",
    "\xef\xba\xa1",
    "\xef\xba\xa5",
    "\xd9\xa0",
    "\xd9\xa1",
    "\xd9\xa2",
    "\xd9\xa3",
    "\xd9\xa4",
    "\xd9\xa5",
    "\xd9\xa6",
    "\xd9\xa7",
    "\xd9\xa8",
    "\xd9\xa9",
    "\xef\xbb\x91",
    "\xd8\x9b",
    "\xef\xba\xb1",
    "\xef\xba\xb5",
    "\xef\xba\xb9",
    "\xd8\x9f",
    "\xc2\xa2",
    "\xef\xba\x80",
    "\xef\xba\x81",
    "\xef\xba\x83",
    "\xef\xba\x85",
    "\xef\xbb\x8a",
    "\xef\xba\x8b",
    "\xef\xba\x8d",
    "\xef\xba\x91",
    "\xef\xba\x93",
    "\xef\xba\x97",
    "\xef\xba\x9b",
    "\xef\xba\x9f",
    "\xef\xba\xa3",
    "\xef\xba\xa7",
    "\xef\xba\xa9",
    "\xef\xba\xab",
    "\xef\xba\xad",
    "\xef\xba\xaf",
    "\xef\xba\xb3",
    "\xef\xba\xb7",
    "\xef\xba\xbb",
    "\xef\xba\xbf",
    "\xef\xbb\x81",
    "\xef\xbb\x85",
    "\xef\xbb\x8b",
    "\xef\xbb\x8f",
    "\xc2\xa6",
    "\xc2\xac",
    "\xc3\xb7",
    "\xc3\x97",
    "\xef\xbb\x89",
    "\xd9\x80",
    "\xef\xbb\x93",
    "\xef\xbb\x97",
    "\xef\xbb\x9b",
    "\xef\xbb\x9f",
    "\xef\xbb\xa3",
    "\xef\xbb\xa7",
    "\xef\xbb\xab",
    "\xef\xbb\xad",
    "\xef\xbb\xaf",
    "\xef\xbb\xb3",
    "\xef\xba\xbd",
    "\xef\xbb\x8c",
    "\xef\xbb\x8e",
    "\xef\xbb\x8d",
    "\xef\xbb\xa1",
    "\xef\xb9\xbd",
    "\xd9\x91",
    "\xef\xbb\xa5",
    "\xef\xbb\xa9",
    "\xef\xbb\xac",
    "\xef\xbb\xb0",
    "\xef\xbb\xb2",
    "\xef\xbb\x90",
    "\xef\xbb\x95",
    "\xef\xbb\xb5",
    "\xef\xbb\xb6",
    "\xef\xbb\x9d",
    "\xef\xbb\x99",
    "\xef\xbb\xb1",
    "\xe2\x96\xa0",
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
    0x00000024 => "\x24",
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
    0x000000a2 => "\xc0",
    0x000000a3 => "\xa3",
    0x000000a4 => "\xa4",
    0x000000a6 => "\xdb",
    0x000000ab => "\x97",
    0x000000ac => "\xdc",
    0x000000ad => "\xa1",
    0x000000b0 => "\x80",
    0x000000b1 => "\x93",
    0x000000b7 => "\x81",
    0x000000bb => "\x98",
    0x000000bc => "\x95",
    0x000000bd => "\x94",
    0x000000d7 => "\xde",
    0x000000f7 => "\xdd",
    0x000003b2 => "\x90",
    0x000003c6 => "\x92",
    0x0000060c => "\xac",
    0x0000061b => "\xbb",
    0x0000061f => "\xbf",
    0x00000640 => "\xe0",
    0x00000651 => "\xf1",
    0x00000660 => "\xb0",
    0x00000661 => "\xb1",
    0x00000662 => "\xb2",
    0x00000663 => "\xb3",
    0x00000664 => "\xb4",
    0x00000665 => "\xb5",
    0x00000666 => "\xb6",
    0x00000667 => "\xb7",
    0x00000668 => "\xb8",
    0x00000669 => "\xb9",
    0x0000066a => "\x25",
    0x00002219 => "\x82",
    0x0000221a => "\x83",
    0x0000221e => "\x91",
    0x00002248 => "\x96",
    0x00002500 => "\x85",
    0x00002502 => "\x86",
    0x0000250c => "\x8d",
    0x00002510 => "\x8c",
    0x00002514 => "\x8e",
    0x00002518 => "\x8f",
    0x0000251c => "\x8a",
    0x00002524 => "\x88",
    0x0000252c => "\x89",
    0x00002534 => "\x8b",
    0x0000253c => "\x87",
    0x00002592 => "\x84",
    0x000025a0 => "\xfe",
    0x0000fe7d => "\xf0",
    0x0000fe80 => "\xc1",
    0x0000fe81 => "\xc2",
    0x0000fe82 => "\xa2",
    0x0000fe83 => "\xc3",
    0x0000fe84 => "\xa5",
    0x0000fe85 => "\xc4",
    0x0000fe8b => "\xc6",
    0x0000fe8d => "\xc7",
    0x0000fe8e => "\xa8",
    0x0000fe8f => "\xa9",
    0x0000fe91 => "\xc8",
    0x0000fe93 => "\xc9",
    0x0000fe95 => "\xaa",
    0x0000fe97 => "\xca",
    0x0000fe99 => "\xab",
    0x0000fe9b => "\xcb",
    0x0000fe9d => "\xad",
    0x0000fe9f => "\xcc",
    0x0000fea1 => "\xae",
    0x0000fea3 => "\xcd",
    0x0000fea5 => "\xaf",
    0x0000fea7 => "\xce",
    0x0000fea9 => "\xcf",
    0x0000feab => "\xd0",
    0x0000fead => "\xd1",
    0x0000feaf => "\xd2",
    0x0000feb1 => "\xbc",
    0x0000feb3 => "\xd3",
    0x0000feb5 => "\xbd",
    0x0000feb7 => "\xd4",
    0x0000feb9 => "\xbe",
    0x0000febb => "\xd5",
    0x0000febd => "\xeb",
    0x0000febf => "\xd6",
    0x0000fec1 => "\xd7",
    0x0000fec5 => "\xd8",
    0x0000fec9 => "\xdf",
    0x0000feca => "\xc5",
    0x0000fecb => "\xd9",
    0x0000fecc => "\xec",
    0x0000fecd => "\xee",
    0x0000fece => "\xed",
    0x0000fecf => "\xda",
    0x0000fed0 => "\xf7",
    0x0000fed1 => "\xba",
    0x0000fed3 => "\xe1",
    0x0000fed5 => "\xf8",
    0x0000fed7 => "\xe2",
    0x0000fed9 => "\xfc",
    0x0000fedb => "\xe3",
    0x0000fedd => "\xfb",
    0x0000fedf => "\xe4",
    0x0000fee1 => "\xef",
    0x0000fee3 => "\xe5",
    0x0000fee5 => "\xf2",
    0x0000fee7 => "\xe6",
    0x0000fee9 => "\xf3",
    0x0000feeb => "\xe7",
    0x0000feec => "\xf4",
    0x0000feed => "\xe8",
    0x0000feef => "\xe9",
    0x0000fef0 => "\xf5",
    0x0000fef1 => "\xfd",
    0x0000fef2 => "\xf6",
    0x0000fef3 => "\xea",
    0x0000fef5 => "\xf9",
    0x0000fef6 => "\xfa",
    0x0000fef7 => "\x99",
    0x0000fef8 => "\x9a",
    0x0000fefb => "\x9d",
    0x0000fefc => "\x9e",
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

Locale::RecodeData::IBM864 - Conversion routines for IBM864

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM864.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 automatically generated from the charDB
 alias CP864

Please note that aliases listed above are not necessarily valid!

=head1 CHARACTER TABLE

The following table is sorted in the same order as the original charmap.
All character codes are in hexadecimal.  Please read 'ISO-10646' as
'ISO-10646-UCS4'.

 Local | ISO-10646 | Description
-------+-----------+-------------------------------------------------
    00 |  00000000 | NULL
    01 |  00000001 | START OF HEADING
    02 |  00000002 | START OF TEXT
    03 |  00000003 | END OF TEXT
    04 |  00000004 | END OF TRANSMISSION
    05 |  00000005 | ENQUIRY
    06 |  00000006 | ACKNOWLEDGE
    07 |  00000007 | BELL
    08 |  00000008 | BACKSPACE
    09 |  00000009 | HORIZONTAL TABULATION
    0A |  0000000A | LINE FEED
    0B |  0000000B | VERTICAL TABULATION
    0C |  0000000C | FORM FEED
    0D |  0000000D | CARRIAGE RETURN
    0E |  0000000E | SHIFT OUT
    0F |  0000000F | SHIFT IN
    10 |  00000010 | DATA LINK ESCAPE
    11 |  00000011 | DEVICE CONTROL ONE
    12 |  00000012 | DEVICE CONTROL TWO
    13 |  00000013 | DEVICE CONTROL THREE
    14 |  00000014 | DEVICE CONTROL FOUR
    15 |  00000015 | NEGATIVE ACKNOWLEDGE
    16 |  00000016 | SYNCHRONOUS IDLE
    17 |  00000017 | END OF TRANSMISSION BLOCK
    18 |  00000018 | CANCEL
    19 |  00000019 | END OF MEDIUM
    1A |  0000001A | SUBSTITUTE
    1B |  0000001B | ESCAPE
    1C |  0000001C | FILE SEPARATOR
    1D |  0000001D | GROUP SEPARATOR
    1E |  0000001E | RECORD SEPARATOR
    1F |  0000001F | UNIT SEPARATOR
    20 |  00000020 | SPACE
    21 |  00000021 | EXCLAMATION MARK
    22 |  00000022 | QUOTATION MARK
    23 |  00000023 | NUMBER SIGN
    24 |  00000024 | DOLLAR SIGN
    25 |  0000066A | ARABIC PERCENT SIGN
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
    7F |  0000007F | DELETE
    80 |  000000B0 | DEGREE SIGN
    81 |  000000B7 | MIDDLE DOT
    82 |  00002219 | BULLET OPERATOR
    83 |  0000221A | SQUARE ROOT
    84 |  00002592 | MEDIUM SHADE
    85 |  00002500 | BOX DRAWINGS LIGHT HORIZONTAL
    86 |  00002502 | BOX DRAWINGS LIGHT VERTICAL
    87 |  0000253C | BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
    88 |  00002524 | BOX DRAWINGS LIGHT VERTICAL AND LEFT
    89 |  0000252C | BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
    8A |  0000251C | BOX DRAWINGS LIGHT VERTICAL AND RIGHT
    8B |  00002534 | BOX DRAWINGS LIGHT UP AND HORIZONTAL
    8C |  00002510 | BOX DRAWINGS LIGHT DOWN AND LEFT
    8D |  0000250C | BOX DRAWINGS LIGHT DOWN AND RIGHT
    8E |  00002514 | BOX DRAWINGS LIGHT UP AND RIGHT
    8F |  00002518 | BOX DRAWINGS LIGHT UP AND LEFT
    90 |  000003B2 | GREEK SMALL LETTER BETA
    91 |  0000221E | INFINITY
    92 |  000003C6 | GREEK SMALL LETTER PHI
    93 |  000000B1 | PLUS-MINUS SIGN
    94 |  000000BD | VULGAR FRACTION ONE HALF
    95 |  000000BC | VULGAR FRACTION ONE QUARTER
    96 |  00002248 | ALMOST EQUAL TO
    97 |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    98 |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    99 |  0000FEF7 | ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
    9A |  0000FEF8 | ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
    9D |  0000FEFB | ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM
    9E |  0000FEFC | ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    A0 |  000000A0 | NO-BREAK SPACE
    A1 |  000000AD | SOFT HYPHEN
    A2 |  0000FE82 | ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
    A3 |  000000A3 | POUND SIGN
    A4 |  000000A4 | CURRENCY SIGN
    A5 |  0000FE84 | ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
    A8 |  0000FE8E | ARABIC LETTER ALEF FINAL FORM
    A9 |  0000FE8F | ARABIC LETTER BEH ISOLATED FORM
    AA |  0000FE95 | ARABIC LETTER TEH ISOLATED FORM
    AB |  0000FE99 | ARABIC LETTER THEH ISOLATED FORM
    AC |  0000060C | ARABIC COMMA
    AD |  0000FE9D | ARABIC LETTER JEEM ISOLATED FORM
    AE |  0000FEA1 | ARABIC LETTER HAH ISOLATED FORM
    AF |  0000FEA5 | ARABIC LETTER KHAH ISOLATED FORM
    B0 |  00000660 | ARABIC-INDIC DIGIT ZERO
    B1 |  00000661 | ARABIC-INDIC DIGIT ONE
    B2 |  00000662 | ARABIC-INDIC DIGIT TWO
    B3 |  00000663 | ARABIC-INDIC DIGIT THREE
    B4 |  00000664 | ARABIC-INDIC DIGIT FOUR
    B5 |  00000665 | ARABIC-INDIC DIGIT FIVE
    B6 |  00000666 | ARABIC-INDIC DIGIT SIX
    B7 |  00000667 | ARABIC-INDIC DIGIT SEVEN
    B8 |  00000668 | ARABIC-INDIC DIGIT EIGHT
    B9 |  00000669 | ARABIC-INDIC DIGIT NINE
    BA |  0000FED1 | ARABIC LETTER FEH ISOLATED FORM
    BB |  0000061B | ARABIC SEMICOLON
    BC |  0000FEB1 | ARABIC LETTER SEEN ISOLATED FORM
    BD |  0000FEB5 | ARABIC LETTER SHEEN ISOLATED FORM
    BE |  0000FEB9 | ARABIC LETTER SAD ISOLATED FORM
    BF |  0000061F | ARABIC QUESTION MARK
    C0 |  000000A2 | CENT SIGN
    C1 |  0000FE80 | ARABIC LETTER HAMZA ISOLATED FORM
    C2 |  0000FE81 | ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
    C3 |  0000FE83 | ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
    C4 |  0000FE85 | ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
    C5 |  0000FECA | ARABIC LETTER AIN FINAL FORM
    C6 |  0000FE8B | ARABIC LETTER YEH WITH HAMZA ABOVE INITIAL FORM
    C7 |  0000FE8D | ARABIC LETTER ALEF ISOLATED FORM
    C8 |  0000FE91 | ARABIC LETTER BEH INITIAL FORM
    C9 |  0000FE93 | ARABIC LETTER TEH MARBUTA ISOLATED FORM
    CA |  0000FE97 | ARABIC LETTER TEH INITIAL FORM
    CB |  0000FE9B | ARABIC LETTER THEH INITIAL FORM
    CC |  0000FE9F | ARABIC LETTER JEEM INITIAL FORM
    CD |  0000FEA3 | ARABIC LETTER HAH INITIAL FORM
    CE |  0000FEA7 | ARABIC LETTER KHAH INITIAL FORM
    CF |  0000FEA9 | ARABIC LETTER DAL ISOLATED FORM
    D0 |  0000FEAB | ARABIC LETTER THAL ISOLATED FORM
    D1 |  0000FEAD | ARABIC LETTER REH ISOLATED FORM
    D2 |  0000FEAF | ARABIC LETTER ZAIN ISOLATED FORM
    D3 |  0000FEB3 | ARABIC LETTER SEEN INITIAL FORM
    D4 |  0000FEB7 | ARABIC LETTER SHEEN INITIAL FORM
    D5 |  0000FEBB | ARABIC LETTER SAD INITIAL FORM
    D6 |  0000FEBF | ARABIC LETTER DAD INITIAL FORM
    D7 |  0000FEC1 | ARABIC LETTER TAH ISOLATED FORM
    D8 |  0000FEC5 | ARABIC LETTER ZAH ISOLATED FORM
    D9 |  0000FECB | ARABIC LETTER AIN INITIAL FORM
    DA |  0000FECF | ARABIC LETTER GHAIN INITIAL FORM
    DB |  000000A6 | BROKEN BAR
    DC |  000000AC | NOT SIGN
    DD |  000000F7 | DIVISION SIGN
    DE |  000000D7 | MULTIPLICATION SIGN
    DF |  0000FEC9 | ARABIC LETTER AIN ISOLATED FORM
    E0 |  00000640 | ARABIC TATWEEL
    E1 |  0000FED3 | ARABIC LETTER FEH INITIAL FORM
    E2 |  0000FED7 | ARABIC LETTER QAF INITIAL FORM
    E3 |  0000FEDB | ARABIC LETTER KAF INITIAL FORM
    E4 |  0000FEDF | ARABIC LETTER LAM INITIAL FORM
    E5 |  0000FEE3 | ARABIC LETTER MEEM INITIAL FORM
    E6 |  0000FEE7 | ARABIC LETTER NOON INITIAL FORM
    E7 |  0000FEEB | ARABIC LETTER HEH INITIAL FORM
    E8 |  0000FEED | ARABIC LETTER WAW ISOLATED FORM
    E9 |  0000FEEF | ARABIC LETTER ALEF MAKSURA ISOLATED FORM
    EA |  0000FEF3 | ARABIC LETTER YEH INITIAL FORM
    EB |  0000FEBD | ARABIC LETTER DAD ISOLATED FORM
    EC |  0000FECC | ARABIC LETTER AIN MEDIAL FORM
    ED |  0000FECE | ARABIC LETTER GHAIN FINAL FORM
    EE |  0000FECD | ARABIC LETTER GHAIN ISOLATED FORM
    EF |  0000FEE1 | ARABIC LETTER MEEM ISOLATED FORM
    F0 |  0000FE7D | ARABIC SHADDA MEDIAL FORM
    F1 |  00000651 | ARABIC SHADDA
    F2 |  0000FEE5 | ARABIC LETTER NOON ISOLATED FORM
    F3 |  0000FEE9 | ARABIC LETTER HEH ISOLATED FORM
    F4 |  0000FEEC | ARABIC LETTER HEH MEDIAL FORM
    F5 |  0000FEF0 | ARABIC LETTER ALEF MAKSURA FINAL FORM
    F6 |  0000FEF2 | ARABIC LETTER YEH FINAL FORM
    F7 |  0000FED0 | ARABIC LETTER GHAIN MEDIAL FORM
    F8 |  0000FED5 | ARABIC LETTER QAF ISOLATED FORM
    F9 |  0000FEF5 | ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
    FA |  0000FEF6 | ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
    FB |  0000FEDD | ARABIC LETTER LAM ISOLATED FORM
    FC |  0000FED9 | ARABIC LETTER KAF ISOLATED FORM
    FD |  0000FEF1 | ARABIC LETTER YEH ISOLATED FORM
    FE |  000025A0 | BLACK SQUARE


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
