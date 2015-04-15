#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: MACTHAI.pm,v 1.1 2011-10-12 23:51:48 pertusus Exp $

# Conversion routines for MACTHAI.
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

package Locale::RecodeData::MACTHAI;

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
    0x00ab,
    0x00bb,
    0x2026,
    0xf88c,
    0xf88f,
    0xf892,
    0xf895,
    0xf898,
    0xf88b,
    0xf88e,
    0xf891,
    0xf894,
    0xf897,
    0x201c,
    0x201d,
    0xf899,
    0xfffd,
    0x2022,
    0xf884,
    0xf889,
    0xf885,
    0xf886,
    0xf887,
    0xf888,
    0xf88a,
    0xf88d,
    0xf890,
    0xf893,
    0xf896,
    0x2018,
    0x2019,
    0xfffd,
    0x00a0,
    0x0e01,
    0x0e02,
    0x0e03,
    0x0e04,
    0x0e05,
    0x0e06,
    0x0e07,
    0x0e08,
    0x0e09,
    0x0e0a,
    0x0e0b,
    0x0e0c,
    0x0e0d,
    0x0e0e,
    0x0e0f,
    0x0e10,
    0x0e11,
    0x0e12,
    0x0e13,
    0x0e14,
    0x0e15,
    0x0e16,
    0x0e17,
    0x0e18,
    0x0e19,
    0x0e1a,
    0x0e1b,
    0x0e1c,
    0x0e1d,
    0x0e1e,
    0x0e1f,
    0x0e20,
    0x0e21,
    0x0e22,
    0x0e23,
    0x0e24,
    0x0e25,
    0x0e26,
    0x0e27,
    0x0e28,
    0x0e29,
    0x0e2a,
    0x0e2b,
    0x0e2c,
    0x0e2d,
    0x0e2e,
    0x0e2f,
    0x0e30,
    0x0e31,
    0x0e32,
    0x0e33,
    0x0e34,
    0x0e35,
    0x0e36,
    0x0e37,
    0x0e38,
    0x0e39,
    0x0e3a,
    0xfeff,
    0x200b,
    0x2013,
    0x2014,
    0x0e3f,
    0x0e40,
    0x0e41,
    0x0e42,
    0x0e43,
    0x0e44,
    0x0e45,
    0x0e46,
    0x0e47,
    0x0e48,
    0x0e49,
    0x0e4a,
    0x0e4b,
    0x0e4c,
    0x0e4d,
    0x2122,
    0x0e4f,
    0x0e50,
    0x0e51,
    0x0e52,
    0x0e53,
    0x0e54,
    0x0e55,
    0x0e56,
    0x0e57,
    0x0e58,
    0x0e59,
    0x00ae,
    0x00a9,
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
    "\xc2\xab",
    "\xc2\xbb",
    "\xe2\x80\xa6",
    "\xef\xa2\x8c",
    "\xef\xa2\x8f",
    "\xef\xa2\x92",
    "\xef\xa2\x95",
    "\xef\xa2\x98",
    "\xef\xa2\x8b",
    "\xef\xa2\x8e",
    "\xef\xa2\x91",
    "\xef\xa2\x94",
    "\xef\xa2\x97",
    "\xe2\x80\x9c",
    "\xe2\x80\x9d",
    "\xef\xa2\x99",
    "\xef\xbf\xbd",
    "\xe2\x80\xa2",
    "\xef\xa2\x84",
    "\xef\xa2\x89",
    "\xef\xa2\x85",
    "\xef\xa2\x86",
    "\xef\xa2\x87",
    "\xef\xa2\x88",
    "\xef\xa2\x8a",
    "\xef\xa2\x8d",
    "\xef\xa2\x90",
    "\xef\xa2\x93",
    "\xef\xa2\x96",
    "\xe2\x80\x98",
    "\xe2\x80\x99",
    "\xef\xbf\xbd",
    "\xc2\xa0",
    "\xe0\xb8\x81",
    "\xe0\xb8\x82",
    "\xe0\xb8\x83",
    "\xe0\xb8\x84",
    "\xe0\xb8\x85",
    "\xe0\xb8\x86",
    "\xe0\xb8\x87",
    "\xe0\xb8\x88",
    "\xe0\xb8\x89",
    "\xe0\xb8\x8a",
    "\xe0\xb8\x8b",
    "\xe0\xb8\x8c",
    "\xe0\xb8\x8d",
    "\xe0\xb8\x8e",
    "\xe0\xb8\x8f",
    "\xe0\xb8\x90",
    "\xe0\xb8\x91",
    "\xe0\xb8\x92",
    "\xe0\xb8\x93",
    "\xe0\xb8\x94",
    "\xe0\xb8\x95",
    "\xe0\xb8\x96",
    "\xe0\xb8\x97",
    "\xe0\xb8\x98",
    "\xe0\xb8\x99",
    "\xe0\xb8\x9a",
    "\xe0\xb8\x9b",
    "\xe0\xb8\x9c",
    "\xe0\xb8\x9d",
    "\xe0\xb8\x9e",
    "\xe0\xb8\x9f",
    "\xe0\xb8\xa0",
    "\xe0\xb8\xa1",
    "\xe0\xb8\xa2",
    "\xe0\xb8\xa3",
    "\xe0\xb8\xa4",
    "\xe0\xb8\xa5",
    "\xe0\xb8\xa6",
    "\xe0\xb8\xa7",
    "\xe0\xb8\xa8",
    "\xe0\xb8\xa9",
    "\xe0\xb8\xaa",
    "\xe0\xb8\xab",
    "\xe0\xb8\xac",
    "\xe0\xb8\xad",
    "\xe0\xb8\xae",
    "\xe0\xb8\xaf",
    "\xe0\xb8\xb0",
    "\xe0\xb8\xb1",
    "\xe0\xb8\xb2",
    "\xe0\xb8\xb3",
    "\xe0\xb8\xb4",
    "\xe0\xb8\xb5",
    "\xe0\xb8\xb6",
    "\xe0\xb8\xb7",
    "\xe0\xb8\xb8",
    "\xe0\xb8\xb9",
    "\xe0\xb8\xba",
    "\xef\xbb\xbf",
    "\xe2\x80\x8b",
    "\xe2\x80\x93",
    "\xe2\x80\x94",
    "\xe0\xb8\xbf",
    "\xe0\xb9\x80",
    "\xe0\xb9\x81",
    "\xe0\xb9\x82",
    "\xe0\xb9\x83",
    "\xe0\xb9\x84",
    "\xe0\xb9\x85",
    "\xe0\xb9\x86",
    "\xe0\xb9\x87",
    "\xe0\xb9\x88",
    "\xe0\xb9\x89",
    "\xe0\xb9\x8a",
    "\xe0\xb9\x8b",
    "\xe0\xb9\x8c",
    "\xe0\xb9\x8d",
    "\xe2\x84\xa2",
    "\xe0\xb9\x8f",
    "\xe0\xb9\x90",
    "\xe0\xb9\x91",
    "\xe0\xb9\x92",
    "\xe0\xb9\x93",
    "\xe0\xb9\x94",
    "\xe0\xb9\x95",
    "\xe0\xb9\x96",
    "\xe0\xb9\x97",
    "\xe0\xb9\x98",
    "\xe0\xb9\x99",
    "\xc2\xae",
    "\xc2\xa9",
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
    0x000000a9 => "\xfb",
    0x000000ab => "\x80",
    0x000000ae => "\xfa",
    0x000000bb => "\x81",
    0x00000e01 => "\xa1",
    0x00000e02 => "\xa2",
    0x00000e03 => "\xa3",
    0x00000e04 => "\xa4",
    0x00000e05 => "\xa5",
    0x00000e06 => "\xa6",
    0x00000e07 => "\xa7",
    0x00000e08 => "\xa8",
    0x00000e09 => "\xa9",
    0x00000e0a => "\xaa",
    0x00000e0b => "\xab",
    0x00000e0c => "\xac",
    0x00000e0d => "\xad",
    0x00000e0e => "\xae",
    0x00000e0f => "\xaf",
    0x00000e10 => "\xb0",
    0x00000e11 => "\xb1",
    0x00000e12 => "\xb2",
    0x00000e13 => "\xb3",
    0x00000e14 => "\xb4",
    0x00000e15 => "\xb5",
    0x00000e16 => "\xb6",
    0x00000e17 => "\xb7",
    0x00000e18 => "\xb8",
    0x00000e19 => "\xb9",
    0x00000e1a => "\xba",
    0x00000e1b => "\xbb",
    0x00000e1c => "\xbc",
    0x00000e1d => "\xbd",
    0x00000e1e => "\xbe",
    0x00000e1f => "\xbf",
    0x00000e20 => "\xc0",
    0x00000e21 => "\xc1",
    0x00000e22 => "\xc2",
    0x00000e23 => "\xc3",
    0x00000e24 => "\xc4",
    0x00000e25 => "\xc5",
    0x00000e26 => "\xc6",
    0x00000e27 => "\xc7",
    0x00000e28 => "\xc8",
    0x00000e29 => "\xc9",
    0x00000e2a => "\xca",
    0x00000e2b => "\xcb",
    0x00000e2c => "\xcc",
    0x00000e2d => "\xcd",
    0x00000e2e => "\xce",
    0x00000e2f => "\xcf",
    0x00000e30 => "\xd0",
    0x00000e31 => "\xd1",
    0x00000e32 => "\xd2",
    0x00000e33 => "\xd3",
    0x00000e34 => "\xd4",
    0x00000e35 => "\xd5",
    0x00000e36 => "\xd6",
    0x00000e37 => "\xd7",
    0x00000e38 => "\xd8",
    0x00000e39 => "\xd9",
    0x00000e3a => "\xda",
    0x00000e3f => "\xdf",
    0x00000e40 => "\xe0",
    0x00000e41 => "\xe1",
    0x00000e42 => "\xe2",
    0x00000e43 => "\xe3",
    0x00000e44 => "\xe4",
    0x00000e45 => "\xe5",
    0x00000e46 => "\xe6",
    0x00000e47 => "\xe7",
    0x00000e48 => "\xe8",
    0x00000e49 => "\xe9",
    0x00000e4a => "\xea",
    0x00000e4b => "\xeb",
    0x00000e4c => "\xec",
    0x00000e4d => "\xed",
    0x00000e4f => "\xef",
    0x00000e50 => "\xf0",
    0x00000e51 => "\xf1",
    0x00000e52 => "\xf2",
    0x00000e53 => "\xf3",
    0x00000e54 => "\xf4",
    0x00000e55 => "\xf5",
    0x00000e56 => "\xf6",
    0x00000e57 => "\xf7",
    0x00000e58 => "\xf8",
    0x00000e59 => "\xf9",
    0x0000200b => "\xdc",
    0x00002013 => "\xdd",
    0x00002014 => "\xde",
    0x00002018 => "\x9d",
    0x00002019 => "\x9e",
    0x0000201c => "\x8d",
    0x0000201d => "\x8e",
    0x00002022 => "\x91",
    0x00002026 => "\x82",
    0x00002122 => "\xee",
    0x0000f884 => "\x92",
    0x0000f885 => "\x94",
    0x0000f886 => "\x95",
    0x0000f887 => "\x96",
    0x0000f888 => "\x97",
    0x0000f889 => "\x93",
    0x0000f88a => "\x98",
    0x0000f88b => "\x88",
    0x0000f88c => "\x83",
    0x0000f88d => "\x99",
    0x0000f88e => "\x89",
    0x0000f88f => "\x84",
    0x0000f890 => "\x9a",
    0x0000f891 => "\x8a",
    0x0000f892 => "\x85",
    0x0000f893 => "\x9b",
    0x0000f894 => "\x8b",
    0x0000f895 => "\x86",
    0x0000f896 => "\x9c",
    0x0000f897 => "\x8c",
    0x0000f898 => "\x87",
    0x0000f899 => "\x8f",
    0x0000feff => "\xdb",
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

Locale::RecodeData::MACTHAI - Conversion routines for MACTHAI

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for MACTHAI.

=head1 COMMENTS

The following comments have been extracted from the original charmap:


 This charmap has been generated automatically from GNU libiconv
 conversions.

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
    80 |  000000AB | LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    81 |  000000BB | RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    82 |  00002026 | HORIZONTAL ELLIPSIS
    83 |  0000F88C | E<lt>CJKE<gt>
    84 |  0000F88F | E<lt>CJKE<gt>
    85 |  0000F892 | E<lt>CJKE<gt>
    86 |  0000F895 | E<lt>CJKE<gt>
    87 |  0000F898 | E<lt>CJKE<gt>
    88 |  0000F88B | E<lt>CJKE<gt>
    89 |  0000F88E | E<lt>CJKE<gt>
    8A |  0000F891 | E<lt>CJKE<gt>
    8B |  0000F894 | E<lt>CJKE<gt>
    8C |  0000F897 | E<lt>CJKE<gt>
    8D |  0000201C | LEFT DOUBLE QUOTATION MARK
    8E |  0000201D | RIGHT DOUBLE QUOTATION MARK
    8F |  0000F899 | E<lt>CJKE<gt>
    91 |  00002022 | BULLET
    92 |  0000F884 | E<lt>CJKE<gt>
    93 |  0000F889 | E<lt>CJKE<gt>
    94 |  0000F885 | E<lt>CJKE<gt>
    95 |  0000F886 | E<lt>CJKE<gt>
    96 |  0000F887 | E<lt>CJKE<gt>
    97 |  0000F888 | E<lt>CJKE<gt>
    98 |  0000F88A | E<lt>CJKE<gt>
    99 |  0000F88D | E<lt>CJKE<gt>
    9A |  0000F890 | E<lt>CJKE<gt>
    9B |  0000F893 | E<lt>CJKE<gt>
    9C |  0000F896 | E<lt>CJKE<gt>
    9D |  00002018 | LEFT SINGLE QUOTATION MARK
    9E |  00002019 | RIGHT SINGLE QUOTATION MARK
    A0 |  000000A0 | NO-BREAK SPACE
    A1 |  00000E01 | THAI CHARACTER KO KAI
    A2 |  00000E02 | THAI CHARACTER KHO KHAI
    A3 |  00000E03 | THAI CHARACTER KHO KHUAT
    A4 |  00000E04 | THAI CHARACTER KHO KHWAI
    A5 |  00000E05 | THAI CHARACTER KHO KHON
    A6 |  00000E06 | THAI CHARACTER KHO RAKHANG
    A7 |  00000E07 | THAI CHARACTER NGO NGU
    A8 |  00000E08 | THAI CHARACTER CHO CHAN
    A9 |  00000E09 | THAI CHARACTER CHO CHING
    AA |  00000E0A | THAI CHARACTER CHO CHANG
    AB |  00000E0B | THAI CHARACTER SO SO
    AC |  00000E0C | THAI CHARACTER CHO CHOE
    AD |  00000E0D | THAI CHARACTER YO YING
    AE |  00000E0E | THAI CHARACTER DO CHADA
    AF |  00000E0F | THAI CHARACTER TO PATAK
    B0 |  00000E10 | THAI CHARACTER THO THAN
    B1 |  00000E11 | THAI CHARACTER THO NANGMONTHO
    B2 |  00000E12 | THAI CHARACTER THO PHUTHAO
    B3 |  00000E13 | THAI CHARACTER NO NEN
    B4 |  00000E14 | THAI CHARACTER DO DEK
    B5 |  00000E15 | THAI CHARACTER TO TAO
    B6 |  00000E16 | THAI CHARACTER THO THUNG
    B7 |  00000E17 | THAI CHARACTER THO THAHAN
    B8 |  00000E18 | THAI CHARACTER THO THONG
    B9 |  00000E19 | THAI CHARACTER NO NU
    BA |  00000E1A | THAI CHARACTER BO BAIMAI
    BB |  00000E1B | THAI CHARACTER PO PLA
    BC |  00000E1C | THAI CHARACTER PHO PHUNG
    BD |  00000E1D | THAI CHARACTER FO FA
    BE |  00000E1E | THAI CHARACTER PHO PHAN
    BF |  00000E1F | THAI CHARACTER FO FAN
    C0 |  00000E20 | THAI CHARACTER PHO SAMPHAO
    C1 |  00000E21 | THAI CHARACTER MO MA
    C2 |  00000E22 | THAI CHARACTER YO YAK
    C3 |  00000E23 | THAI CHARACTER RO RUA
    C4 |  00000E24 | THAI CHARACTER RU
    C5 |  00000E25 | THAI CHARACTER LO LING
    C6 |  00000E26 | THAI CHARACTER LU
    C7 |  00000E27 | THAI CHARACTER WO WAEN
    C8 |  00000E28 | THAI CHARACTER SO SALA
    C9 |  00000E29 | THAI CHARACTER SO RUSI
    CA |  00000E2A | THAI CHARACTER SO SUA
    CB |  00000E2B | THAI CHARACTER HO HIP
    CC |  00000E2C | THAI CHARACTER LO CHULA
    CD |  00000E2D | THAI CHARACTER O ANG
    CE |  00000E2E | THAI CHARACTER HO NOKHUK
    CF |  00000E2F | THAI CHARACTER PAIYANNOI
    D0 |  00000E30 | THAI CHARACTER SARA A
    D1 |  00000E31 | THAI CHARACTER MAI HAN-AKAT
    D2 |  00000E32 | THAI CHARACTER SARA AA
    D3 |  00000E33 | THAI CHARACTER SARA AM
    D4 |  00000E34 | THAI CHARACTER SARA I
    D5 |  00000E35 | THAI CHARACTER SARA II
    D6 |  00000E36 | THAI CHARACTER SARA UE
    D7 |  00000E37 | THAI CHARACTER SARA UEE
    D8 |  00000E38 | THAI CHARACTER SARA U
    D9 |  00000E39 | THAI CHARACTER SARA UU
    DA |  00000E3A | THAI CHARACTER PHINTHU
    DB |  0000FEFF | ZERO WIDTH NO-BREAK SPACE
    DC |  0000200B | ZERO WIDTH SPACE
    DD |  00002013 | EN DASH
    DE |  00002014 | EM DASH
    DF |  00000E3F | THAI CURRENCY SYMBOL BAHT
    E0 |  00000E40 | THAI CHARACTER SARA E
    E1 |  00000E41 | THAI CHARACTER SARA AE
    E2 |  00000E42 | THAI CHARACTER SARA O
    E3 |  00000E43 | THAI CHARACTER SARA AI MAIMUAN
    E4 |  00000E44 | THAI CHARACTER SARA AI MAIMALAI
    E5 |  00000E45 | THAI CHARACTER LAKKHANGYAO
    E6 |  00000E46 | THAI CHARACTER MAIYAMOK
    E7 |  00000E47 | THAI CHARACTER MAITAIKHU
    E8 |  00000E48 | THAI CHARACTER MAI EK
    E9 |  00000E49 | THAI CHARACTER MAI THO
    EA |  00000E4A | THAI CHARACTER MAI TRI
    EB |  00000E4B | THAI CHARACTER MAI CHATTAWA
    EC |  00000E4C | THAI CHARACTER THANTHAKHAT
    ED |  00000E4D | THAI CHARACTER NIKHAHIT
    EE |  00002122 | TRADE MARK SIGN
    EF |  00000E4F | THAI CHARACTER FONGMAN
    F0 |  00000E50 | THAI DIGIT ZERO
    F1 |  00000E51 | THAI DIGIT ONE
    F2 |  00000E52 | THAI DIGIT TWO
    F3 |  00000E53 | THAI DIGIT THREE
    F4 |  00000E54 | THAI DIGIT FOUR
    F5 |  00000E55 | THAI DIGIT FIVE
    F6 |  00000E56 | THAI DIGIT SIX
    F7 |  00000E57 | THAI DIGIT SEVEN
    F8 |  00000E58 | THAI DIGIT EIGHT
    F9 |  00000E59 | THAI DIGIT NINE
    FA |  000000AE | REGISTERED SIGN
    FB |  000000A9 | COPYRIGHT SIGN


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
