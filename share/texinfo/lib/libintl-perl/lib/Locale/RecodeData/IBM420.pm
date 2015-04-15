#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: IBM420.pm,v 1.1 2011-10-12 23:51:37 pertusus Exp $

# Conversion routines for IBM420.
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

package Locale::RecodeData::IBM420;

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
    0x0651,
    0xfe7d,
    0x0640,
    0xfffd,
    0x0621,
    0x0622,
    0xfe82,
    0x0623,
    0x00a2,
    0x002e,
    0x003c,
    0x0028,
    0x002b,
    0x007c,
    0x0026,
    0xfe84,
    0x0624,
    0xfffd,
    0xfffd,
    0x0626,
    0x0627,
    0xfe8e,
    0x0628,
    0xfe91,
    0x0021,
    0x0024,
    0x002a,
    0x0029,
    0x003b,
    0x00ac,
    0x002d,
    0x002f,
    0x0629,
    0x062a,
    0xfe97,
    0x062b,
    0xfe9b,
    0x062c,
    0xfe9f,
    0x062d,
    0x00a6,
    0x002c,
    0x0025,
    0x005f,
    0x003e,
    0x003f,
    0xfea3,
    0x062e,
    0xfea7,
    0x062f,
    0x0630,
    0x0631,
    0x0632,
    0x0633,
    0xfeb3,
    0x060c,
    0x003a,
    0x0023,
    0x0040,
    0x0027,
    0x003d,
    0x0022,
    0x0634,
    0x0061,
    0x0062,
    0x0063,
    0x0064,
    0x0065,
    0x0066,
    0x0067,
    0x0068,
    0x0069,
    0xfeb7,
    0x0635,
    0xfebb,
    0x0636,
    0xfebf,
    0x0637,
    0x0638,
    0x006a,
    0x006b,
    0x006c,
    0x006d,
    0x006e,
    0x006f,
    0x0070,
    0x0071,
    0x0072,
    0x0639,
    0xfeca,
    0xfecb,
    0xfecc,
    0x063a,
    0xfece,
    0xfecf,
    0x00f7,
    0x0073,
    0x0074,
    0x0075,
    0x0076,
    0x0077,
    0x0078,
    0x0079,
    0x007a,
    0xfed0,
    0x0641,
    0xfed3,
    0x0642,
    0xfed7,
    0x0643,
    0xfedb,
    0x0644,
    0xfef5,
    0xfef6,
    0xfef7,
    0xfef8,
    0xfffd,
    0xfffd,
    0xfefb,
    0xfefc,
    0xfedf,
    0x0645,
    0xfee3,
    0x0646,
    0xfee7,
    0x0647,
    0x061b,
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
    0xfeeb,
    0xfffd,
    0xfeec,
    0xfffd,
    0x0648,
    0x061f,
    0x004a,
    0x004b,
    0x004c,
    0x004d,
    0x004e,
    0x004f,
    0x0050,
    0x0051,
    0x0052,
    0x0649,
    0xfef0,
    0x064a,
    0xfef2,
    0xfef3,
    0x0660,
    0x00d7,
    0xfffd,
    0x0053,
    0x0054,
    0x0055,
    0x0056,
    0x0057,
    0x0058,
    0x0059,
    0x005a,
    0x0661,
    0x0662,
    0xfffd,
    0x0663,
    0x0664,
    0x0665,
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
    0x0666,
    0x0667,
    0x0668,
    0x0669,
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
    "\xd9\x91",
    "\xef\xb9\xbd",
    "\xd9\x80",
    "\xef\xbf\xbd",
    "\xd8\xa1",
    "\xd8\xa2",
    "\xef\xba\x82",
    "\xd8\xa3",
    "\xc2\xa2",
    "\x2e",
    "\x3c",
    "\x28",
    "\x2b",
    "\x7c",
    "\x26",
    "\xef\xba\x84",
    "\xd8\xa4",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xd8\xa6",
    "\xd8\xa7",
    "\xef\xba\x8e",
    "\xd8\xa8",
    "\xef\xba\x91",
    "\x21",
    "\x24",
    "\x2a",
    "\x29",
    "\x3b",
    "\xc2\xac",
    "\x2d",
    "\x2f",
    "\xd8\xa9",
    "\xd8\xaa",
    "\xef\xba\x97",
    "\xd8\xab",
    "\xef\xba\x9b",
    "\xd8\xac",
    "\xef\xba\x9f",
    "\xd8\xad",
    "\xc2\xa6",
    "\x2c",
    "\x25",
    "\x5f",
    "\x3e",
    "\x3f",
    "\xef\xba\xa3",
    "\xd8\xae",
    "\xef\xba\xa7",
    "\xd8\xaf",
    "\xd8\xb0",
    "\xd8\xb1",
    "\xd8\xb2",
    "\xd8\xb3",
    "\xef\xba\xb3",
    "\xd8\x8c",
    "\x3a",
    "\x23",
    "\x40",
    "\x27",
    "\x3d",
    "\x22",
    "\xd8\xb4",
    "\x61",
    "\x62",
    "\x63",
    "\x64",
    "\x65",
    "\x66",
    "\x67",
    "\x68",
    "\x69",
    "\xef\xba\xb7",
    "\xd8\xb5",
    "\xef\xba\xbb",
    "\xd8\xb6",
    "\xef\xba\xbf",
    "\xd8\xb7",
    "\xd8\xb8",
    "\x6a",
    "\x6b",
    "\x6c",
    "\x6d",
    "\x6e",
    "\x6f",
    "\x70",
    "\x71",
    "\x72",
    "\xd8\xb9",
    "\xef\xbb\x8a",
    "\xef\xbb\x8b",
    "\xef\xbb\x8c",
    "\xd8\xba",
    "\xef\xbb\x8e",
    "\xef\xbb\x8f",
    "\xc3\xb7",
    "\x73",
    "\x74",
    "\x75",
    "\x76",
    "\x77",
    "\x78",
    "\x79",
    "\x7a",
    "\xef\xbb\x90",
    "\xd9\x81",
    "\xef\xbb\x93",
    "\xd9\x82",
    "\xef\xbb\x97",
    "\xd9\x83",
    "\xef\xbb\x9b",
    "\xd9\x84",
    "\xef\xbb\xb5",
    "\xef\xbb\xb6",
    "\xef\xbb\xb7",
    "\xef\xbb\xb8",
    "\xef\xbf\xbd",
    "\xef\xbf\xbd",
    "\xef\xbb\xbb",
    "\xef\xbb\xbc",
    "\xef\xbb\x9f",
    "\xd9\x85",
    "\xef\xbb\xa3",
    "\xd9\x86",
    "\xef\xbb\xa7",
    "\xd9\x87",
    "\xd8\x9b",
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
    "\xef\xbb\xab",
    "\xef\xbf\xbd",
    "\xef\xbb\xac",
    "\xef\xbf\xbd",
    "\xd9\x88",
    "\xd8\x9f",
    "\x4a",
    "\x4b",
    "\x4c",
    "\x4d",
    "\x4e",
    "\x4f",
    "\x50",
    "\x51",
    "\x52",
    "\xd9\x89",
    "\xef\xbb\xb0",
    "\xd9\x8a",
    "\xef\xbb\xb2",
    "\xef\xbb\xb3",
    "\xd9\xa0",
    "\xc3\x97",
    "\xef\xbf\xbd",
    "\x53",
    "\x54",
    "\x55",
    "\x56",
    "\x57",
    "\x58",
    "\x59",
    "\x5a",
    "\xd9\xa1",
    "\xd9\xa2",
    "\xef\xbf\xbd",
    "\xd9\xa3",
    "\xd9\xa4",
    "\xd9\xa5",
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
    "\xd9\xa6",
    "\xd9\xa7",
    "\xd9\xa8",
    "\xd9\xa9",
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
    0x000000a0 => "\x41",
    0x000000a2 => "\x4a",
    0x000000a6 => "\x6a",
    0x000000ac => "\x5f",
    0x000000ad => "\xca",
    0x000000d7 => "\xe0",
    0x000000f7 => "\xa1",
    0x0000060c => "\x79",
    0x0000061b => "\xc0",
    0x0000061f => "\xd0",
    0x00000621 => "\x46",
    0x00000622 => "\x47",
    0x00000623 => "\x49",
    0x00000624 => "\x52",
    0x00000626 => "\x55",
    0x00000627 => "\x56",
    0x00000628 => "\x58",
    0x00000629 => "\x62",
    0x0000062a => "\x63",
    0x0000062b => "\x65",
    0x0000062c => "\x67",
    0x0000062d => "\x69",
    0x0000062e => "\x71",
    0x0000062f => "\x73",
    0x00000630 => "\x74",
    0x00000631 => "\x75",
    0x00000632 => "\x76",
    0x00000633 => "\x77",
    0x00000634 => "\x80",
    0x00000635 => "\x8b",
    0x00000636 => "\x8d",
    0x00000637 => "\x8f",
    0x00000638 => "\x90",
    0x00000639 => "\x9a",
    0x0000063a => "\x9e",
    0x00000640 => "\x44",
    0x00000641 => "\xab",
    0x00000642 => "\xad",
    0x00000643 => "\xaf",
    0x00000644 => "\xb1",
    0x00000645 => "\xbb",
    0x00000646 => "\xbd",
    0x00000647 => "\xbf",
    0x00000648 => "\xcf",
    0x00000649 => "\xda",
    0x0000064a => "\xdc",
    0x00000651 => "\x42",
    0x00000660 => "\xdf",
    0x00000661 => "\xea",
    0x00000662 => "\xeb",
    0x00000663 => "\xed",
    0x00000664 => "\xee",
    0x00000665 => "\xef",
    0x00000666 => "\xfb",
    0x00000667 => "\xfc",
    0x00000668 => "\xfd",
    0x00000669 => "\xfe",
    0x0000fe7d => "\x43",
    0x0000fe82 => "\x48",
    0x0000fe84 => "\x51",
    0x0000fe8e => "\x57",
    0x0000fe91 => "\x59",
    0x0000fe97 => "\x64",
    0x0000fe9b => "\x66",
    0x0000fe9f => "\x68",
    0x0000fea3 => "\x70",
    0x0000fea7 => "\x72",
    0x0000feb3 => "\x78",
    0x0000feb7 => "\x8a",
    0x0000febb => "\x8c",
    0x0000febf => "\x8e",
    0x0000feca => "\x9b",
    0x0000fecb => "\x9c",
    0x0000fecc => "\x9d",
    0x0000fece => "\x9f",
    0x0000fecf => "\xa0",
    0x0000fed0 => "\xaa",
    0x0000fed3 => "\xac",
    0x0000fed7 => "\xae",
    0x0000fedb => "\xb0",
    0x0000fedf => "\xba",
    0x0000fee3 => "\xbc",
    0x0000fee7 => "\xbe",
    0x0000feeb => "\xcb",
    0x0000feec => "\xcd",
    0x0000fef0 => "\xdb",
    0x0000fef2 => "\xdd",
    0x0000fef3 => "\xde",
    0x0000fef5 => "\xb2",
    0x0000fef6 => "\xb3",
    0x0000fef7 => "\xb4",
    0x0000fef8 => "\xb5",
    0x0000fefb => "\xb8",
    0x0000fefc => "\xb9",
);

sub _recode
{
    if ($_[0]->{_from} eq 'INTERNAL') {
		$_[1] = join '',
	        map $from_ucs4{$_} 
                || (defined $from_ucs4{$_} ? $from_ucs4{$_} : "\xd0"),
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

Locale::RecodeData::IBM420 - Conversion routines for IBM420

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This module is generated and contains the conversion tables and
routines for IBM420.

=head1 COMMENTS

The following comments have been extracted from the original charmap:

 version: 1.0
  source: IBM NLS RM Vol2 SE09-8002-01, March 1990
  IBM NLS RM p 11-11
 alias CP420
 alias EBCDIC-CP-AR1

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
    42 |  00000651 | ARABIC SHADDA
    43 |  0000FE7D | ARABIC SHADDA MEDIAL FORM
    44 |  00000640 | ARABIC TATWEEL
    46 |  00000621 | ARABIC LETTER HAMZA
    47 |  00000622 | ARABIC LETTER ALEF WITH MADDA ABOVE
    48 |  0000FE82 | ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
    49 |  00000623 | ARABIC LETTER ALEF WITH HAMZA ABOVE
    4A |  000000A2 | CENT SIGN
    4B |  0000002E | FULL STOP
    4C |  0000003C | LESS-THAN SIGN
    4D |  00000028 | LEFT PARENTHESIS
    4E |  0000002B | PLUS SIGN
    4F |  0000007C | VERTICAL LINE
    50 |  00000026 | AMPERSAND
    51 |  0000FE84 | ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
    52 |  00000624 | ARABIC LETTER WAW WITH HAMZA ABOVE
    55 |  00000626 | ARABIC LETTER YEH WITH HAMZA ABOVE
    56 |  00000627 | ARABIC LETTER ALEF
    57 |  0000FE8E | ARABIC LETTER ALEF FINAL FORM
    58 |  00000628 | ARABIC LETTER BEH
    59 |  0000FE91 | ARABIC LETTER BEH INITIAL FORM
    5A |  00000021 | EXCLAMATION MARK
    5B |  00000024 | DOLLAR SIGN
    5C |  0000002A | ASTERISK
    5D |  00000029 | RIGHT PARENTHESIS
    5E |  0000003B | SEMICOLON
    5F |  000000AC | NOT SIGN
    60 |  0000002D | HYPHEN-MINUS
    61 |  0000002F | SOLIDUS
    62 |  00000629 | ARABIC LETTER TEH MARBUTA
    63 |  0000062A | ARABIC LETTER TEH
    64 |  0000FE97 | ARABIC LETTER TEH INITIAL FORM
    65 |  0000062B | ARABIC LETTER THEH
    66 |  0000FE9B | ARABIC LETTER THEH INITIAL FORM
    67 |  0000062C | ARABIC LETTER JEEM
    68 |  0000FE9F | ARABIC LETTER JEEM INITIAL FORM
    69 |  0000062D | ARABIC LETTER HAH
    6A |  000000A6 | BROKEN BAR
    6B |  0000002C | COMMA
    6C |  00000025 | PERCENT SIGN
    6D |  0000005F | LOW LINE
    6E |  0000003E | GREATER-THAN SIGN
    6F |  0000003F | QUESTION MARK
    70 |  0000FEA3 | ARABIC LETTER HAH INITIAL FORM
    71 |  0000062E | ARABIC LETTER KHAH
    72 |  0000FEA7 | ARABIC LETTER KHAH INITIAL FORM
    73 |  0000062F | ARABIC LETTER DAL
    74 |  00000630 | ARABIC LETTER THAL
    75 |  00000631 | ARABIC LETTER REH
    76 |  00000632 | ARABIC LETTER ZAIN
    77 |  00000633 | ARABIC LETTER SEEN
    78 |  0000FEB3 | ARABIC LETTER SEEN INITIAL FORM
    79 |  0000060C | ARABIC COMMA
    7A |  0000003A | COLON
    7B |  00000023 | NUMBER SIGN
    7C |  00000040 | COMMERCIAL AT
    7D |  00000027 | APOSTROPHE
    7E |  0000003D | EQUALS SIGN
    7F |  00000022 | QUOTATION MARK
    80 |  00000634 | ARABIC LETTER SHEEN
    81 |  00000061 | LATIN SMALL LETTER A
    82 |  00000062 | LATIN SMALL LETTER B
    83 |  00000063 | LATIN SMALL LETTER C
    84 |  00000064 | LATIN SMALL LETTER D
    85 |  00000065 | LATIN SMALL LETTER E
    86 |  00000066 | LATIN SMALL LETTER F
    87 |  00000067 | LATIN SMALL LETTER G
    88 |  00000068 | LATIN SMALL LETTER H
    89 |  00000069 | LATIN SMALL LETTER I
    8A |  0000FEB7 | ARABIC LETTER SHEEN INITIAL FORM
    8B |  00000635 | ARABIC LETTER SAD
    8C |  0000FEBB | ARABIC LETTER SAD INITIAL FORM
    8D |  00000636 | ARABIC LETTER DAD
    8E |  0000FEBF | ARABIC LETTER DAD INITIAL FORM
    8F |  00000637 | ARABIC LETTER TAH
    90 |  00000638 | ARABIC LETTER ZAH
    91 |  0000006A | LATIN SMALL LETTER J
    92 |  0000006B | LATIN SMALL LETTER K
    93 |  0000006C | LATIN SMALL LETTER L
    94 |  0000006D | LATIN SMALL LETTER M
    95 |  0000006E | LATIN SMALL LETTER N
    96 |  0000006F | LATIN SMALL LETTER O
    97 |  00000070 | LATIN SMALL LETTER P
    98 |  00000071 | LATIN SMALL LETTER Q
    99 |  00000072 | LATIN SMALL LETTER R
    9A |  00000639 | ARABIC LETTER AIN
    9B |  0000FECA | ARABIC LETTER AIN FINAL FORM
    9C |  0000FECB | ARABIC LETTER AIN INITIAL FORM
    9D |  0000FECC | ARABIC LETTER AIN MEDIAL FORM
    9E |  0000063A | ARABIC LETTER GHAIN
    9F |  0000FECE | ARABIC LETTER GHAIN FINAL FORM
    A0 |  0000FECF | ARABIC LETTER GHAIN INITIAL FORM
    A1 |  000000F7 | DIVISION SIGN
    A2 |  00000073 | LATIN SMALL LETTER S
    A3 |  00000074 | LATIN SMALL LETTER T
    A4 |  00000075 | LATIN SMALL LETTER U
    A5 |  00000076 | LATIN SMALL LETTER V
    A6 |  00000077 | LATIN SMALL LETTER W
    A7 |  00000078 | LATIN SMALL LETTER X
    A8 |  00000079 | LATIN SMALL LETTER Y
    A9 |  0000007A | LATIN SMALL LETTER Z
    AA |  0000FED0 | ARABIC LETTER GHAIN MEDIAL FORM
    AB |  00000641 | ARABIC LETTER FEH
    AC |  0000FED3 | ARABIC LETTER FEH INITIAL FORM
    AD |  00000642 | ARABIC LETTER QAF
    AE |  0000FED7 | ARABIC LETTER QAF INITIAL FORM
    AF |  00000643 | ARABIC LETTER KAF
    B0 |  0000FEDB | ARABIC LETTER KAF INITIAL FORM
    B1 |  00000644 | ARABIC LETTER LAM
    B2 |  0000FEF5 | ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
    B3 |  0000FEF6 | ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
    B4 |  0000FEF7 | ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
    B5 |  0000FEF8 | ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
    B8 |  0000FEFB | ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM
    B9 |  0000FEFC | ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    BA |  0000FEDF | ARABIC LETTER LAM INITIAL FORM
    BB |  00000645 | ARABIC LETTER MEEM
    BC |  0000FEE3 | ARABIC LETTER MEEM INITIAL FORM
    BD |  00000646 | ARABIC LETTER NOON
    BE |  0000FEE7 | ARABIC LETTER NOON INITIAL FORM
    BF |  00000647 | ARABIC LETTER HEH
    C0 |  0000061B | ARABIC SEMICOLON
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
    CB |  0000FEEB | ARABIC LETTER HEH INITIAL FORM
    CD |  0000FEEC | ARABIC LETTER HEH MEDIAL FORM
    CF |  00000648 | ARABIC LETTER WAW
    D0 |  0000061F | ARABIC QUESTION MARK
    D1 |  0000004A | LATIN CAPITAL LETTER J
    D2 |  0000004B | LATIN CAPITAL LETTER K
    D3 |  0000004C | LATIN CAPITAL LETTER L
    D4 |  0000004D | LATIN CAPITAL LETTER M
    D5 |  0000004E | LATIN CAPITAL LETTER N
    D6 |  0000004F | LATIN CAPITAL LETTER O
    D7 |  00000050 | LATIN CAPITAL LETTER P
    D8 |  00000051 | LATIN CAPITAL LETTER Q
    D9 |  00000052 | LATIN CAPITAL LETTER R
    DA |  00000649 | ARABIC LETTER ALEF MAKSURA
    DB |  0000FEF0 | ARABIC LETTER ALEF MAKSURA FINAL FORM
    DC |  0000064A | ARABIC LETTER YEH
    DD |  0000FEF2 | ARABIC LETTER YEH FINAL FORM
    DE |  0000FEF3 | ARABIC LETTER YEH INITIAL FORM
    DF |  00000660 | ARABIC-INDIC DIGIT ZERO
    E0 |  000000D7 | MULTIPLICATION SIGN
    E2 |  00000053 | LATIN CAPITAL LETTER S
    E3 |  00000054 | LATIN CAPITAL LETTER T
    E4 |  00000055 | LATIN CAPITAL LETTER U
    E5 |  00000056 | LATIN CAPITAL LETTER V
    E6 |  00000057 | LATIN CAPITAL LETTER W
    E7 |  00000058 | LATIN CAPITAL LETTER X
    E8 |  00000059 | LATIN CAPITAL LETTER Y
    E9 |  0000005A | LATIN CAPITAL LETTER Z
    EA |  00000661 | ARABIC-INDIC DIGIT ONE
    EB |  00000662 | ARABIC-INDIC DIGIT TWO
    ED |  00000663 | ARABIC-INDIC DIGIT THREE
    EE |  00000664 | ARABIC-INDIC DIGIT FOUR
    EF |  00000665 | ARABIC-INDIC DIGIT FIVE
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
    FB |  00000666 | ARABIC-INDIC DIGIT SIX
    FC |  00000667 | ARABIC-INDIC DIGIT SEVEN
    FD |  00000668 | ARABIC-INDIC DIGIT EIGHT
    FE |  00000669 | ARABIC-INDIC DIGIT NINE
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
