package Unicode::EastAsianWidth;

use 5.006;
use strict;
use base 'Exporter';
use vars qw( $VERSION $EastAsian );

BEGIN {
    $VERSION = '1.30';
    $EastAsian = 0;
};

sub InFullwidth {
    return InEastAsianFullwidth().
           InEastAsianWide().
           ($EastAsian ? InEastAsianAmbiguous() : '');
}

sub InHalfwidth {
    return InEastAsianHalfwidth().
           InEastAsianNarrow().
           InEastAsianNeutral().
           ($EastAsian ? '' : InEastAsianAmbiguous());
}

### BEGIN ###
our @EXPORT = qw(
InEastAsianAmbiguous
InEastAsianFullwidth
InEastAsianHalfwidth
InEastAsianNarrow
InEastAsianNeutral
InEastAsianWide
InFullwidth
InHalfwidth
);

sub InEastAsianAmbiguous {
    return <<"END";
00A1\t00A1
00A4\t00A4
00A7\t00A8
00AA\t00AA
00AD\t00AE
00B0\t00B4
00B6\t00BA
00BC\t00BF
00C6\t00C6
00D0\t00D0
00D7\t00D8
00DE\t00E1
00E6\t00E6
00E8\t00EA
00EC\t00ED
00F0\t00F0
00F2\t00F3
00F7\t00FA
00FC\t00FC
00FE\t00FE
0101\t0101
0111\t0111
0113\t0113
011B\t011B
0126\t0127
012B\t012B
0131\t0133
0138\t0138
013F\t0142
0144\t0144
0148\t014B
014D\t014D
0152\t0153
0166\t0167
016B\t016B
01CE\t01CE
01D0\t01D0
01D2\t01D2
01D4\t01D4
01D6\t01D6
01D8\t01D8
01DA\t01DA
01DC\t01DC
0251\t0251
0261\t0261
02C4\t02C4
02C7\t02C7
02C9\t02CB
02CD\t02CD
02D0\t02D0
02D8\t02DB
02DD\t02DD
02DF\t02DF
0300\t036F
0391\t03A9
03B1\t03C1
03C3\t03C9
0401\t0401
0410\t044F
0451\t0451
2010\t2010
2013\t2016
2018\t2019
201C\t201D
2020\t2022
2024\t2027
2030\t2030
2032\t2033
2035\t2035
203B\t203B
203E\t203E
2074\t2074
207F\t207F
2081\t2084
20AC\t20AC
2103\t2103
2105\t2105
2109\t2109
2113\t2113
2116\t2116
2121\t2122
2126\t2126
212B\t212B
2153\t2154
215B\t215E
2160\t216B
2170\t2179
2190\t2199
21B8\t21B9
21D2\t21D2
21D4\t21D4
21E7\t21E7
2200\t2200
2202\t2203
2207\t2208
220B\t220B
220F\t220F
2211\t2211
2215\t2215
221A\t221A
221D\t2220
2223\t2223
2225\t2225
2227\t222C
222E\t222E
2234\t2237
223C\t223D
2248\t2248
224C\t224C
2252\t2252
2260\t2261
2264\t2267
226A\t226B
226E\t226F
2282\t2283
2286\t2287
2295\t2295
2299\t2299
22A5\t22A5
22BF\t22BF
2312\t2312
2460\t24E9
24EB\t254B
2550\t2573
2580\t258F
2592\t2595
25A0\t25A1
25A3\t25A9
25B2\t25B3
25B6\t25B7
25BC\t25BD
25C0\t25C1
25C6\t25C8
25CB\t25CB
25CE\t25D1
25E2\t25E5
25EF\t25EF
2605\t2606
2609\t2609
260E\t260F
2614\t2615
261C\t261C
261E\t261E
2640\t2640
2642\t2642
2660\t2661
2663\t2665
2667\t266A
266C\t266D
266F\t266F
273D\t273D
2776\t277F
E000\tF8FF
FE00\tFE0F
FFFD\tFFFD
E0100\tE01EF
F0000\tFFFFD
100000\t10FFFD
END
}

sub InEastAsianFullwidth {
    return <<"END";
3000\t3000
FF01\tFF60
FFE0\tFFE6
END
}

sub InEastAsianHalfwidth {
    return <<"END";
20A9\t20A9
FF61\tFFDC
FFE8\tFFEE
END
}

sub InEastAsianNarrow {
    return <<"END";
0020\t007E
00A2\t00A3
00A5\t00A6
00AC\t00AC
00AF\t00AF
27E6\t27EB
2985\t2986
END
}

sub InEastAsianNeutral {
    return <<"END";
0000\t001F
007F\t00A0
00A9\t00A9
00AB\t00AB
00B5\t00B5
00BB\t00BB
00C0\t00C5
00C7\t00CF
00D1\t00D6
00D9\t00DD
00E2\t00E5
00E7\t00E7
00EB\t00EB
00EE\t00EF
00F1\t00F1
00F4\t00F6
00FB\t00FB
00FD\t00FD
00FF\t0100
0102\t0110
0112\t0112
0114\t011A
011C\t0125
0128\t012A
012C\t0130
0134\t0137
0139\t013E
0143\t0143
0145\t0147
014C\t014C
014E\t0151
0154\t0165
0168\t016A
016C\t01CD
01CF\t01CF
01D1\t01D1
01D3\t01D3
01D5\t01D5
01D7\t01D7
01D9\t01D9
01DB\t01DB
01DD\t0250
0252\t0260
0262\t02C3
02C5\t02C6
02C8\t02C8
02CC\t02CC
02CE\t02CF
02D1\t02D7
02DC\t02DC
02DE\t02DE
02E0\t02FF
0374\t0390
03AA\t03B0
03C2\t03C2
03CA\t0400
0402\t040F
0450\t0450
0452\t10FC
1160\t200F
2011\t2012
2017\t2017
201A\t201B
201E\t201F
2023\t2023
2028\t202F
2031\t2031
2034\t2034
2036\t203A
203C\t203D
203F\t2071
2075\t207E
2080\t2080
2085\t20A8
20AA\t20AB
20AD\t2102
2104\t2104
2106\t2108
210A\t2112
2114\t2115
2117\t2120
2123\t2125
2127\t212A
212C\t214E
2155\t215A
215F\t215F
216C\t216F
217A\t2184
219A\t21B7
21BA\t21D1
21D3\t21D3
21D5\t21E6
21E8\t21FF
2201\t2201
2204\t2206
2209\t220A
220C\t220E
2210\t2210
2212\t2214
2216\t2219
221B\t221C
2221\t2222
2224\t2224
2226\t2226
222D\t222D
222F\t2233
2238\t223B
223E\t2247
2249\t224B
224D\t2251
2253\t225F
2262\t2263
2268\t2269
226C\t226D
2270\t2281
2284\t2285
2288\t2294
2296\t2298
229A\t22A4
22A6\t22BE
22C0\t2311
2313\t2328
232B\t244A
24EA\t24EA
254C\t254F
2574\t257F
2590\t2591
2596\t259F
25A2\t25A2
25AA\t25B1
25B4\t25B5
25B8\t25BB
25BE\t25BF
25C2\t25C5
25C9\t25CA
25CC\t25CD
25D2\t25E1
25E6\t25EE
25F0\t2604
2607\t2608
260A\t260D
2610\t2613
2616\t261B
261D\t261D
261F\t263F
2641\t2641
2643\t265F
2662\t2662
2666\t2666
266B\t266B
266E\t266E
2670\t273C
273E\t2775
2780\t27E5
27F0\t2984
2987\t2E1D
303F\t303F
4DC0\t4DFF
A700\tA877
D800\tDB7F
DB80\tDBFF
DC00\tDFFF
FB00\tFDFD
FE20\tFE23
FE70\tFEFF
FFF9\tFFFC
10000\t1D7FF
E0001\tE007F
END
}

sub InEastAsianWide {
    return <<"END";
1100\t115F
2329\t232A
2E80\t2FFB
3001\t303E
3041\t33FF
3400\t4DB5
4E00\t9FBB
A000\tA4C6
AC00\tD7A3
F900\tFAD9
FE10\tFE19
FE30\tFE6B
20000\t2A6D6
2A6D7\t2F7FF
2F800\t2FA1D
2FA1E\t2FFFD
30000\t3FFFD
END
}

### END ###

1;

__END__

=head1 NAME

Unicode::EastAsianWidth - East Asian Width properties

=head1 VERSION

This document describes version 1.10 of Unicode::EastAsianWidth,
released October 14, 2007.

=head1 SYNOPSIS

    use Unicode::EastAsianWidth;

    $_ = chr(0x2010); # HYPHEN, an ambiguous-width character

    /\p{InEastAsianAmbiguous}/; # True
    /\p{InFullwidth}/;          # False

    {
        local $Unicode::EastAsianWidth::EastAsian = 1;
        /\p{InFullwidth}/;      # True (this only works on Perl 5.8+)
    }

=head1 DESCRIPTION

This module provide user-defined Unicode properties that deal with
East Asian characters' width status, as specified in
L<http://www.unicode.org/unicode/reports/tr11/>.

It exports the following functions to the caller's scope, to be
used by Perl's Unicode matching system: C<InEastAsianFullwidth>,
C<InEastAsianHalfwidth>, C<InEastAsianAmbiguous>, C<InEastAsianNarrow>
C<InEastAsianWide>, C<InEastAsianNeutral>.

In accord to TR11 cited above, two additional context-sensitive properties
are exported: C<InFullwidth> (union of C<Fullwidth> and C<Wide>) and
C<InHalfwidth> (union of C<Halfwidth>, C<Narrow> and C<Neutral>).

I<Ambiguous> characters are treated by default as part of
C<InHalfwidth>, but you can modify this behaviour by assigning
a true value to C<$Unicode::EastAsianWidth::EastAsian>.

=head1 CAVEATS

Setting C<$Unicode::EastAsianWidth::EastAsian> at run-time only
works on Perl version 5.8 or above.  Perl 5.6 users must use
a BEGIN block to set it before the C<use> statement:

    BEGIN { $Unicode::EastAsianWidth::EastAsian = 1 }
    use Unicode::EastAsianWidth;

=head1 SEE ALSO

L<perlunicode>,
L<http://www.unicode.org/unicode/reports/tr11/>

=head1 AUTHORS

Audrey Tang E<lt>cpan@audreyt.orgE<gt>

=head1 COPYRIGHT

Copyright 2002, 2003, 2007, 2008 by Audrey Tang E<lt>cpan@audreyt.orgE<gt>.

This software is released under the MIT license cited below.

=head2 The "MIT" License

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.

=cut
