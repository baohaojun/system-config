#! /bin/false

# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: UTF_8.pm,v 1.1 2011-10-12 23:51:50 pertusus Exp $

# Conversion routines for UTF-8 (perl < 5.8.0).
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.
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

package Locale::RecodeData::UTF_8;

use strict;

require Locale::RecodeData;
use base qw(Locale::RecodeData);

sub _recode
{
    if ($_[0]->{_from} eq 'INTERNAL') {
	return $_[0]->_fromInternal ($_[1]);
    } else {
	return $_[0]->_toInternal ($_[1]);
    }
}

# This routine assumes that the internal representation is always sane
# and contains valid codes only.
sub _fromInternal
{
    $_[1] = join '', map {
	if ($_ <= 0x7f) {
	    chr $_;
	} elsif ($_ <= 0x7ff) {
	    pack ("C2", 
		  (0xc0 | (($_ >> 6) & 0x1f)),
		  (0x80 | ($_ & 0x3f)));
	} elsif ($_ <= 0xffff) {
	    pack ("C3", 
		  (0xe0 | (($_ >> 12) & 0xf)),
		  (0x80 | (($_ >> 6) & 0x3f)),
		  (0x80 | ($_ & 0x3f)));
	} elsif ($_ <= 0x1fffff) {
	    pack ("C4", 
		  (0xf0 | (($_ >> 18) & 0x7)),
		  (0x80 | (($_ >> 12) & 0x3f)),
		  (0x80 | (($_ >> 6) & 0x3f)),
		  (0x80 | ($_ & 0x3f)));
	} elsif ($_ <= 0x3ffffff) {
	    pack ("C5", 
		  (0xf0 | (($_ >> 24) & 0x3)),
		  (0x80 | (($_ >> 18) & 0x3f)),
		  (0x80 | (($_ >> 12) & 0x3f)),
		  (0x80 | (($_ >> 6) & 0x3f)),
		  (0x80 | ($_ & 0x3f)));
	} else {
	    pack ("C6", 
		  (0xf0 | (($_ >> 30) & 0x3)),
		  (0x80 | (($_ >> 24) & 0x1)),
		  (0x80 | (($_ >> 18) & 0x3f)),
		  (0x80 | (($_ >> 12) & 0x3f)),
		  (0x80 | (($_ >> 6) & 0x3f)),
		  (0x80 | ($_ & 0x3f)));
	}
    } @{$_[1]};
    return 1;
}

# Decode UTF-8 into integers.  We do not bother to care about possibly
# configured replacement characters here and simply fall back to 0xfffd.
# Rationale: the internal format is never output directly and the other
# encoders will handle the replacement character correctly.
sub _toInternal
{
    if ($] >= 5.006) {
	$_[1] = [ unpack "U*", $_[1] ];
	return 1;
    }

    # Sigh, we have to decode ourselves.  FIXME: Should be optimized.
    # The routine is awfully slow.
    # It also does not necessarily detect illegal multi-byte sequences.

    my @chars = ();
    my @bytes = unpack "C*", $_[1];

    BYTE: while (@bytes) {
	my $byte = shift @bytes;
        if ($byte < 0x80) {
            push @chars, $byte;
        } elsif ($byte < 0xc0 || $byte > 0xfd) {
            push @chars, 0xfffd;
        } else {
            my $num_bytes;
            my $char;
            if ($byte < 0xe0) {
                $char = $byte & 0x1f;
                $num_bytes = 1;
            } elsif ($byte < 0xf0) {
                $char = $byte & 0xf;
                $num_bytes = 2;
            } elsif ($byte < 0xf8) {
                $char = $byte & 0x7;
                $num_bytes = 3;
            } elsif ($byte < 0xfc) {
                $char = $byte & 0x3;
                $num_bytes = 4;
            } else {
                $char = $byte & 0x1;
                $num_bytes = 5;
            }
            for (my $i = 0; $i < $num_bytes; ++$i) {
                my $next = shift @bytes;
                if (!defined $next || $next < 0x80 || $next > 0xbf) {
                    push @chars, 0xfffd;
                    next BYTE;
                } else {
                    $char <<= 6;
                    $char |= $next & 0x3f;
                }
            }
            push @chars, $char;
        }
    }
    
    $_[1] = \@chars;
    
    return 1;
}

1;

__END__

=head1 NAME

Locale::RecodeData::UTF_8 - Conversion routines for UTF-8

=head1 SYNOPSIS

This module is internal to libintl.  Do not use directly!

=head1 DESCRIPTION

This modules contains the conversion tables for UTF-8.  It is capable of
converting from UTF-8 to the internal format of libintl-perl and vice
versa.   It is only suitable for Perl versions E<lt>= 5.8.0.  However,
you do not have to bother about version checking, Locale::Recode(3)
will do that for you.


=head1 CHARACTER TABLE

See http://www.unicode.org/.

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
