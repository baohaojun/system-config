#! /bin/false

# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: RecodeData.pm,v 1.1 2011-10-12 23:51:26 pertusus Exp $

# Virtual base class for Locale::Recode converters.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.

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

package Locale::RecodeData;

use strict;

sub new
{
    my ($class, %args) = @_;

    bless {
		_from => $args{from},
		_to => $args{to},
    }, $class;
}

sub _getError
{
	shift->{_error};
}

1;

__END__

=head1 NAME

Locale::RecodeData - Abstract Base Class for Charset Converters

=head1 SYNOPSIS

    # For compatibility with Perl 5.005 and earlier, you must
    # *use* the module before inheriting from it!
    use qw (Locale::RecodeData);
    use base qw (Locale::RecodeData);

=head1 DESCRIPTION

The module B<Locale::RecodeData> serves as an abstract base class to
all converters used by Locale::Recode(3).

Adding new conversion modules is currently not straightforward, and 
you will have to edit the sources of some modules for that purpose.

First, you have to add your new converter class to the list found
in Locale::_Conversions(3), so that Locale::Recode(3) knows about
its presence.  If there are valid aliases for the codeset of your
converter, you will also have to edit Locale::_Aliases(3).

Finally, you have to implement the (protected) conversion routine
_recode().  See below (L<"INTERFACE")> for details.

=head1 CONSTRUCTOR

=over 4

=item B<new (from =E<gt> FROM_CODESET, to =E<gt> TO_CODESET)>

The constructor takes two (named) arguments:

=over 8

=item B<from>

The canonical name of the source codeset.  Aliases have already been
resolved and the name is converted to uppercase.

=item B<to>

The canonical name of the destination codeset.  Aliases have already been
resolved and the name is converted to uppercase.

=back

You normally don't have to implement the constructor.  The default constructor
implemented here will store the source and destination codesets in the
protected members C<_from> and C<_to>.

=back

=head1 METHODS

The class implements one method:

=over 4

=item B<_getError>

Returns the (protected) member C<_error>.

=back

=head1 INTERFACE

New conversion classes must provide the following interface:

=over 4

=item B<new (from =E<gt> FROM_CODESET, to =E<gt> TO_CODESET)>

The constructor takes two (named) arguments:

=over 8

=item B<from>

The canonical name of the source codeset.  Aliases have already been
resolved and the name is converted to uppercase.

=item B<to>

The canonical name of the destination codeset.  Aliases have already been
resolved and the name is converted to uppercase.

=back

=item B<_getError>

Should return the last error (as a string) or false if there was no error.

This method is implemented in the base class already.

=item B<_recode STRINGREF>

Should convert the argument C<STRINGREF> in-place.  In case of failure,
return false, and make provisions that the method C<_getError()> returns
an informative error message.

=back

=head1 AUTHOR

Copyright (C) 2002-2009, Guido Flohr E<lt>guido@imperia.netE<gt>, all
rights reserved.  See the source code for details.

This software is contributed to the Perl community by Imperia 
(L<http://www.imperia.net/>).

=head1 SEE ALSO

Locale::Recode::_Aliases(3pm), Locale::Recode::_Conversions(3pm),
Locale::Recode(3pm), perl(1)

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
