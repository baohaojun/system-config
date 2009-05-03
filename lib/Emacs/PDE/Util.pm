package Emacs::PDE::Util;

use strict; 
use warnings;

use Carp;
use File::Find;
use File::Spec;
use Config;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw( list_shadows list_core_modules );

sub _find_modules {
    my @inc = grep { -d $_ } @_;
    my %mods;
    foreach my $path (@inc) {

        # Make sure it is a path;
        $path = File::Spec->catfile( $path, '' );
        find(
            sub {
                if ( -f $_ && $_ =~ /\.pm/ ) {
                    my $m = File::Spec->abs2rel( $File::Find::name, $path );
                    $m =~ s/\.pm//;
                    $m = join( "::", File::Spec->splitdir($m) );
                    push @{ $mods{ $m } }, File::Spec->canonpath($File::Find::name);
                }
            },
            $path
        );
    }
    return \%mods;
}

sub list_shadows {
    my @inc = grep { -d $_ && ( !-l $_ ) && -r $_ && $_ ne '.' } @INC;
    my $mods = _find_modules(@inc);
    foreach my $mod ( sort keys %$mods ) {
        if ( @{ $mods->{$mod} } > 1 ) {
            print "* $mod\n";
            foreach ( @{ $mods->{$mod} } ) {
                print "\t$_\n";
            }
            print "\n";
        }
    }
}

sub list_core_modules {
    my $mods = _find_modules( $Config{installarchlib},
                              $Config{installprivlib} );
    print join("\n", sort keys %$mods);
}

1;
__END__

=head1 NAME

Emacs::PDE::Util - Utils for PDE

=head1 SYNOPSIS

   use Emacs::PDE::Util;
   list_shadows();

=head1 DESCRIPTION

This module contains a collection of subroutines for PDE (Perl
Development Environment).

=head2 EXPORT

All subroutines.

=head1 SEE ALSO

PDE

=head1 AUTHOR

Ye Wenbin, E<lt>wenbinye@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Ye Wenbin

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
