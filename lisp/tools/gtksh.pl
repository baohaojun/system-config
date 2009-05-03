#! /usr/bin/perl -w
package Gtksh::Subs;
use Data::Dumper qw(Dumper);

sub reload {
    my $mod = shift;
    (my $file = $mod) =~ s/::/\//g;
    delete $INC{$file.".pm"};
    eval("{
      require $mod;
      1;
    }");
    if ( $@ ) {
        print "Reload $mod failed: $@\n";
    } else {
        print "Successfully reload $mod\n";
    }
}

sub Dump {
    print Data::Dumper::Dumper(@_), "\n";
}

sub help {
    use Pod::Perldoc;
    my $kw = shift;
    print `perldoc $kw`;
}

package main;

use Getopt::Long qw(:config no_ignore_case auto_help);
use Gtk2 '-init';
use Glib  qw(TRUE FALSE);

{
    no warnings qw(all);
    *x = \&Gtksh::Subs::Dump;
    *reload = \&Gtksh::Subs::reload;
    sub Gtk2::main_quit {
        warn "Gtk2 quit\n";
        return FALSE;
    }
}
use Gtk2::Helper;
use FileHandle;

our $DEBUG = 0;
our $PROMPT = "gtksh> ";
my $start_up;

GetOptions(
    'debug' => \$DEBUG,
    'prompt=s' => \$PROMPT,
    'start-up=s' => \$start_up,
);

print "This is a simple perl shell for gtk-perl!\n";

if ( defined $start_up && -f $start_up ) {
    print "Load $start_up...\n";
    require $start_up;
}

# Turn all buffering off.
select((select(STDOUT), $| = 1)[0]);
select((select(STDERR), $| = 1)[0]);
select((select(STDIN),  $| = 1)[0]);

print $PROMPT;
Glib::IO->add_watch (fileno(STDIN), [qw/in/], \&watch_callback, \*STDIN);
# Gtk2::Helper->add_watch (STDIN->fileno(), 'in', \&watch_callback, \*STDIN);
Gtk2->main;

# make shell prompt print next line
END {
    print "\n";
}

sub watch_callback {
	my ($__fd__, $__condition__, $__fh__) = @_;
    # internal variable, strange name so that you seldom change them
    my $__exp__ = "";
    my $__line__;
    while (1) {
        $__line__ = <$__fh__>;
        unless (defined($__line__)) {
            exit;
        }
        chomp($__line__);
        if ($__line__ =~ s/\\\s*$//) {
            print "+> ";
            $__exp__ .= $__line__ . "\n";
        } else {
            last;
        }
    }
    $__exp__ .= $__line__ . "\n";
    print "\nYou just input: $__exp__\n" if $DEBUG;
    if ($__exp__ =~ /^(quit|exit|bye)$/) {
        exit;
    } elsif ( $__exp__ =~ /^(help\s+|\?)(.*)\s*$/ ) {
        Gtksh::Subs::help($2);
    } else {
        print "Eval '$__exp__'\n" if $DEBUG;
        my $res = eval(
            "{
                no warnings 'all';
                $__exp__;
            }"
        );
        if ($@) {
            print "Error: $@\n";
        }
        print "\nResult: ", $res, "\n" if defined $res;
    }
    print $PROMPT;
	return TRUE;
}

__END__

=head1 NAME

gtksh.pl - A simple perl shell for gtk-perl

=head1 SYNOPSIS

gtksh.pl

=head1 DESCRIPTION

gtksh.pl is intended to be a perl shell which is better than `perl -d`.
I use this program in emacs, and it works, but not powerful.

=head2 Predefined command

=over

=item x

A alias for C<Data::Dumper::Dumper>. for example:

   perl> $foo = {'a' => 1}
   
   Result: HASH(0x837c674)
   perl> x $foo
   $VAR1 = {
             'a' => 1
           };

=item ?

=item help

Intend for replace input `perldoc` in this program. for example:

   perl> help -f print
       print FILEHANDLE LIST
       print LIST
       print   Prints a string or a list of strings. Returns true if...
   
   perl> help Math::Trig
   NAME
       Math::Trig - trigonometric functions
   
   SYNOPSIS
   ...

=item reload

Reload a module again. Equal to eval "do($INC{Module.pm}". for example:

   perl> require DBI
   
   Result: 1
   perl> reload "DBI"
   Subroutine DBI::SQL_UDT redefined at /usr/lib/perl5/DBI.pm line 154, <STDIN> line 2.
   ....

=item bye

=item exit

=item quit

Quit the program.

=back


=head1 AUTHOR

Ye Wenbin <wenbinye@gmail.com>

=head1 SEE ALSO

http://www.emacswiki.org/cgi-bin/emacs/inf-perl.el

=cut
