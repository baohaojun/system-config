#!/usr/bin/perl -w
# psh.pl --- A simple perl shell
# Last modify Time-stamp: <Ye Wenbin 2007-04-15 23:38:08>
# Version: v 0.0 2005/10/23 14:52:44
# Author: Wenbin Ye <wenbinye@163.com>
package Psh::Subs;
use Data::Dumper;
use Text::Wrap qw(wrap);

sub reload {
    my $mod = shift;
    (my $file = $mod) =~ s/::/\//g;
    delete $INC{$file.".pm"};
    no warnings qw(redefine);
    eval("require $mod");
}

sub dump {
    print Data::Dumper::Dumper(@_), "\n";
}

sub symtable {
    my %module;
    my %sym2type;
    my %symflag = (SCALAR => '$',
                   ARRAY => '@',
                   HASH => '%',
                   CODE => '&',
                   GLOB => '*',
                  );
    modules("main::", \%module);
    foreach (keys %main::) {
        next if /::$/;
        foreach my $type (symtype($_)) {
            push @{$sym2type{$type}}, $symflag{$type}.$_;
        }
    }
    print "Scalars:\n", wrap('', '', join(" ", sort @{$sym2type{SCALAR}})), "\n\n";
    print "Arrays:\n", wrap('', '', join(" ", sort @{$sym2type{ARRAY}})), "\n\n";
    print "Hashs:\n", wrap('', '', join(" ", sort @{$sym2type{HASH}})), "\n\n";
    print "Subs:\n", wrap('', '', join(" ", sort @{$sym2type{CODE}})), "\n\n";
    print "Globs:\n", wrap('', '', join(" ", sort @{$sym2type{GLOB}})), "\n\n";
    print "Modules:\n", wrap('', '', join(" ", sort keys %module)), "\n\n";
}

sub modules {
    my ($name, $modules) = @_;
    return unless $name =~ /::$/;
    no strict;
    my @syms = grep { $_ ne '' } keys %{$name};
    my @submods = grep {/::$/} @syms;
    if ($name eq 'main::') {
        @submods = grep { $_ ne "main::" } @submods;
        $name = '';
    }
    if (@submods) {
        foreach (@submods) {
            modules($name.$_, $modules);
        }
    }
    # if the symbol table contain something but modules, add to module hash
    if ($#syms != $#submods && $name ne '') { 
        $modules->{substr($name, 0, -2)}++; 
    }
}

sub symtype {
    my $name = shift;
    no strict;
    if ($name =~ /::$/) {       # like a module name
        return;
    }
    my @types;
    if (defined ${$name} ) {
        push @types, "SCALAR";
    }
    if (defined @{$name} ) {
        push @types, "ARRAY";
    }
    if (defined %{$name} ) {
        push @types, "HASH";
    }
    if ( defined &{$name} && main->can($name) ) {
        push @types, "CODE";
    }
    if ( defined *{$name} ) {
        push @types, "GLOB";
    }
    return @types;
}

sub subroutinep {
    my $name = shift;
    if ( defined(&{"$name"}) ) {
        return 1;
    }
}

sub variablep {
    my $name = shift;
    if (scalarp($name)) {
        return 1;
    }
    elsif (arrayp{$name}) {
        return 2;
    }
    elsif (hashp($name)) {
        return 3;
    }
}

sub scalarp {
    my $name = shift;
    no strict;
    if (defined ${"$name"}) {
        return 1;
    }
}

sub arrayp {
    my $name = shift;
    no strict;
    if (defined @{"$name"}) {
        return 1;
    }
}

sub hashp {
    my $name = shift;
    no strict;
    if ($name !~ /::$/ && defined %{"$name"}) {
        return 1;
    }
}

sub nosub {
    my $pos = 0;
    my $exp = \$_[0];
    while ($$exp =~ /(?=\s?)sub\s+(\w+)/mg) {
        $pos = pos($$exp);
        #    print "pos: ", $pos, "\n";
        $$exp =~ s/(?=\s?)sub\s+(\w+)/*$1 = sub/;
        $pos = index($$exp, '{', $pos);
        #    print "pos: ", $pos, "\n";
        my $stack = 0;
        while (1) {
            if (substr($$exp, $pos, 1) eq '{') {
                $stack++;
            } elsif (substr ($$exp, $pos, 1) eq '}') {
                $stack--;
            }
            if ($stack == 0 || $pos >= length($$exp)) {
                last;
            }
            $pos++;
        }
        substr($$exp, $pos+1, 0, ";");
        #    last;
    }
}

sub quit {
    print "Byebye!\n";
    exit;
}

sub locate_mod {
    my $mod = shift;
    my @mod = split /::/, $mod;
    my $dir = [ @INC ];
    foreach ( 0..$#mod-1 ) {
        $dir = locate_file($mod[$_], undef, $dir);
    }
    return locate_file($mod[-1], [qw(.pm)], $dir);
}

sub locate_file {
    require Regexp::Trie;
    my ($name, $ext, $dirs) = @_;
    my @files;
    my $re;
    if ( $ext ) {
        $re = new Regexp::Trie;
        foreach ( @$ext ) {
            $re->add($_);
        }
        $re = $re->regexp;
    } else {
        $re = qr(($|\..*$));
    }
    $re = "^" . quotemeta($name) . $re;
    $re = qr($re);
    foreach my $d ( @$dirs ) {
        next unless -d $d;
        if ( substr($d, -1, 1) ne "/" ) {
            $d .= "/";
        }
        opendir(DIR, $d) or die "Can't open directory $d: $!";
        my @f = readdir(DIR);
        push @files, map { $d . $_ } grep { /$re/ } @f;
    }
    return \@files;
}

sub help {
    use Pod::Perldoc;
    my $kw = shift;
    if ( $kw =~ /::/ ) {
        my $file = locate_mod($kw);
        if ( @$file ) {
            @ARGV = ($kw);
        } else {
            warn "Can't find module $kw!\n";
        }
    } else {
        my $file = locate_mod($kw);
        if ( @$file ) {
            @ARGV = ($kw);
        } else {
            @ARGV = ("-f", $kw);
        }
    }
    if ( my $pid = fork ) {
        waitpid $pid, 0;
    } else {
        Pod::Perldoc->new('pagers'=>['cat'])->process;
        exit;
    }
}

package main;

use subs qw(dump help reload symtable);

{
    no warnings qw(all);
    *dump = \&Psh::Subs::dump;
    *x = \&Psh::Subs::dump;
    *help = \&Psh::Subs::help;
    *reload = \&Psh::Subs::reload;
    *symtable = \&Psh::Subs::symtable;
}

use Getopt::Long qw(:config no_ignore_case auto_help);

our $VERSION = v0.01;
our $REMOVE_MY = 0;
our $DEBUG = 0;
our $PROMPT = "perl> ";
my $start_up;

GetOptions(
    'debug' => \$DEBUG,
    'prompt=s' => \$PROMPT,
    'remove-my' => \$REMOVE_MY,
    'start-up=s' => \$start_up,
);

print "This is a simple perl shell!\n";

if ( defined $start_up && -f $start_up ) {
    print "Load $start_up...\n";
    require $start_up;
}

# Turn all buffering off.
select((select(STDOUT), $| = 1)[0]);
select((select(STDERR), $| = 1)[0]);
select((select(STDIN),  $| = 1)[0]);

while (1) {
    print $PROMPT;
    my $__exp__;
    my $__line__;
    while (1) {
        $__line__ = <STDIN>;
        unless (defined($__line__)) {
            Psh::Subs::quit();
        }
        chomp($__line__);
        $__line__ =~ s/^\s*my\s// if $REMOVE_MY;
        if ($__line__ =~ s/\\\s*$//) {
            print "+ ";
            $__exp__ .= $__line__ . "\n";
        } else {
            last;
        }
    }
    $__exp__ .= $__line__ . "\n";
    print "\nYou just input: $__exp__\n" if $DEBUG;
    if ($__exp__ =~ /^(quit|exit|bye)$/) {
        Psh::Subs::quit();
    } elsif ( $__exp__ =~ /^(help\s+|\?)(.*)\s*$/ ) {
        help($2);
    } else {
        # sub func {} => *func = sub {};
        Psh::Subs::nosub($__exp__);
        #     while ($__exp__ =~ /(?=\s?)sub\s+(\w+)/mg) {
        #     }
        print "Eval '$__exp__'\n" if $DEBUG;
        my $res = eval($__exp__);
        if ($@) {
            print "Error: $@\n";
        }
        print "\nResult: ", $res, "\n" if defined $res;
    }
}

__END__

=head1 NAME

psh.pl - A simple perl shell

=head1 SYNOPSIS

psh.pl

=head1 DESCRIPTION

psh.pl is intended to be a perl shell which is better than `perl -d`.
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

=item help

Intend for replace input `perldoc` in this program. for example:

   perl> help print
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

=item symtable

List all variables in package main.

=item quit

=back


=head1 AUTHOR

Ye Wenbin <wenbinye@gmail.com>

=head1 SEE ALSO

http://www.emacswiki.org/cgi-bin/emacs/inf-perl.el

=cut
