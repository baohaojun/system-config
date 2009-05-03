#! /usr/bin/perl -w
use File::Find;
use Data::Dumper qw(Dumper);
use Text::Wrap qw(wrap);
if ( @ARGV ) {
    my $file = shift;
    open(STDOUT, ">", $file) or die "Can't create $file: $!";
}
my $fn = build_function();
print <<'EL';
(setq perldoc-obarray (make-vector 1519 nil))
;; Functions
(mapc (lambda (func)
         (set (intern func perldoc-obarray) t))
'(
EL
my $i = 1;
print wrap('', '', join(' ', map {qq("$_")} sort keys %$fn )), "))\n\n";

print <<'EL';
;; Modules
(mapc (lambda (mod)
         (intern mod perldoc-obarray))
'(
EL
my $mod = build_modules();
print wrap('', '', join(' ', map {qq("$_) . (exists $fn->{$_} ? ".pod" : "") . '"'} sort keys %$mod )), "))\n";

sub build_modules {
    my %mod;
    for my $dir ( @INC ) {
        next if $dir eq '.';
        next unless -d $dir;
        my $len = length($dir)+1;
        find( { wanted => sub {
                    if ( -f $_ && /\.(pm|pod)$/i ) {
                        my $mod = substr($File::Find::name, $len);
                        $mod =~ s#^[pP]od/(?=a2p|perl)##;
                        $mod =~ s/.(pm|pod)$//;
                        $mod =~ s#/#::#g;
                        $mod{$mod}++;
                    }
                },
                follow => 1
            }, $dir);
    }
    return \%mod;
}

sub build_function {
    chomp(my $file = `perldoc -l perlfunc`);
    my %fn;
    open(FH, $file) or die "Can't open file $file: $!";
    while ( <FH> ) {
        last if /^=head2 Alphabetical/;
    }
    while ( <FH> ) {
        last if /^=over/;
    }
    my $stat = 1;
    while ( <FH> ) {
        if ( /^=item/ ) {
            if ( $stat ) {
                my $fn = (split /\s+/, $_)[1];
                $fn =~ s#/.*$##;  #  y///, m// and so on
                $fn =~ s/\(.*$//; # chomp(, chop(
                $fn{$fn}++;
            }
        } elsif ( /^=over/ ) {
            $stat = 0;
        } elsif ( /^=back/ ) {
            $stat = 1;
        }
    }
    map { $fn{'-'.$_}++ } qw/A B C M O R S T W X b c d e f g k l o p r s t u w x z/;
    return \%fn;
}
