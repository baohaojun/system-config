#!/usr/bin/perl
use strict;
use Getopt::Long;

use BhjJava;
sub debug(@) {
    print $debug "@_\n";
}

$ENV{GTAGS_START_FILE} = "";

my $should_recurse = 1;
if ($ENV{DO_RECURSIVE_JAVA_HIERARCHY}) {
    $should_recurse = 0;
}

debug "@ARGV";

$ENV{DO_RECURSIVE_JAVA_HIERARCHY} = 1;
my $method;
my $verbose;
GetOptions(
    "m=s" => \$method,
    "v!"  => \$verbose,
    );

if (@ARGV != 1) {
    die "Usage: $0 Q_CLASS";
}
debug "$0 @ARGV";

my $q_class = $ARGV[0];
if ($q_class !~ m/\./) {
    die "Will only work on qualified classes such as java.lang.String, not $q_class";
}

debug "q_class is $q_class";
chomp(my $working_file= qx(java-find-def.pl -e $q_class));
debug "working_file is $working_file for $q_class";
if (not $working_file) {
    die "No working file for $q_class";
}
my $working_file_dir = $working_file;
$working_file_dir =~ s,(.*)/.*,$1,;

chomp(my $flatten_cache = qx(java-flatten-cache $working_file));
debug "flatten_cache is $flatten_cache";
chomp(my $tags_cache = qx(java-tags-cache $working_file));
debug "tags_cache is $tags_cache";

my $class = $q_class;
$class =~ s/.*\.//;

my $def_line;

sub get_def_line_from_flatten_cache() {
    open(my $file, "<", $flatten_cache) or die "Can't open $flatten_cache";
    while (<$file>) {
        if (m/(?:class|interface).*\b\Q$class\E\b/ and not m/\b\Q$class\E\b\./) {
            close $file;
            return $_;
        }
    }
    close $file;
    return "Can't find def line in $flatten_cache for $class";
}

if ($flatten_cache) {
    $def_line = get_def_line_from_flatten_cache();
} else {
    $def_line = qx(grep-gtags -e '$class' -t 'class|interface' -s |
    perl -ne 'if (m/(?:class|interface) $q_class(?!\\\$)/) {
                  s/.*?> ://;
                  s/,/ /g;
                  print;
              }'|
    flatten.pl);
}
debug "def_line is $def_line";

my $supers = ' ';
if ($def_line =~ m/(class|interface).+?\b$class\b(?:<.*?>)?(.*)\{/) {
    $supers = "$supers $2";

    if ($1 eq "class") {
        my $super = $2;
        if ($super !~ m/\bextends\b/ and $class ne "Object" and $class ne "java.lang.Object") {
            $supers = "$supers java.lang.Object";
        }
    }
    debug "supers: $supers";
}

my $imports = qx(grep "^import " $flatten_cache < /dev/null);
chomp(my $package = qx(head -n 1 $flatten_cache < /dev/null));
$package =~ s/.* |;//g;
debug "package is $package from $flatten_cache";

my %defined_class;
my %simple_qualified_map;
sub map_it($)
{
    my $q = $_[0];
    my $class = $q;
    $class =~ s/.*\.//;
    $simple_qualified_map{$class} = $q;
    $defined_class{$q} = 1;
}

sub import_it($)
{
    my $q = $_[0];
    if ($q =~ m/\*$/) {
        $q =~ s/\.\*$//;
    } else {
        map_it($q);
    }
}
map {$_ and not $keywords{$_} and import_it($_)} split(/\s|;/, $imports);
my $tags = qx(cat $tags_cache /dev/null);
while ($tags =~ m/^(\S+)\s+(?:class|interface)/g) {
    map_it($1);
}

my %super_classes;
my %done_classes;

$super_classes{$q_class} = {};
$done_classes{$q_class} = 1;

# avoid infinite loop in below
$supers =~ s/<.*?>(\w)/ $1/g; # Xx entends Yy<X>implements Zz{
$supers =~ s/<.*?>//g; # Xx implements Yy<X>,Zz{

while ($supers =~ m/($qualified_re)/g) {
    next if $keywords{$1};
    my $class = $1;
    my $out_class = $class;
    $out_class =~ s/\..*//; # implements Xx.Yy
    my $q_super;
    if ($simple_qualified_map{$out_class}) {
        $q_super = $simple_qualified_map{$out_class} . substr($class, length($out_class));
    } elsif (-e "$working_file_dir/$out_class.java" or -e "$working_file_dir/$out_class.aidl") {
        $q_super = "$package.$out_class" . substr($class, length($out_class));
    } elsif (system("beatags -e java.lang.$out_class |grep -q java.lang.$out_class >/dev/null 2>&1") == 0) { # must use -x! 'cause it's my hacked tagsearch
        $q_super = "java.lang.$out_class" . substr($class, length($out_class));
    } else {
        if ($class =~ m/^[a-z].*\./) { # is a packaged class already
            $q_super = $class;
        } else {
            warn "super $class not resolved" if length($class) > 1;
        }
    }

    if ($q_super) {
        debug "q_super is $q_super";
        $super_classes{$q_class}{$q_super} = 1;
        $done_classes{$q_super} = 0 unless $done_classes{$q_super};
    }
}

unless ($should_recurse) {
    print join(" ", keys %{$super_classes{$q_class}});
    exit 0;
}

use DelayedQx;

while (1) {
    my $done = 1;
    for my $q_super (sort keys %done_classes) {
        if (not $done_classes{$q_super}) {
            $done = 0;
            $done_classes{$q_super} = DelayedQx->new($0, $q_super);
        } elsif (ref $done_classes{$q_super}) {
            $done = 0;
            my $supers = $done_classes{$q_super}->value();
            debug "supers is $supers for $q_super";
            for (split(' ', $supers)) {
                $super_classes{$q_super}{$_} = 1;
                $done_classes{$_} = DelayedQx->new($0, $_) unless $done_classes{$_};
            }
            $done_classes{$q_super} = 1;
        }
    }
    last if $done;
}

my %qclass_defs;
my %method_prototypes;
my %qclass_members;

sub print_hierarchy($$)
{
    my ($q_class, $indent) = @_;
    debug "print_hierarchy $q_class $indent";
    my $method_indent;
    if ($method or $verbose) {
        $method_indent = " " x ($indent * 3 + 3);
    }

    my $java_find_def = $qclass_defs{$q_class}->value();
    print " " x ($indent * 3 - 3) . "=> " . $java_find_def;

    if ($method) {
        for my $method_proto (split("\n", $method_prototypes{$q_class.$method}->value())) {
            $method_proto =~ s/^/$method_indent/;
            print "$method_proto\n";
        }
    }

    if ($verbose) {
        for my $member (split("\n", $qclass_members{$q_class}->value())) {
            $member =~ s/^/$method_indent/;
            print "$member\n";
        }
    }
    if ($super_classes{$q_class}) {
        for (sort keys %{$super_classes{$q_class}}) {
            print_hierarchy($_, $indent + 1);
        }
    }
}

if ($ENV{EMACS} eq 't') {
    chomp(my $pwd = qx(pwd));
    print "make: Entering directory \`$pwd'\n\n";
}

for (keys %done_classes) {
    debug "done class: $_";
    $qclass_defs{$_} = DelayedQx->new("java-find-def.pl -e $_ -v");
    if ($method) {
        $method_prototypes{$_.$method} = DelayedQx->new("java-query-qmethod $_.$method|sort -u");
    }
    if ($verbose) {
        $qclass_members{$_} = DelayedQx->new("ajoke-get-members $_ -p");
    }
}

print_hierarchy($q_class, 1);
