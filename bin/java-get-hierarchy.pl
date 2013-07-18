#!/usr/bin/perl
use strict;

chomp(my $code_dir = qx(find-code-reading-dir));
chdir $code_dir or die "can not chdir $code_dir";

$ENV{GTAGS_START_FILE} = "";
my $recursive = 1;
if ($ENV{DO_RECURSIVE_JAVA_HIERARCHY}) {
    $recursive = 0;
} else {
    unlink glob("~/.logs/java-get-hierarchy.log")
}

open(my $debug, ">>", glob("~/.logs/java-get-hierarchy.log"))
    or die "Can not open debug log file ~/.logs/java-get-hierarchy.log";
sub debug(@) {
    print $debug "@_\n";
}

debug "@ARGV";

$ENV{DO_RECURSIVE_JAVA_HIERARCHY} = 1;
use Getopt::Long;
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
    chomp($q_class = qx(java-get-qclass $q_class));
    debug "q_class for $ARGV[0] is $q_class";
    if ($q_class =~ m/\n/) {
        warn "$$ARGV[0] has multi q_class:\n$q_class\n";
    }
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

if ($flatten_cache) {
    $def_line = qx(grep -P -e '(?:class|interface).*\\b$class\\b.*\\{' $flatten_cache | grep -v '\\b$class\\b\\.');
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

my $id_re = qr(\b[a-zA-Z_][a-zA-Z0-9_]*\b);
my $qualified_re = qr($id_re(?:\.$id_re)*\b);

my @keywords = ("abstract", "assert", "boolean", "break", "byte",
              "case", "catch", "char", "class", "const", "continue", "default",
              "double", "else", "enum", "extends", "false", "final", "finally",
              "float", "for", "goto", "implements", "import", "instanceof", "int",
              "interface", "long", "native", "new", "null", "package", "private",
              "protected", "public", "return", "short", "static", "strictfp",
              "super", "switch", "synchronized", "this", "throw", "throws",
              "transient", "true", "try", "void", "volatile", "while"
    );

my $keywords = join('|', @keywords);
my $keywords_re = qr(\b(?:$keywords)\b);
my %keywords;

for my $key (@keywords) {
    $keywords{$key} = 1;
}

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
while ($tags =~ m/(?:class|interface): <(.*?)>/g) {
    map_it($1);
}

my %super_classes;
my %done_classes;

$super_classes{$q_class} = {};
$done_classes{$q_class} = 1;

# avoid infinite loop in below
$supers =~ s/<.*?>(\w)/ $1/g; # public class Preference implements Comparable<Preference>, OnDependencyChangeListener {
$supers =~ s/<.*?>//g; # public class Preference implements Comparable<Preference>, OnDependencyChangeListener {

while ($supers =~ m/($qualified_re)/g) {
    next if $keywords{$1};
    my $class = $1;
    my $out_class = $class;
    $out_class =~ s/\..*//;
    my $q_super;
    if ($simple_qualified_map{$out_class}) {
        $q_super = $simple_qualified_map{$out_class} . substr($class, length($out_class));
    } elsif (-e "$working_file_dir/$out_class.java" or -e "$working_file_dir/$out_class.aidl") {
        $q_super = "$package.$out_class" . substr($class, length($out_class));
    } elsif (-e "libcore/luni/src/main/java/java/lang/$out_class.java") {
        $q_super = "java.lang.$out_class" . substr($class, length($out_class));
    } else {
        if ($class =~ m/^[a-z].*\./) {
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

unless ($recursive) {
    print join(" ", keys $super_classes{$q_class});
    exit 0;
}

while (1) {
    my $done = 1;
    for my $q_super (sort keys %done_classes) {
        unless ($done_classes{$q_super}) {
            $done = 0;
            $done_classes{$q_super} = 1;
            my $supers = qx($0 $q_super);
            debug "supers is $supers";
            for (split(' ', $supers)) {
                $super_classes{$q_super}{$_} = 1;
                $done_classes{$_} = 0 unless $done_classes{$_};
            }
        }
    }
    last if $done;
}

sub print_hierarchy($$)
{
    my ($q_class, $indent) = @_;
    debug "print_hierarchy $q_class $indent";
    my $method_indent;
    if ($method or $verbose) {
        $method_indent = " " x ($indent * 3 + 3);
    }

    my $java_find_def = qx(java-find-def.pl -e $q_class -v);
    print " " x ($indent * 3 - 3) . "=> " . $java_find_def;

    if ($method) {
        my $q_class2 = (split(" ", $java_find_def))[1];
        if ($q_class2) {
            $q_class = $q_class2;
        }
        system("java-query-qmethod $q_class.$method|perl -npe 's/^/$method_indent/'");
    }
    system("java-get-members $q_class -p | perl -npe 's/^/$method_indent/'") if $verbose;
    if ($super_classes{$q_class}) {
        for (sort keys $super_classes{$q_class}) {
            print_hierarchy($_, $indent + 1);
        }
    }
}

if ($ENV{EMACS} eq 't') {
    chomp(my $pwd = qx(pwd));
    print "make: Entering directory \`$pwd'\n\n";
}
print_hierarchy($q_class, 1);
