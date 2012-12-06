#!/usr/bin/perl
use strict;
open(my $debug, ">", glob("~/.logs/java-get-hierarchy.log"))
    or die "Can not open debug log file ~/.logs/java-get-hierarchy.log";
sub debug(@) {
    print $debug "@_\n";
}

my $recursive = 1;
if ($ENV{DO_RECURSIVE_JAVA_HIERARCHY}) {
    $recursive = 0;
}
$ENV{DO_RECURSIVE_JAVA_HIERARCHY} = 1;
use Getopt::Long;
my $method;
GetOptions(
    "m=s" => \$method,
    );

if (@ARGV != 1) {
    die "Usage: $0 Q_CLASS";
}

my $q_class = $ARGV[0];
chomp(my $working_file= qx(java-find-def.pl -e $q_class));
my $working_file_dir = $working_file;
$working_file_dir =~ s,(.*)/.*,$1,;

chomp(my $flatten_cache = qx(java-flatten-cache $working_file));
chomp(my $tags_cache = qx(java-tags-cache $working_file));

my $class = $q_class;
$class =~ s/.*\.//;

my $def_line = qx(grep -P -e '(?:class|interface).*\\b$class\\b.*\\{' $flatten_cache);

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
while ($def_line =~ m/(?:class|interface).*\b$class\b(?:<.*?>)?(.*)\{/g) {
    $supers = "$supers $1";
}

my $imports = qx(grep "^import " $flatten_cache);
chomp(my $package = qx(head -n 1 $flatten_cache));
$package =~ s/.* |;//g;

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

sub get_default_packages($)
{
    my $package = $_[0];
    return unless $package;
    open(my $pipe, "-|", "grep-gtags -e $package -t package -s -c")
	or die "can not open grep-gtags";

    while (<$pipe>) {
	m#/([^/]+)\.(?:java|aidl):.*# or next;
	map_it("$package.$1");
    }
    close($pipe);
}

sub get_wildcards($)
{ 
    my $import = $_[0];
    return 0 unless $import;
    chomp(my $file = qx(java-find-def.pl -e $import));
    -e $file or warn "$import definition not found";

    my $import_save = $import;
    $import =~ s/.*\.//;
    open(my $pipe, "-|", "global-ctags '$import..*' $file")
	or die "can not open global-ctags";
    while (<$pipe>) {
	my ($def) = split;
	$def =~ s/.*\.$import\.//;
	map_it("$import_save.$def");
    }
    close $pipe;
}

sub import_it($)
{
    my $q = $_[0];
    if ($q =~ m/\*$/) {
	$q =~ s/\.\*$//;
	#get_default_packages($q);
	#get_wildcards($q);
    } else {
	map_it($q);
    }
}
map {$_ and not $keywords{$_} and import_it($_)} split(/\s|;/, $imports);
my $tags = qx(cat $tags_cache);
while ($tags =~ m/(?:class|interface): <(.*?)>/g) {
    map_it($1);
}


#get_default_packages($package);
#get_default_packages("java.lang");
my %super_classes;
my %done_classes;

$super_classes{$q_class} = {};
$done_classes{$q_class} = 1;

while ($supers =~ m/($qualified_re)/g) {
    next if $keywords{$1};
    my $class = $1;
    my $prefix = $class;
    $prefix =~ s/\..*//;
    my $q_super;
    if ($simple_qualified_map{$prefix}) {
	$q_super = $simple_qualified_map{$prefix} . substr($class, length($prefix));
    } elsif (-e "$working_file_dir/$prefix.java" or -e "$working_file_dir/$prefix.aidl") {
	$q_super = "$package.$prefix".substr($class, length($prefix));
    } elsif (-e "libcore/luni/src/main/java/java/lang/$prefix.java") {
	$q_super = "java.lang.$prefix" . substr($class, length($prefix));
    } else {
	warn "super $class not resolved";
    }

    if ($q_super) {
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
    for my $q_super (keys %done_classes) {
	unless ($done_classes{$q_super}) {
	    $done = 0;
	    $done_classes{$q_super} = 1;
	    my $supers = qx($0 $q_super);
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
    my $method_indent;
    if ($method) {
	$method_indent = " " x ($indent * 3 + 3);
    }
	
    if (not $indent) {
	print "$q_class\n";
    } else {
	print " " x ($indent * 3 - 3) . "=> " . "$q_class\n";
    }

    system("java-query-qmethod $q_class.$method|perl -npe 's/^/$method_indent/'") if $method;
    if ($super_classes{$q_class}) {
	for (sort keys $super_classes{$q_class}) {
	    print_hierarchy($_, $indent + 1);
	}
    }
}

print_hierarchy($q_class, 0);
