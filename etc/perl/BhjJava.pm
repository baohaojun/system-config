#!/usr/bin/perl

package BhjJava;
use Exporter;

our @ISA=("Exporter");
our @EXPORT = qw($id_re $qualified_re $connect_re %super_classes @keywords $keywords $keywords_re %keywords @modifiers $modifiers $modifier_re $debug $code_dir);
our $id_re = qr(\b[a-zA-Z_][a-zA-Z0-9_]*\b);
our $qualified_re = qr($id_re(?:\.$id_re)*\b);
our $connect_re = qr((?: |(?:\[\])+));
our %super_classes;

our @keywords = ("abstract", "assert", "boolean", "break", "byte",
              "case", "catch", "char", "class", "const", "continue",
              "default", "double", "else", "enum", "extends", "false",
              "final", "finally", "float", "for", "goto", "if",
              "implements", "import", "instanceof", "int",
              "interface", "long", "native", "new", "null", "package",
              "private", "protected", "public", "return", "short",
              "static", "strictfp", "super", "switch", "synchronized",
              "this", "throw", "throws", "transient", "true", "try",
              "void", "volatile", "while" );

our $keywords = join('|', @keywords);
our $keywords_re = qr(\b(?:$keywords)\b);
our %keywords;

map {$keywords{$_} = 1} @keywords;

our @modifiers = ('public', 'protected', 'private', 'static',
'abstract', 'final', 'native', 'synchronized', 'transient',
'volatile', 'strictfp');

our $modifiers = join('|', @modifiers);
our $modifier_re = qr($modifiers);

our $debug;
my $logfile = $0;
$logfile =~ s!.*/!!;
if ($ENV{DEBUG} eq 'true') {
    $debug = *STDERR;
} else {
    open($debug, ">", glob("~/.cache/system-config/logs/$logfile.log"))
        or die "Can not open debug log file ~/.cache/system-config/logs/$logfile.log";
}

our $code_dir = $ENV{PWD};
unless ($ENV{GTAGSROOT}) {
    chomp($code_dir = qx(find-code-reading-dir));

    $code_dir and chdir $code_dir or die "can not chdir $code_dir, you have not run mkgtags yet?";
    $ENV{GTAGSROOT} = $code_dir;
    $ENV{GTAGSDBPATH} = "$ENV{HOME}/.cache/for-code-reading$code_dir";
    $ENV{GTAGSLIBPATH} = join(":", glob("$ENV{GTAGSDBPATH}/.java-fallback.*"));
}

1;
