#!/usr/bin/perl
use HTML::Parser ();

sub start($$) {
    my ($tagname, $attrs) = @_;
    if ($attrs->{name}) {
        $tagname = "$tagname " . $attrs->{name};
    }
    for (keys %$attrs) {
        print "$tagname: $_ = $attrs->{$_}\n";
    }
}

sub end($) {
}

 # Create parser object
 $p = HTML::Parser->new( api_version => 3,
                         start_h => [\&start, "tagname, attr"],
                         end_h   => [\&end,   "tagname"],
                         marked_sections => 1,
                       );

 # Parse document text chunk by chunk
 $p->parse($chunk1);
 $p->parse($chunk2);
 #...
 $p->eof;                 # signal end of document

 # Parse directly from file
 $p->parse_file("foo.html");
 # or
 open(my $fh, "<:utf8", "$ARGV[0]") || die;
 $p->parse_file($fh);
