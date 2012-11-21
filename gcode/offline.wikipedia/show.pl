#!/usr/bin/perl -w
use strict;
use String::ShellQuote;

sub LooseEntities {
    $_ = $_[0];
    s/&lt;/</g;
    s/&gt;/>/g;
    s/&apos;/'/g;
    s/&quot;/"/g;
    s/&amp;/\&/g;
    return $_;
}

chomp(my $mypath = `readlink -f $0`);
chomp(my $mydir = `dirname $mypath`);
our $lang = shift @ARGV or die "Error: must supply lang!";
our $article = $ARGV[0];

open(my $debug, ">", glob("~/.logs/show.pl.log"))
    or die "Can not open debug log file ~/.logs/show.pl.log";
sub debug(@) {
    print $debug "@_\n";
}


$ENV{"LANGOW"} = $lang;
sub ShowTopic {
    my $foundLine = shift;
    open (my $pipe, "-|", "python", "$mydir/get_article.py", $lang, @_);
    debug "Got args @ARGV";
    open RESULT, ">/tmp/ow.result.$$";
    my $line = 0;
    while(<$pipe>) {
	debug "pipe: $_";
	$line++;
	if ($line == 2) {
	    if (m/#REDIRECT \[\[(.*?)(#.*)?\]\]$/) {
		my $redirect = $1;
		$redirect =~ s/_/ /g;
		close RESULT;
		system("wiki-title-query $lang " . shell_quote($redirect) . " >/tmp/ow.result.$$");
		last;
	    }
	}
        print RESULT $_;
    }

    system("cd $mydir/mediawiki_sa/ && php5 testparser.php /tmp/ow.result.$$ > /tmp/ow.result.html.$$");
    if (($? == -1) || ($? & 127) || ($? >> 8)) {
	print "#### mediawiki_sa parser failed! report to woc.fslab.de ####\n";
	open FALLBACK, ">/tmp/ow.result.html.$$";
	print FALLBACK "<html><head><title>woc.fslab.de parser failed - report it!</title></head>\n<body>\n";
	print FALLBACK "<h2>woc.fslab.de parser failed, please report this content (/tmp/ow.result.$$) to them!</h2>";
	open DATA, "/tmp/ow.result.$$";
	while(<DATA>) {
	    chomp;
	    print FALLBACK $_."<BR/>\n";
	}
	close DATA;
	print FALLBACK "</body>\n</html>\n";
	close FALLBACK;
    }

    my $ext;
    if ($ENV{'WIKI_RELOAD_THUMB'}) {
        $ext = $ENV{'WIKI_RELOAD_THUMB'};
    } else {
        $ext = "";
    }

    open IN, "/tmp/ow.result.html.$$";
    while(<IN>) {
        s,<a href="/article/Zh:,<a href="/zh/article/,g;
        s,<a href="/article/En:,<a href="/en/article/,g;
        s,<a href="/article/,<a href="/$lang/article/,g;
	if (/<body/) {
	    print;
	    print <<EOF;
<script type="text/javascript">
function DoSearch(form)
{
top.location="/$lang/searchbar/?data="+form.data.value;
}
</script>
<form name="SearchForm" onSubmit="DoSearch(this.form)" method="get" action="/$lang/searchbar">
Search for
<input type="text" name="data" size="50">
<input type="button" value="Submit" onclick="DoSearch(this.form)">
<br>
<a href="http://$lang.wikipedia.org/wiki/$article"> http://$lang.wikipedia.org/wiki/$article </a>
<hr>
</form>
EOF
	    print "<H1>".$foundLine."</H1>\n";
	} else {
	    print ;
	}
    }
    close IN;
}

die "Usage: $0  Article prev_block_num prev_block_byte prev_block_bit block_num block_byte block_bit block start len\n"
    unless @ARGV == 10;

ShowTopic(@ARGV);
if ($ENV{'WIKI_RELOAD_THUMB'} eq "") {
    $ENV{'WIKI_RELOAD_THUMB'} = '.reload';
    system("reload_thumb.sh", "./show.pl", $lang, @ARGV);
}
