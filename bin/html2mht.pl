#!/usr/bin/perl

use strict;
use File::Basename;
use String::ShellQuote;
my $tag_re = qr,\s*<\s*(/?\!?\w+)\s*((?:[-0-9a-zA-Z]+(?:\s*=\s*(?:(?:'(?:\\.|[^'])+')|(?:"(?:\\.|[^"])+")|(?:[^ \t>]+)))?\s*)*)\s*/?\s*>\s*,ms;
my $sub_re = qr,([-0-9a-zA-Z]+)(?:\s*=\s*((?:'(?:\\.|[^'])+')|(?:"(?:\\.|[^"])+")|(?:[^ \t>]+)))?\s*,ms;

my $boundary_stem="----=_NextPart_000_0076_01C29953.BE473C30";
my $boundary_beg = "--$boundary_stem";
my $boundary_end = "$boundary_beg--";

my %tag_attr_map = (
    "a" => "href",
    "link" => "href",
    "img" => "src",
    "frame" => "src",
    "body" => "background",
    );

my %path_set = ();
my @path_set = ();

sub debug(@) {
  print STDERR "@_\n";
}

sub locationFromPath($) {
  my ($path) = @_;
  my $path_url;
  $path = shell_quote($path);
  chomp($path = qx(wlp $path));
  if ($path =~ m,^/[^/],) { #unix absolute path
    $path_url = "file://$path";
  } else { #windows absolute path, give it extra '/'
    $path_url = "file:///$path";
  }
  return $path_url;
}

sub convertEqHex($) {
    my ($arg) = @_;
    $arg =~ s/(.)/if(ord($1) >= 0x80 or $1 eq "=") { sprintf("=%02X", ord($1)) } else { $1 }/eg;
    return $arg;
}

sub ProcessTagPath($$) {
  my ($dir, $arg) = @_;

  debug "$dir and $arg";
  if ($arg =~ m,://, and $arg !~ m,^file://,) {
    return $arg;
  }

  my $path = $arg;
  $path = substr($path, 1, length($path) - 2);
  $path =~ s,^file://,,;
  if ($path =~ m,^/[a-z]:/,i) {
    $path = my_substr($path, 1)
  }

  my $path_url;
  if ($path =~ m,^/|^[a-zA-Z]:|^\\,) { #this is absolute path
    debug "$path absolute";
    return $arg unless (-e $path);
    $path_url = locationFromPath($path);
  } else {			#relative path
    $path = "$dir/$path";
    debug "got absolute $path";
    $path = shell_quote($path);
	
    chomp($path = qx(readlink -f $path));
    return $arg unless -e $path;
    $path_url = locationFromPath($path);
  }
  if (not $path_set{path}) {
    $path_set{$path} = 1;
    push @path_set, $path;
  }
  return "'$path_url'";
}
sub my_substr($$$) {
  my ($str, $beg, $last) = @_;
  return substr($str, $beg, $last - $beg);
}

sub ProcessHtml($) {
    my ($arg) = @_;
    debug($arg);

    print "\r\n\r\n$boundary_beg\r\nContent-Type: text/html; \r\nContent-Transfer-Encoding: quoted-printable\r\nContent-Location: "
	. locationFromPath($arg) . "\r\n\r\n";

    open(my $file, "<", $arg) or die "Can't open $arg";
    read($file, my $content, -s $arg) == -s $arg or die "Can't read $arg";
    my $dir = dirname($arg);
    my $last_beg = 0;
    while ($content =~ m/$tag_re/g) {
      debug "matched $1 at " . $-[0];
      print convertEqHex(my_substr($content, $last_beg, $-[2]));
	my $save_print = my_substr($content, $+[2], $+[0]);
	$last_beg = $+[0];
	if ($tag_attr_map{lc($1)}) {
	    my $attrs = $2;
	    my $tag = $1;
	    my $last_sub_beg = 0;
	    while ($attrs =~ m/$sub_re/g) {
		print convertEqHex(my_substr($attrs, $last_sub_beg, $-[2]));
		my $save_print = my_substr($attrs, $+[2], $+[0]);
		$last_sub_beg = $+[0];
		if (lc($1) eq $tag_attr_map{$tag}) {
		    
		    print convertEqHex(ProcessTagPath($dir, $2));
		  } else {
		    print convertEqHex($2);
		  }
		print convertEqHex($save_print);
	    }
	  } else {
	    print convertEqHex($2);
	  }

	print convertEqHex($save_print);
    }
}	    

sub ProcessBinary($) {
  my ($arg) = @_;
  print "\r\n\r\n$boundary_beg\r\nContent-Type: application/octet-stream;\r\nContent-Transfer-Encoding: base64\r\nContent-Location: "
    . locationFromPath($arg)  . "\r\n\r\n";
  
  $arg = shell_quote($arg);
  print qx(base64 $arg);
}

sub ProcessTxt($) {
  my ($arg) = @_;
  print "\r\n\r\n$boundary_beg\r\nContent-Type: application/octet-stream;\r\nContent-Transfer-Encoding: text/plain\r\nContent-Location: "
    . locationFromPath($arg) . "\r\n\r\n";
  $arg = shell_quote($arg);
  print qx(cat $arg);
}

sub ProcessPath($) {
    my ($path) = @_;
    if (-f $path) {
	my $re_html = qr,\.[sp]?html?$,i;
	my $re_txt = qr,\.(?:c|cpp|h|hpp|cxx|hxx|txt|inl|ipp|css)$,i;
	if ($path =~ m/$re_html/) {
	    ProcessHtml($path);
	} elsif ($path =~ m/$re_txt/) {
	    ProcessTxt($path);
	} else {
	    ProcessBinary($path);
	}
    }
}

print "MIME-version: 1.0\r\nContent-Type: multipart/related;\r\n\tboundary=\"$boundary_stem\";\r\n\ttype=\"text/html\"\r\nX-MimeOLE: Produced By Microsoft MimeOLE V6.00.2800.1106\r\n\r\nThis is a multi-part message in MIME format.";
	
for my $arg (@ARGV) {
  ProcessPath($arg);
  while (1) {
    my $all_done = 1;
    for my $key (@path_set) {
      if ($path_set{$key} != 2) {
	ProcessPath($key);
	$path_set{$key} = 2;
	$all_done = 0;
      }
    }
    if ($all_done) {
      last;
    }
  }
}

print "\r\n$boundary_end\r\n";
