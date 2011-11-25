#!/usr/bin/perl

use strict;
use File::MimeInfo;
use File::Basename;
use String::ShellQuote;
my $tag_re = qr,\s*<\s*(/?\!?\w+)\s*((?:[-0-9a-zA-Z]+(?:\s*=\s*(?:(?:'(?:\\.|[^'])+')|(?:"(?:\\.|[^"])+")|(?:[^ \t>]+)))?\s*)*)\s*/?\s*>\s*,ms;
my $sub_re = qr,([-0-9a-zA-Z]+)(?:\s*=\s*((?:'(?:\\.|[^'])+')|(?:"(?:\\.|[^"])+")|(?:[^ \t>]+)))?\s*,ms;
my $start_dir;
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

sub relative_url($$) {
  my ($s, $t)  = @_;
  my @s = split("", $s);
  my @t = split("", $t);

  my $n = 0;
  my $last_slash = 0;
  while (@s and @t) {
    if ($s[0] eq $t[0]) {
      $n ++;
      $last_slash = $n if $s[0] eq "/";
      shift @s;
      shift @t;
    } else {
      last;
    }
  }

  my $relative_s = substr($s, $last_slash);
  my $remaining_t = substr($t, $last_slash);

  $remaining_t = grep(m:/:, split("", $remaining_t));
  $relative_s = "../" x $remaining_t . $relative_s;
  return $relative_s;
}

sub formal_path($) {
  my $path = shell_quote shift;
  chomp($path = qx(wlp $path));
  return $path;
}
  
sub locationFromPath($) {
  my $path = shift;
  return relative_url($path, $start_dir);
}

sub convertEqHex($) {
    my ($arg) = @_;
    $arg =~ s/(.)/if(ord($1) >= 0x80 or $1 eq "=") { sprintf("=%02X", ord($1)) } else { $1 }/eg;
    return $arg;
}

sub ProcessTagPath($$) {
  my ($dir, $arg) = @_;

  $arg = my_substr($arg, 1, length($arg)-1);
  if ($arg =~ m,://, and $arg !~ m,^file://,) {
    return $arg;
  }

  my $path = $arg;
  $path =~ s,^file://,,;
  if ($path =~ m,^/[a-z]:/,i) {
    $path = substr($path, 1)
  }

  my $path_url;
  if ($path =~ m,^/|^[a-zA-Z]:|^\\,) { #this is absolute path
    return $arg unless (-e $path);
    $path_url = locationFromPath($path);
  } else {			#relative path
    $path = "$dir/$path";
    $path = shell_quote($path);
	
    chomp($path = qx(readlink -f $path));
    return $arg unless -e $path;
    $path_url = locationFromPath($path);
  }
  if (not $path_set{$path}) {
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

  open(my $file, "<", $arg) or die "Can't open $arg";
  read($file, my $content, -s $arg) == -s $arg or die "Can't read $arg";
  my $dir = dirname($arg);
  ProcessHtmlContent($arg, $dir, $content);
}

sub ProcessHtmlContent($$$) {
  my ($arg, $dir, $content) = @_;

  print "\r\n\r\n$boundary_beg\r\nContent-Type: text/html; \r\nContent-Transfer-Encoding: quoted-printable\r\nContent-Location: "
    . locationFromPath($arg) . "\r\n\r\n";

  my $last_beg = 0;
  while ($content =~ m/$tag_re/g) {
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
  print "\r\n\r\n$boundary_beg\r\nContent-Type: " . mimetype($arg) . ";\r\nContent-Transfer-Encoding: base64\r\nContent-Location: "
    . locationFromPath($arg)  . "\r\n\r\n";
  
  $arg = shell_quote($arg);
  print qx(base64 $arg);
}

sub is_subdir($$) {
  my ($sub, $super) = @_;
  $super =~ s,(/|\\)*$,,;
  if (substr($sub, 0, length($super)) eq $super) {
    debug "is_subdir $sub $super is true";
    return 1;
  }
  debug "is_subdir $sub $super is false";
  return 0;
}

sub ProcessPath($) {
    my ($path) = @_;
    if (-f $path) {
	my $re_html = qr,\.[sp]?html?$,i;
	my $re_txt = qr,\.(?:c|cpp|h|hpp|cxx|hxx|txt|inl|ipp|css)$,i;
	if ($path =~ m/$re_html/) {
	    ProcessHtml($path);
	} else {
	    ProcessBinary($path);
	}
      } elsif (-d $path) {

	$path = formal_path($path);
	return unless is_subdir($path, $start_dir);
	debug "process dir $path";
	my $document = do {
	  local $/;
	  open my $fh, "-|", "dir2html", $path
	    or die "could not exec dir2html on $path: $!";
	  <$fh>;
	};
	ProcessHtmlContent($path, $path, $document);
      }
}

print "MIME-version: 1.0\r\nContent-Type: multipart/related;\r\n\tboundary=\"$boundary_stem\";\r\n\ttype=\"text/html\"\r\nX-MimeOLE: Produced By Microsoft MimeOLE V6.00.2800.1106\r\n\r\nThis is a multi-part message in MIME format.";
	
for my $arg (@ARGV) {
  if (-d $arg) {
    $start_dir = formal_path($arg);
  } else {
    $start_dir = formal_path(dirname($arg));
  }
  $path_set{formal_path $arg} = 2;
  push @path_set, formal_path($arg);

  ProcessPath($arg);

  while (1) {
    my $all_done = 1;
    for my $key (@path_set) {
      if ($path_set{$key} != 2) {
	debug "checking $key";
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
