#!/usr/bin/perl
use CGI;

my $q = CGI->new;
print $q->header('text/plain');

$lightweight_fh  = $q->upload('pic');

# undef may be returned if it's not a valid file handle
if (defined $lightweight_fh) {
    # Upgrade the handle to one compatible with IO::Handle:
    my $io_handle = $lightweight_fh->handle;

    while ($bytesread = $io_handle->read($buffer,1024)) {
        print $buffer;
    }
} else {
    print "fh not there\n";
}
$q->save(\*STDOUT);

$filename = $q->param('pic');
print "$filename\n";

print "hello world\n";
