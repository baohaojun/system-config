#!/usr/bin/perl
use CGI;
my $q = CGI->new;
print $q->header('text/plain');

system("id");
chdir "/log-crawler" or die "Can't chdir into /log-crawler";

my %params = $q->Vars;

my $phone_num = $params{phone_num};
delete $params{phone_num};

my $wifi_mac = $params{wifi_mac};
delete $params{wifi_mac};

sub my_die(@) {
    print "@_\n";
    exit -1;
}
if (not $wifi_mac) {
    my_die "can't proceed without a wifi mac";
}

my $boot_seq = $params{boot_seq};
delete $params{boot_seq};

if (not $boot_seq) {
    my_die "can't proceed without a boot seq number";
}

sub into_private_dir($$) {
    my ($dir, $desc) = @_;
    if (not -d $dir) {
        mkdir $dir or my_die "Can't mkdir with the $desc";
    }
    chdir $dir or my_die "Can't chdir with the $desc";
}

my $log_seq = $params{log_seq};
delete $params{log_seq};

into_private_dir $wifi_mac, "wifi mac";

my $boot_seq_dir = $boot_seq;
my $boot_seq_seq = 0;
while (1) {
    if (-d $boot_seq_dir) {
        $boot_seq_seq += 1;
        $boot_seq_dir = "$boot_seq.$boot_seq_seq";
    } else {
        last;
    }
}
into_private_dir $boot_seq_dir, "boot seq number";

if ($log_seq ne $boot_seq and $log_seq) {
    into_private_dir $log_seq, "old log seq number";
}

sub write_file($$) {
    my ($filename, $content) = @_;
    open(my $fh, ">", $filename) or my_die "Can't open $filename for writing";
    print $fh $content;
}

write_file "phone_num.txt", $phone_num;

for my $param (keys %params) {
    my $lightweight_fh  = $q->upload($param);

    if (not defined $lightweight_fh) {
        my_die "Unknown parameter $param, it is not a file";
    }
    # Upgrade the handle to one compatible with IO::Handle:
    my $io_handle = $lightweight_fh->handle;
    open(my $fh, ">", $param) or my_die "Can't open $param for writing";

    while ($bytesread = $io_handle->read(my $buffer,1024)) {
        print $fh $buffer;
    }
    close $fh;
}
print "Upload is complete!\n";
